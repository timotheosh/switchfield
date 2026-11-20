(ns selfdidactic.runner
  "Run one pipeline: the actions around the calculations in
  selfdidactic.pipeline (lock, run record, processes, git)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [selfdidactic.git :as git]
            [selfdidactic.harness :as harness]
            [selfdidactic.pipeline :as pipeline]
            [selfdidactic.plugins :as plugins]
            [selfdidactic.progress :as progress])
  (:import [java.io File IOException]
           [java.net InetAddress]
           [java.nio.file CopyOption FileAlreadyExistsException Files LinkOption
            OpenOption StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute FileAttribute]
           [java.security MessageDigest]
           [java.time Instant ZoneOffset ZonedDateTime]
           [java.time.format DateTimeFormatter]))

;; -- Small actions ------------------------------------------------------

(defn- hostname [] (.getHostName (InetAddress/getLocalHost)))

(defn- pid-alive? [pid] (.isPresent (java.lang.ProcessHandle/of (long pid))))

(defn- sha256 [^String s]
  (apply str (map #(format "%02x" %)
                  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))

(defn- generate-run-id []
  (str (.format (DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmssSSS'Z'")
                (ZonedDateTime/now ZoneOffset/UTC))
       "-" (format "%06x" (rand-int 0xffffff))))

(defn- append!
  "Append one entry to the run record. Only the runner writes it."
  [^File record entry]
  (spit record (prn-str entry) :append true))

;; -- Lock ---------------------------------------------------------------

(defn acquire-lock!
  "Create the run lock atomically; {:lock file} or {:problem ...}. A held lock
  is never cleared unless force? is set."
  [dir force?]
  (let [f    (io/file dir "lock")
        host (hostname)
        data {:pid (.pid (java.lang.ProcessHandle/current)) :host host
              :since (str (Instant/now))}]
    (when force? (.delete f))
    (try
      (Files/writeString (.toPath f) (pr-str data)
                         (into-array OpenOption [StandardOpenOption/CREATE_NEW
                                                 StandardOpenOption/WRITE]))
      {:lock f}
      (catch FileAlreadyExistsException _
        (let [held (try (edn/read-string (slurp f)) (catch Exception _ {}))]
          {:problem (pipeline/lock-problem
                     held host (and (map? held) (:pid held) (pid-alive? (:pid held))))})))))

;; -- Writes directories -------------------------------------------------

(defn inspect-writes
  "Read what exists on disk for each prefix: {prefix :dir|:file|:missing}."
  [root writes]
  (into (sorted-map)
        (for [p writes :let [f (io/file root p)]]
          [p (cond (.isDirectory f) :dir (.exists f) :file :else :missing)])))

(defn ensure-dirs!
  "Create each missing writes directory; return the prefixes created. Git
  cannot see an empty directory, so this also restores one an agent removed
  or a rollback pruned."
  [root writes]
  (vec (for [p writes :let [f (io/file root p)] :when (not (.isDirectory f))]
         (do (.mkdirs f)
             (when-not (.isDirectory f)
               (throw (ex-info (str "cannot create writes directory " p)
                               {:kind :writes-dir :prefix p})))
             p))))

;; -- Prompts ------------------------------------------------------------

(defn read-prompts
  "Read every prompt the pipeline uses: {prompt-name {:text s} | {:error s}}.
  Paths are relative to the timetable directory."
  [dir timetable pipeline-name]
  (into {}
        (for [p (distinct (keep :prompt (get-in timetable [:pipelines pipeline-name :steps])))
              :let [path (get-in timetable [:prompts p :path])] :when path
              :let [f (io/file dir path)]]
          [p (if (.isFile f)
               {:text (slurp f)}
               {:error (str "not a readable file: " f)})])))

;; -- Worktree -----------------------------------------------------------

(def ^:private state-ignore-lines
  "The volatile parts of .switchyard/ (it is otherwise tracked, so the
  timetable can be committed): run worktrees, locks and the reserved db/."
  ["worktrees/" "*/lock" "*/db/"])

(defn- worktree-path [root eb run-id]
  (str root "/.switchyard/" pipeline/worktrees-dir "/" (pipeline/worktree-name eb run-id)))

(defn- worktree-facts
  "What exists for a run's worktree, as data for the problem check."
  [root eb run-id]
  (let [path (worktree-path root eb run-id)]
    {:registered?    (git/worktree-registered? root path)
     :dir-exists?    (.exists (io/file path))
     :branch-exists? (git/branch-exists? root (pipeline/worktree-branch eb run-id))
     :ref-conflicts  (vec (filter #(git/branch-exists? root %)
                                  ["switchyard" (str "switchyard/" eb)]))}))

(defn ensure-state-gitignore!
  "Make sure .switchyard/.gitignore ignores the volatile state, keeping any
  lines already there."
  [root]
  (let [f        (io/file root ".switchyard" ".gitignore")
        existing (if (.exists f) (str/split-lines (slurp f)) [])
        missing  (remove (set existing) state-ignore-lines)]
    (when (seq missing)
      (spit f (str (when (seq existing) (str (str/join "\n" existing) "\n"))
                   (str/join "\n" missing) "\n")))))

(defn- setup-worktree!
  "Create the run's worktree on a new branch, or re-use the registered one."
  [{:keys [root eb run-id]}]
  (let [path    (worktree-path root eb run-id)
        branch  (pipeline/worktree-branch eb run-id)
        reused? (git/worktree-registered? root path)]
    (when-not reused? (git/worktree-add! root path branch))
    {:path path :branch branch :run-id run-id :reused? reused?}))

;; -- Steps --------------------------------------------------------------

(defn write-agent-profile!
  "Write a plugin's native profile file under work-dir; returns its path."
  [work-dir {:keys [directory suffix contents id]}]
  (let [no-follow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])
        attributes (make-array FileAttribute 0)
        dir (reduce (fn [parent segment]
                      (let [path (.resolve parent ^String segment)]
                        (try
                          (Files/createDirectory path attributes)
                          (catch FileAlreadyExistsException _
                            (when-not (Files/isDirectory path no-follow)
                              (throw (IOException. (str "Not a profile directory: " path))))))
                        path))
                    (.toPath (io/file work-dir)) directory)
        path (.resolve dir (str id suffix))]
    (when (and (Files/exists path no-follow)
               (not (Files/isRegularFile path no-follow)))
      (throw (IOException. (str "Not a regular profile file: " path))))
    (let [temp (Files/createTempFile dir ".switchyard-" ".tmp" attributes)]
      (try
        (Files/writeString temp contents (make-array OpenOption 0))
        (Files/move temp path (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                                     StandardCopyOption/REPLACE_EXISTING]))
        (finally
          (Files/deleteIfExists temp))))
    (str path)))

(defn generate-agent-profiles!
  "Write the profile of every agent the pipeline uses into root, without
  running anything. Returns the written paths (empty when the plugin has no
  profiles). ctx: :config :timetable :pipeline-name :root, and optionally
  :overwrite? (fn [path] bool), asked only for files that already exist;
  default overwrites everything. Skipped files are left out of the result."
  [{:keys [config timetable pipeline-name root overwrite?] :or {overwrite? (constantly true)}}]
  (when-not (get-in timetable [:pipelines pipeline-name])
    (throw (ex-info (str "unknown pipeline: " (name pipeline-name)) {})))
  (vec (for [id (distinct (keep :agent (get-in timetable [:pipelines pipeline-name :steps])))
             :let [agent   (get-in timetable [:agents id])
                   harness (get-in timetable [:harnesses (:harness agent)])
                   profile (:agent-profile (plugins/step-command! config harness id agent "-"))]
             :when profile
             :let [{:keys [directory suffix id]} profile
                   target (str (apply io/file root (conj (vec directory) (str id suffix))))]
             :when (or (not (.exists (io/file target))) (overwrite? target))]
         (write-agent-profile! root profile))))

(defn- write-step-log!
  "Save a step's full captured output under the run's directory; its path."
  [dir run-id i text]
  (let [f (io/file dir "runs" run-id (str "step-" i ".log"))]
    (io/make-parents f)
    (spit f text)
    (str f)))

(defn- run-attempt!
  "One attempt: pre-snapshot, agent, post-snapshot, violations, restore when
  needed, then append the attempt to the record. The record is written after
  the restore so a tampering agent cannot roll back the runner's own entry.
  The agent runs in work-dir (a run worktree, or the project root); in a
  worktree run the runner's state and the main checkout are watched too."
  [{:keys [root dir run-id work-dir eb timetable writes worktree?] :as ctx} i step n context record]
  (let [agent (get-in timetable [:agents (:agent step)])
        harness (get-in timetable [:harnesses (:harness agent)])
        cmd     (when (map? (:command step)) (:command step))
        capture (when (pipeline/stream? (:artifact cmd)) (:artifact cmd))
        artifact (when (string? (:artifact cmd)) (:artifact cmd))
        {:keys [command problem]} (if (pipeline/command-step? step)
                                    {:command {:argv (get-in step [:command :args])}}
                                    (pipeline/step-command ctx i step context))
        launch-error (or (:message problem)
                         (when-let [profile (:agent-profile command)]
                           (try
                             (write-agent-profile! work-dir profile)
                             nil
                             (catch IOException e
                               (str "Cannot write agent profile: " (.getMessage e))))))
        watch   #(when worktree? (git/watch-snapshot root eb))
        before  (git/snapshot work-dir)
        wbefore (watch)
        gbefore (git/git-dir-hashes root)
        run (if launch-error
              {:process :harness-error :exit-code nil :out "" :err launch-error}
              (harness/start (:argv command) work-dir
                             {:stdin (:stdin command)
                              :env {"GIT_DIR" "/nonexistent"}
                              :merge-err? (= :both capture)
                              :exit-reasons (when harness
                                              (plugins/exit-reasons (:config ctx) harness))
                              :timeout-ms (or (:timeout-ms step) (:timeout-ms agent)
                                              pipeline/default-timeout-ms)}))
        missing-artifact (when (and artifact (= :ok (:process run))
                                    (not (.exists (io/file work-dir artifact))))
                           artifact)
        log     (when (and capture (not launch-error))
                  (write-step-log! dir run-id i (pipeline/capture-text capture run)))
        after   (git/snapshot work-dir)
        wafter  (watch)
        gafter  (git/git-dir-hashes root)
        changed (git/changed-paths work-dir before after)
        tree-changes (fn [k] (when worktree?
                               (git/changed-paths root {:tree (k wbefore)} {:tree (k wafter)})))
        state-changed (tree-changes :state)
        parsed (pipeline/parse-agent-output (:out run))
        violations (into (pipeline/step-violations agent before after changed parsed)
                         (pipeline/watcher-violations
                          {:state-changed state-changed
                           :main-changed (tree-changes :main)
                           :git-dir-before gbefore :git-dir-after gafter}))
        retryable? (boolean (some (complement pipeline/stop-violation?) violations))
        _ (when retryable?
            (git/restore! work-dir before after)
            (when (seq state-changed)
              (git/restore-tree! root (:state wbefore) (:state wafter))))
        recreated (ensure-dirs! work-dir writes)
        entry (merge run
                     {:entry :attempt :step i :agent (pipeline/step-label step) :attempt n
                      :argv (:argv command) :stdin (:stdin command)
                      :before before :after after :changed changed
                      :writes-trees {:before (git/dir-trees work-dir before writes)
                                     :after  (git/dir-trees work-dir after writes)}
                      :recreated-dirs recreated
                      :missing-artifact missing-artifact
                      :log log
                      :verdict (pipeline/step-verdict step (assoc run :missing-artifact missing-artifact) parsed)
                      :violations violations
                      :restored? retryable?
                      :stopped? (boolean (some pipeline/stop-violation? violations))
                      :needs-outside-jurisdiction
                      (get-in parsed [:output :needs-outside-jurisdiction] [])})]
    (append! record entry)
    entry))

(defn- run-step!
  "Run a step, retrying it after a rollback while violations persist and the
  agent's retry bound allows. A violation outside the worktree stops the run
  at once. `context` is the first attempt's context (a verdict that redirected
  the run here). Returns the last attempt with :step-status, :redirect when
  the step's verdict sends the run back."
  [ctx i step record context]
  (let [agent (get-in (:timetable ctx) [:agents (:agent step)])
        bound (cond (pipeline/command-step? step) 0 ; nothing to tell a command
                    :else (or (:max-violation-retries agent)
                              pipeline/default-max-violation-retries))]
    (loop [n 0 context context]
      (let [entry (run-attempt! ctx i step n context record)]
        (cond
          (:stopped? entry)
          (assoc entry :step-status :violations)

          (empty? (:violations entry))
          (assoc entry :step-status
                 (cond
                   (and (pipeline/verdict-trips? step (:verdict entry))
                        (or (pipeline/command-step? step) (pipeline/step-continues? entry)))
                   :redirect
                   (pipeline/step-continues? entry) :ok
                   :else :step-failed))

          (< n bound)
          (recur (inc n) {:violations (:violations entry) :writes (:writes agent)})

          :else
          (assoc entry :step-status :violations))))))

(defn- announce!
  "Log a stage event at info, keeping the progress line clear of the log output
  and redrawing it afterwards when there is an event to show."
  [{:keys [progress]} message event]
  (let [{:keys [clear! draw!]} (or progress progress/silent)]
    (clear!)
    (log/info message)
    (when event (draw! event))))

(defn- run-stage!
  "Run step i, announcing its start and end."
  [ctx i step record total context]
  (let [agent (pipeline/step-label step)
        event #(hash-map :index i :total total :agent agent :phase %)]
    (announce! ctx (pipeline/stage-started-message i total agent) (event :running))
    (let [entry (run-step! ctx i step record context)
          status (:step-status entry)]
      (announce! ctx (pipeline/stage-finished-message i total agent status)
                 (when (= :ok status) (event :done)))
      entry)))

(defn- finish!
  [record status steps step-index worktree]
  (append! record (cond-> {:entry :terminal :status status :step step-index}
                    worktree (assoc :worktree worktree)))
  (cond-> {:status status :step step-index :steps steps :record (str record)}
    worktree (assoc :worktree worktree)))

(defn- execute!
  [{:keys [dir root timetable pipeline-name spec writes run-id worktree?] :as ctx}]
  (let [wt       (when worktree? (setup-worktree! ctx))
        _        (when wt (ensure-state-gitignore! root))
        work-dir (if wt (:path wt) root)
        ctx      (assoc ctx :work-dir work-dir)
        created  (ensure-dirs! work-dir writes)
        record   (io/file dir "runs" (str run-id ".edn"))
        steps    (get-in timetable [:pipelines pipeline-name :steps])]
    (io/make-parents record)
    (append! record (cond-> {:entry :header :pipeline pipeline-name
                             :writes writes :created-dirs created
                             :spec-path (:path spec)
                             :spec-sha256 (sha256 (:contents spec))
                             :timetable-sha256 (sha256 (pr-str (pipeline/canonical timetable)))}
                      wt (assoc :worktree wt)
                      (:reused? wt) (assoc :resumed? true)))
    (loop [i 0 done [] redirects {} context {}]
      (if (= i (count steps))
        (finish! record :success done nil wt)
        (let [step  (nth steps i)
              entry (run-stage! ctx i step record (count steps) context)
              done  (conj done entry)]
          (case (:step-status entry)
            :ok (recur (inc i) done redirects {})
            :redirect
            (let [used   (get redirects i 0)
                  target (pipeline/redirect-target steps i step)]
              (if (>= used (pipeline/redirect-max step))
                (finish! record :review-failed done i wt)
                (do (append! record {:entry :redirect :from i :to target :count (inc used)})
                    (recur target done (assoc redirects i (inc used))
                           {:verdict (:verdict entry) :from (pipeline/step-label step)}))))
            (finish! record (:step-status entry) done i wt)))))))

(defn run-pipeline!
  "Run the named pipeline sequentially. ctx: :config :timetable
  :pipeline-name :spec {:path :contents} :root (repo top level) :branch
  :dir (the branch's timetable directory) :force-unlock?, and for worktree
  runs :worktree? :run-id (optional; generated when absent) :allow-dirty?.
  Returns {:status :success|:step-failed|:violations ...} or
  {:status :problems :problems [...]} before anything has run."
  [{:keys [dir root branch timetable pipeline-name force-unlock?] :as ctx}]
  (let [writes   (pipeline/writes-list timetable pipeline-name)
        run-id   (or (:run-id ctx) (generate-run-id))
        eb       (when branch (pipeline/encode-branch branch))
        probe?   (and (:worktree? ctx) root eb (pipeline/valid-run-id? run-id))
        ctx      (assoc ctx
                        :writes writes :run-id run-id :eb eb
                        :writes-fs (if root (inspect-writes root writes) {})
                        :prompt-texts (if dir (read-prompts dir timetable pipeline-name) {})
                        :dirty-paths (when probe? (git/dirty-paths root))
                        :worktree-facts (when probe? (worktree-facts root eb run-id)))
        problems (pipeline/pipeline-problems ctx)]
    (if (seq problems)
      {:status :problems :problems problems}
      (let [{:keys [lock problem]} (acquire-lock! dir force-unlock?)]
        (if problem
          {:status :problems :problems [problem]}
          (try (execute! ctx)
               (finally (.delete ^File lock))))))))
