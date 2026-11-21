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
    (.mkdirs (io/file dir))                 ; a branch may have no overrides
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
  Paths are relative to the timetable directories (lowest precedence first);
  the last directory that has the file wins."
  [dirs timetable pipeline-name]
  (into {}
        (for [p (distinct (keep :prompt (get-in timetable [:pipelines pipeline-name :steps])))
              :let [path (get-in timetable [:prompts p :path])] :when path
              :let [f (or (->> dirs (map #(io/file % path)) (filter #(.isFile %)) last)
                          (io/file (last dirs) path))]]
          [p (if (.isFile f)
               {:text (slurp f)}
               {:error (str "not a readable file: " f)})])))

;; -- Worktree -----------------------------------------------------------

(def ^:private state-ignore-lines
  "The volatile parts of .switchfield/ (it is otherwise tracked, so the
  timetable can be committed): run worktrees, locks, the reserved db/ and logs/."
  ["worktrees/" "*/lock" "*/db/" "*/logs/"])

(defn- worktree-path [root eb run-id]
  (str root "/.switchfield/" pipeline/worktrees-dir "/" (pipeline/worktree-name eb run-id)))

(defn- worktree-facts
  "What exists for a run's worktree, as data for the problem check."
  [root eb run-id]
  (let [path (worktree-path root eb run-id)]
    {:registered?    (git/worktree-registered? root path)
     :dir-exists?    (.exists (io/file path))
     :branch-exists? (git/branch-exists? root (pipeline/worktree-branch eb run-id))
     :ref-conflicts  (vec (filter #(git/branch-exists? root %)
                                  ["switchfield" (str "switchfield/" eb)]))}))

(defn ensure-state-gitignore!
  "Make sure .switchfield/.gitignore ignores the volatile state, keeping any
  lines already there."
  [root]
  (let [f        (io/file root ".switchfield" ".gitignore")
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
    (let [temp (Files/createTempFile dir ".switchfield-" ".tmp" attributes)]
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
  "Save a step's full captured output under the branch's logs/ directory,
  which the checks never look at; its path."
  [dir run-id i text]
  (let [f (io/file dir "logs" run-id (str "step-" i ".log"))]
    (io/make-parents f)
    (spit f text)
    (str f)))

(defn- prepare-member!
  "Build a member's command and write its agent profile. This happens before
  the baseline snapshot, so the profile is not mistaken for a change. A command
  step runs its own argv; a problem or an unwritable profile is a :launch-error."
  [{:keys [work-dir timetable] :as ctx} [i step] context]
  (let [agent (get-in timetable [:agents (:agent step)])
        cmd   (when (map? (:command step)) (:command step))
        {:keys [command problem]} (if (pipeline/command-step? step)
                                    {:command {:argv (:args cmd)}}
                                    (pipeline/step-command ctx i step context))]
    {:i i :step step :agent agent :command command
     :harness (get-in timetable [:harnesses (:harness agent)])
     :capture (when (pipeline/stream? (:artifact cmd)) (:artifact cmd))
     :artifact (when (string? (:artifact cmd)) (:artifact cmd))
     :launch-error (or (:message problem)
                       (when-let [profile (:agent-profile command)]
                         (try
                           (write-agent-profile! work-dir profile)
                           nil
                           (catch IOException e
                             (str "Cannot write agent profile: " (.getMessage e))))))}))

(defn- launch-member!
  "Run a prepared member's process in work-dir; its outcome."
  [{:keys [work-dir config]} {:keys [step agent harness command launch-error capture]}]
  (if launch-error
    {:process :harness-error :exit-code nil :out "" :err launch-error}
    (harness/start (:argv command) work-dir
                   {:stdin (:stdin command)
                    :env {"GIT_DIR" "/nonexistent"}
                    :merge-err? (= :both capture)
                    :exit-reasons (when harness (plugins/exit-reasons config harness))
                    :timeout-ms (or (:timeout-ms step) (:timeout-ms agent)
                                    pipeline/default-timeout-ms)})))

(defn- launch-all!
  "Run every member's process, together when there is more than one; their
  outcomes, in member order."
  [ctx plans]
  (if (= 1 (count plans))
    [(launch-member! ctx (first plans))]
    (->> plans (mapv #(future (launch-member! ctx %))) (mapv deref))))

(defn- run-attempt!
  "One attempt of a group (one step, or concurrent steps): pre-snapshot, the
  agents, post-snapshot, violations, restore when needed, then append each
  member's attempt to the record. The record is written after the restore so a
  tampering agent cannot roll back the runner's own entry. The agents run in
  work-dir (a run worktree, or the project root); in a worktree run the
  runner's state and the main checkout are watched too. Concurrent members
  share one snapshot pair: a change cannot be attributed to one of them, so
  each is held to its own :writes and all are rolled back together. Returns
  the entries, in member order."
  [{:keys [root dir run-id work-dir eb writes worktree?] :as ctx} members n contexts record]
  (let [plans   (mapv #(prepare-member! ctx %1 %2) members contexts)
        watch   #(when worktree? (git/watch-snapshot root eb))
        before  (git/snapshot work-dir)
        wbefore (watch)
        gbefore (git/git-dir-hashes root)
        runs    (launch-all! ctx plans)
        after   (git/snapshot work-dir)
        wafter  (watch)
        gafter  (git/git-dir-hashes root)
        changed (git/changed-paths work-dir before after)
        tree-changes (fn [k] (when worktree?
                               (git/changed-paths root {:tree (k wbefore)} {:tree (k wafter)})))
        state-changed (tree-changes :state)
        watcher (pipeline/watcher-violations
                 {:state-changed state-changed
                  :main-changed (tree-changes :main)
                  :git-dir-before gbefore :git-dir-after gafter})
        judged  (mapv (fn [{:keys [agent artifact]} run]
                        (let [parsed (pipeline/parse-agent-output (:out run))]
                          {:parsed parsed
                           :missing-artifact (when (and artifact (= :ok (:process run))
                                                        (not (.exists (io/file work-dir artifact))))
                                               artifact)
                           :violations (into (pipeline/step-violations agent before after changed parsed)
                                             watcher)}))
                      plans runs)
        retryable? (boolean (some (fn [{:keys [violations]}]
                                    (some (complement pipeline/stop-violation?) violations))
                                  judged))
        _ (when retryable?
            (git/restore! work-dir before after)
            (when (seq state-changed)
              (git/restore-tree! root (:state wbefore) (:state wafter))))
        recreated (ensure-dirs! work-dir writes)
        trees   {:before (git/dir-trees work-dir before writes)
                 :after  (git/dir-trees work-dir after writes)}
        entries (mapv
                 (fn [{:keys [i step command launch-error capture]} run
                      {:keys [parsed missing-artifact violations]}]
                   (merge run
                          {:entry :attempt :step i :agent (pipeline/step-label step) :attempt n
                           :argv (:argv command) :stdin (:stdin command)
                           :before before :after after :changed changed
                           :writes-trees trees
                           :recreated-dirs recreated
                           :missing-artifact missing-artifact
                           :log (when (and capture (not launch-error))
                                  (write-step-log! dir run-id i (pipeline/capture-text capture run)))
                           :verdict (pipeline/step-verdict step (assoc run :missing-artifact missing-artifact) parsed)
                           :violations violations
                           :restored? retryable?
                           :stopped? (boolean (some pipeline/stop-violation? violations))
                           :needs-outside-jurisdiction
                           (get-in parsed [:output :needs-outside-jurisdiction] [])}))
                 plans runs judged)]
    (run! #(append! record %) entries)
    entries))

(defn- entry-status
  "Calculate how one attempt ended: :violations, :redirect, :ok or :step-failed."
  [step entry]
  (cond
    (seq (:violations entry))
    :violations

    (and (pipeline/verdict-trips? step (:verdict entry))
         (or (pipeline/command-step? step) (pipeline/step-continues? entry)))
    :redirect

    (pipeline/step-continues? entry) :ok
    :else :step-failed))

(def ^:private status-priority {:violations 0 :step-failed 1 :redirect 2 :ok 3})

(defn- settled
  "Calculate a group's result: its most serious member entry, with the group's
  :step-status."
  [statuses entries]
  (let [[entry status] (first (sort-by (comp status-priority second)
                                       (map vector entries statuses)))]
    (assoc entry :step-status status)))

(defn- run-step!
  "Run a group of steps, retrying it after a rollback while violations persist
  and the least tolerant member's retry bound allows. A violation outside the
  worktree stops the run at once. `context` is the first attempt's context (a
  verdict that redirected the run here). Returns the most serious member's last
  attempt with :step-status, :redirect when the step's verdict sends the run
  back."
  [ctx members record context]
  (let [agents (mapv (fn [[_ step]] (get-in (:timetable ctx) [:agents (:agent step)])) members)
        bound  (apply min (map (fn [[_ step] agent]
                                 (if (pipeline/command-step? step) ; nothing to tell a command
                                   0
                                   (or (:max-violation-retries agent)
                                       pipeline/default-max-violation-retries)))
                               members agents))]
    (loop [n 0 contexts (vec (repeat (count members) context))]
      (let [entries (run-attempt! ctx members n contexts record)]
        (if (and (< n bound)
                 (not-any? :stopped? entries)
                 (some (comp seq :violations) entries))
          (recur (inc n) (mapv (fn [entry agent]
                                 {:violations (:violations entry) :writes (:writes agent)})
                               entries agents))
          (settled (map (fn [[_ step] entry] (entry-status step entry)) members entries)
                   entries))))))

(defn- announce!
  "Log a stage event at info, keeping the progress line clear of the log output
  and redrawing it afterwards when there is an event to show."
  [{:keys [progress]} message event]
  (let [{:keys [clear! draw!]} (or progress progress/silent)]
    (clear!)
    (log/info message)
    (when event (draw! event))))

(defn- run-stage!
  "Run a group of steps, announcing its start and end."
  [ctx members record total context]
  (let [agent (keyword (str/join "+" (map (comp name pipeline/step-label second) members)))
        event #(hash-map :index (ffirst members) :total total :agent agent :phase %)]
    (announce! ctx (pipeline/stage-started-message (ffirst members) total agent) (event :running))
    (let [entry (run-step! ctx members record context)
          status (:step-status entry)]
      (announce! ctx (pipeline/stage-finished-message (ffirst members) total agent status)
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
        steps    (get-in timetable [:pipelines pipeline-name :steps])
        groups   (pipeline/step-groups steps)]
    (io/make-parents record)
    (append! record (cond-> {:entry :header :pipeline pipeline-name
                             :writes writes :created-dirs created
                             :spec-path (:path spec)
                             :spec-sha256 (sha256 (:contents spec))
                             :timetable-sha256 (sha256 (pr-str (pipeline/canonical timetable)))}
                      wt (assoc :worktree wt)
                      (:reused? wt) (assoc :resumed? true)))
    (loop [g 0 done [] redirects {} context {}]
      (if (= g (count groups))
        (finish! record :success done nil wt)
        (let [members (mapv #(vector % (nth steps %)) (nth groups g))
              entry   (run-stage! ctx members record (count steps) context)
              done    (conj done entry)
              i       (:step entry)
              step    (nth steps i)]
          (case (:step-status entry)
            :ok (recur (inc g) done redirects {})
            :redirect
            (let [used   (get redirects i 0)
                  target (pipeline/redirect-target steps i step)]
              (if (>= used (pipeline/redirect-max step))
                (finish! record :review-failed done i wt)
                (do (append! record {:entry :redirect :from i :to target :count (inc used)})
                    (recur (pipeline/group-index groups target) done (assoc redirects i (inc used))
                           {:verdict (:verdict entry) :from (pipeline/step-label step)}))))
            (finish! record (:step-status entry) done i wt)))))))

(defn run-pipeline!
  "Run the named pipeline sequentially. ctx: :config :timetable
  :pipeline-name :spec {:path :contents} :root (repo top level) :branch
  :dir (the branch's state directory), :timetable-dirs (where prompt files
  are read from, lowest precedence first; defaults to [dir]) :force-unlock?, and for worktree
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
                        :prompt-texts (if dir (read-prompts (or (:timetable-dirs ctx) [dir]) timetable pipeline-name) {})
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
