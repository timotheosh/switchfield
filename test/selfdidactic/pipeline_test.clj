(ns selfdidactic.pipeline-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [selfdidactic.pipeline :as sut]))

(def timetable
  {:harnesses {:fake {:command "sh"}
               :oc   {:command "opencode" :model {:parameter "-m"}}}
   :agents    {:tw   {:harness :fake :model "true" :writes ["test/"]}
               :impl {:harness :fake :model "true" :writes ["src/"]}}
   :prompts   {:p {:path "p.md"}}
   :pipelines {:dev {:steps [{:agent :tw :prompt :p}
                             {:agent :impl :prompt :p}]}}})

(def ctx
  {:config {:plugin "fake"} :timetable timetable :pipeline-name :dev
   :branch "main" :spec {:path "s.md" :contents "SPEC"}
   :prompt-texts {:p {:text "PROMPT"}}})

(defn- problem-kinds [c] (set (map :problem (sut/pipeline-problems c))))

(deftest branch-directory-is-injective
  (is (not= (sut/encode-branch "feat/a-b") (sut/encode-branch "feat-a/b")))
  (is (not= (sut/encode-branch "a%2Fb") (sut/encode-branch "a/b")))
  (is (= "feat%2Fx" (sut/encode-branch "feat/x")))
  (is (= "/r/.switchfield/feat%2Fx" (sut/timetable-dir "/r" "feat/x")))
  (is (= ["/r/.switchfield" "/r/.switchfield/feat%2Fx"] (sut/timetable-dirs "/r" "feat/x"))))

(deftest compose-prompt-test
  (is (= "PROMPT\n\n## Specification (s.md)\n\nSPEC"
         (sut/compose-prompt {:prompt-text "PROMPT" :spec {:path "s.md" :contents "SPEC"}
                              :context {}})))
  (testing "violations in the context are rendered for the agent"
    (let [text (sut/compose-prompt
                {:prompt-text "P" :spec {:path "s" :contents "S"}
                 :context {:violations [{:violation :out-of-jurisdiction :path "deps.edn"}]
                           :writes ["src/"]}})]
      (is (str/includes? text "rolled back"))
      (is (str/includes? text "deps.edn"))
      (is (str/includes? text "\"src/\""))
      (is (str/includes? text ":needs-outside-jurisdiction")))))

(deftest outcomes-test
  (testing "only a clean process and no violations continue the run"
    (is (sut/step-continues? {:process :ok :violations []}))
    (is (not (sut/step-continues? {:process :crashed :violations []})))
    (is (not (sut/step-continues? {:process :timeout :violations []})))
    (is (not (sut/step-continues? {:process :ok :violations [{:violation :no-work}]}))))
  (testing "a verdict is data, never a failure"
    (doseq [v [:none :approve :request-changes :refuse]]
      (is (sut/step-continues? {:process :ok :verdict v :violations []})))))

(deftest jurisdiction-violations-test
  (is (= [{:violation :out-of-jurisdiction :path "deps.edn"}
          {:violation :out-of-jurisdiction :path ".switchfield/x/lock"}]
         (sut/jurisdiction-violations ["src/" "test/"]
                                      ["src/a.clj" "deps.edn" "test/t.clj" ".switchfield/x/lock"])))
  (testing "a prefix is a directory, not a name prefix"
    (is (= 1 (count (sut/jurisdiction-violations ["src/"] ["src2/a.clj"]))))))

(deftest agent-output-test
  (let [fence (fn [body] (str "chatter\n```edn\n" body "\n```\ntrailing"))
        finding "{:problem \"p\" :evidence \"e\" :justification \"j\" :required-outcome \"o\"}"]
    (testing "no block is empty structured output"
      (is (= {:output {}} (sut/parse-agent-output "just prose"))))
    (testing "the last block wins"
      (is (= {:output {:b 2}}
             (sut/parse-agent-output (str (fence "{:a 1}") (fence "{:b 2}"))))))
    (testing "unreadable or non-map blocks are malformed"
      (is (:malformed (sut/parse-agent-output (fence "{:a"))))
      (is (:malformed (sut/parse-agent-output (fence "[1 2]")))))
    (testing "malformed output is a violation"
      (is (= [:malformed-output]
             (map :violation (sut/output-violations {} (sut/parse-agent-output (fence "{:a")))))))
    (testing "a findings agent must supply findings"
      (is (= [:malformed-output]
             (map :violation (sut/output-violations {:expects :findings}
                                                    (sut/parse-agent-output "prose")))))
      (is (empty? (sut/output-violations
                   {:expects :findings}
                   (sut/parse-agent-output (fence (str "{:findings [" finding "]}")))))))
    (testing "needs-outside-jurisdiction is validated"
      (is (empty? (sut/output-violations
                   {} {:output {:needs-outside-jurisdiction
                                [{:path "deps.edn" :problem "p" :evidence "e"}]}})))
      (is (seq (sut/output-violations
                {} {:output {:needs-outside-jurisdiction [{:path "x"}]}}))))))

(deftest step-violations-test
  (let [snap {:head "h" :head-ref "r" :refs {} :index "i"}
        agent {:writes ["src/"] :must-change true}]
    (is (= [:no-work] (map :violation (sut/step-violations agent snap snap [] {:output {}}))))
    (is (= [:git-tamper] (map :violation (sut/step-violations
                                          agent snap (assoc snap :head "h2") ["src/a"] {:output {}}))))
    (is (empty? (sut/step-violations agent snap snap ["src/a"] {:output {}})))))

(deftest overlapping-writes-test
  (is (= [{:problem :overlapping-writes :agents [:a :b] :prefixes ["src/" "src/deep/"]}]
         (sut/overlapping-writes {:a {:writes ["src/"]} :b {:writes ["src/deep/"]}})))
  (is (= 1 (count (sut/overlapping-writes {:a {:writes ["src/"]} :b {:writes ["src/"]}}))))
  (is (empty? (sut/overlapping-writes {:a {:writes ["src/"]} :b {:writes ["test/"]}}))))

(deftest pipeline-problems-test
  (testing "a sound pipeline has no problems"
    (is (empty? (sut/pipeline-problems ctx))))
  (testing "detached HEAD"
    (is (contains? (problem-kinds (assoc ctx :branch nil)) :detached-head)))
  (testing "unknown pipeline"
    (is (= #{:unknown-pipeline} (problem-kinds (assoc ctx :pipeline-name :nope)))))
  (testing "agent without :writes"
    (is (contains? (problem-kinds (update-in ctx [:timetable :agents :tw] dissoc :writes))
                   :missing-writes)))
  (testing "bad and overlapping :writes"
    (is (contains? (problem-kinds (assoc-in ctx [:timetable :agents :tw :writes] ["test"]))
                   :bad-writes-prefix))
    (is (contains? (problem-kinds (assoc-in ctx [:timetable :agents :tw :writes] [".switchfield/"]))
                   :bad-writes-prefix))
    (is (contains? (problem-kinds (assoc-in ctx [:timetable :agents :tw :writes] ["src/x/"]))
                   :overlapping-writes)))
  (testing "references and unsupported step keys"
    (let [steps (fn [s] (assoc-in ctx [:timetable :pipelines :dev :steps] s))]
      (is (contains? (problem-kinds (steps [{:agent :ghost :prompt :p}])) :unknown-agent))
      (is (contains? (problem-kinds (steps [{:agent :tw :prompt :ghost}])) :unknown-prompt))
      (is (contains? (problem-kinds (steps [{:agent :tw}])) :missing-prompt))
      (is (contains? (problem-kinds (steps [{:agent :tw :prompt :p :callback :tw}]))
                     :unsupported-step-key))
      (is (contains? (problem-kinds (steps [{:agent :concurrent :thread []}]))
                     :unsupported-step-key))))
  (testing "an unreadable prompt file"
    (is (contains? (problem-kinds (assoc ctx :prompt-texts {:p {:error "nope"}}))
                   :unreadable-prompt)))
  (testing "a skills/plugin mismatch is data, not an exception"
    (let [c (-> ctx
                (assoc :config {:plugin "opencode"})
                (assoc-in [:timetable :agents :tw]
                          {:harness :oc :model "m" :writes ["test/"] :skills [:acd]})
                (assoc-in [:timetable :agents :impl]
                          {:harness :oc :model "m" :writes ["src/"]}))]
      (is (= #{:command} (problem-kinds c)))
      (is (empty? (sut/pipeline-problems (assoc-in c [:timetable :agents :tw :skills] []))))))
  (testing "an unknown plugin is data too"
    (is (contains? (problem-kinds (assoc ctx :config {:plugin "nope"})) :command)))
  (testing "an argv-delivered prompt over the limit"
    (is (empty? (sut/pipeline-problems
                 (assoc ctx :spec {:path "s" :contents (apply str (repeat 300000 "x"))}))))
    (is (contains? (problem-kinds
                    (-> ctx
                        (assoc :config {:plugin "hermes"})
                        (assoc-in [:timetable :agents :tw :harness] :oc)
                        (assoc-in [:timetable :agents :impl :harness] :oc)
                        (assoc :spec {:path "s" :contents (apply str (repeat 300000 "x"))})))
                   :argv-too-long))))

(deftest lock-problem-test
  (is (= :run-in-progress
         (:problem (sut/lock-problem {:pid 1 :host "h"} "h" true))))
  (is (= :run-in-progress
         (:problem (sut/lock-problem {:pid 1 :host "other"} "h" false))))
  (is (= :stale-lock
         (:problem (sut/lock-problem {:pid 1 :host "h"} "h" false))))
  (is (= :run-in-progress (:problem (sut/lock-problem 'pid "h" false)))))

(deftest writes-list-test
  (testing "the sorted, distinct prefixes of the agents the pipeline uses"
    (is (= ["src/" "test/"] (sut/writes-list timetable :dev))))
  (testing "an agent the pipeline does not use contributes nothing"
    (is (= ["src/" "test/"]
           (sut/writes-list (assoc-in timetable [:agents :unused]
                                      {:harness :fake :model "m" :writes ["unused/"]})
                            :dev))))
  (testing "empty :writes, repeats and a missing key contribute nothing"
    (let [t (-> timetable
                (assoc-in [:agents :ro] {:harness :fake :model "m" :writes []})
                (assoc-in [:agents :bare] {:harness :fake :model "m"})
                (assoc-in [:pipelines :dev :steps]
                          [{:agent :ro :prompt :p} {:agent :bare :prompt :p}
                           {:agent :impl :prompt :p} {:agent :impl :prompt :p}]))]
      (is (= ["src/"] (sut/writes-list t :dev)))))
  (testing "an unknown pipeline has none"
    (is (= [] (sut/writes-list timetable :nope)))))

(deftest writes-not-directory-test
  (is (empty? (sut/pipeline-problems (assoc ctx :writes-fs {"src/" :dir "test/" :missing}))))
  (is (= #{:writes-not-directory}
         (problem-kinds (assoc ctx :writes-fs {"src/" :file "test/" :dir})))))

(deftest run-id-and-worktree-naming
  (is (= "feat%2Fx-r1" (sut/worktree-name "feat%2Fx" "r1")))
  (is (= "switchfield/feat%2Fx/r1" (sut/worktree-branch "feat%2Fx" "r1"))))

(deftest worktree-problems-test
  (let [base {:worktree? true :branch "main" :run-id "r1" :worktree-facts {}}
        kinds #(set (map (juxt :problem :reason) (sut/worktree-problems %)))]
    (is (empty? (sut/worktree-problems base)))
    (testing "nothing is checked when running in place"
      (is (empty? (sut/worktree-problems (assoc base :worktree? false :dirty-paths ["x"])))))
    (testing "a dirty checkout, unless allowed"
      (is (= #{[:dirty-checkout nil]} (kinds (assoc base :dirty-paths ["a"]))))
      (is (empty? (sut/worktree-problems (assoc base :dirty-paths ["a"] :allow-dirty? true)))))
    (testing "a registered worktree is re-used: no conflict, no dirty check"
      (is (empty? (sut/worktree-problems
                   (assoc base :dirty-paths ["a"]
                          :worktree-facts {:registered? true :dir-exists? true :branch-exists? true})))))
    (testing "conflicts are named, never guessed"
      (is (= #{[:worktree-conflict :unregistered-directory]}
             (kinds (assoc base :worktree-facts {:dir-exists? true}))))
      (is (= #{[:worktree-conflict :branch-without-worktree]}
             (kinds (assoc base :worktree-facts {:branch-exists? true}))))
      (is (= #{[:worktree-conflict :ref-conflict]}
             (kinds (assoc base :worktree-facts {:ref-conflicts ["switchfield"]})))))
    (testing "a branch named like the container directory"
      (is (contains? (kinds (assoc base :branch "worktrees")) [:reserved-branch-name nil])))))

(deftest watcher-violations-test
  (testing "runner state is out of jurisdiction; the rest stops the run"
    (is (= [{:violation :out-of-jurisdiction :path ".switchfield/main/runs/x"}
            {:violation :main-checkout-modified :path "notes.txt"}
            {:violation :git-dir-modified :path ".git/hooks/pre-commit"}]
           (sut/watcher-violations
            {:state-changed [".switchfield/main/runs/x"] :main-changed ["notes.txt"]
             :git-dir-before {"config" "h"} :git-dir-after {"config" "h" "hooks/pre-commit" "n"}}))))
  (testing "nothing changed, nothing reported"
    (is (empty? (sut/watcher-violations {:git-dir-before {"config" "h"} :git-dir-after {"config" "h"}}))))
  (testing "only the two outside-the-worktree kinds stop a run"
    (is (every? sut/stop-violation? [{:violation :main-checkout-modified} {:violation :git-dir-modified}]))
    (is (not-any? sut/stop-violation? [{:violation :out-of-jurisdiction} {:violation :git-tamper}
                                       {:violation :no-work} {:violation :malformed-output}]))))

(deftest verdict-test
  (testing "command verdicts are failures; agent verdicts come from output"
    (let [cmd {:command {:args ["x"]}}]
      (is (nil? (sut/step-verdict cmd {:process :ok} nil)))
      (is (= {:exit-code 2} (sut/step-verdict cmd {:process :crashed :exit-code 2} nil)))
      (is (= {:missing-artifact "a"} (sut/step-verdict cmd {:process :ok :missing-artifact "a"} nil)))
      (is (nil? (sut/step-verdict cmd {:process :timeout} nil))))
    (is (= [1] (sut/step-verdict {:agent :r :verdict :findings} {} {:output {:findings [1]}})))
    (is (nil? (sut/step-verdict {:agent :r} {} {:output {:findings [1]}}))))
  (testing "trips on the default and quoted predicates"
    (let [s {:agent :r :redirect {:agent :w}}]
      (is (sut/verdict-trips? s [1]))
      (is (not (sut/verdict-trips? s [])))
      (is (not (sut/verdict-trips? s nil)))
      (is (sut/verdict-trips? (assoc-in s [:redirect :verdict] ''some?) :x))
      (is (not (sut/verdict-trips? {:agent :r} [1])))))
  (testing "redirect problems"
    (let [w {:agent :w :prompt :p}
          probs #(set (map :problem (sut/redirect-problems [w %] 1 %)))]
      (is (empty? (probs {:agent :r :verdict :f :redirect {:agent :w}})))
      (is (= #{:unknown-redirect-agent} (probs {:agent :r :verdict :f :redirect {:agent :zz}})))
      (is (= #{:redirect-without-verdict} (probs {:agent :r :redirect {:agent :w}})))
      (is (= #{:bad-verdict-fn} (probs {:agent :r :verdict :f :redirect {:agent :w :verdict 'eval}})))
      (is (= #{:bad-redirect-max} (probs {:agent :r :verdict :f :redirect {:agent :w :max 0}})))
      (is (= #{:bad-redirect} (probs {:agent :r :verdict :f :redirect {:agent :w :x 1}}))))))

(deftest concurrent-steps-test
  (testing "adjacent :concurrent steps form one group; every other step is alone"
    (is (= [[0] [1 2 3] [4]]
           (sut/step-groups [{} {:concurrent true} {:concurrent true} {:concurrent true} {}])))
    (is (= [[0] [1] [2]] (sut/step-groups [{} {:concurrent false} {}])))
    (is (= 1 (sut/group-index [[0] [1 2] [3]] 2))))
  (testing "concurrent agents may not write"
    (let [with-pipeline (fn [steps agents]
                          (-> ctx
                              (assoc-in [:timetable :agents] (merge (:agents timetable) agents))
                              (assoc-in [:timetable :pipelines :dev :steps] steps)))
          ro {:ro {:harness :fake :model "true" :writes []}}
          rev (fn [& {:as more}] (merge {:agent :ro :prompt :p :concurrent true} more))]
      (is (empty? (sut/pipeline-problems
                   (with-pipeline [(rev) (rev)] ro))))
      (is (= [{:problem :concurrent-writes :step 1 :agent :impl :writes ["src/"]}]
             (filter #(= :concurrent-writes (:problem %))
                     (sut/pipeline-problems
                      (with-pipeline [(rev) (rev :agent :impl)] ro)))))
      (testing "a sequential step is not held to it"
        (is (empty? (sut/pipeline-problems
                     (with-pipeline [(rev) (rev) {:agent :impl :prompt :p}] ro)))))
      (testing "a concurrent step cannot redirect"
        (is (contains? (problem-kinds
                        (with-pipeline [{:agent :impl :prompt :p}
                                        (rev :verdict :f :redirect {:agent :impl}) (rev)] ro))
                       :concurrent-redirect))))))

;; -- Resuming a run --------------------------------------------------------

(def ^:private saved
  {:baseline-tree "t0"
   :cursor {:g 3 :redirects {} :context {}}
   :stages [{:group 0 :fingerprint "f0" :after-tree "t1" :entering {:g 0 :redirects {} :context {}}}
            {:group 1 :fingerprint "f1" :after-tree "t2" :entering {:g 1 :redirects {} :context {}}}
            {:group 2 :fingerprint "f2" :after-tree "t3" :entering {:g 2 :redirects {:x 1} :context {:v 1}}}]})

(def ^:private now-same
  {:fingerprints {0 "f0" 1 "f1" 2 "f2"} :existing-trees #{"t0" "t1" "t2" "t3"}})

(deftest resume-plan-keeps-stages-that-still-match
  (is (= {:state {:g 3 :redirects {} :context {}} :kept [0 1 2] :restore-to "t3" :problems nil}
         (sut/resume-plan saved now-same))))

(deftest resume-plan-reruns-from-the-first-stage-that-does-not-match
  (testing "a changed fingerprint re-runs that stage and every later one, from its entering state"
    (let [plan (sut/resume-plan saved (assoc-in now-same [:fingerprints 1] "changed"))]
      (is (= [0] (:kept plan)))
      (is (= {:g 1 :redirects {} :context {}} (:state plan)))
      (is (= "t1" (:restore-to plan)))))
  (testing "a stage whose output tree git has lost is re-run, even when later ones look fine"
    (let [plan (sut/resume-plan saved (update now-same :existing-trees disj "t2"))]
      (is (= [0] (:kept plan)))
      (is (= "t1" (:restore-to plan)))))
  (testing "the entering state of a re-run stage restores its redirect counts and context"
    (let [plan (sut/resume-plan saved (assoc-in now-same [:fingerprints 2] "changed"))]
      (is (= {:g 2 :redirects {:x 1} :context {:v 1}} (:state plan)))))
  (testing "nothing kept restores the baseline"
    (let [plan (sut/resume-plan saved (assoc-in now-same [:fingerprints 0] "changed"))]
      (is (= [] (:kept plan)))
      (is (= "t0" (:restore-to plan))))))

(deftest resume-plan-reports-lost-state
  (testing "lost stage trees just shrink what is kept"
    (is (nil? (:problems (sut/resume-plan saved (update now-same :existing-trees disj "t3"))))))
  (testing "a baseline git no longer has, with nothing kept, cannot be recovered"
    (is (= [:saved-state-lost]
           (map :problem (:problems (sut/resume-plan saved (-> now-same
                                                               (assoc-in [:fingerprints 0] "changed")
                                                               (update :existing-trees disj "t0")))))))))

(deftest stage-fingerprint-depends-on-everything-the-stage-uses
  (let [steps [{:agent :a :prompt :p}]
        base  {:timetable {:agents {:a {:harness :h :model "m"}} :harnesses {:h {:command "sh"}}}
               :prompt-texts {:p {:text "P"}} :spec-sha256 "s"}
        fp    #(sut/stage-fingerprint % steps [0])]
    (is (= (fp base) (fp base)))
    (is (not= (fp base) (fp (assoc-in base [:timetable :agents :a :model] "other"))))
    (is (not= (fp base) (fp (assoc-in base [:timetable :harnesses :h :command] "bash"))))
    (is (not= (fp base) (fp (assoc-in base [:prompt-texts :p :text] "Q"))))
    (is (not= (fp base) (fp (assoc base :spec-sha256 "t"))))
    (testing "an unrelated agent does not matter"
      (is (= (fp base) (fp (assoc-in base [:timetable :agents :zzz] {:model "x"})))))))

(deftest resume-problems-require-the-same-mode
  (is (empty? (sut/resume-problems {:saved-run {:worktree? true} :worktree? true})))
  (is (empty? (sut/resume-problems {:saved-run {:worktree? true} :worktree? false :restart? true})))
  (is (empty? (sut/resume-problems {:worktree? false})))
  (is (= [:run-mode-mismatch] (map :problem (sut/resume-problems {:saved-run {:worktree? true} :worktree? false})))))
