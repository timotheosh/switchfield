(ns selfdidactic.timetable-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [selfdidactic.timetable :as tt]))

(def full
  {:harnesses {:hermes {:command "hermes"}}
   :agents    {:dev {:harness :hermes :model "m" :skills [:acd]}}
   :prompts   {:p {:path "prompts/p.md"}}
   :pipelines {:main {:steps [{:agent :dev :prompt :p}]}}})

(defn problem-kinds [t] (set (map :problem (tt/problems t))))

(deftest deep-merge-test
  (testing "maps merge recursively"
    (is (= {:agents {:reviewer {:harness :hermes
                                :model "~anthropic/claude-sonnet"
                                :skills [:acd :review]}}}
           (tt/merge-timetables
            {:agents {:reviewer {:harness :hermes
                                 :model "~deepseek/deepseek-flash-latest"
                                 :skills [:acd :review]}}}
            {:agents {:reviewer {:model "~anthropic/claude-sonnet"}}}))))
  (testing "later leaf wins"
    (is (= {:a 2} (tt/merge-timetables {:a 1} {:a 2}))))
  (testing "vectors are replaced, not concatenated"
    (is (= {:skills [:clojure]}
           (tt/merge-timetables {:skills [:acd :review]} {:skills [:clojure]}))))
  (testing "a later nil replaces a map"
    (is (= {:foo nil} (tt/deep-merge {:foo {:a 1}} {:foo nil}))))
  (testing "inputs are unchanged"
    (let [a {:x {:y 1}}]
      (tt/merge-timetables a {:x {:z 2}})
      (is (= {:x {:y 1}} a)))))

(deftest validation-test
  (is (= full (tt/validate full)))
  (testing "skills optional, and may be empty"
    (is (tt/valid? (assoc full :skills {})))
    (is (tt/valid? (assoc full :skills {:acd {:path "skills/acd"}}))))
  (testing "each required root"
    (doseq [k tt/required-roots]
      (is (= #{:missing-root} (problem-kinds (dissoc full k))))))
  (testing "structural failures"
    (is (not (tt/valid? (assoc full :pipelines {}))))
    (is (not (tt/valid? (assoc full :pipelines {:main {:steps []}}))))
    (is (not (tt/valid? (assoc full :pipelines {:main {:steps [{:prompt :p}]}}))))
    (is (tt/valid? (assoc full :pipelines {:main {:steps [{:command {:args ["true"]}}]}})))
    (is (not (tt/valid? (assoc full :pipelines {:main {:steps [{:command ["true"]}]}}))))
    (is (not (tt/valid? (assoc-in full [:agents :dev :model] :not-a-string)))))
  (testing "harness definitions stay open"
    (is (tt/valid? (assoc-in full [:harnesses :hermes :plugin] :whatever))))
  (testing "extra-params is an optional vector of strings"
    (is (tt/valid? (assoc-in full [:harnesses :hermes :extra-params] ["--auto"])))
    (is (tt/valid? (assoc-in full [:harnesses :hermes :extra-params] [])))
    (is (not (tt/valid? (assoc-in full [:harnesses :hermes :extra-params] "--auto"))))
    (is (not (tt/valid? (assoc-in full [:harnesses :hermes :extra-params] [:auto])))))
  (testing "unknown root key fails and is named"
    (let [e (try (tt/validate (assoc full :agent {}))
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= [{:problem :unknown-root-key :root :agent}]
             (:problems (ex-data e)))))))

(deftest fragments-test
  (testing "any file split yields the same logical timetable"
    (is (= full
           (tt/merge-timetables
            (select-keys full [:harnesses :agents])
            (select-keys full [:prompts])
            (select-keys full [:pipelines]))
           (tt/merge-timetables full)))
    (is (= full
           (tt/merge-timetables
            {:agents {:dev {:harness :hermes}}}
            (dissoc full :agents)
            {:agents {:dev {:model "m" :skills [:acd]}}}))))
  (testing "a fragment alone need not be valid; only the merge is checked"
    (is (not (tt/valid? {:agents (:agents full)})))
    (is (tt/valid? (tt/merge-timetables {:agents (:agents full)}
                                        (dissoc full :agents))))))

(deftest load-dir-test
  (is (tt/valid? (tt/load-dir "resources/timetable"))))

(deftest load-dirs-test
  (let [tmp    (.toFile (java.nio.file.Files/createTempDirectory "tt" (make-array java.nio.file.attribute.FileAttribute 0)))
        shared (io/file tmp "shared")
        branch (io/file tmp "branch")]
    (.mkdirs shared)
    (.mkdirs branch)
    (spit (io/file shared "all.edn") (pr-str full))
    (testing "a missing or empty branch directory inherits the shared timetable"
      (is (= full (tt/load-dirs [shared (io/file tmp "nope")])))
      (is (= full (tt/load-dirs [shared branch]))))
    (testing "the branch directory overrides the shared one, key by key"
      (spit (io/file branch "o.edn") (pr-str {:agents {:dev {:model "other"}}}))
      (is (= "other" (get-in (tt/load-dirs [shared branch]) [:agents :dev :model])))
      (is (= :hermes (get-in (tt/load-dirs [shared branch]) [:agents :dev :harness]))))
    (testing "a branch directory nested in the shared one is not read as a fragment"
      (let [nested (io/file shared "main")]
        (.mkdirs nested)
        (spit (io/file nested "o.edn") (pr-str {:agents {:dev {:model "nested"}}}))
        (is (= "m" (get-in (tt/load-dirs [shared]) [:agents :dev :model])))))))

(deftest harness-exit-codes-test
  (testing ":exit-codes names a harness's exit codes"
    (is (tt/valid? (assoc-in full [:harnesses :hermes :exit-codes] {137 :oom-killed}))))
  (testing "codes must be integers and reasons keywords"
    (is (not (tt/valid? (assoc-in full [:harnesses :hermes :exit-codes] {"137" :oom}))))
    (is (not (tt/valid? (assoc-in full [:harnesses :hermes :exit-codes] {137 "oom"}))))
    (is (not (tt/valid? (assoc-in full [:harnesses :hermes :exit-codes] {-1 :oom}))))))
