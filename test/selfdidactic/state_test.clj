(ns selfdidactic.state-test
  (:require [clojure.test :refer [deftest is testing]]
            [selfdidactic.state :as sut])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (str (Files/createTempDirectory "switchfield-state-" (make-array FileAttribute 0))))

(def ^:private start {:g 0 :redirects {} :context {}})

(defn- begin! [dir]
  (sut/begin-run! dir {:run-id "r1" :generation 0 :worktree? true :baseline-tree "t0"}))

(deftest a-branch-that-never-ran-has-no-state-and-no-database
  (let [dir (temp-dir)]
    (is (nil? (sut/unfinished-run dir)))
    (is (= [] (sut/runs dir)))
    (is (not (.exists (java.io.File. dir "db"))) "reading creates nothing")))

(deftest a-stage-and-its-cursor-are-committed-together
  (let [dir (temp-dir)
        eid (begin! dir)
        ok  {:group 0 :fingerprint "f0" :entering start :after-tree "t1"}]
    (sut/record-stage! dir eid ok {:g 1 :redirects {} :context {}})
    (testing "both facts are there"
      (let [run (sut/unfinished-run dir)]
        (is (= [ok] (:stages run)))
        (is (= 1 (:g (:cursor run))))))
    (testing "a transaction that fails leaves neither the stage nor the cursor move"
      (is (thrown? Exception
                   (sut/record-stage! dir eid (assoc ok :group "not a number")
                                      {:g 2 :redirects {} :context {}})))
      (let [run (sut/unfinished-run dir)]
        (is (= 1 (count (:stages run))))
        (is (= 1 (:g (:cursor run))))))))

(deftest only-the-latest-run-that-did-not-succeed-is-unfinished
  (let [dir (temp-dir)
        e1  (begin! dir)]
    (is (= "r1" (:run-id (sut/unfinished-run dir))))
    (sut/finish-run! dir e1 :step-failed)
    (is (= :step-failed (:status (sut/unfinished-run dir))) "a failed run is resumable")
    (sut/finish-run! dir e1 :success)
    (is (nil? (sut/unfinished-run dir)))
    (let [e2 (begin! dir)]
      (sut/finish-run! dir e2 :superseded)
      (is (nil? (sut/unfinished-run dir)))
      (is (= [:success :superseded] (map :status (sut/runs dir))) "history is kept"))))
