(ns selfdidactic.git-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [selfdidactic.git :as sut]
            [selfdidactic.harness :as harness])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn sh! [dir & argv]
  (let [r (harness/start (vec argv) dir)]
    (assert (= :ok (:process r)) (pr-str argv r))
    (:out r)))

(defn put! [dir path content]
  (let [f (io/file dir path)]
    (io/make-parents f)
    (spit f content)))

(defn make-repo
  "A repo with one commit: src/a.clj, test/t.clj, an ignored .switchfield/."
  []
  (let [dir (str (Files/createTempDirectory "switchfield-git-" (make-array FileAttribute 0)))]
    (sh! dir "git" "init" "-q" "-b" "main")
    (sh! dir "git" "config" "user.email" "t@example.com")
    (sh! dir "git" "config" "user.name" "t")
    (sh! dir "git" "config" "commit.gpgsign" "false")
    (sh! dir "git" "config" "tag.gpgsign" "false")
    (put! dir "src/a.clj" "a1")
    (put! dir "test/t.clj" "t1")
    (put! dir ".gitignore" ".switchfield/\n")
    (put! dir ".switchfield/main/lock" "pid")
    (sh! dir "git" "add" ".")
    (sh! dir "git" "commit" "-qm" "init")
    dir))

(deftest branch-and-toplevel
  (let [dir (make-repo)]
    (is (= "main" (sut/branch dir)))
    (is (= (.getCanonicalPath (io/file dir)) (.getCanonicalPath (io/file (sut/toplevel dir)))))
    (sh! dir "git" "checkout" "-q" "--detach")
    (is (nil? (sut/branch dir)) "detached HEAD has no branch")))

(deftest snapshot-is-by-content
  (let [dir (make-repo)
        s0 (sut/snapshot dir)]
    (testing "an unchanged tree snapshots identically"
      (is (= s0 (sut/snapshot dir))))
    (testing "re-editing an already-dirty file changes the snapshot"
      (put! dir "test/t.clj" "dirty-1")
      (let [dirty (sut/snapshot dir)]
        (put! dir "test/t.clj" "dirty-2")
        (is (not= (:tree dirty) (:tree (sut/snapshot dir))))
        (is (= ["test/t.clj"] (sut/changed-paths dir dirty (sut/snapshot dir))))))
    (testing "ignored .switchfield/ is part of the snapshot"
      (let [before (sut/snapshot dir)]
        (put! dir ".switchfield/main/lock" "tampered")
        (is (= [".switchfield/main/lock"]
               (sut/changed-paths dir before (sut/snapshot dir))))))
    (testing "a stat-only index refresh is not a change"
      (let [before (sut/snapshot dir)]
        (sh! dir "touch" "src/a.clj")
        (sh! dir "git" "status")
        (is (empty? (sut/tamper-violations before (sut/snapshot dir))))))))

(deftest tamper-is-detected
  (doseq [[label action what]
          [["commit"  #(do (put! % "src/a.clj" "a2")
                           (sh! % "git" "commit" "-qam" "x")) :head]
           ["stage"   #(do (put! % "src/new.clj" "n")
                           (sh! % "git" "add" "src/new.clj")) :index]
           ["branch switch" #(sh! % "git" "checkout" "-q" "-b" "other") :head-ref]
           ["new ref" #(sh! % "git" "tag" "t") :refs]]]
    (testing label
      (let [dir (make-repo)
            before (sut/snapshot dir)]
        (action dir)
        (is (some #{what} (map :what (sut/tamper-violations before (sut/snapshot dir)))))))))

(deftest restore-reproduces-the-snapshot
  (let [dir (make-repo)
        before (sut/snapshot dir)]
    ;; a mix: in-jurisdiction edit, out-of-jurisdiction edit, new file, deletion,
    ;; file<->dir swap, run-record tamper, plus commit/stage/branch tampering.
    (put! dir "src/a.clj" "edited")
    (put! dir "deps.edn" "new")
    (put! dir "src/deep/new.clj" "n")
    (.delete (io/file dir "test/t.clj"))
    (put! dir ".switchfield/main/lock" "tampered")
    (sh! dir "git" "add" "src/a.clj")
    (sh! dir "git" "commit" "-qm" "sneaky")
    (sh! dir "git" "checkout" "-q" "-b" "other")
    (let [after (sut/snapshot dir)
          now (sut/restore! dir before after)]
      (is (= (:tree before) (:tree now)))
      (is (= (dissoc before :tree) (dissoc now :tree)))
      (is (= "main" (sut/branch dir)))
      (is (= "a1" (slurp (io/file dir "src/a.clj"))))
      (is (= "t1" (slurp (io/file dir "test/t.clj"))))
      (is (not (.exists (io/file dir "deps.edn"))))
      (is (not (.exists (io/file dir "src/deep"))) "empty parents are pruned")
      (is (= "pid" (slurp (io/file dir ".switchfield/main/lock")))))))

(deftest restore-handles-file-dir-swaps
  (let [dir (make-repo)
        before (sut/snapshot dir)]
    (.delete (io/file dir "src/a.clj"))
    (put! dir "src/a.clj/inner.clj" "x")
    (let [after (sut/snapshot dir)]
      (is (= (:tree before) (:tree (sut/restore! dir before after))))
      (is (.isFile (io/file dir "src/a.clj"))))))

(deftest snapshots-skip-worktrees-db-and-logs
  (let [dir (make-repo)
        before (sut/snapshot dir)]
    (testing "logs/ is never part of a snapshot"
      (put! dir ".switchfield/main/logs/r1/step-0.log" "x")
      (is (= (:tree before) (:tree (sut/snapshot dir)))))
    (testing "reserved db/ state is never part of a snapshot"
      (put! dir ".switchfield/main/db/data.mdb" "x")
      (is (= (:tree before) (:tree (sut/snapshot dir)))))
    (testing "other state files still are, even though .switchfield/ is ignored"
      (put! dir ".switchfield/main/runs/r.edn" "x")
      (is (not= (:tree before) (:tree (sut/snapshot dir)))))
    (testing "run worktrees are never recorded, as gitlinks or otherwise"
      (let [s0 (sut/snapshot dir)]
        (sut/worktree-add! dir (str dir "/.switchfield/worktrees/main-r1") "switchfield/main/r1")
        (put! dir ".switchfield/worktrees/main-r1/src/x.clj" "x")
        (is (= (:tree s0) (:tree (sut/snapshot dir))))))))

(deftest watch-snapshots
  (let [dir (make-repo)
        w0 (sut/watch-snapshot dir "main")]
    (testing "a change to runner state moves :state only"
      (put! dir ".switchfield/main/runs/r.edn" "x")
      (let [w1 (sut/watch-snapshot dir "main")]
        (is (not= (:state w0) (:state w1)))
        (is (= (:main w0) (:main w1)))))
    (testing "db/ and logs/ under runner state are not watched"
      (let [w1 (sut/watch-snapshot dir "main")]
        (put! dir ".switchfield/main/db/data.mdb" "x")
        (put! dir ".switchfield/main/logs/r1/step-0.log" "x")
        (is (= w1 (sut/watch-snapshot dir "main")))))
    (testing "a change elsewhere in the main checkout moves :main only"
      (let [w1 (sut/watch-snapshot dir "main")]
        (put! dir "notes.txt" "x")
        (let [w2 (sut/watch-snapshot dir "main")]
          (is (= (:state w1) (:state w2)))
          (is (not= (:main w1) (:main w2))))))
    (testing "state is absent for a branch without a directory"
      (is (string? (:state (sut/watch-snapshot dir "other")))))
    (testing "restore-tree! puts runner state back"
      (let [w1 (sut/watch-snapshot dir "main")]
        (put! dir ".switchfield/main/runs/r.edn" "tampered")
        (sut/restore-tree! dir (:state w1) (:state (sut/watch-snapshot dir "main")))
        (is (= "x" (slurp (io/file dir ".switchfield/main/runs/r.edn"))))))))

(deftest git-dir-hashes-detect-planted-hooks
  (let [dir (make-repo)
        h0 (sut/git-dir-hashes dir)]
    (is (= h0 (sut/git-dir-hashes dir)))
    (is (contains? h0 "config"))
    (put! dir ".git/hooks/pre-commit" "#!/bin/sh\nexit 0\n")
    (is (contains? (sut/git-dir-hashes dir) "hooks/pre-commit"))
    (let [h1 (sut/git-dir-hashes dir)]
      (put! dir ".git/config" "[core]\n\tfoo = bar\n")
      (is (not= (get h1 "config") (get (sut/git-dir-hashes dir) "config"))))
    (testing "the shared directory is found from inside a worktree too"
      (sut/worktree-add! dir (str dir "/wt") "wt-branch")
      (is (= (sut/git-dir-hashes dir) (sut/git-dir-hashes (str dir "/wt")))))))

(deftest dirty-and-worktree-helpers
  (let [dir (make-repo)]
    (testing "a clean checkout, and files that are not tracked, are not dirty"
      (is (= [] (sut/dirty-paths dir)))
      (put! dir "untracked.txt" "x")
      (put! dir ".switchfield/main/t.edn" "x")
      (is (= [] (sut/dirty-paths dir))))
    (testing "tracked changes are, .switchfield/ edits are not"
      (put! dir "src/a.clj" "changed")
      (is (= ["src/a.clj"] (sut/dirty-paths dir))))
    (testing "worktree creation, registration and branches"
      (let [path (str dir "/.switchfield/worktrees/main-r1")]
        (is (not (sut/worktree-registered? dir path)))
        (is (not (sut/branch-exists? dir "switchfield/main/r1")))
        (sut/worktree-add! dir path "switchfield/main/r1")
        (is (sut/worktree-registered? dir path))
        (is (sut/branch-exists? dir "switchfield/main/r1"))
        (testing "it starts from HEAD: committed content, not dirty edits"
          (is (= "a1" (slurp (io/file path "src/a.clj")))))
        (testing "a plain directory is not a registered worktree"
          (.mkdirs (io/file dir ".switchfield/worktrees/plain"))
          (is (not (sut/worktree-registered? dir (str dir "/.switchfield/worktrees/plain")))))))))

(deftest reset-to-tree-touches-only-the-work-dir-files-that-differ
  (let [dir    (make-repo)
        target (:tree (sut/snapshot dir))
        refs   (:refs (sut/snapshot dir))]
    (put! dir "src/a.clj" "changed")
    (put! dir "src/untracked.clj" "new")
    (sut/reset-to-tree! dir target)
    (is (= target (:tree (sut/snapshot dir))))
    (is (= "a1" (slurp (io/file dir "src/a.clj"))))
    (is (not (.exists (io/file dir "src/untracked.clj"))))
    (is (= refs (:refs (sut/snapshot dir))) "refs are never rewritten")))

(deftest tree-exists-tells-a-real-tree-from-a-lost-one
  (let [dir (make-repo)]
    (is (sut/tree-exists? dir (:tree (sut/snapshot dir))))
    (is (not (sut/tree-exists? dir "0123456789012345678901234567890123456789")))))
