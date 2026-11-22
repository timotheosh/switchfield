(ns selfdidactic.state
  "Execution state of pipeline runs, kept in Datalevin under the branch's db/
  directory (which snapshots and rollbacks never touch). Datalevin owns what a
  run has done; git owns the work products, and selfdidactic.runner reconciles
  the two before dispatching anything.

  Every function here is an action. A stage's completion and the run's cursor
  advance are one transaction, so a recovering run sees both or neither."
  (:require [clojure.java.io :as io]
            [datalevin.core :as d]))

(def ^:private schema
  {:run/id        {:db/valueType :db.type/string}
   :run/generation {:db/valueType :db.type/long}
   :run/status    {:db/valueType :db.type/keyword}
   :stage/run     {:db/valueType :db.type/ref}
   :stage/group   {:db/valueType :db.type/long}})

(defn- db-dir [dir] (str (io/file dir "db")))

(defn- with-conn [dir f]
  (let [conn (d/get-conn (db-dir dir) schema)]
    (try (f conn) (finally (d/close conn)))))

(defn- read-run
  "The run at `eid` as plain data, its stages in completion order."
  [db eid]
  (let [m      (d/pull db '[*] eid)
        stages (->> (d/q '[:find [?s ...] :in $ ?r :where [?s :stage/run ?r]] db eid)
                    sort
                    (map #(d/pull db '[*] %)))]
    {:eid           eid
     :run-id        (:run/id m)
     :generation    (:run/generation m)
     :status        (:run/status m)
     :worktree?     (:run/worktree? m)
     :baseline-tree (:run/baseline-tree m)
     :cursor        (:run/cursor m)
     :stages        (mapv (fn [s] {:group       (:stage/group s)
                                   :fingerprint (:stage/fingerprint s)
                                   :entering    (:stage/entering s)
                                   :after-tree  (:stage/after-tree s)})
                          stages)}))

(defn runs
  "Every run recorded for the branch, oldest first: the execution history."
  [dir]
  (if (.isDirectory (io/file (db-dir dir)))
    (with-conn dir
      (fn [conn]
        (let [db (d/db conn)]
          (mapv #(read-run db %)
                (sort (d/q '[:find [?e ...] :where [?e :run/id]] db))))))
    []))

(defn unfinished-run
  "The branch's latest run when it did not succeed and was not superseded, or
  nil. Reads only: a branch that never ran has no db/ and is left alone."
  [dir]
  (when (.isDirectory (io/file (db-dir dir)))
    (with-conn dir
      (fn [conn]
        (let [db (d/db conn)]
          (when-let [eid (some->> (d/q '[:find [?e ...] :where [?e :run/id]] db)
                                  seq (apply max))]
            (let [run (read-run db eid)]
              (when-not (#{:success :superseded} (:status run)) run))))))))

(defn begin-run!
  "Record a new run that starts at the first stage; its entity id. `run`:
  :run-id :generation :worktree? :baseline-tree."
  [dir {:keys [run-id generation worktree? baseline-tree]}]
  (with-conn dir
    (fn [conn]
      (-> (d/transact! conn [{:db/id -1 :run/id run-id :run/generation generation
                              :run/worktree? (boolean worktree?)
                              :run/baseline-tree baseline-tree
                              :run/status :running
                              :run/cursor {:g 0 :redirects {} :context {}}}])
          :tempids (get -1)))))

(defn record-stage!
  "Record a completed stage and move the run's cursor past it, in one
  transaction. `stage`: :group :fingerprint :entering (the loop state the stage
  began in) :after-tree; `cursor` is the loop state to continue from."
  [dir eid {:keys [group fingerprint entering after-tree]} cursor]
  (with-conn dir
    #(d/transact! % [{:stage/run eid :stage/group group :stage/fingerprint fingerprint
                      :stage/entering entering :stage/after-tree after-tree}
                     {:db/id eid :run/cursor cursor}])))

(defn finish-run!
  "Record how a run ended (:success, :step-failed, :violations,
  :review-failed) or that a restart :superseded it. The run and its stages stay."
  [dir eid status]
  (with-conn dir #(d/transact! % [{:db/id eid :run/status status}])))
