(ns selfdidactic.timetable.pipelines
  (:require [clojure.spec.alpha :as s]))

(s/def ::agent keyword?)

;; Open: the step DSL (:prompt, :callback, :thread, ...) is not validated here.
(s/def ::command map?)

;; An agent step has :agent; a command step has :command instead. The command's
;; own shape ({:args ...}) is checked when the pipeline runs, like the rest.
(s/def ::step (s/or :agent (s/keys :req-un [::agent])
                    :command (s/keys :req-un [::command])))

(s/def ::steps (s/coll-of ::step :kind vector? :min-count 1))

(s/def ::pipeline (s/keys :req-un [::steps]))

(s/def ::pipelines (s/map-of keyword? ::pipeline :min-count 1))
