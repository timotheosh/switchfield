(ns selfdidactic.timetable.agents
  (:require [clojure.spec.alpha :as s]))

(s/def ::harness keyword?)
(s/def ::model string?)
(s/def ::description string?)
(s/def ::system-prompt string?)
(s/def ::skills (s/coll-of keyword? :kind vector?))

;; Runner keys. A missing :writes is a pipeline problem, not a spec failure,
;; so other tooling can still load the timetable.
(s/def ::writes (s/coll-of string? :kind vector?))
(s/def ::must-change boolean?)
(s/def ::expects #{:findings})
(s/def ::timeout-ms pos-int?)
(s/def ::max-violation-retries nat-int?)

(s/def ::agent (s/keys :req-un [::harness ::model]
                       :opt-un [::description ::system-prompt ::skills ::writes ::must-change ::expects
                                ::timeout-ms ::max-violation-retries]))

(s/def ::agents (s/map-of keyword? ::agent :min-count 1))
