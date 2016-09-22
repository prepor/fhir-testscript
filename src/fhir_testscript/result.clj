(ns fhir-testscript.result
  (:require [clojure.spec :as s]
            [fhir-testscript.operation :as operation]
            [fhir-testscript.assert :as assert]
            [fhir-testscript.error :as error]))

(s/def ::actions (s/coll-of ::action))
(s/def ::status #{:success :failed :skipped})
(s/def ::setup ::actions)
(s/def ::teardown ::actions)
(defmulti action ::type)
(defmethod action ::operation [_]
  (s/keys :req [::operation/spec ::resolved-variables ::operation/result
                ::action-status]))
(defmethod action ::assert [_]
  (s/keys :req [::assert/spec ::resolved-variables ::action-status]
          :opt [::assert/result]))
(s/def ::action (s/multi-spec action ::type))
(s/def ::tests (s/coll-of ::test))
(s/def ::test (s/keys :req [::test-name ::test-description ::actions ::test-status]))

(s/def ::resolved-variables (s/map-of string? string?))

(s/def ::test-name string?)
(s/def ::test-description string?)
(s/def ::test-status #{:success :failed})
(s/def ::action-status #{:success :failed})

(s/def ::result (s/keys :req [::status]
                        :opt [::error/error
                              ::setup ::tests ::teardown]))
