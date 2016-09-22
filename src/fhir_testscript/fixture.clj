(ns fhir-testscript.fixture
  (:require  [clojure.test :as t]
             [clojure.spec :as s]
             [fhir-testscript.fhir :as fhir]
             [fhir-testscript.operation :as operation]))

;; fhir fixture resource
(s/def ::spec map?)
(s/def ::static-result (s/keys :req [::fhir/resource]))
(s/def :dynamic-fixture/result :operation/result)

(defmulti fixture ::type)
(defmethod fixture ::static [_]
  (s/keys :req [::static-result ::spec]))
(defmethod fixture ::dynamic [_]
  (s/keys :req [::operation/result] :opt [::spec]))
(s/def ::fixture (s/multi-spec fixture ::type))

