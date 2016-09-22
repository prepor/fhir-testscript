(ns fhir-testscript.operation
  (:require [clojure.spec :as s]
            [fhir-testscript.fhir :as fhir]
            [fhir-testscript.http :as http]))

;; fhir operation resource
(s/def ::spec map?)
(s/def ::http-status int?)
(s/def ::headers (s/map-of keyword? string?))
(s/def ::result (s/keys :req [::http-status ::headers ::http/request]
                        :opt [::fhir/resource]))
