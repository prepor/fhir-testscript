(ns fhir-testscript.fhir
  (:require [clojure.spec :as s]))

;; fhir resource
(s/def ::resource map?)
