(ns fhir-testscript.assert
  (:require [clojure.spec :as s]
            [fhir-testscript.error :as error]))

;; fhir assert resource
(s/def ::spec map?)
(s/def ::result (s/keys :opt [::error/error]))

