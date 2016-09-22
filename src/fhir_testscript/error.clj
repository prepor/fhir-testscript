(ns fhir-testscript.error
  (:require [clojure.spec :as s]))

(s/def ::msg string?)
(s/def ::data (s/nilable map?))
(s/def ::error (s/keys :req [::msg] :opt [::data]))

