(ns fhir-testscript.http
  (:require [clojure.spec :as s]))

(s/def ::method #{:get :post :put :delete})
(s/def ::url string?)
(s/def ::headers (s/map-of string? string?))
(s/def ::body (s/nilable string?))
(s/def ::request (s/keys :req-un [::method ::url ::headers]
                         :opt-un [::body]))
