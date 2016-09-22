(ns fhir-testscript.core
  (:gen-class)
  (:require [cheshire.core :as json]
            [clojure
             [set :as set]
             [string :as str]]
            [json-path :as json-path]
            [clojure.spec :as s]
            [clojure.java.io :as io]
            [org.httpkit.client :as http]
            [fhir-testscript.fhir :as fhir]
            [fhir-testscript.fixture :as fixture]
            [fhir-testscript.operation :as operation]
            [fhir-testscript.assert :as assert]
            [fhir-testscript.error :as error]
            [fhir-testscript.result :as result]
            [fhir-testscript.http :as self-http])
  (:import java.net.URL
           java.util.regex.Matcher))

;; fhir variable resource
(s/def ::variable map?)
(s/def ::location #(instance? java.io.File %))
(s/def ::server-url #(instance? URL %))
(s/def ::fixtures (s/map-of string? ::fixture/fixture))
(s/def ::variables (s/map-of string? ::variable))
(s/def ::ctx (s/keys :req [::location ::server-url ::fixtures ::variables ::result/result]))

(def http-status->code
  {200 "okay"
   201 "created"
   204 "noContent"
   304 "notModified"
   400 "bad"
   403 "forbidden"
   404 "notFound"
   405 "methodNotAllowed"
   409 "conflict"
   410 "gone"
   412 "preconditionFailed"
   422 "unprocessable"})

(defn get-dynamic-fixture! [ctx id]
  (or (get-in ctx [::fixtures id ::operation/result])
      (throw (ex-info "Unknown fixture required" {::ctx ctx :fixture id}))))

(defn get-fixture-resource! [ctx id]
  (if-let [v (get-in ctx [::fixtures id])]
    (-> (or (::operation/result v) (::fixture/static-result v))
        ::fhir/resource)
    (throw (ex-info "Unknown fixture required" {::ctx ctx :fixture id}))))

(defn substitute-variable [ctx variable]
  (if-let [v (get-in ctx [::variables variable])]
    (cond
      (:defaultValue v) (:defaultValue v)
      (:headerField v) (get-in (get-dynamic-fixture! ctx (:sourceId v))
                               [:headers (:headerField v)])
      ;; TODO support XPath
      (:path v) (let [res (json-path/at-path (:path v)
                                             (get-fixture-resource! ctx (:sourceId v)))]
                  (cond
                    (nil? res) (throw (ex-info "Empty variable value" {::ctx ctx :variable variable}))
                    (coll? res) (throw (ex-info "Bad variable's value type" {::ctx ctx :variable variable
                                                                             :value res}))
                    :else res)
                  res))
    (throw (ex-info "Undefined variable" {::ctx ctx :variable variable}))))

(defn substitute-variables [ctx s]
  (let [buf (StringBuffer.)
        m (re-matcher #"\$\{([^{}$]+)\}" s)]
    (loop [vars {}]
      (if (.find m)
        (let [v (.group m 1)
              substitution (substitute-variable ctx v)]
          (.appendReplacement m buf (Matcher/quoteReplacement substitution))
          (recur (assoc vars v substitution)))
        (do
          (.appendTail m buf)
          [(str buf) vars])))))

(defn resolve-reference [ctx ref]
  ;; TODO support all reference types
  (let [path (io/file (.getParentFile (::location ctx)) (:reference ref))]
    (try
      ;; touchstone uses variables inside fixtures. but I can't find anything about it in spec
      (-> (slurp path) (#(first (substitute-variables ctx %))) (json/parse-string true))
      (catch Exception e
        (throw (ex-info "Can't resolve fixture" {::ctx ctx
                                                 :ref ref
                                                 :error (.getMessage e)
                                                 :error-info (ex-data e)}))))))

(defn type-from-fixture [ctx id]
  (:resourceType (get-fixture-resource! ctx id)))

(defn id-from-fixture [ctx id]
  (:id (get-fixture-resource! ctx id)))

(defn vid-from-fixture [ctx id]
  (-> (get-fixture-resource! ctx id) :meta :versionId))

(defn execute-operation* [ctx op]
  (let [resource (when (:sourceId op) (get-fixture-resource! ctx (:sourceId op)))
        method (case (-> op :type :code)
                 "create" :post
                 "update" :put
                 ("read" "vread" "search") :get
                 "delete" :delete
                 :get)
        resource-type (cond
                        (:targetId op) (type-from-fixture ctx (:targetId op))
                        (:sourceId op) (type-from-fixture ctx (:sourceId op))
                        (:resource op) (:resource op))
        vars (atom {})
        substitute-variables' (fn [v]
                                (let [[v' vars'] (substitute-variables ctx v)]
                                  (swap! vars merge vars')
                                  v'))
        id (cond
             (:targetId op) (id-from-fixture ctx (:targetId op))
             ;; this is not by spec, but touchstone relies on this
             (and (= "update" (-> op :type :code)) (:params op)) nil
             (:sourceId op) (id-from-fixture ctx (:sourceId op)))
        vid (cond
              (:targetId op) (vid-from-fixture ctx (:targetId op)))
        path (str "/"
                  (->> (keep identity [resource-type id])
                       (str/join "/")))
        path (cond
               (and (= "vread" (-> op :type :code))
                    (not (:params op))) (str path "/_history/" vid)
               :else path)
        path (str path (:params op))
        url (or (:url op)
                (str (::server-url ctx) path))
        resource (cond
                   (and (= "update" (-> op :type :code))
                        (:params op)
                        (nil? (:id resource)))
                   ;; FIXME very strange case but, for example, touchstone
                   ;; scripts expect this behavior. can be rewritten via parse
                   ;; url as route and extract type/id/vid from in general
                   ;; manner
                   (assoc resource :id (substitute-variables' (subs (:params op) 1)))
                   :else resource)
        request {:body (when resource (json/generate-string resource))
                 :method method
                 :url (substitute-variables' url)
                 :headers (into {"Content-Type" "application/json"
                                 "Accept" "application/json"}
                                (map (fn [{:keys [field value]}]
                                       [field (substitute-variables' value)])
                                     (:requestHeader op)))}
        res @(http/request request)]
    (when (:error res)
      (throw (ex-info "Error while executing operation" {::ctx ctx
                                                         :request request
                                                         :error (.getMessage (:error res))})))
    {::result/type ::result/operation
     ::operation/spec op
     ::result/resolved-variables @vars
     ::result/action-status (if (or (<= 200 (:status res) 299)
                                    (and (= :delete method) (= 404 (:status res))))
                              :success
                              :failed)
     ::operation/result
     (-> #::operation {:http-status (:status res)
                       :headers (:headers res)
                       ;; TODO support xml responses
                       ::self-http/request request}
         (cond-> (not-empty (:body res))
           (assoc ::fhir/resource (json/parse-string (:body res) true))))}))

(defn save-operation-in-fixtures [ctx op res]
  (if-let [id (:responseId op)]
    (-> ctx
        (assoc-in [::fixtures id ::operation/result] res)
        (assoc-in [::fixtures id ::fixture/type] ::fixture/dynamic))
    ctx))

(def content-type-mapping
  {"application/fhir+xml" :xml
   "application/fhir+json" :json
   "text/turtle" :ttl})

(defn execute-operation [ctx op]
  (let [res (execute-operation* ctx op)]
    (when (and (::exception-on-failure op)
               (= :failed (-> res :result :status)))
      (throw (ex-info "Failed operation" {::ctx ctx :failed-operation res})))
    [(save-operation-in-fixtures ctx op (::operation/result res)) res]))



(defn execute-operator [ctx operator expected value]
  (case operator
    "equals" (when (not= expected value) #::error{:msg "Not equals"
                                                  :data {:value value
                                                         :expected expected}})
    "notEquals" (when (= expected value) #::error{:msg "Equals"
                                                  :data {:value value}})
    "contains" (when (not (str/includes? value expected))
                 #::error{:msg "Not contains"
                          :data {:value value
                                 :expected expected}})
    "notContains" (when (str/includes? value expected)
                    #::error{:msg "Contains"
                             :data {:value value}})
    "in" (let [s (set (str/split expected #","))]
           (when (not (s (str value))) #::error{:msg "Not included"
                                                :data {:value value
                                                       :expected s}}))
    "notIn" (let [s (set (str/split expected #","))]
              (when (s value) #::error{:msg "Included"
                                       :data {:value value
                                              :expected s}}))
    "greaterThan" (when (not (< expected value)) #::error{:msg "Not greater"
                                                          :data {:value value
                                                                 :expected expected}})
    "lessThan" (when (not (< value expected)) #::error{:msg "Not less"
                                                       :data {:value value
                                                              :expected expected}})
    "empty" (when (not (empty? value)) #::error{:msg "Not empty"
                                                :data {:value value}})
    "notEmpty" (when (empty? value) #::error{:msg "Empty"
                                             :data {:value value}})
    (when (not= expected value) #::error{:msg "Not equal"
                                         :data {:value value
                                                :expected expected}})))

(defn raw-content-type [ct]
  (->> (str/split ct #";") (first)))

(defn assert-content-type [ctx last-result op]
  (let [v (-> last-result ::operation/headers :content-type (raw-content-type) (content-type-mapping :none))
        expected (keyword (:contentType op))]
    (execute-operator ctx (:operator op) expected v)))

(defn assert-header-field [ctx last-result op]
  (let [target (or (when-let [id (:sourceId op)] (get-dynamic-fixture! ctx id))
                   last-result)]
    (when-not target
      (throw (ex-info "Unknown target for assert" {::ctx ctx :assert op})))
    (execute-operator ctx (:operator op)
                      (:value op)
                      (get-in target [::operation/headers
                                      (-> (:headerField op) (str/lower-case) (keyword))]))))

(defn assert-minimum-id [ctx last-result op]
  (let [expected (get-fixture-resource! ctx (:minimumId op))
        v (-> last-result ::fhir/resource)
        compare (fn [expected value]
                  ;; FIXME simplified version. Should be recursive
                  (every? #(= (expected %) (value %)) (keys expected)))]
    (when (compare expected v)
      ;; TODO diff
      #::error{:msg "Not equal"})))

(defn get-bundle-links [ctx resource]
  (when-not (= "Bundle" (:resourceType resource))
    (throw (ex-info "Bundle resource expected" {::ctx ctx :type (:resourceType resource)})))
  (->> (map :relation (:link resource))
       (set)))

(defn assert-navigation-links [ctx last-result op]
  []
  (when-not last-result
    (throw (ex-info "Unknown target for assert" {::ctx ctx :assert op})))
  (let [links (get-bundle-links ctx (::fhir/resource last-result))]
    (when-not (set/subset? #{"next" "first" "last"} links)
      #:error{:msg "Not enough links"
              :data {:links links}})))

;; TODO xpath support
(defn assert-path [ctx last-result op]
  (let [target (or (when-let [id (:sourceId op)] (get-fixture-resource! ctx id))
                   (::fhir/resource last-result))
        coerce-expected (fn [expected value]
                          (cond
                            (integer? value) (Long. expected)
                            (float? value) (Double. expected)
                            :else expected))]
    (when-not target
      (throw (ex-info "Unknown target for assert" {::ctx ctx :assert op})))
    (let [value (json-path/at-path (:path op) target)
          expected (if-let [expected-path (:compareToSourcePath op)]
                     (let [expected-target (get-fixture-resource! ctx (:compareToSourceId op))]
                       (json-path/at-path expected-path expected-target))
                     (:value op))
          expected' (coerce-expected expected value)]
      (execute-operator ctx (:operator op) expected' value))))

(defn assert-resource [ctx last-result op]
  (execute-operator ctx (:operator op) (:resource op) (-> last-result ::fhir/resource :resourceType)))

(defn assert-response [ctx last-result op]
  (execute-operator ctx (:operator op)
                    (:response op)
                    (http-status->code (::operation/http-status last-result))))

(defn assert-response-code [ctx last-result op]
  (let [expected (try (Long. (:responseCode op)) (catch Exception e (:responseCode op)))]
    (execute-operator ctx (:operator op) expected (::operation/http-status last-result))))

(defn assert-validate-profile-id [ctx last-result op]
  ;; TODO profile assertion
  nil)

(def assertions
  {:contentType assert-content-type
   :headerField assert-header-field
   :minimumId assert-minimum-id
   :navigationLinks assert-navigation-links
   :path assert-path
   :resource assert-resource
   :response assert-response
   :responseCode assert-response-code
   :validateProfileId assert-validate-profile-id})

(defn execute-assert [ctx last-result op]
  (let [[value' vars] (when-let [v (:value op)] (substitute-variables ctx v))]
    (try
      (let [assert (some (fn [[k v]] (when (op k) v)) assertions)]
        (when-not assert
          (throw (ex-info "Unknown assertion" {::ctx ctx :assert op})))
        (if-let [err (assert ctx last-result (assoc op :value value'))]
          #::result{:type ::result/assert
                    :resolved-variables (or vars {})
                    :action-status :failed
                    ::assert/spec op
                    ::assert/result {::error/error err}}
          #::result{:type ::result/assert
                    :resolved-variables (or vars {})
                    :action-status :success
                    ::assert/spec op}))
      (catch Exception e
        #::result{:type ::result/assert
                  :resolved-variables (or vars {})
                  :action-status :failed
                  ::assert/spec op
                  ::assert/result {::error/error #::error{:msg (.getMessage e)
                                                          :data (dissoc (ex-data e) ::ctx)}}}))))

(defn execute-actions [ctx actions ignore-fails?]
  (loop [[action & actions] actions ctx ctx last-result nil results []]
    (if action
      (cond
        (:assert action)
        ;; TODO warningOnly support
        (let [res (execute-assert ctx last-result (:assert action))]
          (if (and (= :failed (-> res ::result/action-status)) (not ignore-fails?))
            [ctx (conj results res)]
            (recur actions ctx last-result (conj results res))))
        (:operation action)
        (let [[ctx' res] (execute-operation ctx (:operation action))]
          (if (and (= :failed (-> res ::result/action-status))
                   (not (:assert (first actions)))
                   (not ignore-fails?))
            [ctx' (conj results res)]
            (recur actions ctx' (::operation/result res) (conj results res)))))
      [ctx results])))

(defn load-fixture [ctx fixture]
  (when (get-in ctx [::fixtures (:id fixture)])
    (throw (ex-info "Already defined fixture with this id" {::ctx ctx :fixture fixture})))
  (let [resource (resolve-reference ctx (:resource fixture))
        ctx' (assoc-in ctx [::fixtures (:id fixture)] {::fixture/type ::fixture/static
                                                       ::fixture/static-result {::fhir/resource resource}
                                                       ::fixture/spec fixture})]
    (if (:autocreate fixture)
      (execute-operation ctx' {:type {:code "create"} :sourceId (:id fixture)
                               ::exception-on-failure true})
      ctx')))

(defn load-variables [ctx variables]
  (let [by-name (reduce (fn [res v] (assoc res (:name v) v)) {} variables)]
    (update-in ctx [::variables] #(merge by-name %))))

(defn load-fixtures [ctx fixtures]
  (reduce load-fixture ctx fixtures))

(defn execute-setup [ctx actions]
  (let [[ctx' res] (execute-actions ctx actions false)
        ctx' (assoc-in ctx' [::result/result ::result/setup] res)]
    (when (some #(= :failed (::result/action-status %)) res)
      (throw (ex-info "Error in setup" {::ctx ctx' :reason (:reason res)})))
    ctx'))

(defn execute-tests [ctx tests]
  (reduce (fn [ctx test]
            (let [[ctx' res] (execute-actions ctx (:action test) false)
                  status (if (every? #(= :success (::result/action-status %)) res)
                           :success
                           :failed)]
              (update-in ctx' [::result/result ::result/tests] conj
                         #::result{:test-name (:name test)
                                   :test-description (:description test)
                                   :test-status status
                                   :actions res})))
          ctx tests))

(defn execute-teardown [ctx actions]
  (let [[ctx' _] (execute-actions ctx actions true)]
    ctx'))

(defn unload-fixtures [ctx]
  (doseq [[id fixture] (::fixtures ctx)
          :when (:autodelete (::fixture/spec fixture))]
    (execute-operation* ctx {:type {:code "delete"} :targetId id}))
  ctx)

(defn generate-report [ctx]
  (let [status (if (every? #(= :success (-> % ::result/test-status)) (-> ctx ::result/result ::result/tests))
                 :success
                 :failed)]
    (assoc-in ctx [::result/result ::result/status] status)))

(defn random-variables []
  (let [rnd-str (fn [codes len]
                  (let [chars (map char codes)
                        password (take len (repeatedly #(rand-nth chars)))]
                    (reduce str "" password)))]
    (->> (for [[w codes] [["C" (concat (range 65 91) (range 97 123))]
                          ["D" (range 48 58)]
                          ["CD" (concat (range 48 58) (range 65 91) (range 97 123))]]
               d (range 1 21)]
           [(str w d) (rnd-str codes d)])
         (into {}))))

(defn execute-script [options]
  (let [f (io/file (:path options))
        ctx {::location f
             ::server-url (URL. (:server-url options))
             ::fixtures {}
             ::variables (into {}
                               (map (fn [[k v]]
                                      [(name k) {:name (name k)
                                                 :defaultValue v}])
                                    (:variables options)))
             ::result/result {::result/tests []}}
        body (json/parse-string (slurp f) true)]
    (try
      (-> ctx
          (load-variables (:variable body))
          (load-fixtures (:fixture body))
          (execute-setup (-> body :setup :action))
          (execute-tests (:test body))
          (execute-teardown (-> body :teardown :action))
          (unload-fixtures)
          (generate-report))
      (catch Exception e
        (if-let [ctx (::ctx (ex-data e))]
          (do (execute-teardown ctx (-> body :teardown :action))
              (unload-fixtures ctx)
              (-> ctx
                  (assoc-in [::result/result ::result/status] :failed)
                  (assoc-in [::result/result ::error/error] #::error{:msg (.getMessage e)
                                                                     :data (dissoc (ex-data e) ::ctx)})))
          (throw e))))))
