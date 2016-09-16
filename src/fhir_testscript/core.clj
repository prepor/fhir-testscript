(ns fhir-testscript.core
  (:gen-class)
  (:require [cheshire.core :as json]
            [clojure
             [set :as set]
             [string :as str]]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [org.httpkit.client :as http])
  (:import java.net.URL
           java.util.regex.Matcher))

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

;; (defn make-url
;;   [ctx path]
;;   (let [url (:server-url ctx)
;;         base-path (.getPath url)
;;         path' (io/file base-path path)
;;         url' (URL. (.getProtocol url) (.getHost url) (.getPort url) path')]
;;     (str url')))

(defn get-executed-fixture [ctx id]
  (if-let [v (get-in ctx [:fixtures id])]
    (if (:res v)
      (:res v)
      (throw (ex-info "Unloaded fixture required" {::ctx ctx :fixture id})))
    (throw (ex-info "Unknown fixture required" {::ctx ctx :fixture id}))))

(defn substitute-variable [ctx variable]
  (if-let [v (get-in ctx [:variables variable])]
    (cond
      (:defaultValue v) (:defaultValue v)
      (:headerField v) (get-in (get-executed-fixture ctx (:sourceId v))
                               [:headers (:headerField v)])
      ;; TODO support XPath
      (:path v) (let [res (json-path/at-path (:path v)
                                             (:resource (get-executed-fixture ctx (:sourceId v))))]
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
    (loop []
      (when (.find m)
        (let [v (.group m 1)]
          (.appendReplacement m buf (Matcher/quoteReplacement (substitute-variable ctx v))))
        (recur)))
    (.appendTail m buf)
    (str buf)))

(defn resolve-reference [ctx ref]
  ;; TODO support all reference types
  (let [path (io/file (.getParentFile (:location ctx)) (:reference ref))]
    (try
      ;; variables inside fixtures uses touchstone. but I can't find anything about it in spec
      (-> (slurp path) (#(substitute-variables ctx %)) (json/parse-string true))
      (catch Exception e
        (throw (ex-info "Can't resolve fixture" {::ctx ctx
                                                 :ref ref
                                                 :error (.getMessage e)
                                                 :error-info (ex-data e)}))))))

;; read vread
;; update create
;; search delete

(defn type-from-fixture [ctx id]
  (get-in ctx [:fixtures id :res :resource :resourceType]))

(defn id-from-fixture [ctx id]
  (get-in ctx [:fixtures id :res :resource :id]))

(defn vid-from-fixture [ctx id]
  (get-in ctx [:fixtures id :res :resource :meta :versionId]))

(defn execute-operation* [ctx op]
  (let [resource (when (:sourceId op) (:resource (get-executed-fixture ctx (:sourceId op))))
        method (case (-> op :type :code)
                 "create" :post
                 "update" :put
                 ("read" "vread" "search") :get
                 "delete" :delete)
        resource-type (cond
                        (:targetId op) (type-from-fixture ctx (:targetId op))
                        (:sourceId op) (type-from-fixture ctx (:sourceId op))
                        (:resource op) (:resource op))
        id (cond
             (:targetId op) (id-from-fixture ctx (:targetId op))
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
                (str (:server-url ctx) path))
        resource (cond
                   (and (= "update" (-> op :type :code))
                        (:params op)
                        (nil? (:id resource)))
                   ;; FIXME very strange case but, for example, touchstone
                   ;; scripts expect this behavior. can be rewritten via parse
                   ;; url as route and extract type/id/vid from in general
                   ;; manner
                   (assoc resource :id (substitute-variables ctx (subs (:params op) 1)))
                   :else resource)
        request {:body (when resource (json/generate-string resource))
                 :method method
                 :url (substitute-variables ctx url)
                 :headers (into {"Content-Type" "application/json"
                                 "Accept" "application/json"}
                                (map (fn [{:keys [field value]}]
                                       [field (substitute-variables ctx value)])
                                     (:requestHeader op)))}
        res @(http/request request)]
    (when (:error res)
      (throw (ex-info "Error while executing operation" {::ctx ctx
                                                         :request request
                                                         :error (.getMessage (:error res))})))
    {:status (if (or (<= 200 (:status res) 299)
                     (and (= :delete method) (= 404 (:status res))))
               :success
               :failed)
     :http-status (:status res)
     :headers (:headers res)
     ;; TODO support xml responses
     :request request
     :resource (json/parse-string (:body res) true)}))

(defn save-operation-in-fixtures [ctx op res]
  (if-let [id (:responseId op)]
    (assoc-in ctx [:fixtures id :res] res)
    ctx))

(def content-type-mapping
  {"application/fhir+xml" :xml
   "application/fhir+json" :json
   "text/turtle" :ttl})

(defn execute-operation [ctx op]
  (let [res (execute-operation* ctx op)]
    (when (and (::exception-on-failure op)
               (= :failed (:status res)))
      (throw (ex-info "Failed operation" {::ctx ctx :failed-operation res})))
    (prn "---RES" op res)
    [(save-operation-in-fixtures ctx op res) res]))

(defn execute-operator [ctx operator expected value]
  (case operator
    "equals" (when (not= expected value) {:type :not-equals
                                          :value value
                                          :expected expected})
    "notEquals" (when (= expected value) {:type :equals
                                          :value value})
    "contains" (when (not (str/includes? value expected))
                 {:type :not-contains
                  :value value
                  :expected expected})
    "notContains" (when (str/includes? value expected)
                    {:type :contains
                     :value value})
    "in" (let [s (set (str/split expected #","))]
           (when (not (s value)) {:type :not-included
                                  :value value
                                  :expected s}))
    "notIn" (let [s (set (str/split expected #","))]
              (when (s value) {:type :included
                               :value value
                               :expected s}))
    "greaterThan" (when (not (< value expected)) {:type :not-greater
                                                  :value value
                                                  :expected expected})
    "lessThan" (when (not (< expected value)) {:type :not-less
                                               :value value
                                               :expected expected})
    "empty" (when (not (empty? value)) {:type :not-empty
                                        :value value})
    "notEmpty" (when (empty? value) {:type :empty
                                     :value value})
    (when (not= expected value) {:type :not-equals
                                 :value value
                                 :expected expected})))

(defn assert-content-type [ctx last-result op]
  (let [v (-> last-result :headers :content-type (content-type-mapping :none))
        expected (keyword (:contentType op))]
    (execute-operator ctx (:operator op) expected v)))

(defn assert-header-field [ctx last-result op]
  (let [target (or (when-let [id (:sourceId op)] (get-executed-fixture ctx id))
                   last-result)]
    (when-not target
      (throw (ex-info "Unknown target for assert" {::ctx ctx :assert op})))
    (execute-operator ctx (:operator op)
                      (:value op)
                      (get-in target [:headers (:headerField op)]))))

(defn assert-minimum-id [ctx last-result op]
  (let [expected (-> (get-executed-fixture ctx (:minimumId op))
                     :resource
                     (dissoc :id))
        v (-> last-result
              :resource
              (dissoc :id))]
    (when (not= expected v)
      ;; TODO diff
      {:type :not-equal})))

(defn get-bundle-links [ctx resource]
  (when-not (= "Bundle" (:resourceType resource))
    (throw (ex-info "Bundle resource expected" {::ctx ctx :type (:resourceType resource)})))
  (->> (map :relation (:link resource))
       (set)))

(defn assert-navigation-links [ctx last-result op]
  []
  (when-not last-result
    (throw (ex-info "Unknown target for assert" {::ctx ctx :assert op})))
  (let [links (get-bundle-links ctx (:resource last-result))]
    (when-not (set/subset? #{"next" "first" "last"} links)
      {:type :not-enough-links
       :links links})))

;; TODO xpath support
(defn assert-path [ctx last-result op]
  (let [target (or (when-let [id (:sourceId op)] (get-executed-fixture ctx id))
                   last-result)
        coerce-expected (fn [expected value]
                          (cond
                            (integer? value) (Long. expected)
                            (float? value) (Double. expected)
                            :else expected))]
    (when-not target
      (throw (ex-info "Unknown target for assert" {::ctx ctx :assert op})))
    (let [value (json-path/at-path (:path op) (:resource target))
          expected (if-let [expected-path (:compareToSourcePath op)]
                     (let [expected-target (get-executed-fixture ctx (:compareToSourceId op))]
                       (json-path/at-path expected-path (:resource expected-target)))
                     (:value op))
          expected' (coerce-expected expected value)]
      (execute-operator ctx (:operator op) expected' value))))

(defn assert-resource [ctx last-result op]
  (execute-operator ctx (:operator op) (:resource op) (-> last-result :resource :resourceType)))

(defn assert-response [ctx last-result op]
  (execute-operator ctx (:operator op)
                    (:response op)
                    (http-status->code (:http-status last-result))))

(defn assert-response-code [ctx last-result op]
  (execute-operator ctx (:operator op) (Long. (:responseCode op)) (:http-status last-result)))

(defn assert-validate-profile-id [ctx last-result op]
  ;; TODO profile assertion
  (assert false))

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
  (try
    (let [assert (some (fn [[k v]] (when (op k) v)) assertions)]
      (when-not assert
        (throw (ex-info "Unknown assertion" {::ctx ctx :assert op})))
      (assert ctx last-result (update-in op [:value] #(when % (substitute-variables ctx %)))))
    (catch Exception e
      (assoc (dissoc (ex-data e) ::ctx)
             :msg (.getMessage e)))))

(defn execute-actions [ctx actions ignore-fails?]
  (loop [[action & actions] actions ctx ctx last-result nil]
    (if action
      (cond
        (:assert action)
        ;; TODO warningOnly support
        (if-let [err (and (not ignore-fails?) (execute-assert ctx (:assert action) last-result))]
          [ctx
           {:status :failed
            :reason {:type :assert-failed
                     :assert action
                     :data err}}]
          (recur actions ctx last-result))

        (:operation action)
        (let [[ctx' res] (execute-operation ctx (:operation action))]
          (if (and (= :failed (:status res)) (not (:assert (first actions))) (not ignore-fails?))
            [ctx'
             {:status :failed
              :reason {:type :operation-failed-without-assert
                       :data res}}]
            (recur actions ctx' res))))
      [ctx {:status :success}])))

(defn load-fixture [ctx fixture]
  (when (get-in ctx [:fixtures (:id fixture)])
    (throw (ex-info "Already defined fixture with this id" {::ctx ctx :fixture fixture})))
  (let [resource (resolve-reference ctx (:resource fixture))
        ctx' (assoc-in ctx [:fixtures (:id fixture)] {:res {:resource resource}
                                                      :spec fixture})]
    (if (:autocreate fixture)
      (execute-operation ctx' {:type {:code "create"} :sourceId (:id fixture)
                               ::exception-on-failure true})
      ctx')))

(defn load-variables [ctx variables]
  (let [by-name (reduce (fn [res v] (assoc res (:name v) v)) {} variables)]
    (update-in ctx [:variables] #(merge by-name %))))

(defn load-fixtures [ctx fixtures]
  (reduce load-fixture ctx fixtures))

(defn execute-setup [ctx actions]
  (let [[ctx' res] (execute-actions ctx actions false)]
    (when (= :failed (:status res))
      (throw (ex-info "Error in setup" {::ctx ctx :reason (:reason res)})))
    ctx'))

(defn execute-tests [ctx tests]
  (reduce (fn [ctx test]
            (let [[ctx' res] (execute-actions ctx (:action test) false)]
              (update-in ctx' [:results] conj {:id (:id test)
                                               :name (:name test)
                                               :result res})))
          ctx tests))

(defn execute-teardown [ctx actions]
  (execute-actions ctx actions true))

(defn unload-fixtures [ctx]
  (doseq [[id fixture] (:fixtures ctx)
          :when (:autodelete (:spec fixture))]
    (execute-operation* ctx {:type {:code "delete"} :targetId id}))
  ctx)

(defn generate-report [ctx]
  ctx)

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
        ctx {:location f
             :server-url (URL. (:server-url options))
             :fixtures {}
             :variables (into {}
                              (map (fn [[k v]]
                                     [(name k) {:name (name k)
                                                :defaultValue v}])
                                   (:variables options)))
             :results []}
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
      ;; (catch Exception e
      ;;   (when-let [ctx (::ctx (ex-data e))]
      ;;     (execute-teardown ctx (-> body :teardown :action))
      ;;     (unload-fixture ctx))
      ;;   (log/error e "Error while executing script" (:path options))
      ;;   {:status :skipped
      ;;    :reason {:msg (.getMessage e)
      ;;             :data (dissoc (ex-data e) ::ctx)}})
      )))
