(ns sci.nrepl.server
  (:require [sci.core :as sci]
            [sci.ctx-store :as store]
            [sci.nrepl.completions :refer [completions]]
            [sci.nrepl.info :refer [info]]))

(defonce ^:private default-ns @sci/ns)

(defonce ^:private !session-ns
  (volatile! {}))

(defn- session-ns [session]
  (get @!session-ns session default-ns))

(defn- eval-forms [{:keys [session explicit-ns load-file]} s on-value]
  (sci/binding [sci/ns (or explicit-ns (session-ns session))]
    (let [rdr (sci/reader s)]
      (try
        (loop [res nil]
          (let [form (sci/parse-next (sci.ctx-store/get-ctx) rdr)]
            (if (= :sci.core/eof form)
              res
              (let [res (sci/eval-form (sci.ctx-store/get-ctx) form)]
                (on-value res)
                (recur res)))))
        (finally
          (when-not (or explicit-ns load-file)
            (vswap! !session-ns assoc session @sci/ns)))))))

(defn eval-string [s]
  (eval-forms nil s identity))

(defn nrepl-websocket []
  (.-ws_nrepl js/window))

(defn nrepl-reply [{:keys [id session send-fn] msg-ns :ns} {:keys [ns] :as payload}]
  (let [ns (or ns msg-ns (str (session-ns session)))
        reply (-> (assoc payload :id id :session session :ns ns)
                  (dissoc :ctx))]
    (binding [*print-readably* true
              *print-length* nil
              *print-level* nil
              *print-meta* false]
      (if send-fn
        (send-fn reply)
        (.send (nrepl-websocket) (str reply))))))

(defn- error-name [e]
  (cond (instance? ExceptionInfo e) "ExceptionInfo"
        (instance? js/Error e) (.-name e)
        :else "Error"))

(defn- eval-code [{:keys [code] :as msg}]
  (let [!promises (volatile! [])
        done #(nrepl-reply msg {:status ["done"]})
        fail (fn [e]
               (nrepl-reply msg {:err (str (or (ex-message e) (str e)) "\n")})
               (nrepl-reply msg {:ex (error-name e)
                                 :status ["eval-error"]})
               (done))
        on-value (fn [v]
                   (if (instance? js/Promise v)
                     (vswap! !promises conj
                             (.then v #(nrepl-reply msg {:value (str "#<Promise " (pr-str %) ">")})))
                     (nrepl-reply msg {:value (pr-str v)
                                       :ns (str @sci/ns)})))]
    (try
      (sci/binding [sci/print-fn #(nrepl-reply msg {:out %})
                    sci/print-err-fn #(nrepl-reply msg {:err %})
                    sci/print-newline true]
        (eval-forms msg code on-value))
      (if-let [promises (seq @!promises)]
        (-> (js/Promise.all (into-array promises))
            (.then done)
            (.catch fail))
        (done))
      (catch :default e
        (fail (or (ex-cause e) e))))))

(defn handle-nrepl-eval [{:keys [ns] :as msg}]
  (let [explicit-ns (some->> ns symbol (sci/find-ns (store/get-ctx)))]
    (if (and ns (nil? explicit-ns))
      (nrepl-reply msg {:status ["error" "namespace-not-found" "done"]})
      (eval-code (assoc msg :explicit-ns explicit-ns)))))

(defn- with-ctx-and-ns [{:keys [ns session] :as msg}]
  (assoc msg
         :ctx (store/get-ctx)
         :ns (or ns (str (session-ns session)))))

(defn handle-nrepl-info [msg]
  (nrepl-reply msg (info (with-ctx-and-ns msg))))

(defn- handle-complete [msg]
  (nrepl-reply msg (completions (with-ctx-and-ns msg))))

(declare ops)

(defn- handle-clone [{:keys [session new-session]}]
  (vswap! !session-ns assoc new-session (session-ns session)))

(defn handle-close [{:keys [session] :as request}]
  (vswap! !session-ns dissoc session)
  (nrepl-reply request {:status ["done" "session-closed"]}))

(def ops
  "Operations supported by the nrepl server."
  {:eval handle-nrepl-eval
   :info handle-nrepl-info
   :eldoc handle-nrepl-info
   :lookup handle-nrepl-info
   :complete handle-complete
   :clone handle-clone
   :close handle-close})

(defn handle-nrepl-message [msg]
  (let [op (:op msg)]
    (if-let [handler (ops op)]
      (handler msg)
      (nrepl-reply msg (cond-> {:status ["done" "unknown-op" "error"]}
                         op (assoc :op (name op)))))))
