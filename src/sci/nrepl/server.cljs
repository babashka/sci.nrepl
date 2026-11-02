(ns sci.nrepl.server
  (:require [sci.core :as sci]
            [sci.ctx-store :as store]
            [sci.nrepl.completions :refer [completions]]
            [sci.nrepl.info :refer [info]]))

(defonce !last-ns
  (volatile! @sci/ns))

(defn- eval-forms [s on-value]
  (sci/binding [sci/ns @!last-ns]
    (let [rdr (sci/reader s)]
      (loop [res nil]
        (let [form (sci/parse-next (sci.ctx-store/get-ctx) rdr)]
          (if (= :sci.core/eof form)
            (do
              (vreset! !last-ns @sci/ns)
              res)
            (let [res (sci/eval-form (sci.ctx-store/get-ctx) form)]
              (on-value res)
              (recur res))))))))

(defn eval-string [s]
  (eval-forms s identity))

(defn nrepl-websocket []
  (.-ws_nrepl js/window))

(defn nrepl-reply [{:keys [id session send-fn]} {:keys [ns] :as payload}]
  (let [ns (or ns (str @!last-ns))
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

(defn handle-nrepl-eval [{:keys [code] :as msg}]
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
        (eval-forms code on-value))
      (if-let [promises (seq @!promises)]
        (-> (js/Promise.all (into-array promises))
            (.then done)
            (.catch fail))
        (done))
      (catch :default e
        (fail (or (ex-cause e) e))))))

(defn handle-nrepl-info [msg]
  (let [info (info (assoc msg :ctx (store/get-ctx)))]
    (nrepl-reply msg info)))

(declare ops)

(defn handle-close [request]
  (nrepl-reply request {:status ["done" "session-closed"]}))

(def ops
  "Operations supported by the nrepl server."
  {:eval handle-nrepl-eval
   :info handle-nrepl-info
   :eldoc handle-nrepl-info
   :lookup handle-nrepl-info
   :complete (fn [msg] (let [completions (completions (assoc msg :ctx (store/get-ctx)))]
                         (nrepl-reply msg completions)))
   :close handle-close})

(defn handle-nrepl-message [msg]
  (let [op (:op msg)]
    (if-let [handler (ops op)]
      (handler msg)
      (nrepl-reply msg (cond-> {:status ["done" "unknown-op" "error"]}
                         op (assoc :op (name op)))))))
