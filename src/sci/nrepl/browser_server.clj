(ns sci.nrepl.browser-server
  (:require
   [bencode.core :as bencode]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [org.httpkit.server :as httpkit])
  (:import
   [java.io PushbackInputStream EOFException BufferedOutputStream]
   [java.net ServerSocket]))

(set! *warn-on-reflection* true)

(defn- coerce-bencode [x]
  (if (bytes? x)
    (String. ^bytes x)
    x))

(defn- read-bencode [in]
  (try (let [msg (bencode/read-bencode in)
             msg (zipmap (map keyword (keys msg))
                         (map coerce-bencode (vals msg)))]
         msg)
       (catch Exception e
         #_(def e e)
         (throw e))))

(defonce ^:private !pending
  (atom {}))

(defn- forget-connection! [out]
  (swap! !pending #(into {} (remove (fn [[_ pending-out]] (identical? out pending-out))) %)))

(defn send-response [{:keys [out id session response]}]
  (let [request [session id]
        out (or out (get @!pending request))
        response (cond-> response
                   id (assoc :id id)
                   session (assoc :session session))]
    (when (some #{"done"} (:status response))
      (swap! !pending dissoc request))
    (when out
      (bencode/write-bencode out response)
      (.flush ^java.io.OutputStream out))))

(defonce ^:private !sessions
  (atom #{}))

(defn- handle-clone [ctx]
  (let [id (str (java.util.UUID/randomUUID))]
    (swap! !sessions conj id)
    (send-response (assoc ctx
                          :response {"new-session" id "status" ["done"]}))))

(defonce nrepl-channel (atom nil))

(defn- response-handler [message]
  (let [{:as msg :keys [id session]} (edn/read-string message)]
    (send-response {:id id
                    :session session
                    :response (dissoc msg :id :session)})))

(defn- websocket-send! [msg]
  (when-let [chan @nrepl-channel]
    (httpkit/send! chan (str msg))))

(defn- reply-error [ctx message]
  (send-response (assoc ctx :response {:err message}))
  (send-response (assoc ctx :response {:status ["done" "error"]})))

(defn- fail-pending! []
  (doseq [[[session id] out] @!pending]
    (reply-error {:out out :id id :session session} "The page disconnected.\n")))

(defn- forward! [{:as ctx :keys [out id session send-fn] :or {send-fn websocket-send!}} msg]
  (swap! !pending assoc [session id] out)
  (when-not (send-fn (assoc msg :id id :session session))
    (reply-error ctx "No page is connected.\n")))

(defn- handle-eval [{:as ctx :keys [msg]}]
  (let [code (get msg :code)]
    (if (or (str/includes? code "clojure.main/repl-requires")
            (str/includes? code "System/getProperty"))
      (do
        (send-response (assoc ctx :response {"value" "nil"}))
        (send-response (assoc ctx :response {"status" ["done"]})))
      (forward! ctx {:op :eval
                     :code code
                     :ns (get msg :ns)}))))

(defn- handle-load-file [ctx]
  (let [msg (get ctx :msg)
        code (get msg :file)
        msg (assoc msg :code code)]
    (handle-eval (assoc ctx :msg msg))))

(defn- handle-complete [{:as ctx :keys [msg]}]
  (forward! ctx {:op :complete
                 :symbol (get msg :symbol)
                 :prefix (get msg :prefix)
                 :ns (get msg :ns)}))

(defn- generically-handle-on-server [{:as ctx :keys [op msg]}]
  (forward! ctx (assoc msg :op op)))

(defn- handle-describe [ctx]
  (let [response {"versions" {"sci-nrepl" {"major" "0"
                                           "minor" "0"
                                           "incremental" "1"}}
                  "ops" (zipmap
                         (map
                          name
                          [:eval
                           :info
                           :eldoc
                           :lookup
                           :complete
                           :close :clone :load-file :classpath
                           :describe :ls-sessions])
                         (repeat {}))
                  "aux" {"cwd" (System/getProperty "user.dir")}
                  :status ["done"]}]
    (send-response (assoc ctx :response response))))

(defn- session-loop [in out {:keys [opts]}]
  (loop []
    (when-let [msg (try
                     (let [msg (read-bencode in)]
                       msg)
                     (catch EOFException _
                       (when-not (:quiet opts)
                         (println "Client closed connection."))))]
      (let [ctx (cond-> {:out out :msg msg}
                  (:send-fn opts)
                  (assoc :send-fn (:send-fn opts)))
            id (get msg :id)
            session (get msg :session)
            ctx (assoc ctx :id id :session session)
            op (keyword (get msg :op))]
        (case op
          :clone (handle-clone ctx)
          :eval (handle-eval ctx)
          :describe (handle-describe ctx)
          :load-file (handle-load-file ctx)
          :complete (handle-complete ctx)
          :classpath (send-response (assoc ctx :response {"classpath" [] "status" ["done"]}))
          :ls-sessions (send-response (assoc ctx :response {"sessions" (vec @!sessions) "status" ["done"]}))
          :close (do (swap! !sessions disj session)
                     (generically-handle-on-server (assoc ctx :op op)))
          (generically-handle-on-server (assoc ctx :op op))))
      (recur))))

(defn- listen [^ServerSocket listener {:as opts}]
  (println (str "nREPL server started on port " (:port opts) "..."))
  (let [client-socket (.accept listener)
        in (.getInputStream client-socket)
        in (PushbackInputStream. in)
        out (.getOutputStream client-socket)
        out (BufferedOutputStream. out)]
    (future
      (try
        (session-loop in out {:opts opts})
        (finally
          (forget-connection! out))))
    (recur listener opts)))

(defonce !socket (atom nil))

(defn start-nrepl-server! [{:keys [port] :as opts}]
  (let [port (or port 1339)
        inet-address (java.net.InetAddress/getByName "localhost")
        socket (new ServerSocket port 0 inet-address)
        _ (reset! !socket socket)]
    (future (listen socket opts))))

(defn stop-nrepl-server! []
  (when-let [socket @!socket]
    (.close ^ServerSocket socket)
    (reset! !socket nil)))

(defn- create-channel [req]
  (httpkit/as-channel req
                      {:on-open (fn [ch]
                                  (reset! nrepl-channel ch))
                       :on-close (fn [ch _reason]
                                   (prn :close)
                                   (when (identical? ch @nrepl-channel)
                                     (fail-pending!)))
                       :on-receive
                       (fn [_ch message]
                         (prn :msg message)
                         (response-handler message))}))

(defn- app [{:as req}]
  (when (:websocket? req)
    (case (:uri req)
      "/_nrepl"
      (create-channel req))))

(defonce ^:private !server
  (atom nil))

(defn halt! []
  (when-let [{:keys [port stop-fn]} @!server]
    (stop-fn)
    (println (str "Webserver running on " port ", stopped."))
    (reset! !server nil)))

(defn start-websocket-server! [{:keys [port]}]
  (let [port (or port 1340)]
    (halt!)
    (try
      (reset! !server {:port port :stop-fn (httpkit/run-server #'app {:port port})})
      (println (str "Websocket server started on " port "..."))
      (catch Exception #_java.net.BindException e ;; TODO, add BindException to bb, done for 0.8.3
             (println "Port " port " not available, server not started!")
             (println (.getMessage e))))))

(defn start!
  [{:keys [nrepl-port websocket-port]
    :or {nrepl-port 1339
         websocket-port 1340}}]
  (start-nrepl-server! {:port nrepl-port})
  (start-websocket-server! {:port websocket-port}))
