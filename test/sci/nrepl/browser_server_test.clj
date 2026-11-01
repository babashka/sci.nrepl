;; Tests reply routing in the relay. Run: bb test
(ns sci.nrepl.browser-server-test
  (:require
   [bencode.core :as bencode]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [sci.nrepl.browser-server :as server])
  (:import
   [java.io PushbackInputStream]
   [java.net Socket SocketTimeoutException]))

(def ^:private !page-inbox (atom []))

(defn- page-send! [msg]
  (swap! !page-inbox conj msg))

(defn- page-reply! [{:keys [id session]} response]
  (server/send-response {:id id :session session :response response}))

(defn- server-port []
  (.getLocalPort ^java.net.ServerSocket @server/!socket))

(use-fixtures :each
  (fn [f]
    (reset! !page-inbox [])
    (server/start-nrepl-server! {:port 0 :send-fn page-send! :quiet true})
    (while (or (nil? @server/!socket) (zero? (server-port)))
      (Thread/sleep 1))
    (try (f)
         (finally (server/stop-nrepl-server!)))))

(defn- connect []
  (let [socket (Socket. "localhost" (int (server-port)))]
    (.setSoTimeout socket 1000)
    {:socket socket
     :in (PushbackInputStream. (.getInputStream socket))
     :out (.getOutputStream socket)}))

(defn- send! [{:keys [out]} msg]
  (bencode/write-bencode out msg)
  (.flush ^java.io.OutputStream out))

(defn- string-value [x]
  (cond (bytes? x) (String. ^bytes x)
        (sequential? x) (mapv string-value x)
        :else x))

(defn- recv [{:keys [in]}]
  (try
    (update-vals (bencode/read-bencode in) string-value)
    (catch SocketTimeoutException _ ::timeout)))

(defn- await-page-message [id]
  (loop [n 0]
    (or (first (filter #(= id (:id %)) @!page-inbox))
        (when (< n 1000)
          (Thread/sleep 1)
          (recur (inc n))))))

(defn- await-page-message-count [n]
  (loop [i 0]
    (when (and (< (count @!page-inbox) n) (< i 1000))
      (Thread/sleep 1)
      (recur (inc i)))))

(defn- pending? [id]
  (some (fn [[[_ pending-id]]] (= id pending-id)) @@#'server/!pending))

(defn- await-pending-cleared [id]
  (loop [n 0]
    (when (and (pending? id) (< n 1000))
      (Thread/sleep 1)
      (recur (inc n)))))

(deftest replies-go-to-the-requesting-connection-test
  (let [a (connect)
        b (connect)]
    (testing "a reply reaches b after a sent the last eval"
      (send! a {"op" "eval" "code" "(+ 1 2)" "id" "from-a"})
      (let [msg (await-page-message "from-a")]
        (page-reply! msg {:value "3"})
        (page-reply! msg {:status ["done"]}))
      (is (= "3" (get (recv a) "value")))
      (is (= ["done"] (get (recv a) "status")))
      (send! b {"op" "complete" "prefix" "inc" "id" "from-b"})
      (page-reply! (await-page-message "from-b") {:status ["done"]})
      (is (= "from-b" (get (recv b) "id"))))
    (testing "interleaved replies reach each connection"
      (send! a {"op" "eval" "code" "1" "id" "slow-a"})
      (send! b {"op" "lookup" "sym" "inc" "id" "slow-b"})
      (let [slow-a (await-page-message "slow-a")
            slow-b (await-page-message "slow-b")]
        (page-reply! slow-b {:status ["done"]})
        (page-reply! slow-a {:status ["done"]}))
      (is (= "slow-a" (get (recv a) "id")))
      (is (= "slow-b" (get (recv b) "id"))))
    (.close ^Socket (:socket a))
    (.close ^Socket (:socket b))))

(deftest equal-ids-in-two-sessions-reach-their-own-connections-test
  (let [a (connect)
        b (connect)]
    (send! a {"op" "eval" "code" "1" "id" "1" "session" "session-a"})
    (send! b {"op" "eval" "code" "2" "id" "1" "session" "session-b"})
    (await-page-message-count 2)
    (page-reply! {:id "1" :session "session-b"} {:value "2" :status ["done"]})
    (page-reply! {:id "1" :session "session-a"} {:value "1" :status ["done"]})
    (is (= "1" (get (recv a) "value")))
    (is (= "2" (get (recv b) "value")))
    (.close ^Socket (:socket a))
    (.close ^Socket (:socket b))))

(deftest done-status-ends-request-test
  (let [a (connect)]
    (send! a {"op" "eval" "code" "1" "id" "request"})
    (let [msg (await-page-message "request")]
      (page-reply! msg {:status ["done"]})
      (is (not (pending? "request")))
      (testing "a reply after done is dropped"
        (page-reply! msg {:value "late"})
        (is (= ["done"] (get (recv a) "status")))
        (is (= ::timeout (recv a)))))
    (.close ^Socket (:socket a))))

(deftest classpath-op-replies-with-an-empty-classpath-test
  (let [a (connect)]
    (send! a {"op" "classpath" "id" "classpath-request"})
    (let [reply (recv a)]
      (is (= [] (get reply "classpath")))
      (is (= ["done"] (get reply "status")))
      (is (= "classpath-request" (get reply "id"))))
    (is (empty? (filter #(= "classpath-request" (:id %)) @!page-inbox)))
    (.close ^Socket (:socket a))))

(deftest describe-lists-the-classpath-op-test
  (let [a (connect)]
    (send! a {"op" "describe" "id" "describe-request"})
    (is (contains? (get (recv a) "ops") "classpath"))
    (.close ^Socket (:socket a))))

(deftest closed-connection-drops-pending-requests-test
  (let [a (connect)]
    (send! a {"op" "eval" "code" "1" "id" "abandoned"})
    (let [msg (await-page-message "abandoned")]
      (is (pending? "abandoned"))
      (.close ^Socket (:socket a))
      (await-pending-cleared "abandoned")
      (is (not (pending? "abandoned")))
      (testing "a reply to a closed connection is dropped"
        (is (nil? (page-reply! msg {:status ["done"]})))))))
