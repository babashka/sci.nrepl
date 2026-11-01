;; Tests eval replies from the page. Run: bb test:cljs
(ns sci.nrepl.server-test
  (:require
   [cljs.test :as t :refer [async deftest is]]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [sci.core :as sci]
   [sci.ctx-store :as store]
   [sci.nrepl.server :as server]))

(enable-console-print!)
(sci/alter-var-root sci/print-fn (constantly *print-fn*))
(store/reset-ctx! (sci/init {:classes {'js js/globalThis :allow :all}}))

(defn- eval-code [code on-done]
  (let [!replies (atom [])]
    (server/handle-nrepl-eval
     {:code code
      :id "1"
      :session "s"
      :send-fn (fn [reply]
                 (let [reply (edn/read-string (str reply))]
                   (swap! !replies conj reply)
                   (when (some #{"done"} (:status reply))
                     (on-done @!replies))))})))

(defn- collect [k replies]
  (keep k replies))

(defn- output [k replies]
  (str/join (collect k replies)))

(deftest eval-replies-with-a-value-per-form-test
  (async done
    (eval-code "(println \"hi\") (+ 1 2) (+ 3 4)"
               (fn [replies]
                 (is (= "hi\n" (output :out replies)))
                 (is (= ["nil" "3" "7"] (collect :value replies)))
                 (is (= ["done"] (:status (peek replies))))
                 (done)))))

(deftest out-reply-from-println-is-readable-edn-test
  (async done
    (eval-code "(println \"hello world\")"
               (fn [replies]
                 (is (= "hello world\n" (output :out replies)))
                 (is (every? #(= "1" (:id %)) replies))
                 (done)))))

(deftest out-reply-ignores-print-length-and-print-level-test
  (async done
    (eval-code "(binding [*print-length* 1 *print-level* 1] (println \"hi\"))"
               (fn [replies]
                 (is (= "hi\n" (output :out replies)))
                 (is (every? #(= "s" (:session %)) replies))
                 (done)))))

(deftest output-precedes-the-value-of-its-form-test
  (async done
    (eval-code "(print \"a\") 1 (print \"b\") 2"
               (fn [replies]
                 (is (= ["a" "nil" "1" "b" "nil" "2"]
                        (keep #(or (:out %) (:value %)) replies)))
                 (done)))))

(deftest lazy-last-value-prints-to-out-test
  (async done
    (eval-code "(map println [1])"
               (fn [replies]
                 (is (= "1\n" (output :out replies)))
                 (is (= ["(nil)"] (collect :value replies)))
                 (done)))))

(deftest print-err-fn-replies-with-err-test
  (async done
    (eval-code "(*print-err-fn* \"oops\")"
               (fn [replies]
                 (is (= "oops" (output :err replies)))
                 (is (= "" (output :out replies)))
                 (done)))))

(deftest value-replies-carry-the-current-ns-test
  (async done
    (eval-code "(ns foo) 1 (ns user) 2"
               (fn [replies]
                 (is (= ["foo" "foo" "user" "user"]
                        (map :ns (filter :value replies))))
                 (done)))))

(deftest throwing-form-ends-eval-test
  (async done
    (eval-code "1 (throw (js/Error. \"boom\")) 2"
               (fn [replies]
                 (is (= ["1"] (collect :value replies)))
                 (is (str/includes? (output :err replies) "boom"))
                 (is (= ["error" "done"] (:status (peek replies))))
                 (done)))))

(deftest resolved-promise-replies-before-done-test
  (async done
    (eval-code "1 (js/Promise.resolve 2)"
               (fn [replies]
                 (is (= ["1" "#<Promise 2>"] (collect :value replies)))
                 (is (= ["done"] (:status (peek replies))))
                 (done)))))

(deftest rejected-promise-replies-with-error-test
  (async done
    (eval-code "(js/Promise.reject (js/Error. \"boom\"))"
               (fn [replies]
                 (is (str/includes? (output :err replies) "boom"))
                 (is (= ["error" "done"] (:status (peek replies))))
                 (done)))))

(deftest print-fn-is-restored-after-eval-test
  (let [print-fn @sci/print-fn]
    (async done
      (eval-code "(println \"hi\")"
                 (fn [_]
                   (is (identical? print-fn @sci/print-fn))
                   (done))))))

(deftest eval-string-returns-the-last-value-test
  (is (= 7 (server/eval-string "1 (+ 3 4)"))))

(defn- message-replies [msg]
  (let [!replies (atom [])]
    (server/handle-nrepl-message
     (assoc msg :id "1" :session "s" :send-fn #(swap! !replies conj %)))
    @!replies))

(deftest unknown-op-replies-with-unknown-op-status-test
  (let [replies (message-replies {:op :frobnicate :code "(+ 1 2)"})
        reply (first replies)]
    (is (= 1 (count replies)))
    (is (= ["done" "unknown-op" "error"] (:status reply)))
    (is (= "frobnicate" (:op reply)))
    (is (= "1" (:id reply)))
    (is (not (contains? reply :code)))
    (is (not (contains? reply :err)))))

(deftest message-without-op-replies-with-unknown-op-status-test
  (let [reply (first (message-replies {}))]
    (is (= ["done" "unknown-op" "error"] (:status reply)))
    (is (not (contains? reply :op)))))

(defmethod t/report [::t/default :end-run-tests] [m]
  (when-not (t/successful? m)
    (set! (.-exitCode js/process) 1)))

(defn -main []
  (t/run-tests 'sci.nrepl.server-test))

(set! *main-cli-fn* -main)
