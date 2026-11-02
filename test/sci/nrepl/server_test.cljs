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

(defn- is-eval-error [replies err ex]
  (is (= err (output :err replies)))
  (is (= [ex] (collect :ex replies)))
  (is (= [["eval-error"] ["done"]] (collect :status replies))))

(deftest throwing-form-ends-eval-test
  (async done
    (eval-code "1 (throw (js/Error. \"boom\")) 2"
               (fn [replies]
                 (is (= ["1"] (collect :value replies)))
                 (is-eval-error replies "boom\n" "Error")
                 (done)))))

(deftest ex-info-replies-with-its-message-and-ExceptionInfo-test
  (async done
    (eval-code "(throw (ex-info \"boom\" {}))"
               (fn [replies]
                 (is-eval-error replies "boom\n" "ExceptionInfo")
                 (done)))))

(deftest ex-info-with-a-cause-replies-with-its-own-message-test
  (async done
    (eval-code "(throw (ex-info \"outer\" {} (js/TypeError. \"inner\")))"
               (fn [replies]
                 (is-eval-error replies "outer\n" "ExceptionInfo")
                 (done)))))

(deftest type-error-replies-with-TypeError-test
  (async done
    (eval-code "(throw (js/TypeError. \"bad\"))"
               (fn [replies]
                 (is-eval-error replies "bad\n" "TypeError")
                 (done)))))

(deftest thrown-string-replies-with-the-string-test
  (async done
    (eval-code "(throw \"boom\")"
               (fn [replies]
                 (is-eval-error replies "boom\n" "Error")
                 (done)))))

(deftest unresolved-symbol-replies-with-its-message-test
  (async done
    (eval-code "(foo)"
               (fn [replies]
                 (is-eval-error replies "Unable to resolve symbol: foo\n" "ExceptionInfo")
                 (done)))))

(deftest parse-error-replies-with-its-message-test
  (async done
    (eval-code "(+ 1"
               (fn [replies]
                 (is-eval-error replies "EOF while reading, expected ) to match ( at [1,1]\n" "ExceptionInfo")
                 (done)))))

(deftest resolved-promise-replies-before-done-test
  (async done
    (eval-code "1 (js/Promise.resolve 2)"
               (fn [replies]
                 (is (= ["1" "#<Promise 2>"] (collect :value replies)))
                 (is (= ["done"] (:status (peek replies))))
                 (done)))))

(deftest rejected-promise-replies-with-eval-error-test
  (async done
    (eval-code "(js/Promise.reject (js/TypeError. \"boom\"))"
               (fn [replies]
                 (is-eval-error replies "boom\n" "TypeError")
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
     (merge {:id "1" :session "s"} msg {:send-fn #(swap! !replies conj %)}))
    @!replies))

(defn- eval-value [session code & {:as msg}]
  (->> (message-replies (merge {:op :eval :code code :session session} msg))
       (collect :value)
       last))

(deftest session-keeps-its-own-namespace-test
  (eval-value "a" "(ns session-a)")
  (is (= "\"session-a\"" (eval-value "a" "(str *ns*)")))
  (is (= "\"user\"" (eval-value "b" "(str *ns*)"))))

(deftest ns-applies-to-its-eval-only-test
  (eval-value "a" "(ns explicit-a)")
  (let [replies (message-replies {:op :eval :code "(str *ns*)" :session "c" :ns "explicit-a"})]
    (is (= ["\"explicit-a\""] (collect :value replies)))
    (is (every? #(= "explicit-a" (:ns %)) replies)))
  (is (= "\"user\"" (eval-value "c" "(str *ns*)"))))

(deftest ns-form-in-an-eval-with-ns-keeps-the-session-namespace-test
  (eval-value "d" "(ns explicit-d)" :ns "user")
  (is (= "\"user\"" (eval-value "d" "(str *ns*)"))))

(deftest unknown-ns-replies-with-namespace-not-found-test
  (let [replies (message-replies {:op :eval :code "(def not-evaluated 1)" :session "e" :ns "not.loaded"})
        reply (first replies)]
    (is (= 1 (count replies)))
    (is (= ["error" "namespace-not-found" "done"] (:status reply)))
    (is (= "not.loaded" (:ns reply))))
  (is (= "nil" (eval-value "e" "(resolve 'not-evaluated)"))))

(deftest session-keeps-the-namespace-of-a-failed-eval-test
  (message-replies {:op :eval :code "(ns failed-eval) (throw (js/Error. \"boom\"))" :session "f"})
  (is (= "\"failed-eval\"" (eval-value "f" "(str *ns*)"))))

(deftest done-reply-carries-the-session-namespace-test
  (eval-value "g" "(ns done-g)")
  (is (= "done-g" (:ns (peek (message-replies {:op :eval :code "1" :session "g"}))))))

(deftest close-forgets-the-session-namespace-test
  (eval-value "h" "(ns closed-h)")
  (message-replies {:op :close :session "h"})
  (is (= "\"user\"" (eval-value "h" "(str *ns*)"))))

(deftest load-file-evaluates-in-the-session-namespace-test
  (eval-value "m" "(ns load-file-m)")
  (is (= ["\"load-file-m\""]
         (collect :value (message-replies {:op :eval :load-file true :code "(str *ns*)" :session "m"})))))

(deftest ns-form-in-a-load-file-keeps-the-session-namespace-test
  (message-replies {:op :eval :load-file true :code "(ns load-file-n)" :session "n"})
  (is (= "\"user\"" (eval-value "n" "(str *ns*)"))))

(defn- candidates [msg]
  (->> (message-replies (assoc msg :op :complete))
       first
       :completions
       (map #(get % "candidate"))
       set))

(deftest complete-with-ns-offers-vars-of-that-ns-test
  (eval-value "i" "(ns complete-i) (def in-complete-i 1)")
  (is (contains? (candidates {:prefix "in-complete-i" :session "j" :ns "complete-i"})
                 "in-complete-i")))

(deftest complete-without-ns-offers-vars-of-the-session-namespace-test
  (eval-value "k" "(ns complete-k) (def in-complete-k 1)")
  (is (contains? (candidates {:prefix "in-complete-k" :session "k"})
                 "in-complete-k"))
  (is (not (contains? (candidates {:prefix "in-complete-k" :session "l"})
                      "in-complete-k"))))

(defn- found-name [msg]
  (some-> (message-replies msg) first :name str))

(deftest info-ops-with-ns-find-vars-of-that-ns-test
  (eval-value "m" "(ns info-m) (def in-info-m 1)")
  (doseq [op [:info :eldoc :lookup]]
    (is (= "in-info-m" (found-name {:op op :sym "in-info-m" :session "n" :ns "info-m"})))))

(deftest info-ops-without-ns-find-vars-of-the-session-namespace-test
  (eval-value "o" "(ns info-o) (def in-info-o 1)")
  (doseq [op [:info :eldoc :lookup]]
    (is (= "in-info-o" (found-name {:op op :sym "in-info-o" :session "o"})))
    (is (= "inc" (found-name {:op op :sym "inc" :session "o"})))
    (is (= ["done" "no-eldoc"]
           (:status (first (message-replies {:op op :sym "in-info-o" :session "p"})))))))

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
