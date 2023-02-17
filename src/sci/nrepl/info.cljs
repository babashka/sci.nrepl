(ns
    sci.nrepl.info
    (:require
     [clojure.string :as str]
     [sci.core :as sci]))

(defn format-1 [fmt-str x]
  (str/replace-first fmt-str "%s" x))

(defn info [{:keys [sym ctx] ns-str :ns}]
  (if-not sym
    {:status ["no-eldoc" "done"]
     :err "Message should contain a `sym`"}
    (let [code
          (->
           "(when-let [the-var (ns-resolve '%s '%s)]
               (meta the-var))"
           (format-1 ns-str)
           (format-1 sym))
          [kind val] (try [::success
                           (sci/eval-string* ctx code)]
                          (catch :default e
                            [::error (str e)]))
          {:keys [doc file line name arglists]} val]
      (if
          (and name (= kind ::success))
          (cond->
              {:ns (some-> val :ns ns-name)
               :arglists (pr-str arglists)
               :eldoc (mapv #(mapv str %) arglists)
               :arglists-str (.join (apply array arglists) "\n")
               :status ["done"]
               :name name}
              doc (assoc :doc doc)
              file (assoc :file file)
              line (assoc :line line))
          {:status ["done" "no-eldoc"]}))))
