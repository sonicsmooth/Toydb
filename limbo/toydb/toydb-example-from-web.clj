(ns toydb.core
  (:use (clojure repl reflect pprint)))



(defrecord CreateUser [name email start-date])
(defrecord DeleteUser [email remove-date])
(defprotocol Transaction (update [this db]))
(extend-protocol Transaction
  CreateUser 
  (update [this db]
    (update-in db [:users] conj
               {:name (:name this) :email (:email this) :start-date (:start-date this)}))
  DeleteUser
  (update [this db]
    (update-in db [:users] (fn [coll]
                             (remove (fn [r] (= (:email r) (:email this))) coll)))))


(defrecord DefaultTransactionLog [agnt])
(defrecord Database [state logagent])
(defprotocol TransactionLog (record [this tx]))
(extend-protocol TransactionLog
  DefaultTransactionLog
  (record [this tx]
    (send-off (:agnt this)
              (fn [out]
                (binding [*out* out
                          *flush-on-newline* true]
                  (io! (prn tx)) out))))

  Database
  (record [this tx]
    (dosync
     (alter (:state this) (partial update tx))
     (record (:logagent this) tx))))


(defn read-transactions [f]
  (if-not (.exists f) []
          (let [rdr (java.io.PushbackReader. (clojure.java.io/reader f))]
            (take-while identity
                        (repeatedly
                         (fn [] (read rdr false nil)))))))



(def db
  (Database.
   (ref (reduce (fn [db tx] (update tx db)) {}
                (read-transactions
                 (clojure.java.io/file "src/toydb/my.db.clj"))))
   (DefaultTransactionLog.
     (agent (clojure.java.io/writer
             (clojure.java.io/file "src/toydb/my.db.clj")
             :append true)))))

(defn create-user [name email]
  (record db (CreateUser. name email (java.util.Date.))))

(defn delete-user [email]
  (record db (DeleteUser. email (java.util.Date.))))


