(ns clojurepackage.jdbc
   (:require [clojure.java.jdbc :as jdbc]))

(def db-map {:classname "com.mysql.jdbc.Driver" 
             :dbtype "mysql"
             :dbname "my_database"
             :host "127.0.0.1"
             :port "3306"
             :user "ganesh"
             :password "Kannada123@"
             :useSSL false})

(defn fetch-result[]
(try 
  (def result (jdbc/query db-map ["select * from employee"]))
  (finally (prn "final exception."))))

(defn evaluate-result[]
   (let [countresult (count result)])
   (doseq [countresult result]
     (println countresult)))



(def id (jdbc/get-by-id db-map "employee" 2))

(println id)



(jdbc/insert! db-map :employee {:id "7" ,:name "Ravi",:dept "tce", :salary "90000"})

(jdbc/update! db-map :employee {:dept "tce"} ["dept = ?" "Civil"])

(println "====1111====")
(fetch-result)

(evaluate-result)

(defn loopexample [] 
  (println "====1=")
  (loop [x 0]
    (when(<  x 10)
      (println x)
      (recur (inc x)))))

#_(loopexample)



    



  