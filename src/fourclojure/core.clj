(ns fourclojure.core
 (:require [clojure.string :as str])
 (:use flatland.ordered.map)
 (import java.lang.StringBuffer))


(set! *warn-on-reflection* true)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn convertToUpperCase [ args ]
  ( -> args (str/upper-case)))

(convertToUpperCase "helloWorld")


(def listone '( 1 3 4))
(def newListone (conj listone 2))
(cons 3 newListone)

(def vectorone [1 2 3 4])
(vector? vectorone)

;;Hashmap
(def hashmapone ((hash-map 1 1, :2 2,:3 3,:4 4 ) :2 :3))
(println hashmapone)

;; 


(def restlist (rest [10 20 30 40]))
(println restlist)


((fn [x] 
   (+ x x)) 3)

(#(+ % 5)3)

((partial + 5) 3)
(ordered-map :b 2 :a 1 :d 4)

(def filterSequence )

(println "Filter Sequence =" filterSequence)

(type filterSequence)

(map (fn[x] (+ x 1)) 
     (filter #( > % 5) '( 1 2 3 4 6 7 8)))


(= ( second (reverse (list 1 2 3 4 5))) 4)

( second (reverse (list 1 2 3 4 5))) 

;loop [binding]
;(condition
;   (statement)
;   (recur (binding)))

(defn printToHundred [x]
   (loop [y x]
     (when (< y 100)
       (println y)
       (recur (inc y)))))
              
#_(printToHundred 5)         


#_(defn fibonacci[n]
   (loop [x 0
          y n]
         (println "===" x y)
         (when( < x y)
            (println x)
            (recur (+ x y)))))

#_(fibonacci 5)


(defn sdsu-nth
        [input-list n]
        (loop [cnt n tmp-list input-list]
              (if (zero? cnt)
                  (first tmp-list)
                (recur (dec cnt) (rest tmp-list)))))
  
 (sdsu-nth [0 1 2 3 4] 2)
 
 (def listCount '( 1 2 3 4))
 (count listCount)

 
 (def string-value "Hello World")
 
 (def spliValue (str/split "Hello World" #""))

 
(defmacro dbg [& body]
  `(let [x# ~@body]
     (println (str "dbg: " (quote ~@body) "=" x#))
     x#))
 
(defn findCounts [parms]
  (dbg (loop [x 0]
             (println (count (str/split parms #"")) "====1===" x)
              (when (< x (count (str/split parms #"")))
                   (get parms x)
                  (recur  (inc x))))))


(findCounts "Hello World")
 
(println "=======================2================")

(def factorial
  (fn [n]
    (loop [cnt n acc 1]
      (if (zero? cnt)
        acc
        (recur (dbg (dec cnt)) (dbg (* acc cnt)))))))

 
#_(factorial 5)

(defn greetings-from-santa [n]
                (if (> n 0)
                     (do 
                       (greetings-from-santa (dec n)))))

(greetings-from-santa 10)


(map inc [1 2 3 4])

(def sum #(reduce + %))

(def average #(/ (sum %) (count %)))


(defn results [coll]
  (dbg (map #(% coll) [sum average count])))

(results [10 20 30 40 50])

(apply max [1 2 3])



(defn reverse-string
  "
  Given a string, return its mirror image. An 'asdf' example:
  (conj (conj (conj (conj '() \"a\") \"s\") \"d\") \"f\")
  "
  [s]
  (dbg (str/join (apply conj '() (str/split s #"")))))


(reverse-string "asdf")

(defn addnumbers
  [parms]
  (reduce + parms))

(addnumbers [1 2 7])

;; return only odd numbers

(defn returnOdnumbers
  [parms]
   (filter odd? parms))
  

(returnOdnumbers [1 2 7])

(defn Example [parms]
   (for [n parms]
    (cond (odd? n)
      n)))

(Example [1 2 3 4 5 6])

;; Polindrome

(def split (#(str/split % #"") "Hello World"))


(defn polindrome[arg]
  (let [split (#(str/split % #"") arg) 
        stringBuffer (atom (new StringBuffer))
        countString (count split)]
        (doseq [countString (reverse split) ] 
          (println countString)
          (println "-----")
          (swap! (-> stringBuffer (.append (str countString)) .toString)))
        (if (= arg stringBuffer)
          "Polindrome"
          "Not a poindrome"
         )))

#_(polindrome "abba")


(defn polindrome-new[arg]
  (let [split (#(str/split % #"") arg)
        tempString ""
        countString (count split)]
        (println "--" tempString)
        (doseq [countString (reverse split) ] 
          (println countString)
          (apply concat tempString countString))
        (println arg)
        (println tempString)
        (if (= arg tempString)
          "Polindrome"
          "Not a poindrome")))

#_(polindrome-new "abba")

;; Clojure String

(defn unique-character [parms]
  (let [split (#(str/split % #"") parms)
        countString (count split)
        listvalue #{}]
  (dbg (doseq [countString split]
               (println countString)
               (conj listvalue countString)
               (println "==="listvalue "====")
         ))listvalue))

#_(unique-character "Leeeeeerrroyyy")

(defn unique-characters-2 [parms]
  (apply str ;; <-- Build a string from the value returned by reduce below
         (reduce (fn [loop-state input-character]
                   (if (some #(= input-character %) loop-state)
                     loop-state
                     (conj loop-state input-character)))

                 [] ;; <-- Initial loop state

                 parms ;; <-- Input string (sequence of characters)
                 )))

#_(unique-characters-2 '([1 2] [1 2] [3 4] [3 4]))

;; find max

(fn drop-every-n [col n] 
  (keep-indexed 
    (defn removeIndex [index item] 
      (if
        (not= 0 (mod (inc index) n))
          item 
          nil))
    col))



(defn factorial-using-recur [n]
  (loop [current n
         next (dec current)
         total 1]
         (println "current" current)
         (println "next" next)
         (println "total" total)
         (println " ")
    (if (> current 1)
        (recur next (dec next) (* total current))
      total))) 

#_(factorial-using-recur 8)

(take 5 (iterate #(+ 3 %) 1))

(dorun (map #(println "hi" %) ["mum" "dad" "sister"]))


(split-at 2 [1 2 3 4 5])

(polindrome "abba")

