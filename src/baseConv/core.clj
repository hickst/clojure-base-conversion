(ns baseConv.core
  (:require [clojure.string :as str])
  (:gen-class)
)

(def digitChars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")


(defn powerOf [base]
  "Returns a function which takes a number and raises the given base to that number.
   The returned function returns its result as a Long unless the result exceeds
   java.lang.Long/MAX_VALUE, in which case -1 is returned."
  (partial (fn [base exponent]
             (try
               (long (Math/pow base exponent))
               (catch Exception e -1)
             ))
           base) )


(defn digitIndexer [digits]
  "Returns a function which takes a digit character and returns its zero-based index
   in the originally given digits sequence."
  (let [ digitIndex (zipmap digits (range (count digits))) ]
    (partial (fn [digitMap aDigit] (digitMap aDigit)) digitIndex) ))


(defn convertToLong [digits numStr]
  "Takes a sequence of digit characters and a number string in some base and
   converts the number string to a number in base 10."
  (let [ multipliers (reverse (map (powerOf (count digits)) (range (count numStr))))
         indices (map (digitIndexer digits) numStr) ]
    (reduce + (map * multipliers indices)) ))


(defn convertFromBase [base]
  "Returns a function which converts a number string in the given base to an integer."
  (partial convertToLong (take base digitChars)) )


(defn buckets [base]
  (take-while (comp not neg?) (map (powerOf base) (iterate inc 1))) )


(defn convertFromLong [base buckets aNum]
  (loop [acc aNum, digitIndices []]
    ;; (println "base:" base)
    ;; (println "acc:" acc)
    ;; (println "dIs:" digitIndices)
    (if (= acc base)
      (recur 0 (conj digitIndices 1))
      (if (< acc base)
        (str/join (map #(nth digitChars %) (conj digitIndices acc)))
        (let [ bucketValue (last (take-while #(< % acc) buckets))
               digitIndex (long (/ acc bucketValue)) ]
          ;; (println "bV:" bucketValue)
          ;; (println "dIs:" digitIndices)
          ;; (println "dI:" digitIndex)
          (recur (- acc (* bucketValue digitIndex)) (conj digitIndices digitIndex)) ) )) ))


(defn convertToBase [base]
  "Returns a function which converts an integer to a string representation in the given base."
  (partial convertFromLong base (buckets base)) )


(defn -main [& args]
  (let [ from62 (convertFromBase 62)
         from16 (convertFromBase 16)
         from2 (convertFromBase 2) ]
    (println "'0' in base 62 is" (from62 "0") "in base 10")
    (println "'10' in base 62 is" (from62 "10") "in base 10")
    (println "'zz' in base 62 is" (from62 "zz") "in base 10")
    (println "'DTVD3O' in base 62 is" (from62 "DTVD3O") "in base 10")
    (println "'AzL8n0Y58m7' in base 62 is" (from62 "AzL8n0Y58m7") "in base 10")
    (println)
    (println "'0' in base 16 is" (from16 "0") "in base 10")
    (println "'10' in base 16 is" (from16 "10") "in base 10")
    (println "'FF' in base 16 is" (from16 "FF") "in base 10")
    (println "'FFFF' in base 16 is" (from16 "FFFF") "in base 10")
    (println "'7FFFFFFFFFFFFFFF' in base 16 is" (from16 "7FFFFFFFFFFFFFFF") "in base 10")
    (println)
    (println "'0' in base 2 is" (from2 "0") "in base 10")
    (println "'10' in base 2 is" (from2 "10") "in base 10")
    (println "'1111' in base 2 is" (from2 "1111") "in base 10")
    (println "'111111111111111111111111111111111111111111111111111111111111111' in base 2 is"
             (from2 "111111111111111111111111111111111111111111111111111111111111111")
             "in base 10")
    (println)
  )

  (let [ to62 (convertToBase 62)
         to16 (convertToBase 16)
         to2 (convertToBase 2) ]
    (println)
    (println "0 in base 10 is" (to62 0) "in base 62")
    (println "61 in base 10 is" (to62 61) "in base 62")
    (println "62 in base 10 is" (to62 62) "in base 62")
    (println "63 in base 10 is" (to62 63) "in base 62")
    (println "3843 in base 10 is" (to62 3843) "in base 62")
    (println "12345678910 in base 10 is" (to62 12345678910) "in base 62")
    (println "9223372036854775807 in base 10 is" (to62 9223372036854775807) "in base 62")
    (println)
    (println "0 in base 10 is" (to16 0) "in base 16")
    (println "15 in base 10 is" (to16 15) "in base 16")
    (println "16 in base 10 is" (to16 16) "in base 16")
    (println "17 in base 10 is" (to16 17) "in base 16")
    (println "255 in base 10 is" (to16 255) "in base 16")
    (println "65535 in base 10 is" (to16 65535) "in base 16")
    (println "9223372036854775807 in base 10 is" (to16 9223372036854775807) "in base 16")
    (println)
    (println "0 in base 2 is" (to2 0) "in base 2")
    (println "1 in base 2 is" (to2 1) "in base 2")
    (println "2 in base 2 is" (to2 2) "in base 2")
    (println "3 in base 2 is" (to2 3) "in base 2")
    (println "15 in base 2 is" (to2 15) "in base 2")
    (println "9223372036854775807 in base 2 is" (to2 9223372036854775807) "in base 2")
  )
)
