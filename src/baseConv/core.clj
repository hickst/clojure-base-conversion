(ns baseConv.core
  (:require [clojure.string :as str])
  (:gen-class)
)

(def digitChars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defn powerOf
  "Returns a function which takes a number and raises the given base to that number.
   The returned function returns its result as a Long unless the result exceeds
   java.lang.Long/MAX_VALUE, in which case -1 is returned."
  [base]
  (partial (fn [base exponent]
             (try                           ; catch exponentiation > Long/MAX_VALUE
               (long (Math/pow base exponent))
               (catch Exception e -1)
               ))
           base) )

(defn digitIndexer
  "Returns a function which takes a digit character and returns its zero-based index
   in the originally given sequence of digit characters."
  [digitChars]
  (let [ digitIndex (zipmap digitChars (range (count digitChars))) ]
    (partial (fn [digitMap aDigit] (digitMap aDigit)) digitIndex) ))


(defn convertToLong
  "Takes a sequence of digit characters and a number string in some base and
   converts the number string to a number in base 10."
  [digitChars numStr]
  (let [ powersAt (reverse (map (powerOf (count digitChars)) (range (count numStr))))
         factors (map (digitIndexer digitChars) numStr) ]
    (reduce + (map * powersAt factors)) ))

(defn convertFromBase
  "Returns a function which converts a number string in the given base to an integer."
  [base]
  (partial convertToLong (take base digitChars)) )


(defn powers
  "Returns a sequence of all powers, for the given base, which
   do not exceed the maximum value of a long integer (java.lang.Long/MAX_VALUE).
   Returns nil if the provided base is not > 1."
  [base]
  (if (> base 1)
    (take-while (comp not neg?) (map (powerOf base) (iterate inc 0)))
    nil) )

(defn makeDigitString
  "Return a digit string from the given (possibly empty) sequence of digit indices."
  [digitIndices]
  (str/join (map #(nth digitChars %)
                 (if (empty? digitIndices) [0] digitIndices))) )


(defn convertFromLong
  "Process each power of the base in the given sequence to extract the digit for that power.
   Returns a digit string for the given number (in the base of the powers sequence)."
  [powers aNum]
  (let [ buckets (reverse (take-while #(<= % aNum) powers)) ]
    (loop [buckets buckets, aNum aNum, digitIndices []]
      (let [ bucketValue (first buckets) ]
        (if (nil? bucketValue)              ; if all done
          (makeDigitString digitIndices)    ; return the digit string
          (let [ digitIndex (long (/ aNum bucketValue)) ]
            (recur (rest buckets)           ; else loop for next digit
                   (- aNum (* bucketValue digitIndex))
                   (conj digitIndices digitIndex) )))))) )


(defn convertToBase
  "Returns a function which converts an integer to a string representation in the given base."
  [base]
  (partial convertFromLong (powers base)) )


;; In test in the user namespace:
;;
;; user=> (load-file "src/baseConv/core.clj")
;; user=> (require '[baseConv.core :refer :all])
;; user=> (-main)

(defn -main [& args]
  (let [ from62 (convertFromBase 62)
         from16 (convertFromBase 16)
         from2 (convertFromBase 2) ]
    (println "'0' in base 62 is" (from62 "0") "in base 10")
    (println "'z' in base 62 is" (from62 "z") "in base 10")
    (println "'10' in base 62 is" (from62 "10") "in base 10")
    (println "'11' in base 62 is" (from62 "11") "in base 10")
    (println "'zz' in base 62 is" (from62 "zz") "in base 10")
    (println "'100' in base 62 is" (from62 "100") "in base 10")
    (println "'DTVD3O' in base 62 is" (from62 "DTVD3O") "in base 10")
    (println "'AzL8n0Y58m7' in base 62 is" (from62 "AzL8n0Y58m7") "in base 10")
    (println)
    (println "'0' in base 16 is" (from16 "0") "in base 10")
    (println "'F' in base 16 is" (from16 "F") "in base 10")
    (println "'10' in base 16 is" (from16 "10") "in base 10")
    (println "'11' in base 16 is" (from16 "11") "in base 10")
    (println "'FF' in base 16 is" (from16 "FF") "in base 10")
    (println "'FFFF' in base 16 is" (from16 "FFFF") "in base 10")
    (println "'7FFFFFFFFFFFFFFF' in base 16 is" (from16 "7FFFFFFFFFFFFFFF") "in base 10")
    (println)
    (println "'0' in base 2 is" (from2 "0") "in base 10")
    (println "'01' in base 2 is" (from2 "01") "in base 10")
    (println "'10' in base 2 is" (from2 "10") "in base 10")
    (println "'11' in base 2 is" (from2 "11") "in base 10")
    (println "'1111' in base 2 is" (from2 "1111") "in base 10")
    (println "'10000' in base 2 is" (from2 "10000") "in base 10")
    (println "'111111111111111111111111111111111111111111111111111111111111110' in base 2 is"
             (from2 "111111111111111111111111111111111111111111111111111111111111110")
             "in base 10")
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
    (println "16 in base 2 is" (to2 16) "in base 2")
    (println "17 in base 2 is" (to2 17) "in base 2")
    (println "9223372036854775806 in base 2 is" (to2 9223372036854775806) "in base 2")
    ;; (println "9223372036854775807 in base 2 is" (to2 9223372036854775807) "in base 2")
  )
)
