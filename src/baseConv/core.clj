;; (ns baseConv.core
;;  (:gen-class) )


(def digitChars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")


(defn powerOf [base]
  "Returns a function which takes a number and raises the given base to that number."
  (partial (fn [base exponent] (long (Math/pow base exponent))) base) )


(defn digitIndexer [digits]
  "Returns a function which takes a digit character and returns its zero-based index
   within the originally given digits sequence."
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


(defn -main [& args]
  (let [ from62 (convertFromBase 62)
         from16 (convertFromBase 16)
         from2 (convertFromBase 2) ]
    (println "'10' in base 62 is" (from62 "10") "in base 10")
    (println "'zz' in base 62 is" (from62 "zz") "in base 10")
    (println "'DTVD3O' in base 62 is" (from62 "DTVD3O") "in base 10")
    (println)
    (println "'10' in base 16 is" (from16 "10") "in base 10")
    (println "'FF' in base 16 is" (from16 "FF") "in base 10")
    (println)
    (println "'10' in base 2 is" (from2 "10") "in base 10")
    (println "'1111' in base 2 is" (from2 "1111") "in base 10")
))
