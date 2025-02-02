(ns compiler-clojure.code-gen-core 
  (:require
   [clojure.string :as str]
   [compiler-clojure.extract-core :as extractor]))

(defn pascal-case->camel-case [s]
  (if (Character/isUpperCase (first s))
    (str (str/lower-case (first s)) (subs s 1))
    s))

(defn convert-type [type]
  (case type
    "int" "I"
    "double" "D"
    "string" "Ljava/lang/String;"
    "bool" "Z"
    "void" "V"

    "string[]" "[Ljava/lang/String;"

    (throw (Exception. (str "Unknown type: " type)))))

(defn generate-method [method]
  (let [current-method (:current-method method)]
    (str ".method public static "
         (pascal-case->camel-case (last (:method-name current-method)))
         "("
         (str/join (map #(convert-type (last (extractor/extract-parameter %)))
                        (:params current-method)))
         ")"
         (convert-type (:method-type current-method))
         "\n"
         (when (= "void" (:method-type current-method))
           "return\n")

         ".end method")))

(defn generate-class [xs]
  (println (str ".class public " (last (:class-name xs)) "\n"
                ".super java/lang/Object\n\n"
                (str/join "\n\n" (map generate-method (:class-content xs)))))
;;   (spit "resources/abc.j" (str ".class public " (last (:class-name xs)) "\n"
;;                                ".super java/lang/Object\n\n"
;;                                (map generate-method (:class-content xs))))
  )