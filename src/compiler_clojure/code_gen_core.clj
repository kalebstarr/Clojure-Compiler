(ns compiler-clojure.code-gen-core 
  (:require
   [clojure.string :as str]))

(defn pascal-case->camel-case [s]
  (if (Character/isUpperCase (first s))
    (str (str/lower-case (first s)) (subs s 1))
    s))

(defn generate-method [method]
  (println (str ".method public static " (pascal-case->camel-case 
                                          (last (:method-name (:current-method method)))))))

(defn generate-class [xs]
  (spit "resources/abc.j" (str ".class public " (last (:class-name xs)) "\n"
                               ".super java/lang/Object\n\n"
                               (map generate-method (:class-content xs)))))