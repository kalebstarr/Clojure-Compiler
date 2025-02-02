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

(defn generate-instruction [instruction]
  (let [extract (:extract instruction)] 
    (case (:type extract)
      :VariableDeclaration
      "Variable Declaration\n"

      :VariableAssignment
      "Variable Assignment\n"

      :IfBlock
      "If Block\n"

      :ElseBlock
      "Else Block\n"

      :WhileBlock
      "While Block\n"

      :InstructionBlock
      "Instruction Block\n"

      :InstructionReturn
      "Instruction Return\n"

      :ConsoleWrite
      "Console Write\n"

      :MethodCall
      "Method Call\n"

      (throw (Exception. (str "Unknown instruction type: " (:type extract)))))))

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

         (str/join "\n" (map generate-instruction (:instructions method)))

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