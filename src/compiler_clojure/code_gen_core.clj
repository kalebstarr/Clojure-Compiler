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

(defn rpn [tokens]
  (let [ops {"+" "iadd", "-" "isub", "*" "imul", "/" "idiv", "%" "irem"}
        result (first
                (reduce
                 (fn [stack token]
                   (if (contains? ops token)
                     (cons (str "ldc " (first stack) "\n"
                                "ldc " (second stack) "\n"
                                (ops token))
                           (drop 2 stack))
                     (cons token stack)))
                 [] tokens))]
    (if (sequential? result) result [(str "ldc " result)])))

(defn shunting-yard [tokens]
  (let [ops {"+" 1, "-" 1, "*" 2, "/" 2, "%" 2}]
    (flatten
     (reduce
      (fn [[rpn stack] token]
        (let [less-op? #(and (contains? ops %) (<= (ops (last token)) (ops %)))
              not-open-paren? #(not= "(" %)]
          (cond
            (= (last token) "(") [rpn (cons (last token) stack)]
            (= (last token) ")") [(vec (concat rpn (take-while not-open-paren? stack))) (rest (drop-while not-open-paren? stack))]
            (contains? ops (last token)) [(vec (concat rpn (take-while less-op? stack))) (cons (last token) (drop-while less-op? stack))]
            :else [(conj rpn (last token)) stack])))
      [[] ()]
      tokens))))

(defn convert-var-type [var-type]
  (case var-type
    "int" "istore"
    "double" "dstore"
    "string" "astore"
    "bool" "istore"

    (throw (Exception. (str "Unknown type: " type)))))

(defn generate-instruction [instruction]
  (let [extract (:extract instruction)]
    (case (:type extract)
      :VariableDeclaration
      (str (first (rpn (shunting-yard (:expression extract))))
           "\n"
           (convert-var-type (:vartype extract))
           " "
           (:index (get (:vars (:var-stack instruction)) (:varname extract))))

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

        ;; TODO: Add limits 
         ".limit stack 100\n"
         ".limit locals 100\n"

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