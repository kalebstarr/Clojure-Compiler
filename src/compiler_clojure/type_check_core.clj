(ns compiler-clojure.type-check-core
  (:require 
   [compiler-clojure.extract-core :as extractor]))

;; Add more expressive print including expected and received types (?)
(defn type-print-failure [token msg]
  (let [{:instaparse.gll/keys [end-line end-column]} (meta token)]
    (println (str "Type Error: Line " end-line ": Column " end-column ": " msg))
    (System/exit 1)))

(defn evaluate-var [expected expression var-stack method-stack]
  (when (seq expression)
    (let [token (first expression)
          rest-expr (rest expression)
          token-type (first token)
          token-value (rest token)]
      (case expected
        "int"
        (case token-type
          :int (evaluate-var expected rest-expr var-stack method-stack)
          :Identifier (if (contains? var-stack token)
                        (evaluate-var expected rest-expr var-stack method-stack)
                        (type-print-failure token (str "Variable '" (last token) "' has not been initialized")))
          :Plus (evaluate-var expected rest-expr var-stack method-stack)
          :Minus (evaluate-var expected rest-expr var-stack method-stack)
          :Star (evaluate-var expected rest-expr var-stack method-stack)
          :Modulo (evaluate-var expected rest-expr var-stack method-stack)
          :LeftParen (evaluate-var expected rest-expr var-stack method-stack)
          :RightParen (evaluate-var expected rest-expr var-stack method-stack)
          (type-print-failure token "Invalid token in int expression"))

        "double"
        (case token-type
          :int (evaluate-var expected rest-expr var-stack method-stack)
          :double (evaluate-var expected rest-expr var-stack method-stack)
          :Identifier (if (contains? var-stack token)
                        (evaluate-var expected rest-expr var-stack method-stack)
                        (type-print-failure token (str "Variable '" (last token) "' has not been initialized")))
          :Plus (evaluate-var expected rest-expr var-stack method-stack)
          :Minus (evaluate-var expected rest-expr var-stack method-stack)
          :Star (evaluate-var expected rest-expr var-stack method-stack)
          :Slash (evaluate-var expected rest-expr var-stack method-stack)
          :Modulo (evaluate-var expected rest-expr var-stack method-stack)
          :LeftParen (evaluate-var expected rest-expr var-stack method-stack)
          :RightParen (evaluate-var expected rest-expr var-stack method-stack)
          (type-print-failure token "Invalid token in double expression"))

        "string"
        (case token-type
          :string (evaluate-var expected rest-expr var-stack method-stack)
          :Plus (evaluate-var expected rest-expr var-stack method-stack)
          :Identifier (if (contains? var-stack token)
                        (evaluate-var expected rest-expr var-stack method-stack)
                        (type-print-failure token (str "Variable '" (last token) "' has not been initialized")))
          (type-print-failure token "Invalid token in string expression"))

        "bool"
        (case token-type
          :bool (evaluate-var expected rest-expr var-stack method-stack)
          :LogicOperator (evaluate-var expected rest-expr var-stack method-stack)
          :LeftParen (evaluate-var expected rest-expr var-stack method-stack)
          :RightParen (evaluate-var expected rest-expr var-stack method-stack)
          :Identifier (if (contains? var-stack token)
                        (let [varname (first token-value)
                              var-type (get var-stack varname)]
                          (if (some #{var-type} ["int" "double"])
                            (evaluate-var "arithmetic-in-bool" expression var-stack method-stack)
                            var-stack))
                        (type-print-failure token (str "Variable '" (last token) "' has not been initialized")))
          :int (evaluate-var "arithmetic-in-bool" expression var-stack method-stack)
          :double (evaluate-var "arithmetic-in-bool" expression var-stack method-stack)
          :LogicNot (evaluate-var expected rest-expr var-stack method-stack)
          :MethodCall (println (first (extractor/extract-info expression)))
          (type-print-failure token "Invalid token in bool expression"))

        "arithmetic-in-bool"
        (let [next-token (first rest-expr)]
          (case token-type
            :int (if (and next-token (= (first next-token) :ComparisonOperator))
                   (evaluate-var "comparison" rest-expr var-stack method-stack)
                   (type-print-failure token "Expected a comparison operator after arithmetic expression"))
            :double (if (and next-token (= (first next-token) :ComparisonOperator))
                      (evaluate-var "comparison" rest-expr var-stack method-stack)
                      (type-print-failure token "Expected a comparison operator after arithmetic expression"))
            :Identifier (if (and next-token (= (first next-token) :ComparisonOperator))
                          (evaluate-var "comparison" rest-expr var-stack method-stack)
                          (type-print-failure token "Expected a comparison operator after identifier"))
            (println "Invalid arithmetic expression in bool context:" token-type)))

        "comparison"
        (case token-type
          :ComparisonOperator (evaluate-var "arithmetic" rest-expr var-stack method-stack)
          (type-print-failure token "Expected a comparison operator"))

        "arithmetic"
        (case token-type
          :int (evaluate-var "bool-next" rest-expr var-stack method-stack)
          :double (evaluate-var "bool-next" rest-expr var-stack method-stack)
          :Identifier (evaluate-var "bool-next" rest-expr var-stack method-stack)
          (type-print-failure token "Invalid token anfter comparison operator"))

        "bool-next"
        (let [next-token (first rest-expr)]
          (if (and next-token (= (first next-token) :LogicOperator))
            (evaluate-var "bool" rest-expr var-stack method-stack)
            (if next-token
              (evaluate-var expected rest-expr var-stack method-stack)
              nil)))

        "ConsoleWrite"
        (case token-type
          :string (evaluate-var expected rest-expr var-stack method-stack)
          :int (evaluate-var expected rest-expr var-stack method-stack)
          :double (evaluate-var expected rest-expr var-stack method-stack)
          :bool (evaluate-var expected rest-expr var-stack method-stack)
          :Identifier (let [var-type (get var-stack token)]
                        (if (some #{var-type} ["int" "double" "bool" "string"])
                          (evaluate-var expected rest-expr var-stack method-stack)
                          (do
                            (type-print-failure token (str "Variable '" (last token) "' has not been initialized"))
                            var-stack)))
          :Plus (evaluate-var expected rest-expr var-stack method-stack)
          (type-print-failure token "Invalid expression in console write"))

        (println "Not handled yet:" expected))))
  var-stack)

(defn evaluate-method [pairs var-stack method-stack]
  (reduce (fn [acc [expected expr]]
            (evaluate-var expected expr acc method-stack))
          var-stack
          pairs))

;; Add variable scope
(defn evaluate [extract var-stack method-stack]
  (let [expression (:expression (:values extract))
        type (:type extract)]
    (case type
      :VariableDeclaration
      (let [varname (:varname (:values extract))]
        (if (not (contains? var-stack varname))
          (let [expected (:vartype (:values extract))
                updated-var-stack (assoc var-stack varname expected)]
            (evaluate-var expected expression updated-var-stack method-stack))
          (do (type-print-failure varname "Invalid variable declaration")
              var-stack)))

      :VariableAssignment
      (let [varname (:varname (:values extract))]
        (if (contains? var-stack varname)
          (let [expected (get var-stack varname)
                updated-var-stack (evaluate-var expected expression var-stack method-stack)]
            updated-var-stack)
          (do (type-print-failure varname (str "Variable '" (last varname) "' has not been initialized"))
              var-stack)))

      :IfBlock
      (let [expected (:vartype (:values extract))]
        (evaluate-var expected expression var-stack method-stack)
        var-stack)

      :WhileBlock
      (let [expected (:vartype (:values extract))]
        (evaluate-var expected expression var-stack method-stack)
        var-stack)

      :ConsoleWrite
      (let [expected "ConsoleWrite"]
        (evaluate-var expected expression var-stack method-stack)
        var-stack)

      :MethodCall
      (let [{:keys [name arguments]} (:values extract)
            filtered-arguments (filter #(not= "," %) arguments)
            extracted-arguments (map extractor/extract filtered-arguments)]
        (if (contains? method-stack name)
          (if (= (count (:params (get method-stack name))) (count extracted-arguments))
            (let [pairs (map vector (:params (get method-stack name)) extracted-arguments)]
              (evaluate-method pairs var-stack method-stack))
            (type-print-failure name (str "Wrong amount of arguments in method call '" (last name) "'")))
          (type-print-failure name (str "Method '" (last name) "' has not been declared")))
        var-stack)

      ;; :InstructionReturn (do
      ;;                     (evaluate-var expected expression)) 
      (do (println extract)
          var-stack))))

(defn collect-methods [extract method-stack]
  (let [method-name (:method-name (:values extract))
        {:keys [method-type params]} (:values extract)
        param-type (map #(second (second %)) params)
        expected {:method-type method-type :params param-type}
        updated-var-stack (assoc method-stack method-name expected)]
    updated-var-stack))

(defn collect-method-stack [method-extracts]
  (reduce
   (fn [method-stack extract]
     (collect-methods extract method-stack))
   {}
   method-extracts))

(defn type-check [extracts]
  (let [method-extract (filter #(= (:type %) :MethodDeclaration) extracts)
        method-stack (collect-method-stack method-extract)] 
    (reduce
     (fn [var-stack extract]
       (evaluate extract var-stack method-stack))
     {}
     (filter #(not= (:type %) :MethodDeclaration) extracts))))
