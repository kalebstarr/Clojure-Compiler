(ns compiler-clojure.type-check-core
  (:require
   [compiler-clojure.extract-core :as extractor]))

;; Add more expressive print including expected and received types (?)
(defn type-print-failure [token msg]
  (let [{:instaparse.gll/keys [end-line end-column]} (meta token)
        error-msg (str "Type Error: Line " end-line ": Column " end-column ": " msg)]
    (throw (ex-info error-msg {}))))

(declare evaluate-var)

(defn evaluate-method [expected extract var-stack method-stack]
  (let [token (first extract)
        rest-expr (rest extract)
        method-call (first (extractor/extract token))
        {:keys [name]} method-call]
    (when (contains? method-stack name)
      (case expected
        "int"
        (case (:method-type (get method-stack name))
          "int" (evaluate-var expected rest-expr var-stack method-stack)

          (type-print-failure token (str "Invalid Method return type. Expected type: " expected)))

        "double"
        (case (:method-type (get method-stack name))
          "int" (evaluate-var expected rest-expr var-stack method-stack)
          "double" (evaluate-var expected rest-expr var-stack method-stack)

          (type-print-failure token (str "Invalid Method return type. Expected type: " expected)))

        ("string" "ConsoleWrite")
        (case (:method-type (get method-stack name))
          "int" (evaluate-var expected rest-expr var-stack method-stack)
          "double" (evaluate-var expected rest-expr var-stack method-stack)
          "string" (evaluate-var expected rest-expr var-stack method-stack)
          "bool" (evaluate-var expected rest-expr var-stack method-stack)

          (type-print-failure token (str "Invalid Method return type. Expected type: " expected)))

        "bool"
        (case (:method-type (get method-stack name))
          "int" (evaluate-var "arithmetic-in-bool" extract var-stack method-stack)
          "double" (evaluate-var "arithmetic-in-bool" extract var-stack method-stack)
          "bool" (evaluate-var expected rest-expr var-stack method-stack)

          (type-print-failure token (str "Invalid Method return type. Expected type: " expected)))

        (println "Not handled yet:" expected)))))

(defn evaluate-var [expected expression var-stack method-stack]
  (when (seq expression)
    (let [token (first expression)
          rest-expr (rest expression)
          token-type (first token)]
      (case expected
        "int"
        (case token-type
          :int (evaluate-var expected rest-expr var-stack method-stack)
          :Identifier (if (contains? var-stack token)
                        (let [var-type (get var-stack token)]
                          (if (some #{var-type} ["int"])
                            (evaluate-var expected rest-expr var-stack method-stack)
                            (do (type-print-failure token (str "Invalid variable '" (last token) "' in int expression"))
                                var-stack)))
                        (type-print-failure token (str "Variable '" (last token) "' has not been initialized")))
          :Plus (evaluate-var expected rest-expr var-stack method-stack)
          :Minus (evaluate-var expected rest-expr var-stack method-stack)
          :Star (evaluate-var expected rest-expr var-stack method-stack)
          :Slash (evaluate-var expected rest-expr var-stack method-stack)
          :Modulo (evaluate-var expected rest-expr var-stack method-stack)
          :LeftParen (evaluate-var expected rest-expr var-stack method-stack)
          :RightParen (evaluate-var expected rest-expr var-stack method-stack)
          :MethodCall (evaluate-method expected expression var-stack method-stack)
          (type-print-failure token "Invalid token in int expression"))

        "double"
        (case token-type
          :int (evaluate-var expected rest-expr var-stack method-stack)
          :double (evaluate-var expected rest-expr var-stack method-stack)
          :Identifier (if (contains? var-stack token)
                        (let [var-type (get var-stack token)]
                          (if (some #{var-type} ["int" "double"])
                            (evaluate-var expected rest-expr var-stack method-stack)
                            (do (type-print-failure token (str "Invalid variable '" (last token) "' in double expression"))
                                var-stack)))
                        (type-print-failure token (str "Variable '" (last token) "' has not been initialized")))
          :Plus (evaluate-var expected rest-expr var-stack method-stack)
          :Minus (evaluate-var expected rest-expr var-stack method-stack)
          :Star (evaluate-var expected rest-expr var-stack method-stack)
          :Slash (evaluate-var expected rest-expr var-stack method-stack)
          :Modulo (evaluate-var expected rest-expr var-stack method-stack)
          :LeftParen (evaluate-var expected rest-expr var-stack method-stack)
          :RightParen (evaluate-var expected rest-expr var-stack method-stack)
          :MethodCall (evaluate-method expected expression var-stack method-stack)
          (type-print-failure token "Invalid token in double expression"))

        "string"
        (case token-type
          :string (evaluate-var expected rest-expr var-stack method-stack)
          :Plus (evaluate-var expected rest-expr var-stack method-stack)
          :Identifier (if (contains? var-stack token)
                        (evaluate-var expected rest-expr var-stack method-stack)
                        (type-print-failure token (str "Variable '" (last token) "' has not been initialized")))
          :MethodCall (evaluate-method expected expression var-stack method-stack)
          (type-print-failure token "Invalid token in string expression"))

        "bool"
        (case token-type
          :bool (evaluate-var expected rest-expr var-stack method-stack)
          :LogicOperator (evaluate-var expected rest-expr var-stack method-stack)
          :LeftParen (evaluate-var expected rest-expr var-stack method-stack)
          :RightParen (evaluate-var expected rest-expr var-stack method-stack)
          :Identifier (if (contains? var-stack token)
                        (let [var-type (get var-stack token)]
                          (if (some #{var-type} ["int" "double"])
                            (evaluate-var "arithmetic-in-bool" expression var-stack method-stack)
                            (if (some #{var-type} ["string"])
                              (evaluate-var "string-in-bool" expression var-stack method-stack)
                              var-stack)))
                        (type-print-failure token (str "Variable '" (last token) "' has not been initialized")))
          :int (evaluate-var "arithmetic-in-bool" expression var-stack method-stack)
          :double (evaluate-var "arithmetic-in-bool" expression var-stack method-stack)
          :string (evaluate-var "string-in-bool" expression var-stack method-stack)
          :LogicNot (evaluate-var expected rest-expr var-stack method-stack)
          :MethodCall (evaluate-method expected expression var-stack method-stack)
          (type-print-failure token "Invalid token in bool expression"))

        "arithmetic-in-bool"
        (let [next-token (first rest-expr)]
          (case token-type
            :int (if (and next-token (some #{(first next-token)} [:ComparisonOperator :Modulo]))
                   (evaluate-var "comparison" rest-expr var-stack method-stack)
                   (type-print-failure token "Expected a comparison operator after arithmetic expression"))
            :double (if (and next-token (some #{(first next-token)} [:ComparisonOperator :Modulo]))
                      (evaluate-var "comparison" rest-expr var-stack method-stack)
                      (type-print-failure token "Expected a comparison operator after arithmetic expression"))
            :Identifier (if (and next-token (some #{(first next-token)} [:ComparisonOperator :Modulo]))
                          (evaluate-var "comparison" rest-expr var-stack method-stack)
                          (type-print-failure token "Expected a comparison operator after identifier"))
            :MethodCall (evaluate-method expected expression var-stack method-stack)
            (type-print-failure token (str "Invalid arithmetic expression in bool context: " token-type))))

        "string-in-bool"
        (let [next-token (first rest-expr)]
          (case token-type
            :string
            (if (and next-token (= (first next-token) :ComparisonOperator))
              (evaluate-var "string-comparison" rest-expr var-stack method-stack)
              (type-print-failure token "Expected a string comparison operator after string expression"))

            :Identifier
            (if (and (contains? var-stack token)
                     (= (get var-stack token) "string")
                     next-token
                     (= (first next-token) :ComparisonOperator))
              (evaluate-var "string-comparison" rest-expr var-stack method-stack)
              (type-print-failure token "Expected a string variable and comparison operator"))

            :MethodCall
            (evaluate-method "string" expression var-stack method-stack)

            (type-print-failure token "Invalid string expression in bool context")))

        "comparison"
        (case token-type
          :ComparisonOperator (evaluate-var "arithmetic" rest-expr var-stack method-stack)
          :Modulo (evaluate-var "arithmetic" rest-expr var-stack method-stack)
          (type-print-failure token "Expected a comparison operator"))

        "string-comparison"
        (case token-type
          :ComparisonOperator (evaluate-var "string-operand" rest-expr var-stack method-stack)
          (type-print-failure token "Expected a string comparison operator"))

        "arithmetic"
        (case token-type
          :int (evaluate-var "bool-next" rest-expr var-stack method-stack)
          :double (evaluate-var "bool-next" rest-expr var-stack method-stack)
          :Identifier (if (contains? var-stack token)
                        (let [var-type (get var-stack token)]
                          (if (some #{var-type} ["int" "double"])
                            (evaluate-var "bool-next" expression var-stack method-stack)
                            (if (some #{var-type} ["string"])
                              (type-print-failure token (str "Invalid variable '" (last token) "' in bool expression"))
                              var-stack)))
                        (type-print-failure token (str "Variable '" (last token) "' has not been initialized")))
          :MethodCall (evaluate-method expected expression var-stack method-stack)
          (type-print-failure token "Invalid token anfter comparison operator"))

        "string-operand"
        (case token-type
          :string (evaluate-var "bool-next" rest-expr var-stack method-stack)
          :Identifier (if (and (contains? var-stack token) (= (get var-stack token) "string"))
                        (evaluate-var "bool-next" rest-expr var-stack method-stack)
                        (type-print-failure token "Invalid variable in string comparison"))
          :MethodCall (evaluate-method "string" expression var-stack method-stack)
          (type-print-failure token "Invalid operand in string comparison"))

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
          :Identifier (if (contains? var-stack token)
                        (let [var-type (get var-stack token)]
                          (if (some #{var-type} ["int" "double" "bool" "string"])
                            (evaluate-var expected rest-expr var-stack method-stack)
                            (do
                              (type-print-failure token (str "Invalid variable '" (last token) "' in Console Write"))
                              var-stack)))
                        (type-print-failure token (str "Variable '" (last token) "' has not been initialized")))
          :Plus (evaluate-var expected rest-expr var-stack method-stack)
          :MethodCall (evaluate-method expected expression var-stack method-stack)
          (type-print-failure token "Invalid expression in console write"))

        (println "Not handled yet:" expected))))
  var-stack)

(defn evaluate-methodcall [pairs var-stack method-stack]
  (reduce (fn [acc [expected expr]]
            (evaluate-var expected expr acc method-stack))
          var-stack
          pairs))

(declare type-check)

(defn evaluate [extract var-stack method-stack current-method]
  (let [expression (:expression extract)
        type (:type extract)
        updated-var-stack
        (case type
          :VariableDeclaration
          (let [varname (:varname extract)]
            (if (not (contains? var-stack varname))
              (let [expected (:vartype extract)
                    new-stack (assoc var-stack varname expected)]
                (evaluate-var expected expression new-stack method-stack)
                new-stack)
              (do (type-print-failure varname "Invalid variable declaration")
                  var-stack)))

          :VariableAssignment
          (let [varname (:varname extract)]
            (if (contains? var-stack varname)
              (let [expected (get var-stack varname)]
                (evaluate-var expected expression var-stack method-stack))
              (do (type-print-failure varname (str "Variable '" (last varname) "' has not been initialized"))
                  var-stack)))

          :IfBlock
          (let [expected (:vartype extract)
                instruction (:instruction extract)]
            (evaluate-var expected expression var-stack method-stack)
            (type-check instruction var-stack method-stack current-method)
            var-stack)

          :ElseBlock
          (let [instruction (:instruction extract)]
            (type-check instruction var-stack method-stack current-method)
            var-stack)

          :WhileBlock
          (let [expected (:vartype extract)
                instruction (:instruction extract)]
            (evaluate-var expected expression var-stack method-stack)
            (type-check instruction var-stack method-stack current-method)
            var-stack)

          :InstructionBlock
          (let [instructions (:instructions extract)]
            (type-check instructions var-stack method-stack current-method)
            var-stack)

          :InstructionReturn
          (let [expected (:method-type current-method)]
            (evaluate-var expected expression var-stack method-stack)
            var-stack)

          :ConsoleWrite
          (let [expected "ConsoleWrite"]
            (evaluate-var expected expression var-stack method-stack)
            var-stack)

          :MethodCall
          (let [{:keys [name arguments]} extract
                filtered-arguments (filter #(not= "," %) arguments)
                extracted-arguments (map extractor/extract-expression filtered-arguments)]
            (if (contains? method-stack name)
              (if (= (count (:params (get method-stack name))) (count extracted-arguments))
                (let [pairs (map vector (:params (get method-stack name)) extracted-arguments)]
                  (evaluate-methodcall pairs var-stack method-stack))
                (type-print-failure name (str "Wrong amount of arguments in method call '" (last name) "'")))
              (type-print-failure name (str "Method '" (last name) "' has not been declared")))
            var-stack)

          var-stack)]

    {:extract extract
     :var-stack updated-var-stack}))

(defn collect-methods [extract method-stack]
  (let [method-name (:method-name extract)]
    (if (not (contains? method-stack method-name))
      (let [{:keys [method-type params method-return]} extract
            param-type (map #(second (second %)) params)
            expected {:method-type method-type,
                      :params param-type,
                      :method-return method-return}
            updated-method-stack (assoc method-stack method-name expected)]
        (if (= method-type "void")
          (when (not (or (nil? method-return) (empty? method-return)))
            (type-print-failure method-name (str method-type " method expects no return")))
          (when (or (nil? method-return) (empty? method-return))
            (type-print-failure method-name (str method-type " method expects return"))))
        updated-method-stack)
      (do (type-print-failure method-name "Invalid method declaration")
          method-stack))))

(defn collect-method-stack [method-extracts]
  (reduce
   (fn [method-stack extract]
     (collect-methods extract method-stack))
   {}
   method-extracts))

(defn collect-static-vars [extract var-stack]
  (let [varname (:varname extract)]
    (if (not (contains? var-stack varname))
      (let [expected (:vartype extract)
            updated-var-stack (assoc var-stack varname expected)]
        updated-var-stack)
      (do (type-print-failure varname "Invalid variable declaration")
          var-stack))))

(defn collect-static-var-stack [static-vars]
  (reduce
   (fn [var-stack extract]
     (collect-static-vars extract var-stack))
   {}
   static-vars))

(defn type-check [extracts var-stack method-stack current-method]
  (let [results
        (reduce
         (fn [{:keys [acc var-stack]} extract]
           (let [evaluation (evaluate extract var-stack method-stack current-method)]
             {:acc (conj acc evaluation)
              :var-stack (:var-stack evaluation)}))
         {:acc [] :var-stack var-stack}
         extracts)]
    (:acc results)))

(defn type-check-method-declaration [method-declaration var-stack method-stack]
  (let [{:keys [params method-body]} method-declaration
        updated-var-stack
        (if (seq params)
          (reduce
           (fn [acc p]
             (let [[varname vartype] (extractor/extract-parameter p)]
               (assoc acc varname vartype)))
           var-stack
           params)
          var-stack)]
    (type-check method-body updated-var-stack method-stack method-declaration)))

(defn check [tree]
  (let [extracted (extractor/extract-class-content tree)
        static-var-stack (collect-static-var-stack
                          (extractor/extract-static-var-declarations extracted))
        method-declarations (extractor/extract-method-declaration extracted)
        method-stack (collect-method-stack method-declarations)]
    (mapcat #(type-check-method-declaration % static-var-stack method-stack) method-declarations)))
