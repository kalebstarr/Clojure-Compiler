(ns compiler-clojure.type-check-core
  (:require
   [compiler-clojure.extract-core :as extractor]))

;; Add more expressive print including expected and received types (?)
(defn type-print-failure [token msg]
  (let [{:instaparse.gll/keys [end-line end-column]} (meta token)]
    (println (str "Type Error: Line " end-line ": Column " end-column ": " msg))
    ;; (System/exit 1)
    ))

(declare evaluate-var)

(defn evaluate-method [expected extract var-stack method-stack]
  (let [token (first extract)
        rest-expr (rest extract)
        method-call (first (extractor/extract-info token))
        {:keys [name]} (:values method-call)]
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
          token-type (first token)
          token-value (rest token)]
      (case expected
        "int"
        (case token-type
          :int (evaluate-var expected rest-expr var-stack method-stack)
          :Identifier (if (contains? var-stack token)
                        (let [varname (first token-value)
                              var-type (get var-stack varname)]
                          (if (some #{var-type} ["int"])
                            (evaluate-var expected rest-expr var-stack method-stack)
                            (do (type-print-failure token (str "Invalid variable '" varname "' in int expression"))
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
                        (let [varname (first token-value)
                              var-type (get var-stack varname)]
                          (if (some #{var-type} ["int" "double"])
                            (evaluate-var expected rest-expr var-stack method-stack)
                            (do (type-print-failure token (str "Invalid variable '" varname "' in double expression"))
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
                        (let [varname (first token-value)
                              var-type (get var-stack varname)]
                          (if (some #{var-type} ["int" "double"])
                            (evaluate-var "arithmetic-in-bool" expression var-stack method-stack)
                            (if (some #{var-type} ["string"])
                              (type-print-failure token (str "Invalid variable '" varname "' in bool expression"))
                              var-stack)))
                        (type-print-failure token (str "Variable '" (last token) "' has not been initialized")))
          :int (evaluate-var "arithmetic-in-bool" expression var-stack method-stack)
          :double (evaluate-var "arithmetic-in-bool" expression var-stack method-stack)
          :LogicNot (evaluate-var expected rest-expr var-stack method-stack)
          :MethodCall (evaluate-method expected expression var-stack method-stack)
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
            :MethodCall (evaluate-method expected expression var-stack method-stack)
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
          :MethodCall (evaluate-method expected expression var-stack method-stack)
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
          :MethodCall (evaluate-method expected expression var-stack method-stack)
          (type-print-failure token "Invalid expression in console write"))

        (println "Not handled yet:" expected))))
  var-stack)

(defn evaluate-methodcall [pairs var-stack method-stack]
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
            (evaluate-var expected expression updated-var-stack method-stack)
            updated-var-stack)
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
              (evaluate-methodcall pairs var-stack method-stack))
            (type-print-failure name (str "Wrong amount of arguments in method call '" (last name) "'")))
          (type-print-failure name (str "Method '" (last name) "' has not been declared")))
        var-stack)

      ;; This includes checking for :InstructionReturn as they are bound
      :MethodDeclaration
      (let [{:keys [method-type method-name method-return]} (:values extract)]
        (if (= method-type "void")
          (when (not= method-return nil)
            (type-print-failure method-name (str method-type " method expects no return")))
          (if (nil? method-return)
            (type-print-failure method-name (str method-type " method expects return"))
            (evaluate-var method-type (extractor/extract method-return) var-stack method-stack)))
        var-stack)

      var-stack)))

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

(defn mapcat-method [method-body]
  (mapcat #(extractor/extract %) method-body))

(defn collect-static-var-stack [static-vars]
  (reduce
   (fn [var-stack extract]
     (collect-static-vars extract var-stack))
   {}
   static-vars))

;; currently exists for only debug purposes
(defn ex [tree]
  (let [extracted (extractor/extract-class-content tree)
        static-vars (extractor/extract-static-var-declarations extracted)
        method-declaratations (extractor/extract-method-declaration extracted)

        static-var-stack (collect-static-var-stack static-vars)
        method-stack (collect-method-stack method-declaratations)]
    (println static-var-stack)
    (println method-stack)

    (println (map #(mapcat-method (:method-body %)) method-declaratations))

    (println method-declaratations)
    (println (map #(:params %) method-declaratations))))

(defn type-check [extracts]
  (let [method-extract (filter #(= (:type %) :MethodDeclaration) extracts)
        method-stack (collect-method-stack method-extract)]
    (reduce
     (fn [var-stack extract]
       (evaluate extract var-stack method-stack))
     {}
     extracts)))
