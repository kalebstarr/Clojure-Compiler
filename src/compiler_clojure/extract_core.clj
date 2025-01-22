(ns compiler-clojure.extract-core
  (:require
   [meander.epsilon :as m]))

(defn extract-expression [expr]
  (m/match expr
    [:Expression & ?other]
    (extract-expression ?other)

    [[:Expression & ?nested-exp] & ?other]
    (concat (extract-expression ?nested-exp) (extract-expression ?other))

    [?one & ?other]
    (concat [?one] (extract-expression ?other))

    _ expr))

(defn extract-arguments [arguments]
  (m/match arguments
    [:Arguments & ?other]
    ?other

    _ arguments))

(declare mapcat-instruction-block-extract)

(defn extract [node]
  (m/match node
    [:Instruction]
    nil

    [:Instruction ?other]
    (extract ?other)

    [:VariableDeclaration [:Type ?t] ?id "=" ?expr]
    [{:type :VariableDeclaration,
      :vartype ?t,
      :varname ?id,
      :expression (extract-expression ?expr)}]

    [:VariableAssignment ?id "=" ?expr]
    [{:type :VariableAssignment,
      :varname ?id,
      :expression (extract-expression ?expr)}]

    [:IfElseBlock ?if ?else]
    (vec (concat (extract ?if) (extract ?else)))

    [:IfElseBlock ?if]
    (extract ?if)

    [:IfBlock "if" ?expr ?instruction]
    [{:type :IfBlock,
      :vartype "bool",
      :expression (extract-expression ?expr),
      :instruction (extract ?instruction)}]

    [:ElseBlock "else" ?instruction]
    [{:type :ElseBlock,
      :instruction (extract ?instruction)}]

    [:WhileBlock "while" ?expr ?instruction]
    [{:type :WhileBlock,
      :vartype "bool",
      :expression (extract-expression ?expr),
      :instruction (extract ?instruction)}]

    [:InstructionBlock & ?instructions]
    [{:type :InstructionBlock,
      :instructions (mapcat-instruction-block-extract ?instructions)}]

    [:InstructionReturn "return" ?expr]
    [{:type :InstructionReturn,
      :expression (extract-expression ?expr)}]

    [:ConsoleWrite "Console.WriteLine" ?expr]
    [{:type :ConsoleWrite,
      :expression (extract-expression ?expr)}]

    [:MethodCall ?id [:LeftParen ?x] ?other [:RightParen ?y]]
    [{:type :MethodCall,
      :name ?id,
      :arguments (extract-arguments ?other)}]

    [:MethodCall ?id [:LeftParen ?x] [:RightParen ?y]]
    [{:type :MethodCall,
      :name ?id,
      :arguments nil}]

    _ node))

(defn mapcat-instruction-block-extract [instruction-body]
  (mapcat #(extract %) instruction-body))

(defn extract-static-var-declarations [node]
  (->> node
       (filter #(= :StaticVariableDeclaration (first %)))
       (map #(m/match %
               [:StaticVariableDeclaration _ [:VariableDeclaration [:Type ?t] ?id "=" & ?other]]
               {:vartype ?t,
                :varname ?id,
                :expression (extract ?other),
                :static true}

               _ %))))

(defn extract-method-return [node]
  (when (sequential? node)
    (case (first node)
      :InstructionReturn
      (m/match node
        [:InstructionReturn "return" ?other]
        ?other

        _ node)

      (mapcat extract-method-return node))))

(defn extract-parameter-list [param-list]
  (filter #(not= "," %) (rest param-list)))

(defn extract-method-declaration [node]
  (->> node
       (filter #(= :MethodDeclaration (first %)))
       (map #(m/match (rest %)
               ([:GenericMethodDeclaration "static" [:Type ?t] ?id ?params [:InstructionBlock & ?other]])
               {:method-type ?t,
                :method-name ?id,
                :params (extract-parameter-list ?params),
                :method-return (extract-method-return ?other),
                :method-body (mapcat-instruction-block-extract ?other)}

               ([:GenericMethodDeclaration "static" [:Type ?t] ?id [:InstructionBlock & ?other]])
               {:method-type ?t,
                :method-name ?id,
                :params nil,
                :method-return (extract-method-return ?other),
                :method-body (mapcat-instruction-block-extract ?other)}

               ([:VoidMethodDeclaration "static" "void" ?id ?params [:InstructionBlock & ?other]])
               {:method-type "void",
                :method-name ?id,
                :params (extract-parameter-list ?params),
                :method-return (extract-method-return ?other),
                :method-body (mapcat-instruction-block-extract ?other)}

               ([:VoidMethodDeclaration "static" "void" ?id [:InstructionBlock & ?other]])
               {:method-type "void",
                :method-name ?id,
                :params nil,
                :method-return (extract-method-return ?other),
                :method-body (mapcat-instruction-block-extract ?other)}

               _ %))))

(defn extract-class-content [tree]
  (when (sequential? tree)
    (case (first tree)
      :ClassDeclaration
      (m/match tree
        [:ClassDeclaration _ _ & ?other]
        ?other

        _ tree)

      (mapcat extract-class-content tree))))
