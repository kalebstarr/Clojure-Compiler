(ns compiler-clojure.extract-core
  (:require
   [meander.epsilon :as m]))

(defn extract [node]
  (m/match node
    [:Instruction]
    nil

    [:Instruction ?other]
    [(extract ?other)]

    [:VariableDeclaration [:Type ?t] ?id "=" ?expr]
    [:type :VariableDeclaration,
     :values {:vartype ?t,
              :varname ?id,
              :expression ?expr}]

    [:VariableAssignment ?id "=" ?expr]
    [:type :VariableAssignment,
     :values {:varname ?id,
              :expression ?expr}]

    ;; IfElseBlock that redirects to separate IfBlock and ElseBlock extraction

    [:IfBlock "if" ?expr ?instruction]
    [:type :IfBlock,
     :values {:vartype "bool",
              :expression ?expr,
              :instruction ?instruction}]

    [:ElseBlock "else" ?instruction]
    [:type :ElseBlock,
     :values {:instruction ?instruction}]

    [:WhileBlock "while" ?expr ?instruction]
    [:type :WhileBlock,
     :values {:vartype "bool",
              :expression ?expr,
              :instruction ?instruction}]

    [:InstructionReturn "return" ?expr]
    [:type :InstructionReturn,
     :values {:expression ?expr}]

    [:ConsoleWrite "Console.WriteLine" ?expr]
    [:type :InstructionReturn,
     :values {:expression ?expr}]

    [:MethodCall ?id [:LeftParen ?x] ?other [:RightParen ?y]]
    [:type :MethodCall,
     :values {:name ?id,
              :arguments ?other}]

    [:MethodCall ?id [:LeftParen ?x] [:RightParen ?y]]
    [:type :MethodCall,
     :values {:name ?id,
              :arguments nil}]

    _ node))

(defn extract-info [node]
  (if (sequential? node)
    (case (first node)
      :VariableDeclaration
      (concat [{:type :VariableDeclaration
                :values (extract (rest node))}]
              (mapcat extract-info (rest node)))

      :VariableAssignment
      (concat [{:type :VariableAssignment
                :values (extract (rest node))}]
              (mapcat extract-info (rest node)))

      :InstructionReturn
      (concat [{:type :InstructionReturn
                :values (extract (rest node))}]
              (mapcat extract-info (rest node)))

      :IfBlock
      (concat [{:type :IfBlock
                :values (extract (rest node))}]
              (mapcat extract-info (rest node)))

      :ElseBlock
      (concat [{:type :ElseBlock
                :values (extract (rest node))}]
              (mapcat extract-info (rest node)))

      :WhileBlock
      (concat [{:type :WhileBlock
                :values (extract (rest node))}]
              (mapcat extract-info (rest node)))

      :ConsoleWrite
      (concat [{:type :ConsoleWrite
                :values (extract (rest node))}]
              (mapcat extract-info (rest node)))

      :MethodCall
      (concat [{:type :MethodCall
                :values (extract (rest node))}]
              (mapcat extract-info (rest node)))

      (mapcat extract-info node))
    []))

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
                :method-body ?other}

               ([:GenericMethodDeclaration "static" [:Type ?t] ?id [:InstructionBlock & ?other]])
               {:method-type ?t,
                :method-name ?id,
                :params nil,
                :method-return (extract-method-return ?other),
                :method-body ?other}

               ([:VoidMethodDeclaration "static" "void" ?id ?params [:InstructionBlock & ?other]])
               {:method-type "void",
                :method-name ?id,
                :params (extract-parameter-list ?params),
                :method-return (extract-method-return ?other),
                :method-body ?other}

               ([:VoidMethodDeclaration "static" "void" ?id [:InstructionBlock & ?other]])
               {:method-type "void",
                :method-name ?id,
                :params nil,
                :method-return (extract-method-return ?other),
                :method-body ?other}

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
