(ns compiler-clojure.extract-core 
  (:require
   [meander.epsilon :as m]))

(defn extract [node]
  (m/match node
    ([:Type ?t] ?id "=" & ?other)
    {:vartype ?t,
     :varname ?id,
     :expression (extract ?other)}

    (?id "=" & ?other)
    {:varname ?id,
     :expression (extract ?other)}

    ("if" ?exp _)
    {:vartype "bool",
     :expression (extract ?exp)}

    ("while" ?exp _)
    {:vartype "bool"
     :expression (extract ?exp)}

    ("return" ?exp)
    {:expression (extract ?exp)}

    ;; :MethodCall with arguments
    ([:Identifier ?id] ?other)
    {:name ?id,
     :arguments (extract ?other)}

    [:Arguments & ?other]
    (extract ?other)

    ;; Expr in :VarDec and :VarAssign
    ([:Expression & ?other])
    (extract ?other)

    ;; Nested Expr in :VarDec and :VarAssign
    [[:Expression & ?nested-exp] & ?other]
    (concat (extract ?nested-exp) (extract ?other))

    ;; Expr in :IfBlock, :WhileBlock and :ConsoleWrite
    [:Expression & ?other]
    (extract ?other)

    ("Console.WriteLine" ?exp)
    {:expression (extract ?exp)}

    [?one & ?other]
    (concat [?one] (extract ?other))

    ;; :MethodCall without any arguments
    ([:Identifier ?id])
    {:name ?id}

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