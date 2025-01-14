(ns compiler-clojure.extract-core 
  (:require
   [meander.epsilon :as m]))

(defn methodcall-extract [node]
  (m/match node
    [:InstructionReturn "return" ?other]
    ?other

    ;; This extracts nested instructions as :InstructionReturn is nested
    [?one ?other]
    (methodcall-extract ?other)

    [?one & ?other]
    (methodcall-extract ?other)

    _ nil))

(defn extract [node]
  (m/match node
    ([:Type ?t] ?id "=" & ?other)
    {:vartype ?t,
     :varname ?id,
     :expression (extract ?other),
     :static false}

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
    (?id [:LeftParen ?x] ?other [:RightParen ?y])
    {:name ?id,
     :arguments (extract ?other)}

    ;; :MethodCall without arguments
    (?id [:LeftParen ?x] [:RightParen ?y])
    {:name ?id
     :arguments nil}

    [:Arguments & ?other]
    ?other

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

    ;; :MethodDeclaration
    ([:GenericMethodDeclaration "static" [:Type ?t] ?id ?params ?other])
    {:method-type ?t
     :method-name ?id
     :params (extract ?params)
     :method-return (methodcall-extract ?other)}

    ([:VoidMethodDeclaration "static" "void" ?id ?params ?other])
    {:method-type "void"
     :method-name ?id
     :params (extract ?params)
     :method-return (methodcall-extract ?other)}

    [:ParameterList & ?params]
    (filter #(not= "," %) ?params)

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

      :MethodDeclaration
      (concat [{:type :MethodDeclaration
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

(defn extract-method-declaration [node]
  (->> node
       (filter #(= :MethodDeclaration (first %)))
       (map #(m/match (rest %)
               ([:GenericMethodDeclaration "static" [:Type ?t] ?id ?params [:MethodBody & ?other]])
               {:method-type ?t
                :method-name ?id
                :params (extract ?params)
                :method-return (methodcall-extract ?other)
                :method-body ?other}

               ([:VoidMethodDeclaration "static" "void" ?id ?params [:MethodBody & ?other]])
               {:method-type "void"
                :method-name ?id
                :params (extract ?params)
                :method-return (methodcall-extract ?other)
                :method-body ?other}))))

(defn extract-class-content [tree]
  (when (sequential? tree)
    (case (first tree)
      :ClassDeclaration
      (m/match tree
        [:ClassDeclaration _ _ & ?other]
        ?other

        _ tree)

      (mapcat extract-class-content tree))))

;; currently exists for only debug purposes
(defn ex [tree]
  (let [extracted (extract-class-content tree)
        static-vars (extract-static-var-declarations extracted)
        method-declaratations (extract-method-declaration extracted)]
    (println static-vars)
    (println method-declaratations)))