(ns compiler-clojure.core
  (:require
   [clojure.string :as str]
   [clojure.tools.cli :refer [parse-opts]]
   [compiler-clojure.parser-core :as parser]
   [instaparse.core :as insta]
   [meander.epsilon :as m])
  (:gen-class))

(def cli-options
  [["-c" "--compile FILE-PATH" "Specify file to compile"
    :validate [#(seq %) "Filepath cannot be empty"]]
   ["-h" "--help" "Show this help"]
   ["-d" "--debug FILE-PATH" "Specify file to call parser debug for ambiguous print"
    :validate [#(seq %) "Filepath cannot be empty"]]])

(defn parse-args [args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (cond
      errors
      {:status :error, :message (str "Error(s): " (str/join "\n" errors))}

      (:help options)
      {:status :help, :message summary}

      (:compile options)
      {:status :success, :file (:compile options)}

      (:debug options)
      {:status :debug, :file (:debug options), :message "Debugging... Calling parse debugger"}

      :else
      {:status :error, :message "No file specified. Use -h or --help for usage information."})))

(defn read-file [filename]
  (try
    (slurp filename)
    (catch java.io.FileNotFoundException e
      (throw (Exception. (str "File was not found: " e))))))

(defn transform-parsed [parsed]
  (insta/transform
   {:IntegerLiteral #(vector :int (Integer/parseInt %))
    :DoubleLiteral #(vector :double (Double/parseDouble %))
    :StringLiteral #(vector :string %)
    :BooleanLiteral #(vector :bool (Boolean/parseBoolean %))}
   parsed))

(defn extract-expression [node]
  (if (sequential? node)
    (case (first node)
      :Expression
      (rest node)

      (mapcat extract-expression node))
    []))

(defn extract-variable [node]
  (m/match node
    ([:Type ?t] [:Identifier ?id] "=" & ?other)
    (do (println "VariableDec:" ?t ?id)
        {:vartype ?t,
         :varname ?id,
         :expression (extract-variable ?other)})

    ([:Identifier ?id] "=" & ?other)
    (do (println "VariableAssign:" ?id)
        {:varname ?id,
         :expression (extract-variable ?other)})

    ("if" ?exp _)
    (do (println "If:")
        {:vartype "bool",
         :expression (extract-variable ?exp)})

    ("while" ?exp _)
    (do (println "While")
        {:vartype "bool"
         :expression (extract-variable ?exp)})

    ;; Expr in :VarDec and :VarAssign
    ([:Expression & ?other])
    (do (println "Expression:" ?other)
        (extract-variable ?other))

    ;; Nested Expr in :VarDec and :VarAssign
    ;; TODO: Add parenthesis to indicate priority
    [[:Expression & ?nested-exp] & ?other]
    (concat (extract-variable ?nested-exp) (extract-variable ?other))

    ;; Expr in :IfBlock, :WhileBlock and :ConsoleWrite
    [:Expression & ?other]
    (do (println "Nested Expression:" ?other)
        ?other)

    ("Console.WriteLine" ?exp)
    (extract-variable ?exp)

    [?one & ?other]
    (do (println "Other:" ?one)
        (concat [?one] (extract-variable ?other)))

    _ (println "Nothing:" node)))

(defn extract [k node]
  (case k
    :VariableDeclaration (extract-variable node)
    :VariableAssignment (extract-variable node)
    :IfBlock (extract-variable node)
    :WhileBlock (extract-variable node)
    :ConsoleWrite (extract-variable node)))

(defn extract-info [node]
  (if (sequential? node)
    (case (first node)
      :VariableDeclaration
      (concat [{:type :VariableDeclaration
                :values (extract :VariableDeclaration (rest node))}]
              (mapcat extract-info (rest node)))

      :VariableAssignment
      (concat [{:type :VariableAssignment
                :values (extract :VariableAssignment (rest node))}]
              (mapcat extract-info (rest node)))

      :InstructionReturn
      (concat [{:type :InstructionReturn
                :values (rest node)
                :expression (extract-expression node)}]
              (mapcat extract-info (rest node)))

      :IfBlock
      (concat [{:type :IfBlock
                :values (extract :IfBlock (rest node))}]
              (mapcat extract-info (rest node)))

      :WhileBlock
      (concat [{:type :WhileBlock
                :values (extract :WhileBlock (rest node))}]
              (mapcat extract-info (rest node)))

      :ConsoleWrite
      (concat [{:type :ConsoleWrite
                :values (extract :ConsoleWrite (rest node))}]
              (mapcat extract-info (rest node)))

      :MethodCall
      (concat [{:type :MethodCall
                :values (rest node)
                :expression (extract-expression node)}]
              (mapcat extract-info (rest node)))

      (mapcat extract-info node))
    []))

(defn -main [& args]
  (let [{:keys [status message file]} (parse-args args)]
    (case status
      :error (do (println message) (System/exit 1))
      :help (println message)
      :success (let [file-content (read-file file)
                     parsed (parser/parse-content file-content)]
                 (println (rest parsed))
                 (extract-info (transform-parsed parsed)))
      :debug (do
               (println message)
               (parser/parser-debug (read-file file))))))

(-main "-c" "resources/Sample.cs")