(ns compiler-clojure.core
  (:require
   [clojure.string :as str]
   [clojure.tools.cli :refer [parse-opts]]
   [compiler-clojure.parser-core :as parser]
   [instaparse.core :as insta]
   [compiler-clojure.extract-core :as extractor])
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

;; Add more expressive print including expected and received types (?)
(defn type-print-failure [token]
  (let [{:instaparse.gll/keys [start-line start-column]} (meta token)]
    (println "Type Error: Line" start-line ": Column:" start-column)))

;; Add variable stack as param
(defn evaluate-var [expected expression var-stack]
  (when (seq expression)
    (let [token (first expression)
          rest-expr (rest expression)
          token-type (first token)
          token-value (rest token)]
      (case expected
        "int"
        (case token-type
          :int (evaluate-var expected rest-expr var-stack)
          :Identifier (evaluate-var expected rest-expr var-stack)
          :Plus (evaluate-var expected rest-expr var-stack)
          :Minus (evaluate-var expected rest-expr var-stack)
          :Star (evaluate-var expected rest-expr var-stack)
          :Modulo (evaluate-var expected rest-expr var-stack)
          :LeftParen (evaluate-var expected rest-expr var-stack)
          :RightParen (evaluate-var expected rest-expr var-stack)
          (type-print-failure token))

        "double"
        (case token-type
          :int (evaluate-var expected rest-expr var-stack)
          :double (evaluate-var expected rest-expr var-stack)
          :Identifier (evaluate-var expected rest-expr var-stack)
          :Plus (evaluate-var expected rest-expr var-stack)
          :Minus (evaluate-var expected rest-expr var-stack)
          :Star (evaluate-var expected rest-expr var-stack)
          :Slash (evaluate-var expected rest-expr var-stack)
          :Modulo (evaluate-var expected rest-expr var-stack)
          :LeftParen (evaluate-var expected rest-expr var-stack)
          :RightParen (evaluate-var expected rest-expr var-stack)
          (type-print-failure token))

        "bool"
        (case token-type
          :bool (evaluate-var expected rest-expr var-stack)
          :LogicOperator (evaluate-var expected rest-expr var-stack)
          :LeftParen (evaluate-var expected rest-expr var-stack)
          :RightParen (evaluate-var expected rest-expr var-stack)
          :Identifier (evaluate-var expected rest-expr var-stack)
          :int (evaluate-var "arithmetic-in-bool" expression var-stack)
          :double (evaluate-var "arithmetic-in-bool" expression var-stack)
          (type-print-failure token))

        "arithmetic-in-bool"
        (let [next-token (first rest-expr)]
          (case token-type
            :int (if (and next-token (= (first next-token) :ComparisonOperator))
                   (evaluate-var "comparison" rest-expr var-stack)
                   (println "Expected a comparison operator after arithmetic expression"))
            :double (if (and next-token (= (first next-token) :ComparisonOperator))
                      (evaluate-var "comparison" rest-expr var-stack)
                      (println "Expected a comparison operator after arithmetic expression"))
            :Identifier (if (and next-token (= (first next-token) :ComparisonOperator))
                          (evaluate-var "comparison" rest-expr var-stack)
                          (println "Expected a comparison operator after identifier"))
            (println "Invalid arithmetic expression in bool context:" token-type)))

        "comparison"
        (case token-type
          :ComparisonOperator (evaluate-var "arithmetic" rest-expr var-stack)
          (type-print-failure token))

        "arithmetic"
        (case token-type
          :int (evaluate-var "bool-next" rest-expr var-stack)
          :double (evaluate-var "bool-next" rest-expr var-stack)
          :Identifier (evaluate-var "bool-next" rest-expr var-stack)
          (type-print-failure token))

        "bool-next"
        (let [next-token (first rest-expr)]
          (if (and next-token (= (first next-token) :LogicOperator))
            (evaluate-var "bool" rest-expr var-stack)
            (if next-token
              (evaluate-var expected rest-expr var-stack)
              nil)))

        "string"
        (case token-type
          :string (evaluate-var token-type rest-expr var-stack)
          :Plus (evaluate-var token-type rest-expr var-stack)
          (type-print-failure token))

        (println "Not handled yet:" expected))))
  var-stack)

(defn evaluate [extract var-stack]
  (let [expected (:vartype (:values extract))
        varname (:varname (:values extract))
        expression (:expression (:values extract))
        type (:type extract)]
    (case type
      :VariableDeclaration
      (let [updated-var-stack (assoc var-stack varname expected)]
        (evaluate-var expected expression updated-var-stack))
      ;; :IfBlock (do
      ;;            (evaluate-var expected expression))
      ;; :VariableAssignment (do
      ;;                      (evaluate-var expected expression))
      ;; :MethodCall (do
      ;;              (evaluate-var expected expression))
      ;; :InstructionReturn (do
      ;;                     (evaluate-var expected expression))
      (do (println "Unknown type" type)
          var-stack))))

(defn type-check [extracts]
  (reduce
   (fn [var-stack extract]
     (evaluate extract var-stack))
   {}
   extracts))

(defn -main [& args]
  (let [{:keys [status message file]} (parse-args args)]
    (case status
      :error (do (println message) (System/exit 1))
      :help (println message)
      :success (->> file
                    (read-file)
                    (parser/parse-content)
                    (transform-parsed)
                    (extractor/extract-info)
                    (type-check))
      :debug (do
               (println message)
               (parser/parser-debug (read-file file))))))

(-main "-c" "resources/Sample.cs")