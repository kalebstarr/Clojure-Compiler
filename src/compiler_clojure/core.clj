(ns compiler-clojure.core
  (:require
   [clojure.string :as str]
   [clojure.tools.cli :refer [parse-opts]]
   [compiler-clojure.parser-core :as parser]
   [instaparse.core :as insta])
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
   {:IntegerLiteral #(vec (concat [:IntegerLiteral] [(Integer/parseInt %)]))
    :DoubleLiteral #(vec (concat [:DoubleLiteral] [(Double/parseDouble %)]))
    :StringLiteral #(vec (concat [:StringLiteral] [(str %)]))
    :BooleanLiteral #(vec (concat [:BooleanLiteral] [(Boolean/parseBoolean %)]))}
   parsed))

(defn extract-type [node]
  (if (sequential? node)
    (case (first node)
      :Type
      (rest node) 

      (mapcat extract-type node))
    []))

(defn extract-expression [node]
  (if (sequential? node)
    (case (first node)
      :Expression
      (rest node)
  
      (mapcat extract-expression node))
    []))

(defn extract-info [node] 
  (if (sequential? node)
    (case (first node)
      :VariableDeclaration
      (concat [{:type :VariableDeclaration
                :values (rest node)
                :vartype (extract-type node)
                :expression (extract-expression node)}]
              (mapcat extract-info (rest node)))

      :VariableAssignment
      (concat [{:type :VariableAssignment
                :values (rest node)}]
              (mapcat extract-info (rest node)))

      :InstructionReturn
      (concat [{:type :InstructionReturn
                :values (rest node)}]
              (mapcat extract-info (rest node)))

      :IfBlock
      (concat [{:type :IfBlock
                :values (rest node)}]
              (mapcat extract-info (rest node)))

      :WhileBlock
      (concat [{:type :WhileBlock
                :values (rest node)}]
              (mapcat extract-info (rest node)))

      :ConsoleWrite
      (concat [{:type :ConsoleWrite
                :values (rest node)}]
              (mapcat extract-info (rest node)))

      :MethodCall
      (concat [{:type :MethodCall
                :values (rest node)}]
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
