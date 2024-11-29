(ns compiler-clojure.core
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def cli-options
  [["-c" "--compile FILE-PATH" "Specify file to compile"
    :parse-fn str
    :validate [#(seq %) "Filepath cannot be empty"]]
   ["-h" "--help" "Show this help"]])

(defn parse-args [args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (cond
      errors
      {:status :error, :message (str "Error(s): " (str/join "\n" errors))} 

      (:help options)
      {:status :help, :message summary}

      (:compile options)
      {:status :success, :file (:compile options)}

      :else
      {:status :error, :message "No file specified. Use -h or --help for usage information."})))

(defn read-file [filename]
  (try
    (with-open [rdr (clojure.java.io/reader filename)]
      (str/join "\n" (line-seq rdr)))
    (catch java.io.FileNotFoundException e
      (throw (Exception. (str "File was not found: " e))))))

(def csharp-grammar
  (insta/parser
   "S = Program
    Program = Using* Namespace
    Using = 'using' Identifier ';'
    Namespace = 'namespace' Identifier '{' Class '}'
    Class = 'class' Identifier '{' Method+ '}'
    Method = 'static' Type Identifier '(' Type Identifier ')' '{' Methodcall+ '}'
    Methodcall = #'[a-zA-Z0-9.]*' '(' (Methodcall | Identifier) ')' (';' | #'\\s'*)
    Type = 'void' | 'int' | 'string' | 'string[]'
    Identifier = #'[a-zA-Z0-9.]*'"
   :auto-whitespace :standard))

(defn -main [& args]
  (let [{:keys [status message file]} (parse-args args)]
    (case status
      :error (do (println message) (System/exit 1))
      :help (println message)
      :success (let [file-content (read-file file)]
                 (println (csharp-grammar file-content))))))
