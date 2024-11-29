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

(defn read-file [filename]
  (try
    (with-open [rdr (clojure.java.io/reader filename)]
      (str/join "\n" (line-seq rdr)))
    (catch java.io.FileNotFoundException e
      (throw (Exception. (str "File was not found: " e))))))

(def grammar
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
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      errors
      (do
        (println "Error: " (str/join "\n" errors))
        (System/exit 1))

      (:help options)
      (println summary)

      (:compile options)
      (let [file-path (:compile options)
            file-content (read-file file-path)]
        (println (grammar file-content)))

      :else
      (do
        (println "No file specified. Use -h or --help for usage information.")
        (System/exit 1)))))
