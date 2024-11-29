(ns compiler-clojure.core
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

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
  (let [file-content (read-file (first args))]  
    (println (grammar file-content))))
