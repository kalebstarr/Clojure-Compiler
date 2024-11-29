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
    Program = Using* <Whitespace> Namespace
    Using = 'using' <Whitespace> Identifier <Whitespace> ';'
    Namespace = 'namespace' <Whitespace> Identifier <Whitespace> '{' <Whitespace> Class <Whitespace> '}'
    Class = 'class' <Whitespace> Identifier <Whitespace> '{' <Whitespace> Method (<Whitespace> Method)* <Whitespace> '}'
    Method = 'static' <Whitespace> Type <Whitespace> Identifier <Whitespace> '(' <Whitespace> Type <Whitespace> Identifier <Whitespace> ')' <Whitespace> '{' <Whitespace> Methodcall (<Whitespace> Methodcall)* <Whitespace> '}'
    Methodcall = (Methodcall | #'[a-zA-Z0-9.\\(\\)]*') <Whitespace> ';'
    Type = 'void' | 'int' | 'string' | 'string[]'
    Identifier = #'[a-zA-Z0-9]*'
    <Whitespace> = #'\\s*'"))

(defn -main [& args]
  (let [file-content (read-file (first args))]  
    (grammar file-content)))
