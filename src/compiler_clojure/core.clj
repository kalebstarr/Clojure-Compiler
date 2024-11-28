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
   "S = Namespace
    Namespace = 'namespace' <Whitespace> Identifier <Whitespace> '{' <Whitespace> Class <Whitespace> '}'
    Class = 'class' <Whitespace> Identifier <Whitespace> '{' <Whitespace> Method (<Whitespace> Method)* <Whitespace> '}'
    Method = 'static' <Whitespace> Type <Whitespace> Identifier <Whitespace> '(' <Whitespace> Type <Whitespace> Identifier <Whitespace> ')' <Whitespace> '{' <Whitespace> '}'
    Type = 'void' | 'int' | 'string'
    Identifier = #'[a-zA-Z0-9]*'
    <Whitespace> = #'\\s*'"))
(grammar "namespace there {
          class this {
          static void Main(int args){}
          static int Main(string args){}}}")

(defn -main [& args]
  (let [file-content (read-file (first args))]  
    (grammar file-content)))
