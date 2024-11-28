(ns compiler-clojure.core
  (:require [instaparse.core :as insta])
  (:gen-class))

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
  (println "Hello, World!"))
