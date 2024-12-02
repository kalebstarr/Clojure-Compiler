(ns compiler-clojure.parser-core
  (:require [instaparse.core :as insta]))

(def csharp-grammar
  (insta/parser
   "S = Program

    Program = Using* Namespace

    Using = 'using' Identifier ';'

    Namespace = 'namespace' Identifier '{' ClassDeclaration '}'

    ClassDeclaration = 'class' Identifier '{' MethodDeclaration+ '}'

    MethodDeclaration = GenericMethodDeclaration | VoidMethodDeclaration
    GenericMethodDeclaration = 'static' Type Identifier '(' ParameterList? ')' '{' (Comment | VariableDeclaration | VariableAssignment)* '}'
    VoidMethodDeclaration = 'static' 'void' Identifier '(' ParameterList? ')' '{' (Comment | VariableDeclaration | VariableAssignment)* '}'
    ParameterList = Parameter (',' Parameter)*
    Parameter = (Type Identifier)

    Identifier = #'[a-zA-Z_][a-zA-Z0-9_]*'

    Type = 'bool' | (('int' | 'double' | 'string') #'\\[\\]'?)

    Literal = IntegerLiteral | DoubleLiteral | StringLiteral | BooleanLiteral
    IntegerLiteral = #'-?[0-9]+'
    DoubleLiteral = #'-?[0-9]+\\.[0-9]+'
    StringLiteral = #'\"(?:[^\\\"]|\\.)*\"'
    BooleanLiteral = 'true' | 'false'

    Operator = ArithmeticOperator | ComparisonOperator | BooleanOperator
    ArithmeticOperator = Plus | Minus | Star | Slash | Modulo
    Plus = '+'
    Minus = '-'
    Star = '*'
    Slash = '/'
    Modulo = '%'
    ComparisonOperator = Equals | Smaller | Greater | Seq | Geq | Uneq
    Equals = '=='
    Smaller = '<'
    Greater = '>'
    Seq = '<=' 
    Geq = '>='
    Uneq = '!='
    BooleanOperator = LogicAnd | LogicOr | LogicNot
    LogicAnd = '&&'
    LogicOr = '||'
    LogicNot = '!'


    Expression = LogicalExpression
    
    LogicalExpression = ComparisonExpression (LogicalOperator ComparisonExpression)*
    LogicalOperator = LogicAnd | LogicOr
    
    ComparisonExpression = ArithmeticExpression (ComparisonOperator ArithmeticExpression)?
    
    ArithmeticExpression = Term ((Plus | Minus) Term)*
    Term = Factor ((Star | Slash | Modulo) Factor)*
    Factor = LogicNot? (Literal | '(' Expression ')' | Identifier)


    VariableDeclaration = Type Identifier '=' Expression ';'
    VariableAssignment = Identifier '=' Expression ';'
    VariableDeclarationBlock = '{' VariableDeclaration+ '}'


    Comment = SingleLineComment | MultiLineComment
    SingleLineComment = '//' #'.*' #'(?=\\r?\\n)'
    MultiLineComment = '/*' (#'[^*/]+' | '*' #'[^/]' | '/' #'[^*]')* '*/'"
   :auto-whitespace :standard))

(defn parser-debug [file-content]
  (if (empty? file-content)
    (let [grammar-string "using System;
                         
                         namespace FibonacciRecursive {
                           class TestClass {
                             static void Main(string[] args) {
                                 int this = 9 + 9 + 9;
                        that = 9 || 9 && 9;
                        int then = 8 == that && there != then && 8 + 10;
                             }
                           }
                         }"]
      (println "Non-ambiguous:")
      (println (csharp-grammar grammar-string)) 
      (println "Ambiguous:")
      (println (insta/parses csharp-grammar grammar-string)))
    ((println "Non-ambiguous:")
     (println (csharp-grammar file-content))
     (println "Ambiguous:")
     (println (insta/parses csharp-grammar file-content)))))
(parser-debug "")

(defn custom-print-failure [{:keys [reason line]}]
  (print (str "Parse Error: Line: " line ": "))
  (let [full-reasons (distinct (map :expecting
                                    (filter :full reason)))
        partial-reasons (distinct (map :expecting
                                       (filter (complement :full) reason)))
        total (+ (count full-reasons) (count partial-reasons))
        all-reasons (interpose ", " (concat full-reasons partial-reasons))]
    (if (<= total 1)
      (print "Expected ")
      (print "Expected one of "))

    (doseq [r all-reasons]
      (print r))
    (println)))
