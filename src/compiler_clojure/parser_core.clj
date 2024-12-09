(ns compiler-clojure.parser-core
  (:require 
   [instaparse.core :as insta]))

(def csharp-grammar
  (insta/parser
   "Program = Using* Namespace

    Using = 'using' Identifier <Semicolon>

    Namespace = 'namespace' Identifier <LeftBracket> ClassDeclaration <RightBracket>

    ClassDeclaration = 'class' Identifier <LeftBracket> MethodDeclaration+ <RightBracket>

    MethodDeclaration = GenericMethodDeclaration | VoidMethodDeclaration
    GenericMethodDeclaration = 'static' Type Identifier <LeftParen> ParameterList? <RightParen> MethodBody
    VoidMethodDeclaration = 'static' 'void' Identifier <LeftParen> ParameterList? <RightParen> MethodBody
    MethodBody = <LeftBracket> (Instruction)* <RightBracket>
    ParameterList = Parameter (',' Parameter)*
    Parameter = Type Identifier

    Identifier = !('true' | 'false') #'[a-zA-Z_][a-zA-Z0-9_]*'

    Type = 'bool' | (('int' | 'double' | 'string') #'\\[\\]'?)


    Expression = LogicalExpression

    <LogicalExpression> = ComparisonExpression (LogicOperator ComparisonExpression)*
    <ComparisonExpression> = ArithmeticExpression (ComparisonOperator ArithmeticExpression)?
    <ArithmeticExpression> = Term ((Plus | Minus) Term)*
    <Term> = Factor ((Star | Slash | Modulo) Factor)*
    <Factor> = LogicNot? (Literal | <LeftParen> Expression <RightParen> | Identifier | MethodCall)


    Instruction = VariableDeclaration
                | VariableAssignment
                | IfElseBlock
                | WhileBlock
                | InstructionBlock
                | InstructionReturn
                | ConsoleWrite
                | <Comment>
                | MethodCall <Semicolon>
    InstructionBlock = <LeftBracket> Instruction* <RightBracket>
    InstructionReturn = 'return' Expression <Semicolon>

    VariableDeclaration = Type Identifier '=' Expression <Semicolon>
    VariableAssignment = Identifier '=' Expression <Semicolon>

    MethodCall = Identifier <LeftParen> Arguments? <RightParen>
    Arguments = Expression (',' Expression)*


    IfElseBlock = IfBlock ElseBlock?
    IfBlock = 'if' <LeftParen> Expression <RightParen> Instruction
    ElseBlock = 'else' Instruction

    WhileBlock = 'while' <LeftParen> Expression <RightParen> Instruction


    ConsoleWrite = 'Console.WriteLine' <LeftParen> Expression <RightParen> <Semicolon>


    <Literal> = IntegerLiteral
            | DoubleLiteral
            | StringLiteral
            | BooleanLiteral
    IntegerLiteral = #'-?[0-9]+'
    DoubleLiteral = #'-?[0-9]+\\.[0-9]+'
    StringLiteral = #'\"(?:[^\\\"]|\\.)*\"'
    BooleanLiteral = 'true' | 'false'

    Operator = ArithmeticOperator
             | ComparisonOperator
             | LogicOperator
    ArithmeticOperator = Plus
                       | Minus
                       | Star
                       | Slash
                       | Modulo
    Plus = '+'
    Minus = '-'
    Star = '*'
    Slash = '/'
    Modulo = '%'
    ComparisonOperator = Equals
                       | Smaller
                       | Greater
                       | Seq
                       | Geq
                       | Uneq
    Equals = '=='
    Smaller = '<'
    Greater = '>'
    Seq = '<='
    Geq = '>='
    Uneq = '!='
    LogicOperator = LogicAnd | LogicOr
    LogicAnd = '&&'
    LogicOr = '||'
    LogicNot = '!'


    Comment = SingleLineComment | MultiLineComment
    SingleLineComment = #'//.*'
    MultiLineComment = '/*' #'[^*]*\\*+(?:[^/*][^*]*\\*+)*/'


    LeftParen = '('
    RightParen = ')'
    LeftBracket = '{'
    RightBracket = '}'
    Semicolon = ';'"
   :auto-whitespace :standard
   :input-format :ebnf
   :output-format :hiccup))

(defn parser-debug [file-content]
  (if (empty? file-content)
    (let [grammar-string "using System;
                         
                         namespace FibonacciRecursive {
                           class TestClass {
                             static void Main(string[] args) {
                                 int this = 9 + 9 + 9;
                        that = 9 || 9 && 9;
                        int then = 8 == that && there != then && 8 + 10;
                          {that = 9;
                          int thre = 19;}
                             }
                           }
                         }"]
      (println "Non-ambiguous:")
      (println (csharp-grammar grammar-string :unhide :all))
      (println "Ambiguous:")
      (println (count (insta/parses csharp-grammar grammar-string :unhide :all))))
    (do
      (println "Non-ambiguous:")
      (println (csharp-grammar file-content :unhide :all))
      (println "Ambiguous:")
      (println (count (insta/parses csharp-grammar file-content :unhide :all))))))
;; (parser-debug "")

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

(defn parse-content [file-content]
  (let [parsed (insta/add-line-and-column-info-to-metadata
                file-content
                (csharp-grammar file-content))]
    (if (insta/failure? (csharp-grammar file-content))
      (custom-print-failure (insta/get-failure parsed))
      parsed)))
