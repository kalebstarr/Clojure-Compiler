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
    GenericMethodDeclaration = 'static' Type Identifier '(' ParameterList? ')' '{' (Comment | VariableDeclaration | Instruction)* '}'
    VoidMethodDeclaration = 'static' 'void' Identifier '(' ParameterList? ')' '{' (Comment | VariableDeclaration | Instruction)* '}'
    ParameterList = Parameter (',' Parameter)*
    Parameter = (Type Identifier)

    Identifier = #'[a-zA-Z_][a-zA-Z0-9_]*'

    Type = 'bool' | (('int' | 'double' | 'string') #'\\[\\]'?)

    Literal = IntegerLiteral | DoubleLiteral | StringLiteral | BooleanLiteral
    IntegerLiteral = #'-?[0-9]+'
    DoubleLiteral = #'-?[0-9]+\\.[0-9]+' | IntegerLiteral
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

    Expression = Literal | Variable | MethodCall | '(' Expression ')' | Expression Operator Expression | LogicNot Expression

    Variable = Identifier
    VariableDeclaration = Type Variable '=' Expression ';' | Type Variable ';' | VariableDeclarationBlock
    VariableDeclarationBlock = '{' VariableDeclaration* '}'

    Assignment = Variable '=' Expression ';'


    Instruction = IfElseBlock | WhileBlock | MethodCall | ConsoleWrite | Assignment | InstructionBlock | InstructionReturn
    InstructionBlock = '{' Instruction* '}'
    IfElseBlock = IfBlock ElseBlock?
    IfBlock = 'if' '(' Expression ')' (VariableDeclaration | InstructionBlock | Instruction)
    ElseBlock = 'else' (VariableDeclaration | InstructionBlock | Instruction)
    InstructionReturn = 'return' Expression ';'

    WhileBlock = 'while' '(' Expression ')' (VariableDeclaration | InstructionBlock | Instruction)

    MethodCall = Identifier '(' ArgumentList? ')'
    ArgumentList = Expression (',' Expression)*

    ConsoleWrite = 'Console.WriteLine' '(' Expression ')' ';'


    Comment = SingleLineComment | MultiLineComment
    SingleLineComment = '//' #'.*' #'(?=\\r?\\n)'
    MultiLineComment = '/*' #'[^*]*\\*+(?:[^/*][^*]*\\*+)*/'"
   :auto-whitespace :standard))

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
