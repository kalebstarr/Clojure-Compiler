(ns compiler-clojure.core-test
  (:require [clojure.test :refer :all]
            [compiler-clojure.core :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(deftest read-file-test
  (testing "File is given and read successfull"
    (with-redefs [io/reader (fn [_] (java.io.BufferedReader. (java.io.StringReader. "Line 1\nLine 2\nLine 3")))]
      (is (= (read-file "dummy-file.txt") "Line 1\nLine 2\nLine 3"))))

  (testing "File is not found"
    (is (thrown-with-msg? Exception #"File was not found"
                          (read-file "nonexistent-file.txt")))))

(deftest parse-args-test
  (testing "parse-args is called with -c but no file"
    (let [{:keys [status message]} (parse-args ["-c"])]
      (is (= status :error))
      (is (str/includes? message "Error(s): "))))

  (testing "parse-agrs is called with -h"
    (let [{:keys [status message]} (parse-args ["-h"])]
      (is (= status :help))
      (is (str/includes? message "-c"))
      (is (str/includes? message "--compile FILE-PATH"))
      (is (str/includes? message "Specify file to compile"))
      (is (str/includes? message "-h"))
      (is (str/includes? message "--help"))
      (is (str/includes? message "Show this help"))))

  (testing "parse-args is called with --compile and a file"
    (is (= (parse-args ["-c test-file.test"]) {:status :success, :file " test-file.test"})))

  (testing "parse-args is called without any arguments"
    (is (= (parse-args [""]) {:status :error, :message "No file specified. Use -h or --help for usage information."})))

  (testing "parse-args is called with -d"
    (is (= (parse-args ["-d test-file.test"]) {:status :debug, :file " test-file.test" :message "Debugging... Calling parse debugger"}))))

(deftest extract-type-test
  (testing "extract-type extracts one type correctly"
    (let [input [:a [:Type "double"]]
          expected '("double")
          actual (extract-type input)]
      (is (= expected actual))))

  (testing "extract-type returns nothing when no type exists"
    (let [input [:a [:b "double"] [:c]]
          expected []
          actual (extract-type input)]
      (is (= expected actual))))

  (testing "extract-type finds all types in tree"
    (let [input [:a [:Type "double"] [:Type "string"] [:b [:c [:Type "bool"]]]]
          expected '("double" "string" "bool")
          actual (extract-type input)]
      (is (= expected actual)))))

(deftest extract-expression-test
  (testing "extract-expression extracts correctly given one Expression"
    (let [input [:Expression [:a "double"] [:b 2] [:c true] [:c [:e 9]]]
          expected '([:a "double"] [:b 2] [:c true] [:c [:e 9]])
          actual (extract-expression input)]
      (is (= expected actual))))

  (testing "extract-expression extracts nothing when no expression exists"
    (let [input [:z [:a "double"] [:b 2] [:c true] [:c [:e 9]]]
          expected []
          actual (extract-expression input)]
      (is (= expected actual))))

  ;; (testing "extract-expression concatinates nested expressions"
  ;;   (let [input [:Expression [:a "double"] [:b 2] [:c true] [:Expression [:a 12] [:b true]] [:c [:e 9]]]
  ;;         expected '([:a "double"] [:b 2] [:c true] [:a 12] [:b true] [:c [:e 9]])
  ;;         actual (extract-expression input)]
  ;;     (is (= expected actual))))

  (testing "Current extract-expression does not extract nested expressions"
    (let [input [:Expression [:a "double"] [:b 2] [:c true] [:Expression [:a 12] [:b true]] [:c [:e 9]]]
          expected '([:a "double"] [:b 2] [:c true] [:Expression [:a 12] [:b true]] [:c [:e 9]])
          actual (extract-expression input)]
      (is (= expected actual)))))

(deftest extract-info-test
  (testing "extract-info extracts VariableDeclarations"
    (with-redefs [extract-expression (fn [_] '([:Expression 9]))
                  extract-type (fn [_] '("bool"))]
      (let [input [:a 9 :b [:VariableDeclaration 9]]
            expected {:type :VariableDeclaration
                      :values '(9)
                      :vartype '("bool")
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual)))))

  (testing "extract-info extracts multiple VariableDeclarations"
    (with-redefs [extract-expression (fn [_] '([:Expression 9]))
                  extract-type (fn [_] '("string"))]
      (let [input [:a 9 :b [:VariableDeclaration 9] [:c 10] [:VariableDeclaration 11]]
            expected (list {:type :VariableDeclaration
                            :values '(9)
                            :vartype '("string")
                            :expression '([:Expression 9])}
                           {:type :VariableDeclaration
                            :values '(11)
                            :vartype '("string")
                            :expression '([:Expression 9])})
            actual (extract-info input)]
        (is (= expected actual))))) 

  (testing "extract-info extracts VariableAssignments"
    (with-redefs [extract-expression (fn [_] '([:Expression 9]))]
      (let [input [:a 9 :b [:VariableAssignment 9]]
            expected {:type :VariableAssignment
                      :values '(9)
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual)))))

  (testing "extract-info extracts InstructionReturn"
    (with-redefs [extract-expression (fn [_] '([:Expression 9]))]
      (let [input [:a 9 :b [:InstructionReturn 9]]
            expected {:type :InstructionReturn
                      :values '(9)
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual)))))

  (testing "extract-info extracts IfBlock"
    (with-redefs [extract-expression (fn [_] '([:Expression 9]))]
      (let [input [:a 9 :b [:IfBlock 9]]
            expected {:type :IfBlock
                      :values '(9)
                      :vartype '("bool")
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual)))))

  (testing "extract-info extracts WhileBlock"
    (with-redefs [extract-expression (fn [_] '([:Expression 9]))]
      (let [input [:a 9 :b [:WhileBlock 9]]
            expected {:type :WhileBlock
                      :values '(9)
                      :vartype '("bool")
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual)))))

  (testing "extract-info extracts ConsoleWrite"
    (with-redefs [extract-expression (fn [_] '([:Expression 9]))]
      (let [input [:a 9 :b [:ConsoleWrite 9]]
            expected {:type :ConsoleWrite
                      :values '(9)
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual)))))

  (testing "extract-info extracts MethodCall"
    (with-redefs [extract-expression (fn [_] '([:Expression 9]))]
      (let [input [:a 9 :b [:MethodCall 9]]
            expected {:type :MethodCall
                      :values '(9)
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual))))))
