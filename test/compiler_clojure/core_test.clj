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

(deftest extract-info-test
  (testing "extract-info extracts VariableDeclarations"
    (with-redefs []
      (let [input [:a 9 :b [:VariableDeclaration 9]]
            expected {:type :VariableDeclaration
                      :values '(9)
                      :vartype '("bool")
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual)))))

  (testing "extract-info extracts multiple VariableDeclarations"
    (with-redefs []
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
    (with-redefs []
      (let [input [:a 9 :b [:VariableAssignment 9]]
            expected {:type :VariableAssignment
                      :values '(9)
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual)))))

  (testing "extract-info extracts InstructionReturn"
    (with-redefs []
      (let [input [:a 9 :b [:InstructionReturn 9]]
            expected {:type :InstructionReturn
                      :values '(9)
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual)))))

  (testing "extract-info extracts IfBlock"
    (with-redefs []
      (let [input [:a 9 :b [:IfBlock 9]]
            expected {:type :IfBlock
                      :values '(9)
                      :vartype '("bool")
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual)))))

  (testing "extract-info extracts WhileBlock"
    (with-redefs []
      (let [input [:a 9 :b [:WhileBlock 9]]
            expected {:type :WhileBlock
                      :values '(9)
                      :vartype '("bool")
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual)))))

  (testing "extract-info extracts ConsoleWrite"
    (with-redefs []
      (let [input [:a 9 :b [:ConsoleWrite 9]]
            expected {:type :ConsoleWrite
                      :values '(9)
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual)))))

  (testing "extract-info extracts MethodCall"
    (with-redefs []
      (let [input [:a 9 :b [:MethodCall 9]]
            expected {:type :MethodCall
                      :values '(9)
                      :expression '([:Expression 9])}
            actual (first (extract-info input))]
        (is (= expected actual))))))
