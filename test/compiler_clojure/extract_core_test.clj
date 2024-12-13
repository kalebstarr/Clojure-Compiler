(ns compiler-clojure.extract-core-test
  (:require
   [clojure.test :refer :all]
   [compiler-clojure.extract-core :refer :all]))

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