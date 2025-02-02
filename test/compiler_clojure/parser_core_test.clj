(ns compiler-clojure.parser-core-test
  (:require [clojure.test :refer :all]
            [compiler-clojure.parser-core :refer :all]
            [instaparse.core :as insta]))

(deftest custom-print-failure-test
  (testing "custom-print-failure receives one reason"
    (let [input {:reason [{:expecting "}"}],
                 :line 21}
          expected "Parse Error: Line: 21: Expected }\r\n"
          actual (with-out-str (custom-print-failure input))]
      (is (= expected actual))))

  (testing "custom-print-failure receives more than one reason"
    (let [input {:reason [{:expecting "}"}
                          {:expecting "string"}
                          {:expecting "int"}],
                 :line 21}
          expected "Parse Error: Line: 21: Expected one of }, string, int\r\n"
          actual (with-out-str (custom-print-failure input))]
      (is (= expected actual))))

  (testing "custom-print-failure with full and partial reasons"
    (let [input {:reason [{:expecting "}" :full true}
                          {:expecting "string" :full false}],
                 :line 21}
          expected "Parse Error: Line: 21: Expected one of }, string\r\n"
          actual (with-out-str (custom-print-failure input))]
      (is (= expected actual)))) 

  (testing "custom-print-failure with duplicate reasons"
    (let [input {:reason [{:expecting "}" :full true}
                          {:expecting "}" :full true}
                          {:expecting "string" :full false}],
                 :line 21}
          expected "Parse Error: Line: 21: Expected one of }, string\r\n"
          actual (with-out-str (custom-print-failure input))]
      (is (= expected actual)))))

(deftest parse-content-test
  (testing "parse-content succeeds in parsing a file using a grammar"
    (with-redefs [insta/add-line-and-column-info-to-metadata (fn [_ _] "content")
                  insta/failure? (fn [_] false)]
      (is (= (parse-content "file-content") "content"))))
  
  (testing "parse-content fails parsing a file using a grammar"
    (with-redefs [insta/add-line-and-column-info-to-metadata (fn [_ _] "content")
                  insta/failure? (fn [_] true)
                  custom-print-failure (fn [_] "Parse Error")]
      (is (= (parse-content "file-content") "Parse Error")))))
