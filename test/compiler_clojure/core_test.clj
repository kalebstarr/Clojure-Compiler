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
