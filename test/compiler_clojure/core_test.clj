(ns compiler-clojure.core-test
  (:require [clojure.test :refer :all]
            [compiler-clojure.core :refer :all]
            [clojure.java.io :as io]))

(deftest read-file-test
  (testing "File is given and read successfull"
    (with-redefs [io/reader (fn [_] (java.io.BufferedReader. (java.io.StringReader. "Line 1\nLine 2\nLine 3")))]
      (is (= (read-file "dummy-file.txt") "Line 1\nLine 2\nLine 3"))))

  (testing "File is not found"
    (is (thrown-with-msg? Exception #"File was not found"
                          (read-file "nonexistent-file.txt")))))
