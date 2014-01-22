(defvar *test-name* nil)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro deftest (name parameters &body body)
  "別のテスト関数を呼んだり、個々のテストケースを走らせるのに'check'を使う"
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "各'forms'の式をテストケースとして実行する"
 `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "'forms'の評価結果（真偽値）を順番に結合する"
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "単体のテストケースの結果を報告する。'check'から呼び出される"
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -5)))

(deftest test-* ()
  (check
    (= (* 1 3) 3)
    (= (* 3 5) 15)))
