;;;; various utility functions and macros
(in-package pyco8)

(defun split (seq index)
 "Given a sequence and an index, returns left and right of index, with index not in either subsequence."
 (values (subseq seq 0 index) (subseq seq (+ 1 index) (length seq))))

(defun lines (str)
 "Given a string, lines separates it into a list of all the lines in the string, with newline chars removed."
 (let ((chr (position #\Newline str)))
  (if chr
   (multiple-value-bind (left right) (split str chr)
    (cons left (lines right)))
   (cons str ()))))