;;;; interprets script8 bytecode

(in-package pyco8)

(defclass interpreter () 
 ((stack 
   :initform ()
   :accessor get-stack)
  (instructions
   :writer set-instructions)
  (current-instruction
   :initform 0
   :writer goto
   :reader current-instruction)))

(defmethod goto-if (line (self interpreter))
 (if (pop-vm self)
  (goto line self)
  nil))

(defmethod goto-if-not (line (self interpreter))
 (if (not (pop-vm self))
  (goto line self)
  nil))

(defmethod next-instr ((self interpreter))
 (goto (+ 1 (current-instruction self)) self))

(defmethod pop-vm ((self interpreter))
 (let ((head (car (get-stack self))))
  (setf (get-stack self) (cdr (get-stack self)))
  head))

(defmethod push-vm (value (self interpreter))
 (setf (get-stack self) (cons value (get-stack self))))

;; declare a function


;; generates a method on the interpreter that applies
;; some binary operator to two arguments popped off the stack,
;; and pushes the result onto the stack

;; TODO: defmacro make-operator (name operator num-args)
(defmacro make-binary-operator (name operator)
 `(defmethod ,name ((self interpreter))
   (push-vm self 
    (,operator (pop-vm self) (pop-vm self)))))

(make-binary-operator add-vm +)
(make-binary-operator sub-vm -)
(make-binary-operator eq-vm equal)
(make-binary-operator gt-vm >)
(make-binary-operator lt-vm <)
(make-binary-operator and-vm and)
(make-binary-operator or-vm or)
(make-binary-operator not-vm not)

