;;;; interprets script8 bytecode

(in-package pyco8)

;; creates a new, dynamic, empty vector
(defun new-vector ()
 (make-array 0 :fill-pointer 0 :adjustable t))

(defclass interpreter () 
 ((stack 
   :initform ()
   :accessor get-stack)
  (instructions
   :writer set-instructions)
  (current-instruction
   :initform 0
   :writer goto
   :reader current-instruction)
  (statics
   :initform (new-vector))
  (locals 
   :initform (new-vector))))

;; generates four helper methods for an array
;; the first, a get method, fetches from array
;; the second sets in the array
;; the third adds a new item to end of array
;; the fourth deletes item at end of array
(defmacro gen-array-helper-methods (array-name get-name set-name new-name del-name)
 `(progn
   (defmethod ,get-name (index (self interpreter))
    (elt (slot-value self ',array-name) index))
   (defmethod ,set-name (index value (self interpreter))
    (setf (elt (slot-value self ',array-name) index) value))
   (defmethod ,new-name (value (self interpreter))
    (vector-push-extend value (slot-value self ',array-name)))
   (defmethod ,del-name ((self interpreter))
    (vector-pop (slot-value self ',array-name)))))

(gen-array-helper-methods statics get-static set-static new-static kill-static)
(gen-array-helper-methods locals get-local set-local new-local end-local)

;;; VM instructions

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

;; generates a method on the interpreter that applies
;; operator to num-args arguments popped off the stack,
;; and pushes the result onto the stack
(defmacro make-operator (name operator num-args)
 `(defmethod ,name ((self interpreter))
   (push-vm ,(cons operator (repeat '(pop-vm self) num-args)) self)))


(make-operator not-vm not 1)
(make-operator add-vm + 2)
(make-operator sub-vm - 2)
(make-operator eq-vm equal 2)
(make-operator gt-vm > 2)
(make-operator lt-vm < 2)
(make-operator and-vm and 2)
(make-operator or-vm or 2)

;; run the current line, advance to next instruction
(defmethod run-and-advance ((self interpreter))
 (let ((instructions (slot-value self 'instructions))
       (current (current-instruction self)))
  (goto (+ current 1) self)
  (apply 
   (elt instructions current)
   (elt instructions (+ current 1)))))