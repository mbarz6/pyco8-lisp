;;;; functions related to keyboard input

(in-package pyco8)

(defparameter *keys-down* ())

(def-key-callback key-tracker (window key scancode action mod-keys)
 (declare (ignore scancode mod-keys))
 (when (and (eq key :escape) (eq action :press))
   (set-window-should-close))
 (if (eq action :press)
  (pushnew key *keys-down*)
  (deletef *keys-down* key))) ; if released, get rid of it 

(defun key-down (key)
 (member key *keys-down*))