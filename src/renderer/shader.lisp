;;;; A class for loading, compiling, and using shaders
;;;; that hides all of the opengl stuff
(in-package pyco8)

(defclass shader-program ()
 ((program :reader get-program)
  (vert-shader :reader get-vert-shader :writer set-vert-shader)
  (frag-shader :reader get-frag-shader :writer set-frag-shader)))

(defmethod set-vert-shader ((self shader-program) source)
 (setf (slot-value self 'vert-shader) (gl:create-shader :vertex-shader))
 (add-shader self source (get-vert-shader self)))

(defmethod set-frag-shader ((self shader-program) source)
 (setf (slot-value self 'frag-shader) (gl:create-shader :fragment-shader))
 (add-shader self source (get-frag-shader self)))

(defmethod add-shader ((self shader-program) source shader-id)
 (gl:shader-source shader-id source)
 (gl:compile-shader shader-id)
 ; todo: check for compiler errors before attaching
 (gl:attach-shader (get-program self) shader-id))

(defmethod load-from-files ((self shader-program) vertex-path fragment-path)
 (setf (slot-value self 'program) (gl:create-program))
 (set-vert-shader self (alexandria:read-file-into-string vertex-path))
 (set-frag-shader self (alexandria:read-file-into-string fragment-path))
 ; todo: detach/delete shaders after linking; check for linker errors
 (gl:link-program (get-program self)))

(defmethod use-program ((self shader-program))
 (gl:use-program (get-program self)))

(defmethod get-location ((self shader-program) name)
 (gl:get-uniform-location (get-program self) name))

;; set uniforms
;; the apply cons is because args is a list and uniform_'s arguments are the location + the contents of args,
;; and it's easiest to add the location to the list and than apply

;; todo: make a macro which autogenerates the apply cons
(defmethod addf ((self shader-program) uniform-name &rest args)
 (apply #'gl:uniformf (cons (get-location self uniform-name) args)))

(defmethod addi ((self shader-program) uniform-name &rest args)
 (apply #'gl:uniformi (cons (get-location self uniform-name) args)))