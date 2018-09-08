;;;; contains start, which starts a new instance of pyco8

(in-package pyco8)
(export '(start))

(defun start ()
 ;; you need a main thread to make it work on mac
 (with-body-in-main-thread ()
  (with-init-window (:title "Pyco-8" :width (truncate *size*) :height (truncate *size*))
   (setf %gl:*gl-get-proc-address* #'get-proc-address)
   (load-from-files *shader-program* "assets/shaders/pixel.vert" "assets/shaders/color_picker.frag")
   (load-sprites "assets/test_cart/sprites.s8d")
   (gl:clear-color 0 0 0 0)

   ;;; create/configure the vao
   (setf *vbo-vertices* (gl:gen-buffer))
   (setf *vao* (gl:gen-vertex-array))
   
   (gl:bind-vertex-array *vao*)
   (gl:bind-buffer :array-buffer *vbo-vertices*)

   ;; create the vertex array
   ;; we need to load a lisp vector into a gl-array to pass it to opengl functions
   (let ((arr (gl:alloc-gl-array :float 8))
         (verts (vector 0.0 0.0
                        *ndc-coord* 0.0
                        *ndc-coord* (- *ndc-coord*)
                        0.0 (- *ndc-coord*))))
    (dotimes (i (length verts))
     (setf (gl:glaref arr i) (aref verts i))) ; copy contents of vector in gl array
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))
   
   ;; setup attribute pointer for location in the shader
   (gl:enable-vertex-attrib-array 0)
   (gl:vertex-attrib-pointer 0 2 :float nil 0 (cffi:null-pointer))

   ;; set event callbacks
   (set-key-callback 'key-tracker)
   (set-window-size-callback 'window-size-callback)
   (loop until (window-should-close-p)
    do (render)
    do (swap-buffers)
    do (poll-events)))))