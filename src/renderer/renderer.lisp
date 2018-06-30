;;;; does the rendering and context creation

(in-package pyco8)
(export '(start)) ; starts new instance

(defvar *scale* 4)
(defvar *size* 512)
(defvar *shader-program* (make-instance 'shader-program))
(defvar *vao* nil)
(defvar *vbo-vertices* nil)
(defvar *ndc-coord* (* 2.0 (/ *scale* *size*)))
(defvar *sprites* nil)

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
   (set-window-should-close)))

;; resizes the gl viewport to maintain aspect ratio after resizes
(def-window-size-callback window-size-callback (window w h)
 (setf *size* 
  (if (< w h)
    w
    h))
  (gl:viewport (/ (- w *size*) 2) (/ (- h *size*) 2) *size* *size*))

(defun fat-to-normalized (x y)
 "Converts fat pixel coordinates to NDC"
 (values
  (+ -1.0 (* x *ndc-coord*))
  (- 1.0 (* y *ndc-coord*))))

(defun draw-pixel (x y color)
 (gl:bind-vertex-array *vao*)
 (use-program *shader-program*)
 
 ;; set uniforms
 (addi *shader-program* "color" color)
 (multiple-value-bind (xn yn) (fat-to-normalized x y)
  (addf *shader-program* "offsetx" xn)
  (addf *shader-program* "offsety" yn))
 
 (gl:draw-arrays :quads 0 4))

(defun render ()
 (gl:clear :color-buffer)
 
 ;; four corner tests
 (draw-pixel 0 0 1)
 (draw-pixel 0 127 1)
 (draw-pixel 127 127 1)
 (draw-pixel 127 0 1)

 ;; sprite tests
 (draw-sprite 125 10 0) ; slightly off-screen
 (draw-sprite 10 10 0))

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
   (set-key-callback 'quit-on-escape)
   (set-window-size-callback 'window-size-callback)
  
   (loop until (window-should-close-p)
    do (render)
    do (swap-buffers)
    do (poll-events)))))

