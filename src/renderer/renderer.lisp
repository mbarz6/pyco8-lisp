;;;; does the rendering and context creation

(in-package pyco8)
(export '(start)) ; starts new instance

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
   (set-window-should-close)))

(defvar *scale* 4.0)
(defvar *size* 512.0)
(defvar *shader-program* (make-instance 'shader-program))
(defvar *vao* nil)
(defvar *vbo_vertices* nil)
(defvar *ndc-coord* (* 2.0 (/ *scale* *size*)))
(defvar *width* *size*)
(defvar *height* *size*)
(defvar *sprites* nil)

(defun fat-to-normalized (x y)
 "Converts fat pixel coordinates to NDC"
 (values
  (+ -1.0 (* x *ndc_coord*))
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

(defvar *shader-time* 0)

(defun render ()
 (gl:clear :color-buffer)
 (draw-pixel 0 0 1)
 (draw-pixel 0 127 1)
 (draw-pixel 127 127 1)
 (draw-pixel 127 0 1))

(defun start ()
 ;; you need a main thread to make it work on mac
 (with-body-in-main-thread ()
  (with-init-window (:title "Pyco-8" :width (truncate *size*) :height (truncate *size*))
   (setf %gl:*gl-get-proc-address* #'get-proc-address)
   (set-viewport 512 512)
   (load-from-files *shader-program* "assets/shader/pixel.vert" "assets/shaders/color_picker.frag")

   ;;; create/configure the vao
   (setf *vbo_vertices* (gl:gen-buffer))
   (setf *vao* (gl:gen-vertex-array))
   
   (gl:bind-vertex-array *vao*)
   (gl:bind-buffer :array-buffer *vbo_vertices*)

   ;; create the vertex array
   (let ((arr (gl:alloc-gl-array :float 8))
         (verts (vector 0.0 0.0
                        *ndc-coord* 0.0
                        *ndc-coord* (- *ndc-coord*)
                        0.0 (- *ndc-coord*))))
    (dotimes (i (length verts))
     (setf (gl:glaref arr i) (aref verts i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))
   (gl:enable-vertex-attrib-array 0)
   (gl:vertex-attrib-pointer 0 2 :float nil 0 (cffi:null-pointer))

   (set-key-callback 'quit-on-escape)
   
   (gl:clear-color 0 0 0 0)
   (loop until (window-should-close-p)
    do (render)
    do (swap-buffers)
    do (poll-events)))))
