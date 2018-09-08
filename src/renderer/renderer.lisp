;;;; does the rendering and context creation

(in-package pyco8)

(defvar *scale* 4)
(defvar *size* 512)
(defvar *shader-program* (make-instance 'shader-program))
(defvar *vao* nil)
(defvar *vbo-vertices* nil)
(defvar *ndc-coord* (* 2.0 (/ *scale* *size*)))
(defvar *sprites* nil)
(defvar *cam-x* 0)
(defvar *cam-y* 0)
(defvar *current-color* 0)

;; color code to rgb list
(defun resolve-color-rgb (col) 
  (cond 
    ((= col 1) '(29 43 83)) 
    ((= col 2) '(126 37 83)) 
    ((= col 3) '(0 135 81))
    ((= col 4) '(171 82 54))
    ((= col 5) '(95 87 79))
    ((= col 6) '(194 195 199))
    ((= col 7) '(255 241 232))
    ((= col 8) '(255 0 77))
    ((= col 9) '(255 163 0))
    ((= col 10) '(255 236 39))
    ((= col 11) '(0 228 54))
    ((= col 12) '(41 173 255))
    ((= col 13) '(131 118 156))
    ((= col 14) '(255 119 168))
    ((= col 15) '(255 204 170))
    (t '(0 0 0))))

;; color code to normalized decimal rgb list
(defun resolve-color (col) 
  (mapcar (lambda (x) (/ x 255.0)) (resolve-color-rgb col)))

(defun color (col)
  (setf *current-color* col))

(defun camera (x y)
  (setf *cam-x* x)
  (setf *cam-y* y))

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
 (multiple-value-bind (xn yn) (fat-to-normalized (- x *cam-x*) (- y *cam-y*))
  (addf *shader-program* "offsetx" xn)
  (addf *shader-program* "offsety" yn))
 
 (gl:draw-arrays :quads 0 4))

(defun cls (r g b)
  (gl:clear-color r g b 0)
  (gl:clear :color-buffer))

(defun render ()
 (cls 0 0 0)

  (if (key-down :E)
    (draw-sprite 125 10 0) ; slightly off-screen
    (draw-sprite 10 10 0)))