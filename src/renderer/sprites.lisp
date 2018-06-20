;;;; code to load and draw sprites

(in-package pyco8)

(defvar *sprite-data* ())

(defun load-sprites (path)
 "Takes a file containing sprite data and loads the data"
 (dolist (data-string (reverse (lines (alexandria:read-file-into-string path))))
  (setq *sprite-data* (cons (read-from-string data-string) *sprite-data*))))

;; todo: optimize this to use raw opengl instead of draw-pixel
(defun draw-sprite (x y sprite-number)
 (let ((sprite-data (nth sprite-number *sprite-data*)))
  (dotimes (i 8)
   (dotimes (j 8)
    (draw-pixel (+ x i) (+ y j) (nth (+ i (* j 8)) sprite-data))))))