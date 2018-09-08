;;;; pyco8.asd

(asdf:defsystem #:pyco8
 :description "It's like Pyco8 but Lisp."
 :author "Michael Barz <mbarz6@gmail.com>"
 :version "0.0.1"
 :serial t
 :depends-on (#:cl-glfw3 #:cl-opengl #:alexandria #:trivial-main-thread)
 :components ((:file "package")
              (:file "src/renderer/shader")
              (:file "src/renderer/renderer")
              (:file "src/renderer/sprites")
              (:file "src/utilities")
              (:file "src/input")
              (:file "src/main")))