;;; lexer for script8

(in-package pyco8)

(defvar *keywords* '("if" "else" "elif"))
(defvar *operators* '("+" "-"))

(defclass lexeme () 
 ((kind
   :reader get-kind
   :writer set-kind)
  (data 
   :reader get-data
   :writer set-data)
  (line-number 
   :writer set-line-number
   :reader get-line-number))
  (:documentation 
   "kind represents the kind of lexeme, either :keyword, :identifier, :operator, or :paren
    data depends on kind. :keyword, :operator, and :identifier give name, :paren gives :open or :closed
    line-number is line-number lexeme occurs on, used for error reporting"))

(defmethod fill-lexeme ((instance lexeme) kind data line-number)
 (progn
  (set-kind kind instance)
  (set-data data instance)
  (set-line-number line-number instance))
  instance)

(defun new-lexeme (kind data line-number)
 (fill-lexeme (make-instance 'lexeme) kind data line-number))

(defun lexeme-from-source (word line-number)
 "Takes one word from a line and creates a lexeme from it. 
  It assumes () have been filtered already; use lex-word if you need that."
 (cond
  ((member word *keywords*) (new-lexeme :keyword word line-number))
  ((member word *operators*) (new-lexeme :operator word line-number))
  ((equal word "(") (new-lexeme :paren :open line-number))
  ((equal word ")") (new-lexeme :paren :closed line-number))
  ((equal 0 (length word)) nil)
  (t (new-lexeme :identifier word line-number))))

(defun find-first (str &rest chars)
 "Given a string and some chars, finds the index of the first char which occurs.
  If none are found, nil is returned."
 (dotimes (i (length str))
  (if (member (char str i) chars)
   (return i))))

(defun lex-word (word line-number)
 "Lexes a word like foo() into a list of lexemes."
 (let ((index (find-first word #\( #\))))
  (if index
   (if (equal index 0)
    (cons ; lex the paren
     (lexeme-from-source (subseq word 0 1) line-number)
     (lex-word (subseq word 1) line-number))
    (cons ; lex everything up to the paren
     (lexeme-from-source (subseq word 0 index) line-number)
     (lex-word (subseq word index) line-number)))
   (let ((lexeme (lexeme-from-source word line-number)))
    (if lexeme
     (cons lexeme nil) ; this way, (lex-word "foo" 0) still returns a list
     nil)))))