
(load "~/.sbclrc")


(ql:quickload "alexandria")
(ql:quickload "cl-ppcre")
(ql:quickload "minghu6")
(ql:quickload "serapeum")


(use-package '#:minghu6)
(use-package '#:serapeum)
(use-package '#:alexandria)


(defun load-repl-env ()
  (ql:quickload "trivia")
  ;(use-package :trivia)
  )


(defun load-lex-env ()
  (defparameter *lex-conf*
    (with-open-file (stream "/mnt/d/Coding/CL/smart-code-modifier/php.lex.conf.lisp")
      (read stream)))

  ;; In essence, it just converts the regex pattern into dfa map manually.
  ;; I'm a little genius maybe, [doge]
  (defparameter *lex-dfa-map* (acdr :lex-dfa-map *lex-conf*))
  (load "lexer.lisp")
  )


(load-lex-env)


;;; Temporary Utils
(defun file-contents-string (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))


(defun create-php-lexer (php-file-path)
  (let ((source (file-contents-string php-file-path)))
    (make-instance 'lexer :source source)
  ))


(defparameter *tested-lexer* (create-php-lexer "draft/test-0.php"))

(defparameter *tested-lexer*  (make-instance 'lexer :source "(-2, +200, -100, 20)"))

(run-all *tested-lexer*)

;; (next-step *tested-lexer*)

;(next-token *tested-lexer*)

;(print (token-queue *tested-lexer*))

