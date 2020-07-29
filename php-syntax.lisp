(eval-when (:compile-toplevel :load-toplevel)
  (in-package #:php-hacker))


(eval-when (:execute :load-toplevel)
  (load "~/.sbclrc")


  (ql:quickload "alexandria")
  (ql:quickload "cl-ppcre")
  (ql:quickload "minghu6")
  (ql:quickload "serapeum")
  (ql:quickload "cl-fad")
  (ql:quickload "ppath")

  (use-package '#:minghu6)
  (use-package '#:serapeum)
  (use-package '#:alexandria)
  )


(defun load-repl-env ()
  (ql:quickload "trivia")
  ;(use-package :trivia)
  )


(eval-when (:execute)
  (load "lexer.lisp"))


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


;; (defparameter *tested-lexer* (create-php-lexer "draft/test-0.php"))

;; (defparameter *tested-lexer*  (make-instance 'lexer :source "(-2, +200, -100, 20)"))

;; (run-all *tested-lexer*)

(defun run-test ()
  (let ((lexer (create-php-lexer "draft/test-0.php")))
    (run-all lexer)))
;; (next-step *tested-lexer*)

;(next-token *tested-lexer*)

;(print (token-queue *tested-lexer*))

(run-test)

(setq path-queue (queue))

(cl-fad:walk-directory "/mnt/d/Coding/Python3/smart_code_modifier/draft/phpmyadmin/libraries"
                       (lambda (path) (enq (namestring path) path-queue))
                       :test (lambda (path) (string$= ".php" (namestring path))))


(defclass syntax ()
  nil)

;;; Class PHPSyntax
(defclass php-syntax ()
  ())
