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
  ;(ql:quickload "trivia")
  (load "lexer.lisp")
  (load "php-preprocessor.lisp")
  )


(load-repl-env)


;;; Temporary Utils
(defun file-contents-string (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))


(defun relative-path (full-path common-path)
  (string-replace common-path full-path "."))


(defun create-php-lexer (php-file-path)
  (let* ((source (file-contents-string php-file-path))
         (php-preprocs (make-instance 'php-preprocessors :source source)))
    (make-instance 'lexer :source (run-all-preprocessors php-preprocs :output nil))
  ))


;; (defparameter *tested-lexer* (create-php-lexer "draft/test-0.php"))

;; (defparameter *tested-lexer*  (make-instance 'lexer :source "(-2, +200, -100, 20)"))

;; (run-all *tested-lexer*)

(defun run-test (php-path &optional (start-dir "."))
  (let ((lexer (create-php-lexer php-path)))
    (format t "Lexer Parsing: ~a~%~%" (ppath:relpath php-path start-dir))
    (run-all lexer :output nil)))
;; (next-step *tested-lexer*)

;(next-token *tested-lexer*)

;(print (token-queue *tested-lexer*))


(defparameter *path-queue* (queue))
(defparameter *scaned-dir*
  "/mnt/d/Coding/Python3/smart_code_modifier/draft/phpmyadmin/libraries/classes/")
(cl-fad:walk-directory *scaned-dir*
                       (lambda (path) (enq (namestring path) *path-queue*))
                       :test (lambda (path) (string$= ".php" (namestring path))))

(defun test-scanned-dir ()
  (do ((php-path (front *path-queue*) (front *path-queue*)))
      ((queue-empty-p *path-queue*) nil)
    (run-test php-path *scaned-dir*)
    (deq *path-queue*)))


;(test-scanned-dir)

(run-test
 "/mnt/d/Coding/Python3/smart_code_modifier/draft/phpmyadmin/libraries/classes/OpenDocument.php"
 *scaned-dir*)

(load-repl-env)


(defclass syntax ()
  nil)

;;; Class PHPSyntax
(defclass php-syntax ()
  ())
