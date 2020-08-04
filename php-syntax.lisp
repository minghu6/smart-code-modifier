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


;;; Temporary Utils Start
(defun file-contents-string (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))


;;; Temporary Utils End


(defun create-php-lexer (php-file-path)
  (let* ((source (file-contents-string php-file-path))
         (php-preprocs (make-instance 'php-preprocessors :source source)))
    (make-instance 'lexer :source (run-all-preprocessors php-preprocs :output nil))
  ))


(defun get-php-token-queue (php-file-path)
  (let ((mylexer (create-php-lexer php-file-path)))
    (run-all mylexer :output nil)
    (token-queue mylexer)))


(defparameter *php-token-labels*
  '(("'((\\s|\\S)*)'" singlequote-string)
    ("\"((\\s|\\S)*)\"" doublequote-string)
    ("(?i)^([_\\\\a-z][_\\\\a-z0-9]*)$" identity)
    ("(?i)^(\\$+[_\\a-z][_\\a-z0-9]*)$" var)
    ("^([\\[|\\]|\\(|\\)|\\{|\\}])$" parenthesis)
    ("^([,|;])$" delimiter)
    ("(?i)^((0|0x)?[-|+]?[0-9a-f]+)$" number)
    ("(?i)^(\\+|\\-|\\*|\\/)$" operator)
    ("(/\\*(\\s|\\S)*\\*/|//(\\s|\\S)*)" comment)
    ("((\\s|\\S)*)" raw)
    )
  )


(defun regularize-token (token)
  "regularize token content"
  (let* ((label-result (find-if (lambda (item) (ppcre:scan (first item) token))
                              *php-token-labels*))
         (pat (first label-result))
         (type-name (second label-result))
         (content (ppcre:regex-replace pat token "\\1")))
    `(:type ,type-name :value ,content)
  ))


(regularize-token "'asas\"\\'a'")


(defun regularize-token-queue (token-queue)
  (mapcar (lambda (item)
            (let ((token-content (cdr item)))
              (regularize-token token-content)
              ))
          (qlist token-queue))
  )


(defparameter *tested-token-queue*
  (get-php-token-queue "/mnt/d/Coding/CL/smart-code-modifier/draft/test-2.php"))


(filter (lambda (item) (!= (getf item :type) 'raw))
        (regularize-token-queue *tested-token-queue*))

;; (defun run-test (php-path &optional (start-dir "."))
;;   (let ((lexer (create-php-lexer php-path)))
;;     (format t "Lexer Parsing: ~a~%~%" (ppath:relpath php-path start-dir))
;;     (run-all lexer :output nil)))


;; (defparameter *path-queue* (queue))
;; (defparameter *scaned-dir*
;;   "/mnt/d/Coding/Python3/smart_code_modifier/draft/phpmyadmin/libraries/classes/")
;; (cl-fad:walk-directory *scaned-dir*
;;                        (lambda (path) (enq (namestring path) *path-queue*))
;;                        :test (lambda (path) (string$= ".php" (namestring path))))

;; (defun test-scanned-dir ()
;;   (do ((php-path (front *path-queue*) (front *path-queue*)))
;;       ((queue-empty-p *path-queue*) nil)
;;     (run-test php-path *scaned-dir*)
;;     (deq *path-queue*)))


;(test-scanned-dir)

;; (run-test
;;  "/mnt/d/Coding/Python3/smart_code_modifier/draft/phpmyadmin/libraries/classes/OpenDocument.php"
;;  *scaned-dir*)

(load-repl-env)


(defclass syntax ()
  nil)

;;; Class PHPSyntax
(defclass php-syntax ()
  ())
