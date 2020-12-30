(eval-when (:compile-toplevel :load-toplevel)
  (in-package #:php-hacker))


(eval-when (:execute)
  (ql:quickload "alexandria")
  (ql:quickload "cl-ppcre")
  (ql:quickload "minghu6")
  (ql:quickload "serapeum")
  (ql:quickload "cl-fad")

  (use-package '#:minghu6)
  (use-package '#:serapeum)
  (use-package '#:alexandria))


(eval-when (:execute)
  (defparameter *source-home* "/mnt/d/Coding/CL/smart-code-modifier/")


  (defparameter *lex-conf*
    (with-open-file (stream (merge-pathnames *source-home* #P"php.lex.conf.lisp"))
      (read stream)))
  (defparameter *lex-dfa-map* (acdr :lex-dfa-map *lex-conf*))
  (defparameter *mypackage* *package*)

  (load (merge-pathnames *source-home* "lexer.lisp"))
  (load (merge-pathnames *source-home* "preprocessor.lisp"))
  (load (merge-pathnames *source-home* "php-preprocessor.lisp"))
  (load (merge-pathnames *source-home* "parser.lisp"))
  (load (merge-pathnames *source-home* "php-parser.lisp"))
  )


(defun test-gen-ast ()
  (defparameter myparser
    (make-instance 'php-parser
                   :source-path "/mnt/d/Coding/CL/smart-code-modifier/draft/test-2.php"))

  (defparameter myast (parse myparser))
  myast
  )

(ast-update-all
 (ast-find-all (test-gen-ast) '(:TYPE EVAL))
 :keypair '(:SYMBOL "schemas2"))


(mlist myast)


(defun php-gen (ast)
  (loop for node in ast collect
       (cond
         ((== (getf node :TYPE) 'RAW) (raw-gen node))
         (t node))))


(defun raw-gen (ast)
  (getf ast :VALUE))

;(defun items-gen (ast))
;(defun array-gen (ast))
