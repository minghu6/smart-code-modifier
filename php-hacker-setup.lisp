(in-package #:php-hacker)

(eval-when (:compile-toplevel)
  (defvar *mypackage* (find-package 'php-hacker))
  (defvar *source-home* (path:dirname *compile-file-truename*))
  )

(load (merge-pathnames *source-home* #P"lexer.lisp"))
(load (merge-pathnames *source-home* #P"preprocessor.lisp"))


