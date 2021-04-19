(in-package #:php-hacker)

(eval-when (:compile-toplevel)
  (defvar *mypackage* (find-package 'php-hacker))
  (defvar *source-home* (path:dirname *compile-file-truename*))

  (let* ((lex-conf
          (with-open-file (stream (merge-pathnames *source-home* #P"php.lex.conf.lisp"))
            (read stream)))
         (lex-dfa-map (acdr :lex-dfa-map lex-conf)))

    (defparameter *lex-conf* lex-conf)

    ;; In essence, it just converts the regex pattern into dfa map manually.
    ;; I'm a little genius maybe, [doge]
    (defparameter *lex-dfa-map* lex-dfa-map)
    )
  )


(load (merge-pathnames *source-home* #P"lexer.lisp"))
(load (merge-pathnames *source-home* #P"preprocessor.lisp"))
