(asdf:defsystem #:php-hacker
  :description "Smart Code Modifier"
  :author "minghu6 <a19678zy@163.com>"
  :license  "BSD-3"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-package-locks
               #:cl-fad
               #:ppath
               #:serapeum
               #:alexandria
               #:minghu6)
  :components ((:static-file "php.lex.conf.lisp")
               (:file "php-hacker-package")
               (:file "php-hacker-setup")
               (:file "lexer")
               (:file "php-syntax")
               )
  :compile-bundle-op
  )
