
(eval-when (:compile-toplevel :load-toplevel)
  (in-package #:php-hacker))


(eval-when (:execute)
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

  (defun load-repl-env ()
    (load "/mnt/d/Coding/CL/smart-code-modifier/lexer.lisp")
    (load "/mnt/d/Coding/CL/smart-code-modifier/preprocessor.lisp")
    (load "/mnt/d/Coding/CL/smart-code-modifier/php-preprocessor.lisp")

    (defparameter *mypackage* *package*)
    )

  (load-repl-env)
  )





;;; Temporary Utils Start
(defun file-contents-string (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))


(defun next-queue (source-queue end-test-fun &key (recycle-tail nil))
  (loop
     with new-queue = (queue)
     for item = (deq source-queue) then (deq source-queue)
     while (and item
                (if (functionp end-test-fun)
                    (not (funcall end-test-fun item))
                    (!= end-test-fun item)))
     do (enq item new-queue)
     finally (progn
               (when recycle-tail (undeq item source-queue))
               (return new-queue))))


(defun qlist* (source-queue)
  (map-tree (lambda (item)
              (if (queuep item) (qlist* item)
                  item))
            (qlist source-queue)))


(qlist* (queue 1 (queue 2 (queue 'a) 3) 6))


(setq queue0 (qconc (queue) '("int" "public" "function" "(" "aaa" "=" "bbb" ")" "this" "is")))
(next-queue
 queue0
 ")"
 :recycle-tail t
 )

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
    ("^([,|;|:])$" delimiter)
    ("(?i)^((0|0x)?[-|+]?[0-9a-f]+)$" number)
    ("(?i)^(\\+|\\-|\\*|\\/)$" operator)
    ("(/\\*(\\s|\\S)*\\*/|//(\\s|\\S)*)" comment)
    ("((\\s|\\S)*)" raw)
    )
  )


(defun raw-node-p (token)
  (== (getf token :type) 'raw))


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
  (apply
   'queue
   (mapcar (lambda (item)
             (let ((token-content (cdr item)))
               (regularize-token token-content)
               ))
           (qlist token-queue))
   ))


(defun syntax-parse-identity (cur-node token-value)
  (cond
    ((== token-value "use")
     (setf (getf cur-node :type) 'use))

    ((== token-value "static")
     (setf (getf cur-node :static) t))

    ((== token-value "public")
     (setf (getf cur-node :scope) "public"))

    ((== token-value "function")
     (setf (getf cur-node :type) 'function))

    ((== (getf cur-node :type) 'function)
     (cond
       ((not (getf cur-node :name))
        (setf (getf cur-node :name) token-value))
       ((not (getf cur-node :return-type))
        (setf (getf cur-node :return-type) token-value))
       )
     )

    ((== (getf cur-node :type) 'use)
     (cond
       ((not (getf cur-node :type))
        (se
         tf (getf cur-node :name) token-value))
       ((not (getf cur-node :return-type))
        (setf (getf cur-node :return-type) token-value))
       )
     )

    )

  cur-node
  )


(defun syntax-parse-parenthesis-recur-enq (cur-node token-value token-queue ast)
  (cond
    ((== token-value "(")
     (cond
       ((== (getf cur-node :type) 'function)
        (setf (getf cur-node :args)
              (gen-ast (next-queue token-queue
                                   (lambda (item) (== (getf item :value) ")"))))))))

    ((== token-value "{")
     (cond
       ((== (getf cur-node :type) 'function)
        (progn
          (setf (getf cur-node :body)
                (gen-ast (next-queue token-queue
                                     (lambda (item) (== (getf item :value) "}")))))
          (enq cur-node ast)
          (setq cur-node nil))))))

  cur-node
  )


(defun syntax-parse-delimiter-enq (cur-node token-value ast)
  (cond
    ((== token-value ";")
     (cond
       (cur-node (progn
                   (enq cur-node ast)
                   (setq cur-node nil)))
       ))
    )
  cur-node
  )


(defun gen-ast (token-queue)
  (loop
     with ast = (queue)
     with fun-scope = nil
     with static = nil
     with cur-node = nil
     for token = (deq token-queue) then (deq token-queue) while token
     do
       (block nil
         (let ((token-value (getf token :value))
               (token-type (getf token :type))
               )
           (switch (token-type :test '==)
             ('raw
              (enq token ast))

             ('identity
              (setq cur-node
                    (syntax-parse-identity cur-node token-value)))

             ('parenthesis
              (setq cur-node
                    (syntax-parse-parenthesis-recur-enq cur-node token-value token-queue ast)))

             ('delimiter
              (setq cur-node
                    (syntax-parse-delimiter-enq cur-node token-value ast)))
             )

           ))

     finally (return (qlist* ast))))


;(ast-parse-func (qappend (queue) '(1 2 3)))
(defparameter *tree* nil)


(defun test-gen-ast ()
  (defparameter *tested-token-queue*
    (get-php-token-queue "/mnt/d/Coding/CL/smart-code-modifier/draft/test-2.php"))
  (defparameter *reg-tested-token-queue* (regularize-token-queue *tested-token-queue*))
  (gen-ast *reg-tested-token-queue*)
  )

;(symbol-package *package*)

;; ((== (getf token :value) ":")
;;  (cond
;;    ((== (getf cur-node :type) "function")
;;     (unless (getf cur-node :return-type)
;;       (set))))
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


;  (load-repl-env)


(defclass syntax ()

  nil)

;;; Class PHPSyntax
(defclass php-syntax ()
  ())

