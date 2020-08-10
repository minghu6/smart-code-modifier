
(eval-when (:compile-toplevel :load-toplevel)
  (in-package #:php-hacker))


(eval-when (:execute)
  (ql:quickload "alexandria")
  (ql:quickload "cl-ppcre")
  (ql:quickload "minghu6")
  (ql:quickload "serapeum")
  (ql:quickload "cl-fad")
  (ql:quickload "ppath")

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
  )


;;; Temporary Utils Start
(defun file-contents-string (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))


(defun next-queue (source-queue end-test-fun &key (recycle-tail nil) (state-stack (stack)))
  (loop
     with new-queue = (queue)
     for item = (deq source-queue) then (deq source-queue)
     while (and item
                (if (functionp end-test-fun)
                    (not (if (emptyp state-stack) (funcall end-test-fun item)
                             (funcall end-test-fun item :state-stack state-stack)))
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

(setq s0 (apply 'stack '(1 2)))


(defun bala (l)
  (ens 9 l)
  (print l)
  )


(defgeneric the-other-paren (paren)
  (:documentation " (the-other-paren \"{\" => })
 (the-other-paren #\\*) => *
"))

(defmethod the-other-paren ((paren string))
  (switch (paren :test '==)
    ("(" ")")
    ("[" "]")
    ("{" "}")
    (otherwise paren)))


(defmethod the-other-paren ((paren character))
  (switch (paren :test '==)
    (#\( #\))
    (#\[ #\])
    (#\{ #\})
    (otherwise paren)))


(defun paren-matcher (match-identifier &key (key 'identity))
  (let ((match-0 match-identifier)
        (match-1 (the-other-paren match-identifier)))
    (lambda (item &key state-stack)
        (switch (item :test '== :key (lambda (item) (funcall key item)))
          (match-0 (ens match-0 state-stack))
          (match-1 (des state-stack)))
        (emptyp state-stack))
    ))

(next-queue
 queue0
 ")"
 :recycle-tail t
 )

(next-queue
 (queue "b" "[" "cd" "]" "sss" "ddd" "]" "end" "end2")
 (paren-matcher "[") :state-stack
 (stack "["))

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
    ("^([\\[|\\]|\\(|\\)|\\{|\\}])$" parenthesis)
    ("^([,|;|:])$" delimiter)
    ("(?i)^((0|0x)?[-|+]?[0-9a-f]+)$" number)
    ("(?i)^(\\+|\\-|\\*|\\/|\\$(\\s*\\$)*|\\=)$" operator)
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


(defun parse-var (token-queue)
  (loop
     with ast = (queue)
     with cur-node = nil
     for token = (deq token-queue) then (deq token-queue) while token
     do
       (let ((token-value (getf token :value))
             (token-type (getf token :type)))

         (cond
           ((== token-type 'operator)
            (if (== token-value "$")
                (if (null cur-node)
                    (progn
                      (setq cur-node '(:type eval))
                      (setf (getf cur-node :prefix) (queue "$")))
                    (enq "$" (getf cur-node :prefix))
                    )
                (enq token ast)))

           ((== token-type 'identity)
            (cond
              ((== (getf cur-node :type) 'eval)
               (progn
                 (setf (getf cur-node :symbol) token-value)
                 (enq cur-node ast)
                 (setq cur-node nil)))

              (t (enq token ast))))

           (t (enq token ast)))
         )
     finally (return ast))
  )



(defparameter *type-identity*
  '("function" "array" "const" ""))

(defun syntax-parse-operator-recur-enq (cur-node token-value token-queue ast)
  (cond
    ((== token-value "=")
     (progn
       (setq cur-node `(:type assignment :var ,cur-node)))))
  cur-node
  )




(defun syntax-parse-identity-enq (cur-node token-value ast)
  (cond
    ((== (getf cur-node :type) 'function)
     (cond
       ((null (getf cur-node :name))
        (setf (getf cur-node :name) token-value))
       ((null (getf cur-node :return-type))
        (setf (getf cur-node :return-type)
              token-value))
       )
     )

    ((== (getf cur-node :type) 'class)
     (cond
       ((null (getf cur-node :name))
        (setf (getf cur-node :name) token-value))

       )
     )

    ((== (getf cur-node :type) 'use)
     (let ((use-items (getf cur-node :items)))
       (cond
         ((and (null (getf (qback use-items) :as-type))
               (of token-value '("function" "const")))
          (setf (getf (qback use-items) :as-type) token-value))

         ((null (getf (qback use-items) :from))
          (setf (getf (qback use-items) :from) token-value))

         ((null (getf (qback use-items) :as-type))
          (setf (getf (qback use-items) :as-type) token-value))
         )
       ))

    ((== (getf cur-node :type) 'eval)
     (progn
       (setf (getf cur-node :symbol) token-value)
       (enq cur-node ast)
       (setq cur-node nil)))

    ((== token-value "use")
     (progn
       (setf (getf cur-node :type) 'use)
       (setf (getf cur-node :items) (queue))
       ))

    ((== token-value "static")
     (setf (getf cur-node :static) t))

    ((== token-value "public")
     (setf (getf cur-node :scope) "public"))

    ((== token-value "function")
     (setf (getf cur-node :type) 'function))

    ((== token-value "class")
     (setf (getf cur-node :type) 'class))
    )

  cur-node
  )


(defun remove-raw-token (token-queue)
  (filter (lambda (token) (!= (getf token :type) 'raw))
          (if (queuep token-queue) (qlist token-queue)
              token-queue)))


(defun parse-array-value (token-queue)
  (remove-raw-token
   (gen-ast (next-queue
             token-queue
             (paren-matcher
              "["
              :key (lambda (token)
                     (when (== (getf token :type) 'parenthesis)
                       (getf token :value))
                     ))
             :state-stack (stack "[")))))


(defun syntax-parse-parenthesis-recur-enq (cur-node token-value token-queue ast)
  (cond
    ((== token-value "(")
     (cond
       ((== (getf cur-node :type) 'function)
        (setf (getf cur-node :args)
              (gen-ast (next-queue token-queue
                                   (lambda (item) (== (getf item :value) ")"))))))

       ((== (getf cur-node :type) 'class)
        (setf (getf cur-node :super-classes)
              (gen-ast (next-queue token-queue
                                   (lambda (item) (== (getf item :value) ")"))))))
       ))

    ((== token-value "{")
     (cond
       ((of (getf cur-node :type) '(function class))
        (progn
          (setf (getf cur-node :body)
                (gen-ast (next-queue token-queue
                                     (lambda (item) (== (getf item :value) "}")))))
          (enq cur-node ast)
          (setq cur-node nil)))))

    ((== token-value "[")
     (cond
       ((null cur-node)
        (progn
          (setq cur-node
                `(:type array :value ,(parse-array-value token-queue)))
          (enq cur-node ast)
          (setq cur-node nil)
          ))

       ((== (getf cur-node :type) 'assignment)
        (setf (getf cur-node :value) (parse-array-value token-queue)))

       ))
    )

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
    ((== token-value ",")
     (cond ((== (getf cur-node :type) 'use)
            (let ((last-use-item (qback (getf cur-node :items))))
              (when last-use-item
                (enq (queue) (getf cur-node :items)))
              ))
           )

           ))
  cur-node
  )


;; (defun parse-statement (token-list)
;;   (loop
;;      with ast (queue)
;;      for token in token-list do
;;        (let ((token-value (getf token :value))
;;              (token-type (getf token :type)))
;;          (switch (token-type :test '==)
;;            ()
;;            ()
;;            )

;;          ))
;;   )


(defun gen-ast (token-queue)
  (loop
     with ast = (queue)
     with cur-node = nil
     for token = (deq token-queue) then (deq token-queue) while token
     do
       (block nil
         (let ((token-value (getf token :value))
               (token-type (getf token :type))
               )
           (switch (token-type :test '==)
             ('raw
              (unless cur-node
                (enq token ast)))

             ('number
              (unless cur-node
                (enq token ast)))

             ('singlequote-string
              (unless cur-node
                (enq token ast)))

             ('doublequote-string
              (unless cur-node
                (enq token ast)))

             ('operator
              (setq cur-node
                    (syntax-parse-operator-recur-enq cur-node token-value token-queue ast)))

             ('eval
              (cond
                ((null cur-node)
                 (setq cur-node token))))

             ('identity
              (setq cur-node
                    (syntax-parse-identity-enq cur-node token-value ast)))

             ('parenthesis
              (setq cur-node
                    (syntax-parse-parenthesis-recur-enq cur-node token-value token-queue ast)))

             ('delimiter
              (setq cur-node
                    (syntax-parse-delimiter-enq cur-node token-value ast)))
             )

           ))

     finally (return ast)))



;(ast-parse-func (qappend (queue) '(1 2 3)))
(defparameter *tree* nil)


(defun test-gen-ast ()
  (defparameter *tested-token-queue*
    (get-php-token-queue "/mnt/d/Coding/CL/smart-code-modifier/draft/test-2.php"))
  (defparameter *reg-tested-token-queue* (regularize-token-queue *tested-token-queue*))
  ;(parse-var *reg-tested-token-queue*)
  (qlist* (gen-ast (parse-var *reg-tested-token-queue*)))
  )

;(test-gen-ast)
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

