
(eval-when (:compile-toplevel :load-toplevel)
  (in-package #:php-hacker))


(defun create-php-lexer (php-file-path)
  (let* ((source (file-contents-string php-file-path))
         (php-preprocs (make-instance 'php-preprocessors :source source)))
    (make-instance 'lexer :source (run-all-preprocessors php-preprocs :output nil))
  ))


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


(defun regularize-php-token (token)
  "regularize token content"
  (let* ((label-result (find-if (lambda (item) (ppcre:scan (first item) token))
                              *php-token-labels*))
         (pat (first label-result))
         (type-name (second label-result))
         (content (ppcre:regex-replace pat token "\\1")))
    `(:type ,type-name :value ,content)
  ))


(defun regularize-php-token-queue (token-queue)
  (apply
   'queue
   (mapcar (lambda (item)
             (let ((token-content (cdr item)))
               (regularize-php-token token-content)
               ))
           (qlist token-queue))
   ))


(defun parse-php-var (token-queue)
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
  '("function" "array" "const" "class"))


(defun syntax-parse-operator-recur-enq (cur-node token-value token-queue ast)
  (declare (ignore ast)
           (ignore token-queue))
  (cond
    ((== token-value "=")
     (progn
       (setq cur-node `(:type assignment :var ,cur-node)))))
  cur-node
  )


(defun syntax-parse-identity-recur-enq (cur-node token-value token-queue ast)
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

    (t (progn
         (enq `(:type identity :value ,token-value) ast)))
    )

  cur-node
  )


(defun remove-raw-token (token-queue)
  (filter (lambda (token) (!= (getf token :type) 'raw))
          (if (queuep token-queue) (qlist token-queue)
              token-queue)))


(defun parse-array-value (token-queue)
  (remove-raw-token
   (gen-php-ast-2 (next-queue
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
              (gen-php-ast-2 (next-queue token-queue
                                   (lambda (item) (== (getf item :value) ")"))))))

       ((== (getf cur-node :type) 'class)
        (setf (getf cur-node :super-classes)
              (gen-php-ast-2 (next-queue token-queue
                                   (lambda (item) (== (getf item :value) ")"))))))
       ))

    ((== token-value "{")
     (cond
       ((of (getf cur-node :type) '(function class))
        (progn
          (setf (getf cur-node :body)
                (gen-php-ast-2 (next-queue token-queue
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


(defun gen-php-ast-2 (token-queue)
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
                    (syntax-parse-identity-recur-enq cur-node token-value token-queue ast)))

             ('parenthesis
              (setq cur-node
                    (syntax-parse-parenthesis-recur-enq cur-node token-value token-queue ast)))

             ('delimiter
              (setq cur-node
                    (syntax-parse-delimiter-enq cur-node token-value ast)))
             )

           ))

     finally (return (qlist* ast))))


(defclass php-parser (parser)
  nil)


(defmethod initialize-instance :after ((myparser php-parser) &key)
  (with-slots (source-path lexer gen-ast regularize-token-queue) myparser
    (setf lexer (create-php-lexer source-path))
   (setf gen-ast (lambda (token-queue) (gen-php-ast-2 (parse-php-var token-queue))))
    ;(setf gen-ast (lambda (token-queue) (parse-php-var token-queue)))
    (setf regularize-token-queue 'regularize-php-token-queue)
    )
  )

