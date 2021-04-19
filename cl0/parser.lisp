





(defclass parser ()
  ((source-path
    :initarg :source-path
    :initform (error "Must supply source path for parser")
    :reader source-path)
   (lexer)
   (gen-ast)
   (regularize-token-queue
    :initform 'identity
    ))
  (:documentation ""))


(defmethod parse ((myparser parser) &key (output nil))
  (with-slots (lexer regularize-token-queue gen-ast) myparser
    (run-all lexer :output output)
    ;(funcall regularize-token-queue (token-queue lexer))

    (~>> lexer
         token-queue
         (funcall regularize-token-queue)
         (funcall gen-ast)

         (instance-from (list-mut)))
    )
  )


(defun ast-find-all (l &rest all-keypairs)
  (~> (loop with res = l
         for keypair in all-keypairs
         do (setf res (ast-find-all-1 res :keypair keypair))
         collect res)
      lastcar
      )
  )


(defun ast-find-all-1 (l &key keypair)
  (labels ((ast-find-all-1-0 (l0 res &key keypair)
             (let ((seq (value l0))
                   (test-key (first keypair))
                   (test-value (second keypair)))

               (if (and (plistp seq) (== (getf seq test-key) test-value)) (enq l0 res))
               (loop for item in seq do
                    (if (list-mut-p item) (ast-find-all-1-0 item res :keypair keypair))
                    )
               )))
    (let ((res (queue)))
      (ast-find-all-1-0 l res :keypair keypair)
      (instance-from (list-mut) (qlist res)))))


(define-condition more-than-one-result-error (error)
  ((text
    :initarg :text
    :reader text)))


(define-condition wrong-number-error (error)
  ((text
    :initarg :text
    :reader text)))


(defun ast-update-one (find-res &key keypair append-list (output t))
  (if (> (length (value find-res)) 1)
      (restart-case (error 'more-than-one-result-error
                           :text (format nil "~a" (mlist (value find-res))))
        (use-first () (ast-update-one
                       (instance-from (list-mut) (list (first (value find-res)))))))
      )

  (let ((unpacked-res (~> find-res value first)))
    (if keypair (setf (getf (value unpacked-res) (first keypair))
                      (second keypair)))
    (if append-list (setf (value unpacked-res) (append (value node) append-list)))

    unpacked-res
    )
  )


;; There should be restart-casr for 1.nil find-res and 2.nil (getf key)
;; But I have decided to omit it.
(defun ast-update-all (find-res &key keypair append-list)
  (loop for node in (value find-res) do
       (if keypair (setf (getf (value node) (first keypair))
                         (second keypair)))
       (if append-list (setf (value node) (append (value node) append-list)))
       )
  find-res
  )
