;;; Load configure env
(eval-always
  (unlock-package '#:serapeum)

  (defun load-lex-env ()
    (let* ((lex-conf
            (with-open-file (stream "/mnt/d/Coding/CL/smart-code-modifier/php.lex.conf.lisp")
              (read stream)))
           (lex-dfa-map (acdr :lex-dfa-map lex-conf)))

      (defparameter *lex-conf* lex-conf)

      ;; In essence, it just converts the regex pattern into dfa map manually.
      ;; I'm a little genius maybe, [doge]
      (defparameter *lex-dfa-map* lex-dfa-map)
      ))
  (load-lex-env)
  )


;;; Code generation start:
(eval-always
  (defun pred-fun-sym (name)
    (sym
      (concatenate
       'string
       name
       (if (ppcre:scan (ppcre:create-scanner "^(\\w+-)+\\w+$") name)
           "-p"
           "p"))))


  (defun parse-type-fun-name (fun-name)
    "get the token list of fun name"
    (filter (lambda (s) (and
                         (string/= s "BUT")
                         (string/= s "P")))
            (split-sequence #\- (string-upcase fun-name))))
  )


(defmacro gen-char-type-fun-1 (nested-list)
  "for (name value) of nested-list => *,name* => value and aaap"
  (eval
   `(cons 'list
          (flatten-1 (map 'list (lambda (item)
                                    (let ((name (first item))
                                          (char (second item)))
                                      `((defparameter
                                            ,(sym (pp "*" name "*"))
                                          ,char)
                                        (defun ,(pred-fun-sym name)
                                                       (c) (char= c ,char)))))
                                     ,nested-list)))))


(defmacro gen-char-type-fun-2 (fun-name-list)
   (cons 'list
           (map 'list (lambda (fun-name)
               (flet ((gen-cond (char-name)
                        `(,(pred-fun-sym char-name) c)))

                 (let* ((char-names (parse-type-fun-name fun-name))
                        (positive-name (first char-names))
                        (negative-names (rest char-names)))
                   `(defun  ,(pred-fun-sym fun-name) (c)
                      ,(cons 'and
                             (cons (gen-cond positive-name)
                                   (map 'list (lambda (char-name)
                                               `(not ,(gen-cond char-name)))
                                        negative-names)))))))
         fun-name-list)))

(eval-always
  ;;; Some default type fun
  (defun anycharp (c)
    (declare (ignore c))
    t)


  ;; non-graphic-but-space-p
  (defun ngbsp (c)
    (or
     (not (graphic-char-p c))
     (char= c #\ )))


  (defun identity-head-p (c)
    (of c (pp *ascii-letters* '(#\_))))


  (defun identityp (c)
    (of c (pp *ascii-letters* *digits* '(#\_))))


  (defun parenthesisp (c)
    (of c '(#\( #\) #\{ #\} #\[ #\])))


  (defun operatorp (c)
    (of c '(#\+ #\- #\* #\/ #\%
            #\^ #\| #\& #\~ #\!
            #\? #\: #\= #\@
            #\> #\<
            #\.)))


  (defun numbercharp (c)
    (of c *digits* ))


  (defun hex-numberchar-p (c)
    (of c *hex-digits*))


  ;;; Generate one meta type fun
  (gen-char-type-fun-1
   '(("slash" #\/)
     ("question" #\?)
     ("asterisk" #\*)
     ("<" #\<)
     ("semicolon" #\;)
     ("singlequote" #\')
     ("doublequote" #\")
     ("backslash" #\\ )
     ("comma" #\,)
     ("newline" #\Newline)
     ("colon" #\:)
     ("zerochar" #\0) ; zerop has been defined in 'cl
     ("x" #\x)
     ("equalchar" #\=)
     ))

  ;; (gen-char-type-fun-2
  ;;  ("anychar-but-slash"
  ;;   "anychar-but-slash-asterisk"
  ;;   "anychar-but-asterisk"
  ;;   "anychar-but-question"))
  )



(defmacro gen-extra-type-fun (fun-sym char-scope)
    `(defun ,fun-sym (c)
       (of c ,(cons 'list char-scope))))


(eval-always
  (defun extra-type-fun-list ()
    (flet ((parse-char-scope (char-scope)
             (if (characterp char-scope) `(,char-scope)
                 (reduce 'pp
                         (map 'list (lambda (char-placeholder)
                                      (if (characterp char-placeholder)
                                          `(,char-placeholder)
                                          (if (sequencep char-placeholder)
                                              char-placeholder
                                              (symbol-value char-placeholder))
                                          ))
                              char-scope)))))
      (map 'list (lambda (form)
                   (let ((type-fun (first form))
                         (char-scope (second form)))
                     `(,type-fun ,(parse-char-scope char-scope))
                     ))
           (acdr :extra-type-fun *lex-conf*))))

  (defmacro gen-extra-type-fun-l ()
    `(lis-apply gen-extra-type-fun ,(extra-type-fun-list)))

  (gen-extra-type-fun-l)


  (defun raw-type-fun-list ()
    "=> [type-fun-symbol*]"
    (filter (lambda (type-fun) (and
                                type-fun
                                (not
                                 (fboundp (pred-fun-sym (symbol-name type-fun))))))
            (remove-duplicates
             (flatten-1
              (loop for state-output-map in *lex-dfa-map* collect
                   (loop for each-output in (cdr state-output-map) collect
                        (car each-output)))))))

  (defmacro gen-raw-type-fun ()
    `(gen-char-type-fun-2
      ,(mapcar (lambda (fun-sym) (symbol-name fun-sym))
               (raw-type-fun-list)))
    )

  (gen-raw-type-fun)
  )



;;; Code generation end



;;; Class Lexer

(defclass lexer ()
  ((source
    :initarg :source)
   (source-ptr
    :reader source-ptr
    :documentation "source pointer, used for method next-step")
   (states-stack
    :reader states-stack)
   (lineno
    :reader lineno)
   (colno
    :reader colno)
   (token
    :accessor token)
   (token-queue
    :accessor token-queue))
  (:documentation "Code Lexer"))


(defmethod initialize-instance :after ((automaton lexer) &key)
  (with-slots (source-ptr states-stack token token-queue lineno colno) automaton
    (setf source-ptr 0)
    (setf lineno 1)
    (setf colno 1)
    (setf states-stack '(blank-state))
    (setf token `((,lineno ,colno) . ,(queue)))
    (setf token-queue (queue))))


(defmethod ptr-nil-p ((automaton lexer))
  (with-slots (source source-ptr) automaton
    ;(format t "source-ptr: ~a~%" source-ptr)
    (>= source-ptr (length source))))


(defun show-lexer-step (states-stack c token-end-p &key (output t))
  (let ((state-0 (nth 0 states-stack))
        (state-1 (nth 1 states-stack)))
    (format* output "~a~a~a~%"
            c
            (if token-end-p "[token] " (repeat-sequence " " 8))
            (cond ((eql state-1 state-0) state-1)
                  (t (format* nil "~a => ~a" state-1 state-0))))))

;;; Key Function
(defun next-state-map (state c)
  (let ((state-map (acdr state *lex-dfa-map*))
        (*package* *mypackage*))
    (acdr
     (first
      (filter
       'identity
       (mapcar (lambda (name)
                 (if (eval `(,(pred-fun-sym (symbol-name name)) ,c)) name nil))
               (mapcar 'car state-map))))
     state-map)
    ))


(defun token-empty-p (token)
  (let ((inner-token-raw (cdr token)))
    (cond ((queuep inner-token-raw) (queue-empty-p inner-token-raw))
          ((stringp inner-token-raw) (emptyp inner-token-raw))
          (t (error "Unrecognized token type")))))


(defun token-enq (token token-queue)
  (let ((meta (car token))
        (inner-token-raw (cdr token)))
    (enq `(,meta . ,(list-string (qlist inner-token-raw))) token-queue)))


(defun show-token (token &key (output t))
  (let* ((meta (car token))
         (inner-token-raw (cdr token))
         (inner-token  (if (stringp inner-token-raw) inner-token-raw
                           (list-string (qlist inner-token-raw))))
         (lineno (first meta))
         (colno (second meta)))

    (format output "Ln ~a, Col ~a~%~a~%" lineno colno inner-token)))


;;; Define error
(define-condition unknown-lex-pattern-error (error)
  ((token
    :initarg :token
    :reader token)
   (c
    :initarg :c
    :reader c)
   (state
    :initarg :state
    :reader state)
   (error-info
    :initarg :error-info
    :reader error-info)))


(defun format-unknown-lex-pattern-error-info (token c state &key (output t))
  (format output "~a~%~a: \"~s\" " (show-token token :output nil)
          state c))

(eval-always
  (lis-apply gen-restart-fun (("use-blank-state")))
  )


(defmethod next-step ((automaton lexer) &key (output t))
  (with-slots (source source-ptr states-stack token token-queue lineno colno) automaton
    (if (ptr-nil-p automaton) nil
        (let* ((c (elt source source-ptr))
               (cur-state (first states-stack))
               (state-multiple-value
                (unless* (next-state-map cur-state c)
                         (let ((error-info
                                (format-unknown-lex-pattern-error-info token c cur-state :output nil)))
                           (format* *ERROR-OUTPUT* error-info)
                           (restart-case (error 'unknown-lex-pattern-error
                                                :token token :c c :state cur-state
                                                :error-info error-info)
                             (use-value (value) value)
                             (use-blank-state () '(blank-state t)))
                           ))))


          (multiple-value-bind (next-state token-end-p)
              (values-list state-multiple-value)
            ;; state-stack <- state
            (push next-state states-stack)

            ;; extra revision for token-end-p
            (when (token-empty-p token)
              (setf token-end-p nil))

            ;; token-queue <- last-token
            (when token-end-p
              (token-enq token token-queue)
              (setf token `((,lineno ,colno) . ,(queue))))

            ;; token <- char
            (enq c (cdr token))

            ;; update source-ptr, (lineno, colno)
            (incf source-ptr)
            (if (char= c #\Newline) (progn (incf lineno) (setf colno 1))
                (incf colno))

            ;; show step
            (show-lexer-step states-stack c token-end-p :output output)

            ;; return
            `(,token-end-p))
          ))))


(defmethod recycle-tail-token ((automaton lexer))
  (with-slots (token token-queue) automaton
    (when token
      (progn
        (unless (token-empty-p token)
          (token-enq token token-queue))
        (setf token nil)))))


(defmethod next-token ((automaton lexer) &key (output t))
  (block nil
    (let ((step-result
           (do ((step-result (next-step automaton :output nil) (next-step automaton :output nil)))
               ((or (not step-result) (first step-result)) step-result))))

      (flet ((tail-call (automaton)
               (let ((last-token (qback (token-queue automaton))))
                 (show-token last-token :output output)
                 `(,last-token ,(first (states-stack automaton)))
                 )))
        (if (first step-result) (tail-call automaton)
            (if (token automaton) (progn (recycle-tail-token automaton) (tail-call automaton))
                (return)))))))


(defmethod run-all ((automaton lexer) &key (output t))
  (loop while (next-token automaton :output output)))
