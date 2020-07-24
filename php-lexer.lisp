

(load "~/.sbclrc")

(ql:quickload "trivia")
(ql:quickload "cl-utilities")
(ql:quickload "alexandria")
(ql:quickload "cl-ppcre")
(ql:quickload "minghu6")
(ql:quickload "serapeum")


;; test code
;; (shadow 'match '#:trivia)
;; (import 'match '#:trivia)
;(find-symbol "MATCH" 'trivia)
;; (match '(1 2 3)
;;        ((cons x y)
;;         ; ^^ pattern
;;        (print x)
;;        (print y)))


;(use-package :alexandria)
;(use-package :trivia)
;(use-package :cl-utilities)
(use-package '#:minghu6)
(use-package '#:serapeum)
(use-package '#:alexandria)


;;; Some temporary utils
(defun back (serapeum-queue)
  (last-elt (qlist serapeum-queue)))



;;; code generation start:

(defun pred-fun-sym (name)
  (intern
   (string-upcase
    (concatenate
     'string
     name
     (if (ppcre:scan (ppcre:create-scanner "^(\\w+-)+\\w+$") name)
         "-p"
         "p")))))


(defun parse-type-fun-name (fun-name)
  "get the token list of fun name"
  (filter (lambda (s) (and
                       (string/= s "but")
                       (string/= s "p")))
          (split-sequence #\- fun-name)))


(defmacro gen-char-type-fun-1 (nested-list)
  "upper case symbol maybe specific for sbcl?"
  (eval
   `(cons 'progn
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
   (cons 'progn
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


(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun anycharp (c) t)

  ; use serapeum' blankp for sequence and whitespacep for character
  ;; (defun blankp (c)
  ;;   (of c '(#\Space #\Tab #\Return #\Newline)))

  (defun identityp (c)
    (of c (pp *ascii-letters* *digits* '(#\_))))

  (defun phpp (c)
    (of c '(#\p #\h)))

  (gen-char-type-fun-1
   '(("slash" #\/)
     ("question" #\?)
     ("asterisk" #\*)
     ("<" #\<)))

  (gen-char-type-fun-2
   ("anychar-but-slash"
    "anychar-but-slash-asterisk"
    "anychar-but-asterisk"
    "anychar-but-question")))



;;; code generation end


(defparameter *lex-dfa-map* '((blank-state . ((slash . (comment-start-state t))
                                              (whitespace . (blank-state nil))
                                              (<     . (tag-start-state t))
                                              (identity . (identity-name-state t))
                                              ))
                              (tag-start-state . ((question . (tag-start-naming-state nil))
                                                  
                                                  ))
                              (tag-start-naming-state . ((php . (php-tagname-state nil))))
                              (php-tagname-state . ((php . (php-tagname-state nil))
                                                    (whitespace . (blank-state t))))

                              (identity-name-state . ((identity . (identity-name-state nil))
                                                      (whitespace . (blank-state t))))

                              ))

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
    (format output "~a~a~a~%"
            c
            (if token-end-p "[token] " (repeat-sequence " " 8))
            (cond ((eql state-1 state-0) state-1)
                  (t (format nil "~a => ~a" state-1 state-0))))))


(defun next-state-map (state c)
  (let ((state-map (acdr state *lex-dfa-map*)))
    (acdr
     (first
      (filter
       'self
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
    (enq `(,meta . ,(list2string (qlist inner-token-raw))) token-queue)))


(defun show-token (token &key (output t))
  (let* ((meta (car token))
         (inner-token-raw (cdr token))
         (inner-token  (if (stringp inner-token-raw) inner-token-raw
                           (list2string (qlist inner-token-raw))))
         (lineno (first meta))
         (colno (second meta)))

    (format output "Ln ~a, Col ~a~%~a~%" lineno colno inner-token)))


(defmethod next-step ((automaton lexer) &key (output t))
  (with-slots (source source-ptr states-stack token token-queue lineno colno) automaton
    (if (ptr-nil-p automaton) nil
        (let ((c (elt source source-ptr)))
          (multiple-value-bind (next-state token-end-p)
              (values-list (next-state-map (first states-stack) c))
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


(defmethod next-token ((automaton lexer))
  (block nil
    (let ((step-result
           (do ((step-result (next-step automaton :output nil) (next-step automaton :output nil)))
               ((or (not step-result) (first step-result)) step-result))))

      (flet ((tail-call (automaton)
               (let ((last-token (back (token-queue automaton))))
                 (show-token last-token)
                 `(,last-token ,(first (states-stack automaton)))
                 )))
        (if (first step-result) (tail-call automaton)
            (if (token automaton) (progn (recycle-tail-token automaton) (tail-call automaton))
                (return)))))))


(defun test-lexer ()
  (format t "~%~%~%")
  (let ((tested-lexer (make-instance 'lexer :source  "<?php aaa ")))
    (loop while (next-step tested-lexer :output t))
    (recycle-tail-token tested-lexer)
    (print (qlist (token-queue tested-lexer)))
    ))


(test-lexer)


(defparameter *tested-lexer* (make-instance 'lexer :source  "<?php aaa "))

(next-step *tested-lexer*)

(next-token *tested-lexer*)

(print (token-queue *tested-lexer*))

(next-state-map 'blank-state #\<)


(defvar *tested-php-code-slice-1*
  "<?php
declare(strict_types=1);

if (false === @include_once 'OpenID/RelyingParty.php') {
exit;

}


/* Change this to true if using phpMyAdmin over https */
$secure_cookie = false;

/**
 * Map of authenticated users to MySQL user/password pairs.
 */
$AUTH_MAP = [
'https://launchpad.net/~username' => [
'user' => 'root',
        'password' => '',
    
],

];
 ")

