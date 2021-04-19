
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
  (defparameter *source-home*
    (ppath:expanduser "~/coding/CL/myworkspaces/smart-code-modifier/"))


  (defparameter *lex-conf*
    (with-open-file (stream (merge-pathnames *source-home* #P"php.lex.conf.lisp"))
      (read stream)))
  (defparameter *lex-dfa-map* (acdr :lex-dfa-map *lex-conf*))
  (defparameter *mypackage* *package*)

  (load (merge-pathnames *source-home* "lexer.lisp"))
  (load (merge-pathnames *source-home* "preprocessor.lisp"))
  (load (merge-pathnames *source-home* "php-preprocessor.lisp"))
  )


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


(defun regularize-token-queue (token-queue)
  (apply
   'queue
   (mapcar (lambda (item)
             (let ((token-content (cdr item)))
               (regularize-token token-content)
               ))
           (qlist token-queue))
   ))

(defparameter *type-identity*
  '("function" "array" "const" "class"))


(setq tree0 '((:type A :value 123)
              (:type B :value 456 :other ((:type E) (:type F)))
              (:type C :value 789)
              (:type E)
              )
      )

(setq tree1 (instance-from (list-mut) tree0))

(setq item (ast-find-all tree1 :keypair '(:type E)))

(defparameter *php-files-dir* (ppath:join *source-home* "examples" "PHP"))

(defun test-gen-ast ()
  (defparameter *tested-token-queue*
    (get-php-token-queue (ppath:join *php-files-dir* "database.php")))
  (defparameter *ast-0* (qlist* (regularize-token-queue *tested-token-queue*)))
                                        ;(parse-var *reg-tested-token-queue*)
  ;(defparameter ast0 (qlist* (gen-ast (parse-var *reg-tested-token-queue*))))
  ;(defparameter ast (instance-from (list-mut) ast0))
  )

(test-gen-ast)

;; Parser Rule
(defparameter *parser-rules*
  '(
    (prog . (block-statements block))
    (block-statements . (
                         (#\{ block #\})
                         ))
    )
  )






;; Write Back





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

