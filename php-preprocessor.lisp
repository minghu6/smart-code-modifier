
(load "preprocessor.lisp")

(defparameter *tested-s0* "<?php
// Static variables
function foo()
{
    static $bar = <<<LABEL
Nothing in here...
This is \"Kids\"
LABEL;

}

1\\2

// Class properties/constants
class foo
{
    const BAR = <<<FOOBAR
Constant example
FOOBAR;

    public $baz = <<<FOOBAR
Property example
FOOBAR;

    public $baz = <<<\"FOOBAR\"
Property example
FOOBAR;
}
?>")

(defparameter *tested-s1* "<?php
echo <<<'EOD'
Example of string spanning multiple lines
using nowdoc syntax. Backslashes are always treated literally,
e.g. \\ and \'.
EOD;

$foo = new foo();
$name = 'MyName';

echo <<<'EOT'
My name is \"$name\". I am printing some $foo->foo.
Now, I am printing some {$foo->bar[1]}.
This should not print a capital 'A': \x41
EOT;")

;; (defun elt-string-queue-p (instance)
;;   (and (queuep instance)
;;        (every 'stringp (qlist instance))))

;; (deftype elt-string-queue ()
;;   `(satisfies elt-string-queue-p))


;; (defgeneric queue-string (queue-instance customer-queue)
;;   (:documentation "transform serapeum queue to string"))


(defclass php-preprocessors (preprocessors)
  nil
  (:documentation "PHP PreProcessors"))


(defun heredoc-preproc (source)
  "heredoc => doublequote string"
  (step-replace-all "<<<\"?(\\w+)\"?\\n(((?!<<<).|\\n)*)\\n\\1"
                    source
                    (lambda (match &rest registers)
                      (declare (ignore match))
                      (format nil "\"~a\""
                              (let ((content (second registers)))
                                (string-replace-all "\""  content "\\\"")
                                )
                              ))
                    :simple-calls t))


(defun nowdoc-preproc (source)
  "nowdoc => singlequote string"
  (step-replace-all "<<<'(\\w+)'\\n(((?!<<<).|\\n)*)\\n\\1"
                    source
                    (lambda (match &rest registers)
                      (declare (ignore match))
                      (format nil "'~a'"
                              (let ((content (second registers)))
                                (string-replace-all "'"  content "\\'")
                                )
                              ))
                    :simple-calls t))


(defmethod initialize-instance :after ((preprocs php-preprocessors) &key)
  (with-slots (preprocessor-stack) preprocs
    (setf preprocessor-stack (cons 'heredoc-preproc preprocessor-stack))
    (setf preprocessor-stack (cons 'nowdoc-preproc preprocessor-stack))
    )
  )

(setq ppcre:*use-bmh-matchers* t)
;(setq ppcre:*allow-named-registers* t)
