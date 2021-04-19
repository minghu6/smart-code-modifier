

(defclass preprocessors ()
  ((preprocessor-stack
    :initarg :preprocessor-stack
    :initform nil
    :accessor preprocessor-stack)
   (inner-source
    :initarg :source
    :initform (error "Must supply source for preprocessors")
    :reader inner-source))
  (:documentation "Code PreProcessors"))


(defmethod run-next-preprocessor ((preprocs preprocessors) &key (output t))
  "run next preprocessor of stack"
  (with-slots (preprocessor-stack inner-source) preprocs
    (if preprocessor-stack
        (let ((cur-preproc (car preprocessor-stack)))
          (setf inner-source (funcall cur-preproc inner-source))
          (setf preprocessor-stack (cdr preprocessor-stack))
          (format output "~a has been processed.~%" cur-preproc)
          inner-source)
        nil)))


(defmethod run-all-preprocessors ((preprocs preprocessors) &key (output t))
  (loop while (run-next-preprocessor preprocs :output output))
  (inner-source preprocs))
