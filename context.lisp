(defpackage :context
  (:use :common-lisp :store)
  (:export :initial-data :goals :context-name :defcontext))

(in-package :context)

;;; making index number facility

(defparameter *instance-number-of-classes* nil)

(defun next-index (name)
  (let ((number (get name *instance-number-of-classes*)))
    (if (null number)
        (setf (get name *instance-number-of-classes*) 1)
        (incf (get name *instance-number-of-classes*)))))

(defun make-instance-name (name)
  (format nil "~A-~A" name (next-index name)))

#|
(next-index 'a) => 0
(next-index 'a) => 1
(make-instance-name 'foo) => "FOO-1"
(make-instance-name 'foo) => "FOO-2"
(make-instance-name 'bar) => "BAR-1"
|#

; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L154
(defclass context ()
    ((name)
     (initial-data)
     (goals)))

#|
defcontext はクラス生成マクロだ。
このようなコードに展開する。
(defcontext PATIENT (name sex age) ())
    ↓
(defclass patient (context)
  ((name :initform (make-instance-name 'PATIENT) :accessor name)
   (initial-data :initform '(name sex age) :accessor initial-data)
   (goal :initform nil :accessor goals)))
|#

(defmacro defcontext (name initial-data goals)
  `(defclass ,name (context)
     ((name :initform (make-instance-name ',name) :accessor context-name)
      (initial-data :initform ',initial-data :accessor initial-data)
      (goals :initform ',goals :accessor goals))))

#|
(defcontext PATIENT (name sex age) ())
==macroexpand-1==>
(DEFCLASS PATIENT (CONTEXT)
          ((NAME :INITFORM (MAKE-INSTANCE-NAME 'PATIENT) :ACCESSOR NAME)
           (INITIAL-DATA :INITFORM '(NAME SEX AGE) :ACCESSOR INITIAL-DATA)
           (GOALS :INITFORM 'NIL :ACCESSOR GOALS)))

(defcontext PATIENT (name sex age) ())
(make-instance 'patient) => #<PATIENT {1003EE6BA3}>
(name (make-instance 'patient)) => "PATIENT-2"

(defparameter p (defcontext PATIENT  (name sex age)  ()))
(class-name p) => PATIENT
|#

