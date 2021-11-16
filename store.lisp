;;; storage facirity
;;; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L43

(defpackage :store
  (:use :common-lisp)
  (:export :store :get-store :put-store :clear-store))

(in-package :store)

(defclass store ()
  ((db :initform (make-hash-table :test #'equal) :accessor db)))

(defmethod get-store (key (store store))
  (gethash key (db store)))

(defmethod put-store (key val (store store))
  (setf (gethash key (db store)) val))

(defmethod clear-store ((store store))
  (clrhash (db store)))

#|
(defparameter a (make-instance 'store))
(put-store 'hello 'lisp a)
(get-store 'hello a) => LISP
(clear-store a)
(get-store 'hello a) => NIL
|#
