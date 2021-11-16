; https://github.com/cl-aip/mycin/blob/master/mycinutils.lisp

(defpackage :utilities
  (:nicknames :utils)
  (:use :common-lisp)
  (:export :rest2 :partition-if))

(in-package :utilities)

(defun rest2 (x)
  "The rest of a list after the first TWO elements."
  (rest (rest x)))

(defun partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))
