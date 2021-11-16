(defpackage :parameter-with-value-cf
  (:use :common-lisp :store :cf)
  (:export :get-parameter-vals :get-parameter-cf :update-parameter-cf))

(in-package :parameter-with-value-cf)

(defun get-parameter-vals (parameter-key context)
  "Return a list of (val cf) pairs for this (parameter-key context)."
  (get-store (list parameter-key context)))

(defun get-parameter-cf (parameter-key context val)
  "Look up the certainty factor or return unknown."
  (or (second (assoc val (get-parameter-vals parameter-key context)))
      unknown))

(defun update-parameter-cf (parameter-key context val cf)
  "Change the certianty factor for (parameter-key context is val),
  by combining the given cf with the old."
  (let ((new-cf (cf-or cf (get-parameter-cf parameter-key context val))))
    (put-store (list parameter-key context)
            (cons (list val new-cf)
                  (remove val (get-store (list parameter-key context)) :key #'first)))))

