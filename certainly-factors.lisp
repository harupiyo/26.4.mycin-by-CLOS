(defpackage :certainly-factors
  (:use :common-lisp)
  (:export :certainly-factors :true :true-p :false :false-p :unknown :cf-or :cf-and :cf->english :cf-p)
  (:nicknames cf))

(in-package :cf)

(defconstant true   +1.0)
(defconstant false  -1.0)
(defconstant unknown 0.0)
(defconstant cf-cut-off 0.2 "Below this certainty we cut off search.")

(defclass certainly-factors ()
  ;; :initarg には、文字列指定子を使って:value, 'value と書く必要があるが、:accessor にはシンボルを書くこと
  ((value :initarg :value :initform unknown :accessor value)))

;  ここでは名前OR を使うにはshadow しなければならない
;  そうしないと次のエラーが出る
;    Lock on package COMMON-LISP violated when
;    proclaiming OR as a function while in package CERTAINTY-FACTORS.
;       [Condition of type SB-EXT:SYMBOL-PACKAGE-LOCKED-ERROR]
; 
;  or をシャドウしたら、OR マクロが使えなくなる
;  このパッケージではAND を使っているので, AND, OR をshadow することは悪手だ
; 
;  Original:  https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L14 
(defmethod cf-or ((ob-a certainly-factors) (ob-b certainly-factors))
  "Combine the certainty factors for the formula (A or B).
   This is used when two rules support the same conclusion."
  (let ((a (value ob-a))
        (b (value ob-a)))
    (cond ((and (> a 0) (> b 0))
           (+ a b (* -1 a b)))
          ((and (< a 0) (< b 0))
           (+ a b (* a b)))
          (t (/ (+ a b)
                (- 1 (min (abs a) (abs b))))))))

; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L24
(defmethod cf-and ((ob-a certainly-factors) (ob-b certainly-factors))
  "Combine the certainty factors for the formula (A and B)."
  (let ((a (value ob-a))
        (b (value ob-a)))
    (min a b)))

(defmethod true-p ((cf certainly-factors))
  "Is this certainty factor considered true?"
  (> (value cf) cf-cut-off))

(defmethod false-p ((cf certainly-factors))
  "Is this certainty factor considered false?"
  (< (value cf) (- cf-cut-off 1.0)))

#|
;;; test
(defparameter a (make-instance 'certainly-factors :value 0.75))
(defparameter b (make-instance 'certainly-factors :value 0.6))
(value a)
(cf-and a b) => 0.75
(cf-or a b) => 0.9375
(true-p a)
(false-p a)
|#

(defun cf-p (x)
  "Is X a valid numeric certainty factor?"
  (and (numberp x) (<= false x true)))

; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L362
(defun cf->english (cf)
  "Convert a certainy factor to an English phrase."
  (cond ((= cf  1.0) "there is certain evidence")
        ((> cf   .8) "there is strongly suggestive evidence")
        ((> cf   .5) "there is suggestive evidence")
        ((> cf  0.0) "there is weakly suggestive evidence")
        ((= cf  0.0) "there is NO evidence either way")
        ((< cf  0.0) (concatenate 'string (cf->english (- cf))
                                  " AGAINST the conclusion")))) 

