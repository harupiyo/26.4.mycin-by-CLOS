(defpackage :rule
  (:use :common-lisp :cf :parameter :utils :store)
  (:export :defrule :get-rules :put-rule :clear-rules :premises :rule-number :conclusions :cf :parse-condition))

(in-package :rule)

; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L175
(let ((rules (make-hash-table)))

  (defun put-rule (rule)
    "Put the rule in a table, indexed under each
    parameter in the conclusion."
    (dolist (c (conclusions rule))
      (push rule (gethash (first c) rules)))
    rule)

  (defun get-rules (parameter-name)
    "A list of rules that help determine this parameter."
    (gethash parameter-name rules))

  (defun clear-rules () (clrhash rules)))


; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L172
; [TODO] :print-function print-rule
(defclass rule ()
  ((number      :initarg :number        :accessor rule-number)
   (premises    :initarg :premises      :accessor premises)
   (conclusions :initarg :conclusions   :accessor conclusions)
   (cf          :initarg :cf            :accessor cf)))

;https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L288
(defmacro defrule (number &body body)
  (assert (eq (first body) 'if))
  (let* ((then-part (member 'then body))
         (premises (ldiff (rest body) then-part))
         (conclusions (rest2 then-part))
         (cf (second then-part)))
    ;; [TODO] 後回し Do some error checking:
    ; (check-conditions number premises 'premise)
    ; (check-conditions number conclusions 'conclusion)
    (format t "DEBUG(in defrule): then:~A ~%cf:~A" then-part cf)
    (when (not (cf-p cf)) (error "Rule ~a: Illegal certainty factor: ~a" number cf) 
    ;; Now build the rule:
    `(put-rule
         (make-instance 'rule :number ,number :cf ,cf :premises ',premises :conclusions ',conclusions)))))

(defrule 165
  if (gram ORGANISM is pos)
     (morphology ORGANISM is coccus)
     (growth-conformation ORGANISM is chains)
  then .7
     (identity ORGANISM is streptococcus))
#|
(defrule 165
  if (gram ORGANISM is pos)
     (morphology ORGANISM is coccus)
     (growth-conformation ORGANISM is chains)
  then .7
     (identity ORGANISM is streptococcus))
==macroexpand-1==>
(PUT-RULE
 (MAKE-INSTANCE 'RULE :NUMBER 165 :CF 0.7 :PREMISES
                '((GRAM ORGANISM IS POS) (MORPHOLOGY ORGANISM IS COCCUS)
                  (GROWTH-CONFORMATION ORGANISM IS CHAINS))
                :CONCLUSIONS '((IDENTITY ORGANISM IS STREPTOCOCCUS))))
==> #<RULE {10031E6A33}>

CONTEXT> (get-rules 'identity)
(#<RULE {10031E6A33}>) <-- 登録されている
T
|#

; [TODO] 後回し
; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L306
(defun check-conditions (rule-num conditions kind)
  "Warn if any conditions are invalid."
  (when (null conditions)
    (warn "Rule ~a: Missing ~a" rule-num kind))
  (dolist (condition conditions)
    (when (not (consp condition))
      (warn "Rule ~a: Illegal ~a: ~a" rule-num kind condition))
    (multiple-value-bind (parameter inst op val)
        (parse-condition condition)
      (declare (ignore inst))
      (when (and (eq kind 'conclusion) (not (eq op 'is)))
        (warn "Rule ~a: Illegal operator (~a) in conclusion: ~a"
              rule-num op condition))
      (when (not (typep val (type-restriction (get-parameter-from-store parameter))))
        (warn "Rule ~a: Illegal value (~a) in ~a: ~a"
              rule-num val kind condition)))))

(defun parse-condition (condition)
  "A condition is of the form (parameter-key context op val).
  So for (age patient is 21), we would return 4 values:
  (age patient-1 is 21), where patient-1 is the current patient."
  (values (first condition)
          (get-store (second condition))
          (third condition)
          (fourth condition)))
