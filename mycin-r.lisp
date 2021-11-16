; https://github.com/cl-aip/mycin/blob/master/mycin-r.lisp

; 必要かな？
; (eval-when (:compile-toplevel :load-toplevel :execute)
;   (require "mycin")
;   )

(defpackage :mycin-r
  (:use :common-lisp :utilities :cf :parameter :rule :store :context :expert-system))

(in-package :mycin-r)

;;; Parameters for patient:
(defparameter&store name PATIENT t "Patient's name: " t read-line)
(defparameter&store sex PATIENT (member male female) "Sex:" t)
(defparameter&store age PATIENT number "Age:" t)
(defparameter&store burn PATIENT (member no mild serious) "Is ~a a burn patient?  If so, mild or serious?" t)
(defparameter&store compromised-host PATIENT yes/no "Is ~a a compromised host?")

;;; Parameters for culture:
(defparameter&store site CULTURE (member blood) "From what site was the specimen for ~a taken?" t)
(defparameter&store days-old CULTURE number "How many days ago was this culture (~a) obtained?" t)

;;; Parameters for organism:
(defparameter&store identity ORGANISM
  (member pseudomonas klebsiella enterobacteriaceae
          staphylococcus bacteroides streptococcus)
  "Enter the identity (genus) of ~a:" t)
(defparameter&store gram ORGANISM (member acid-fast pos neg) "The gram stain of ~a:" t)
(defparameter&store morphology ORGANISM (member rod coccus) "Is ~a a rod or coccus (etc.):")
(defparameter&store aerobicity ORGANISM (member aerobic anaerobic))
(defparameter&store growth-conformation ORGANISM (member chains pairs clumps))

(clear-rules)

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

TODO debug
(member 'then '(165
  if (gram ORGANISM is pos)
     (morphology ORGANISM is coccus)
     (growth-conformation ORGANISM is chains)
  then .7
     (identity ORGANISM is streptococcus)))

(defrule 52
  if (site CULTURE is blood)
     (gram ORGANISM is neg)
     (morphology ORGANISM is rod)
     (burn PATIENT is serious)
  then .4
     (identity ORGANISM is pseudomonas))

(defrule 71
  if (gram ORGANISM is pos)
     (morphology ORGANISM is coccus)
     (growth-conformation ORGANISM is clumps)
  then .7
     (identity ORGANISM is staphylococcus))

(defrule 73
  if (site CULTURE is blood)
     (gram ORGANISM is neg)
     (morphology ORGANISM is rod)
     (aerobicity ORGANISM is anaerobic)
  then .9
     (identity ORGANISM is bacteroides))

(defrule 75
  if (gram ORGANISM is neg)
     (morphology ORGANISM is rod)
     (compromised-host PATIENT is yes)
  then .6
     (identity ORGANISM is pseudomonas))

(defrule 107
  if (gram ORGANISM is neg)
     (morphology ORGANISM is rod)
     (aerobicity ORGANISM is aerobic)
  then .8
     (identity ORGANISM is enterobacteriaceae))

(defrule 165
  if (gram ORGANISM is pos)
     (morphology ORGANISM is coccus)
     (growth-conformation ORGANISM is chains)
  then .7
     (identity ORGANISM is streptococcus))
