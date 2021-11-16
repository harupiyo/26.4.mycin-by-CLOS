(defpackage :mycin
  (:use :common-lisp :utilities :cf :parameter :rule :store :context :expert-system))

; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L391
(defun mycin ()
  "Determine what organism is infecting a patient."
  (emycin
    (list (context:defcontext PATIENT  (name sex age)  ())
          (context:defcontext CULTURE  (site days-old) ())
          (context:defcontext ORGANISM ()              (identity)))))

; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L265
(defun emycin (contexts)
  "An Expert System Shell.  Accumulate data for instances of each
  context, and solve for goals.  Then report the findings."
  (store:clear-store)
  (expert-system:get-context-data contexts))

