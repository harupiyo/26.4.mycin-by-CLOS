(defpackage :expert-system
  (:use :common-lisp :store :parameter :parameter-with-value-cf :rule :certainly-factors :context :utilities)
  (:export :get-context-data))

(in-package :expert-system)

; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L271
(defun get-context-data (context-classes)
  "For each context, create an instance and try to find out
  required data.  Then go on to other contexts, depth first,
  and finally ask if there are other instances of this context."
  (unless (null context-classes)
    (let* ((cc (first context-classes))
           (context (make-instance (class-name cc))))
      (put-store 'current-rule 'initial)
      (mapc #'find-out (initial-data context))
      (put-store 'current-rule 'goal)
      (mapc #'find-out (goals context))
      (report-findings context)
      (get-context-data (rest context-classes))
      (when (y-or-n-p "Is there another ~a? "
                      (context-name context))
        (get-context-data context-classes)))))

; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L190
(defun find-out (parameter-key &optional (context (get-store 'current-instance)))
  "Find the value(s) of this parameter for this instance,
  unless the values are already known.
  Some parameters we ask first; others we use rules first."
  (or (get-store `(known ,parameter-key ,context))
      (put-store `(known ,parameter-key ,context)
              (if (ask-first (get-parameter-from-store parameter-key))
                  (or (ask-vals parameter-key context) (use-rules parameter-key))
                  (or (use-rules parameter-key) (ask-vals parameter-key context))))))

;;;--------------------------------------------------------------------------------
;;; ask-vals Facility
;;;--------------------------------------------------------------------------------

(defun ask-vals (parameter-key context)
  "Ask the user for the value(s) of context's parameter-key parameter,
  unless this has already been asked.  Keep asking until the
  user types UNKNOWN (return nil) or a valid reply (return t)."
  (unless (get-store `(asked ,parameter-key ,context))
    (put-store `(asked ,parameter-key ,context) t)
    (loop
      (let ((ans (prompt-and-read-vals parameter-key context)))
        (case ans
;          (help (format t +help-string+))
;          (why  (print-why (get-store 'current-rule) parameter-key))
;          (rule (princ (get-store 'current-rule)))
          ((unk unknown) (return nil))
;          (?    (format t "~&A ~a must be of type ~a"
;                        parameter-key (type-restriction (get-parameter-from-store parameter-key))) nil)
          (t    (if (check-reply ans parameter-key context)
                    (return t)
                    (format t "~&Illegal reply.  ~
                             Type ? to see legal ones."))))))))

(defun prompt-and-read-vals (parameter-key context)
  "Print the prompt for this parameter (or make one up) and
  read the reply."
  (fresh-line)
  (format t (prompt (get-parameter-from-store parameter-key)) (context-name context) parameter-key)
  (princ " ")
  (finish-output)
  (funcall (reader (get-parameter-from-store parameter-key))))

(defun check-reply (reply parameter-key context)
  "If reply is valid for this parameter-key, update the DB.
  Reply should be a val or (val1 cf1 val2 cf2 ...).
  Each val must be of the right type for this parameter."
  (let ((answers (parse-reply reply)))
    (when (every #'(lambda (pair)
                     (and (typep (first pair) (type-restriction (get-parameter-from-store parameter-key)))
                          (cf-p (second pair))))
                 answers)
      ;; Add replies to the data base
      (dolist (pair answers)
        (update-parameter-cf parameter-key context (first pair) (second pair)))
      answers)))

(defun parse-reply (reply)
  "Convert the reply into a list of (value cf) pairs."
  (cond ((null reply) nil)
        ((atom reply) `((,reply ,true)))
        (t (cons (list (first reply) (second reply))
                 (parse-reply (rest2 reply))))))

;;;--------------------------------------------------------------------------------
;;; use-rules Facility
;;;--------------------------------------------------------------------------------

(defun use-rules (parameter-key)
  "Try every rule associated with this parameter.
  Return true if one of the rules returns true."
  (some #'true-p (mapcar #'use-rule (get-rules parameter-key))))

(defun use-rule (rule)
  "Apply a rule to the current situation."
  ;; Keep track of the rule for the explanation system:
  (put-store 'current-rule rule)
  ;; If any premise is known false, give up.
  ;; If every premise can be proved true,  then
  ;; draw conclusions (weighted with the certainty factor).
  (unless (some #'reject-premise (premises rule))
    (let ((cf (satisfy-premises (premises rule) true)))
      (when (true-p cf)
        (dolist (c (conclusions rule))
          (conclude c (* cf (cf rule))))
        cf))))

(defun satisfy-premises (premises cf-so-far)
  "A list of premises is satisfied if they are all true.
  A combined cf is returned."
  ;;
  ;; so far ここまでの
  ;; https://ejje.weblio.jp/content/so+far
  ;; 
  ;; cf-so-far is an accumulator of certainty factors
  (cond ((null premises) cf-so-far)
        ((not (true-p cf-so-far)) false)
        (t (satisfy-premises
             (rest premises)
             (cf-and cf-so-far
                     (eval-condition (first premises)))))))

(defun eval-condition (condition &optional (find-out-p t))
  "See if this condition is true, optionally using FIND-OUT
  to determine unknown parameters."
  (multiple-value-bind (parameter-key context op val)
      (parse-condition condition)
    (when find-out-p (find-out parameter-key context))
    ;; Add up all the (val cf) pairs that satisfy the test
    (loop :for value-cf-pair :in (get-parameter-vals parameter-key context)
          :when (funcall op (first value-cf-pair) val)
          :sum (second value-cf-pair))))

(defun reject-premise (premise)
  "A premise is rejected if it is known false, without
  needing to call find-out recursively."
  (false-p (eval-condition premise nil)))

(defun conclude (conclusion cf)
  "Add a conclusion (with specified certainty factor) to DB."
  (multiple-value-bind (parameter-key context op val)
      (parse-condition conclusion)
    (declare (ignore op))
    (update-parameter-cf parameter-key context val cf)))

(defun is (a b) (equal a b))

;;;--------------------------------------------------------------------------------
;;; report Facility
;;;--------------------------------------------------------------------------------

(defun report-findings (context)
  "Print findings on each goal for this instance."
  (when (goals context)
    (format t "~&Findings for ~a:" (context-name context))
    (dolist (goal (goals context))
      (let ((values (get-parameter-vals goal context)))
        ;; If there are any values for this goal,
        ;; print them sorted by certainty factor.
        (if values
            (format t "~& ~a:~{~{ ~a (~,3f)  ~}~}" goal
                    (sort (copy-list values) #'> :key #'second))
            (format t "~& ~a: unknown" goal))))))


