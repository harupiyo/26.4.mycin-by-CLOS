;;;
;;; parameter manager
;;;

(defpackage :parameter
  (:use :common-lisp :store)
  (:nicknames :parm)
  (:export :parameter :parameter-name :context :prompt :ask-first :reader :type-restriction :get-parameter-from-store :defparameter&store))

(in-package :parameter)

(defvar *store* (make-instance 'store))

;;; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L48

;;; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L132
(defclass parameter ()
  ((name :initarg :name :accessor parameter-name)
   (context :initform nil :initarg :context :accessor context)
   (prompt :initform "~&What is the ~*~a of ~2:*~a?" :initarg :prompt :accessor prompt)
   (ask-firlst :initform nil :initarg :ask-first :accessor ask-first)
   (type-restriction :initform t :initarg :type-restriction :accessor type-restriction)
   (reader :initform 'read :initarg :reader :accessor reader)))

;;; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L138
(defmacro defparameter&store (name &optional (context nil) (type t) (prompt "~&What is the ~*~a of ~2:*~a?") (ask-first nil) (reader 'read))
  `(store:put-store ',name
                    (make-instance 'parameter
                                   :name ',name
                                   :context ',context
                                   :type-restriction ',type
                                   :prompt ,prompt
                                   :ask-first ,ask-first
                                   :reader ',reader
                                   ) *store*))

#|
(trace put-store) ; defmethod もトレースできる

(defparameter&store name patient (member a b c) "...?" t)
==macroexpand-1==>
(PUT-STORE 'NAME
 (MAKE-INSTANCE 'PARAMETER :NAME 'NAME :CONTEXT 'PATIENT :TYPE-RESTRICTION
                '(MEMBER A B C) :PROMPT "...?" :ASK-FIRST T)
 *STORE*)
==>
#<PARAMETER {100507CF63}>

(get-store 'name *store*) => #<PARAMETER {100339DC63}>
(hash-table-count (store::db *store*)) => 1
|#

;https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L146
(defun get-parameter-from-store (parameter-name)
  "Look up the parameter structure with this name."
  (or (get-store parameter-name *store*)
      ;; 次行、
      ;; (put-store parameter-name (make-instance 'parameter :name parameter-name) *store*) 
      ;; は、
      ;; (defparameter&store parameter-name) <--- おっと、ここ、変数展開されないぞ！ 
      ;; を展開したもので、マクロに変数を渡せないことによる。
      ;;
      ;; オリジナルのソースでも同様のことをしている
      ;; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L146
      ;; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L138
      (put-store parameter-name (make-instance 'parameter :name parameter-name) *store*))) 

#|
(trace put-store)
(get-parameter-from-store 'foo) => #<PARAMETER {100339DC63}>
(get-parameter-from-store 'bar)
  0: (STORE:PUT-STORE PARAMETER::BAR #<PARAMETER:PARAMETER {10036350A3}> #<STORE:STORE {1001BDD803}>)
  0: PUT-STORE returned #<PARAMETER {10036350A3}>
#<PARAMETER {10036350A3}>
|#

