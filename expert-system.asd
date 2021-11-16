(defsystem :expert-system
           :name "expert-system by Mycin"
           :licence "MIT"
           :description "A Famous Expert System for Infections Disease"
           :serial t
           :components ((:file "utilities")
                        (:file "certainly-factors")
                        (:file "store")
                        (:file "parameter")
                        (:file "parameter-with-value-cf")
                        (:file "context")
                        (:file "rule")
                        (:file "expert-system")
                        (:file "mycin")
                        (:file "mycin-r")))
