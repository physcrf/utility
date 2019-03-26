(defsystem :utility
    :description "utility: my utility collection"
    :version "0.1"
    :author "Chen Ruofan <physcrf@qq.com>"
    :depends-on (:alexandria
		 :serapeum
		 :uiop
		 :parse-number
		 :split-sequence
		 :cl-store)
    :components ((:file "package"))
			
