(defsystem :utility
    :description "utility: my utility collection"
    :version "1.1"
    :author "Chen Ruofan <physcrf@qq.com>"
    :depends-on (:alexandria
		 :serapeum
		 :uiop
		 :parse-number
		 :parse-float
		 :split-sequence
		 :cl-ppcre
		 :str)
    :components ((:file "package")
		 (:file "array" :depends-on ("function"))
		 (:file "control-flow" :depends-on ("function"))
		 (:file "function" :depends-on ("package"))
		 (:file "hash-table" :depends-on ("function"))
		 (:file "list" :depends-on ("function"))
		 (:file "macro" :depends-on ("function"))
		 (:file "number" :depends-on ("function"))
		 (:file "sequence" :depends-on ("function"))
		 (:file "stream" :depends-on ("function"))
		 (:file "symbols" :depends-on ("function"))
		 (:file "types" :depends-on ("function"))
		 ))
			
