(defsystem :utility
    :description "utility: my utility collection"
    :version "1.00"
    :author "Chen Ruofan <physcrf@qq.com>"
    :depends-on (:alexandria
		 :serapeum
		 :uiop
		 :parse-number
		 :split-sequence
		 :cl-ppcre
		 :str)
    :components ((:file "package")
		 (:file "array" :depends-on ("package"))
		 (:file "control-flow" :depends-on ("package"))
		 (:file "function" :depends-on ("package"))
		 (:file "hash-table" :depends-on ("package"))
		 (:file "list" :depends-on ("package"))
		 (:file "macro" :depends-on ("package"))
		 (:file "number" :depends-on ("package"))
		 (:file "sequence" :depends-on ("package"))
		 (:file "stream" :depends-on ("package"))
		 (:file "symbols" :depends-on ("package"))
		 (:file "types" :depends-on ("package"))
		 ))
			
