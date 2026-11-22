(defsystem "css-protocol"
  :version "0.1.0"
  :description "CLOS stylesheet model, pretty / minified serializer, themes, scoping and used-rule pruning (stack-css)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("html-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")))
