(defsystem "css-protocol"
  :version "0.1.0"
  :description "CLOS stylesheet model, pretty / minified serializer, themes, scoping and used-rule pruning (stack-css)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("html-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "model")
               (:file "selector")
               (:file "values")
               (:file "serialize")
               (:file "protocol")
               (:file "theme")
               (:file "transform"))
  :in-order-to ((test-op (test-op "css-protocol/tests"))))

(defsystem "css-protocol/tests"
  :depends-on ("css-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "serialize-test")
               (:file "escape-test")
               (:file "selector-test")
               (:file "theme-test")
               (:file "scope-test")
               (:file "used-rules-test")
               (:file "condition-test")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
