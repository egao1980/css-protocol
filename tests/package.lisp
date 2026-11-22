(defpackage #:css-protocol/tests
  (:use #:cl #:rove #:css-protocol)
  (:import-from #:html-protocol
                #:make-html-document
                #:make-html-element
                #:html-text))

(in-package #:css-protocol/tests)

(defun lines (&rest lines)
  "Each line followed by a newline: the pretty golden format."
  (format nil "~{~A~%~}" lines))

(defun pretty (node)
  (write-stylesheet node))

(defun mini (node)
  (write-stylesheet node :pretty nil))

(defun decl (property value &rest keys)
  (apply #'make-declaration property value keys))

(defun r (selectors &rest declarations)
  (make-rule selectors declarations))

(defun sheet (&rest rules)
  (make-stylesheet rules))

(defun text-node (string)
  (make-instance 'html-text :data string))

(defun el (name attributes &rest children)
  "ATTRIBUTES is a plist of strings: (\"class\" \"a b\" \"id\" \"x\")."
  (make-html-element name
                     :attributes (loop for (k v) on attributes by #'cddr
                                       collect (cons k v))
                     :children children))

(defun selector-texts (rule &key (pretty t))
  (mapcar (lambda (s) (selector-text s :pretty pretty)) (rule-selectors rule)))
