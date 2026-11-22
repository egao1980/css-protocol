;;;; Serializer. GFs GIVEN; methods are STUBS. Brief "Serializer format".
;;;;
;;;; WRITE-STYLESHEET node &key stream (pretty t)
;;;;   STREAM NIL -> return the string; T -> *standard-output*; a stream ->
;;;;   write to it and return NIL. A non-stylesheet CSS-NODE is printed as a
;;;;   stylesheet holding only that node.
;;;;
;;;; Pretty:
;;;;   - indent two spaces per nesting level;
;;;;   - "SELECTORS {" newline, one "property: value;" per line (or
;;;;     "property: value !important;"), "}" at the block's indent;
;;;;   - an empty block prints as "PRELUDE {}" on one line;
;;;;   - sibling rules at any level are separated by one blank line;
;;;;   - the sheet ends with exactly one newline; an empty sheet is "".
;;;;   - selector lists join with ", ".
;;;; Minified: no newlines, no indent, no space before "{", selector lists
;;;;   join with ",", "property:value" joined by ";" with no trailing ";",
;;;;   "!important" with no space before it.
;;;; At-rule preludes (both modes unless noted):
;;;;   @media QUERY {            @supports CONDITION {
;;;;   @layer NAME {  / @layer { (anonymous) / @layer A, B;  (minified "A,B")
;;;;   @font-face {              @keyframes NAME {   (NAME via CSS-IDENT)
;;;;   keyframe selectors join ", " / ","
;;;;   @import url("U")[ layer| layer(N)][ supports(C)][ MEDIA];  (same in both)
;;;;   @scope (START)[ to (END)] {   START / END in the mode's selector form
;;;;   Layer names: segments through CSS-IDENT joined with ".".
;;;; Minified at-rules keep the space after the at-keyword and inside the
;;;; prelude ("@media print{", "@scope (.a) to (.b){").

(in-package #:css-protocol)

(defgeneric write-stylesheet (node &key stream pretty)
  (:documentation "Serialize NODE (a stylesheet or any CSS-NODE)."))

(defgeneric write-css-node (node stream pretty depth)
  (:documentation "Write one NODE at nesting DEPTH (0 = top level) to STREAM.
Extension point for new node classes; WRITE-STYLESHEET drives it and owns the
separators between siblings."))

(defmethod write-stylesheet ((node css-node) &key stream (pretty t))
  (let ((sheet (if (typep node 'stylesheet)
                   node
                   (make-instance 'stylesheet :rules (list node)))))
    (if (null stream)
        (with-output-to-string (s)
          (write-css-node sheet s pretty 0))
        (progn
          (write-css-node sheet (if (eq stream t) *standard-output* stream) pretty 0)
          nil))))

;;; Layout helpers: the only places that write indentation, braces and the
;;; blank lines between siblings.

(defun %indent (stream pretty depth)
  (when pretty
    (loop repeat (* 2 depth) do (write-char #\Space stream))))

(defun %write-siblings (nodes stream pretty depth)
  "NODES at DEPTH; one blank line between siblings in pretty mode."
  (loop for (node . more) on nodes
        do (write-css-node node stream pretty depth)
           (when (and pretty more) (terpri stream))))

(defun %write-block (prelude stream pretty depth &key declarations rules)
  "PRELUDE { DECLARATIONS or RULES } at DEPTH."
  (%indent stream pretty depth)
  (write-string prelude stream)
  (cond ((or declarations rules)
         (write-string (if pretty " {" "{") stream)
         (when pretty (terpri stream))
         (if declarations
             (loop for (declaration . more) on declarations
                   do (write-css-node declaration stream pretty (1+ depth))
                      (when (and more (not pretty)) (write-char #\; stream)))
             (%write-siblings rules stream pretty (1+ depth)))
         (%indent stream pretty depth)
         (write-char #\} stream))
        (t (write-string (if pretty " {}" "{}") stream)))
  (when pretty (terpri stream)))

(defun %write-statement (text stream pretty depth)
  (%indent stream pretty depth)
  (write-string text stream)
  (write-char #\; stream)
  (when pretty (terpri stream)))

(defun %join (strings pretty)
  (format nil (if pretty "~{~A~^, ~}" "~{~A~^,~}") strings))

(defun %selector-list-text (selectors pretty)
  (%join (mapcar (lambda (s) (selector-text s :pretty pretty)) selectors) pretty))

(defun %layer-name-out (name)
  "Layer NAME with each dotted segment through CSS-IDENT."
  (format nil "~{~A~^.~}"
          (loop for start = 0 then (1+ dot)
                for dot = (position #\. name :start start)
                collect (css-ident (subseq name start dot))
                while dot)))

;;; Node writers

(defmethod write-css-node ((node css-node) stream pretty depth)
  (no-applicable-method #'write-css-node node stream pretty depth))

(defmethod write-css-node ((node stylesheet) stream pretty depth)
  (%write-siblings (stylesheet-rules node) stream pretty depth))

(defmethod write-css-node ((node css-declaration) stream pretty depth)
  (%indent stream pretty depth)
  (format stream (if pretty "~A: ~A~:[~; !important~];~%" "~A:~A~:[~;!important~]")
          (declaration-property node)
          (declaration-value node)
          (declaration-important-p node)))

(defmethod write-css-node ((node rule) stream pretty depth)
  (%write-block (%selector-list-text (rule-selectors node) pretty) stream pretty depth
                :declarations (rule-declarations node)))

(defmethod write-css-node ((node media-rule) stream pretty depth)
  (%write-block (concatenate 'string "@media " (media-rule-query node)) stream pretty depth
                :rules (at-rule-rules node)))

(defmethod write-css-node ((node supports-rule) stream pretty depth)
  (%write-block (concatenate 'string "@supports " (supports-rule-condition node))
                stream pretty depth
                :rules (at-rule-rules node)))

(defmethod write-css-node ((node layer-rule) stream pretty depth)
  (let ((names (mapcar #'%layer-name-out (layer-rule-names node))))
    (if (layer-rule-statement-p node)
        (%write-statement (concatenate 'string "@layer " (%join names pretty))
                          stream pretty depth)
        (%write-block (format nil "@layer~@[ ~A~]" (first names)) stream pretty depth
                      :rules (at-rule-rules node)))))

(defmethod write-css-node ((node font-face-rule) stream pretty depth)
  (%write-block "@font-face" stream pretty depth
                :declarations (rule-declarations node)))

(defmethod write-css-node ((node keyframes-rule) stream pretty depth)
  (%write-block (concatenate 'string "@keyframes " (css-ident (keyframes-rule-name node)))
                stream pretty depth
                :rules (keyframes-rule-keyframes node)))

(defmethod write-css-node ((node keyframe) stream pretty depth)
  (%write-block (%join (keyframe-selectors node) pretty) stream pretty depth
                :declarations (rule-declarations node)))

(defmethod write-css-node ((node import-rule) stream pretty depth)
  (let ((layer (import-rule-layer node)))
    (%write-statement
     (format nil "@import ~A~:[~; layer~]~@[ layer(~A)~]~@[ supports(~A)~]~@[ ~A~]"
             (css-url (import-rule-url node))
             (eq layer t)
             (and (stringp layer) (%layer-name-out layer))
             (import-rule-supports node)
             (import-rule-media node))
     stream pretty depth)))

(defmethod write-css-node ((node scope-rule) stream pretty depth)
  (%write-block (format nil "@scope (~A)~@[ to (~A)~]"
                        (%selector-list-text (scope-rule-start node) pretty)
                        (and (scope-rule-end node)
                             (%selector-list-text (scope-rule-end node) pretty)))
                stream pretty depth
                :rules (at-rule-rules node)))
