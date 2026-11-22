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

(defmethod write-stylesheet ((node css-node) &key stream (pretty t))
  (declare (ignore stream pretty))
  (%css0-pending 'write-stylesheet))

(defgeneric write-css-node (node stream pretty depth)
  (:documentation "Write one NODE at nesting DEPTH (0 = top level) to STREAM.
Extension point for new node classes; WRITE-STYLESHEET drives it and owns the
separators between siblings."))

(defmethod write-css-node ((node css-node) stream pretty depth)
  (declare (ignore stream pretty depth))
  (%css0-pending 'write-css-node))
