;;;; Stylesheet model. Classes, slots and readers are GIVEN; constructors are
;;;; STUBS (%css0-pending).
;;;;
;;;; Constructor rules (brief "Validation"):
;;;;
;;;; MAKE-DECLARATION property value &key important
;;;;   PROPERTY: a string. "--" + one or more of [A-Za-z0-9_-] or chars >= U+0080
;;;;     is a custom property, case kept. Otherwise it must match
;;;;     -?[A-Za-z_][A-Za-z0-9_-]* and is lowercased. Else CSS-INVALID-PROPERTY.
;;;;     A non-string property is a TYPE-ERROR.
;;;;   VALUE: a string or a REAL, else TYPE-ERROR. Reals: integers in decimal;
;;;;     ratios and floats in plain decimal (no exponent), trailing zeros and a
;;;;     trailing "." dropped: 2.0 -> "2", 1.5 -> "1.5", 1/4 -> "0.25",
;;;;     -0.5 -> "-0.5". Strings are normalised: /* comments */ outside quoted
;;;;     strings become one space; runs of whitespace (space, tab, CR, LF, FF)
;;;;     outside strings become one space; the result is trimmed. Quoted strings
;;;;     ("..." or '...') are copied as written, backslash escapes included.
;;;;     Outside strings a backslash escapes the next character too ("a\;b"
;;;;     has no separator). Braces may appear when balanced ("--x: {a: b}").
;;;;     CSS-INVALID-VALUE when the normalised text is empty, or has an
;;;;     unterminated string, comment or escape (a trailing "\"), a raw newline
;;;;     inside a string, unbalanced or mismatched () [] {}, a ";" or "!" outside
;;;;     strings, or the two characters "</" anywhere (strings included).
;;;;   IMPORTANT: generalized boolean, stored as T / NIL.
;;;;
;;;; MAKE-RULE selectors declarations
;;;;   SELECTORS: a string (comma list allowed), a SELECTOR, or a list of
;;;;   those; strings go through PARSE-SELECTOR-LIST; the result is the flat
;;;;   list of SELECTOR objects in order. DECLARATIONS: a list of
;;;;   CSS-DECLARATION or NIL (NILs dropped); anything else is a TYPE-ERROR.
;;;;   If the selector list is skipped (SKIP-INVALID) the rule is NIL.
;;;;
;;;; MAKE-STYLESHEET rules            list of CSS-NODE / NIL (dropped); else TYPE-ERROR.
;;;; MERGE-STYLESHEETS &rest sheets   a new stylesheet with the rules appended.
;;;; MAKE-MEDIA-RULE query rules      QUERY: prelude text rules (as values)
;;;;                                  -> CSS-INVALID-PRELUDE.
;;;; MAKE-SUPPORTS-RULE condition rules  same.
;;;; MAKE-LAYER-RULE names &optional (rules nil rules-p)
;;;;   NAMES: a string or list of strings, each dotted ("base.reset"); every
;;;;   segment non-empty, else CSS-INVALID-IDENTIFIER. No RULES argument ->
;;;;   statement (@layer a, b;), at least one name (none ->
;;;;   CSS-INVALID-IDENTIFIER). With RULES -> block, zero
;;;;   names (anonymous) or one; two or more names with rules ->
;;;;   CSS-INVALID-PRELUDE.
;;;; MAKE-FONT-FACE-RULE declarations
;;;; MAKE-KEYFRAMES-RULE name keyframes  NAME: non-empty string, else
;;;;                                  CSS-INVALID-IDENTIFIER (escaped on output).
;;;; MAKE-KEYFRAME selectors declarations
;;;;   SELECTORS: a string (comma list) or list of strings; each trimmed;
;;;;   "from" / "to" (any case, stored lowercased) or digits[.digits]"%";
;;;;   else CSS-INVALID-SELECTOR.
;;;; MAKE-IMPORT-RULE url &key layer supports media
;;;;   URL: a string (serialized with CSS-URL). LAYER: NIL, T (anonymous) or a
;;;;   dotted layer name. SUPPORTS / MEDIA: prelude text or NIL.
;;;; MAKE-SCOPE-RULE start rules &key end
;;;;   START / END: selector arguments as for MAKE-RULE (END may be NIL).
;;;;
;;;; Every constructor that validates text offers USE-VALUE and SKIP-INVALID
;;;; (see src/conditions.lisp). Type errors are plain CL TYPE-ERRORs.

(in-package #:css-protocol)

(defclass css-node () ()
  (:documentation "Base class of every stylesheet node."))

(defclass declaration-block ()
  ((declarations :initarg :declarations :reader rule-declarations :initform nil))
  (:documentation "Mixin: a node that holds CSS-DECLARATIONs."))

(defclass stylesheet (css-node)
  ((rules :initarg :rules :reader stylesheet-rules :initform nil)))

(defclass rule (css-node declaration-block)
  ((selectors :initarg :selectors :reader rule-selectors :initform nil))
  (:documentation "A style rule: a selector list and declarations."))

(defclass css-declaration (css-node)
  ((property :initarg :property :reader declaration-property)
   (value :initarg :value :reader declaration-value)
   (important :initarg :important :reader declaration-important-p :initform nil)))

(defclass at-rule (css-node) ()
  (:documentation "Base class of at-rules."))

(defgeneric at-rule-name (at-rule)
  (:documentation "The at-keyword without @, e.g. \"media\"."))

(defgeneric at-rule-rules (at-rule)
  (:documentation "Child rules of a grouping at-rule; NIL for others.")
  (:method ((r at-rule)) nil))

(defclass grouping-rule (at-rule)
  ((rules :initarg :rules :reader at-rule-rules :initform nil))
  (:documentation "Mixin for at-rules whose block holds rules."))

(defclass media-rule (grouping-rule)
  ((query :initarg :query :reader media-rule-query)))

(defclass supports-rule (grouping-rule)
  ((supports-condition :initarg :condition :reader supports-rule-condition)))

(defclass layer-rule (grouping-rule)
  ((names :initarg :names :reader layer-rule-names :initform nil)
   (statement-p :initarg :statement-p :reader layer-rule-statement-p :initform nil)))

(defclass font-face-rule (at-rule declaration-block) ())

(defclass keyframes-rule (at-rule)
  ((name :initarg :name :reader keyframes-rule-name)
   (keyframes :initarg :keyframes :reader keyframes-rule-keyframes :initform nil)))

(defclass keyframe (css-node declaration-block)
  ((selectors :initarg :selectors :reader keyframe-selectors :initform nil)))

(defclass import-rule (at-rule)
  ((url :initarg :url :reader import-rule-url)
   (layer :initarg :layer :reader import-rule-layer :initform nil)
   (supports :initarg :supports :reader import-rule-supports :initform nil)
   (media :initarg :media :reader import-rule-media :initform nil)))

(defclass scope-rule (grouping-rule)
  ((start :initarg :start :reader scope-rule-start :initform nil)
   (end :initarg :end :reader scope-rule-end :initform nil)))

(defmethod at-rule-name ((r media-rule)) "media")
(defmethod at-rule-name ((r supports-rule)) "supports")
(defmethod at-rule-name ((r layer-rule)) "layer")
(defmethod at-rule-name ((r font-face-rule)) "font-face")
(defmethod at-rule-name ((r keyframes-rule)) "keyframes")
(defmethod at-rule-name ((r import-rule)) "import")
(defmethod at-rule-name ((r scope-rule)) "scope")

(defun custom-property-p (declaration-or-property)
  "True when the property (a CSS-DECLARATION's or a string) starts with --."
  (declare (ignore declaration-or-property))
  (%css0-pending 'custom-property-p))

(defun make-declaration (property value &key important)
  (declare (ignore property value important))
  (%css0-pending 'make-declaration))

(defun make-rule (selectors declarations)
  (declare (ignore selectors declarations))
  (%css0-pending 'make-rule))

(defun make-stylesheet (rules)
  (declare (ignore rules))
  (%css0-pending 'make-stylesheet))

(defun merge-stylesheets (&rest stylesheets)
  (declare (ignore stylesheets))
  (%css0-pending 'merge-stylesheets))

(defun make-media-rule (query rules)
  (declare (ignore query rules))
  (%css0-pending 'make-media-rule))

(defun make-supports-rule (condition rules)
  (declare (ignore condition rules))
  (%css0-pending 'make-supports-rule))

(defun make-layer-rule (names &optional (rules nil rules-p))
  (declare (ignore names rules rules-p))
  (%css0-pending 'make-layer-rule))

(defun make-font-face-rule (declarations)
  (declare (ignore declarations))
  (%css0-pending 'make-font-face-rule))

(defun make-keyframes-rule (name keyframes)
  (declare (ignore name keyframes))
  (%css0-pending 'make-keyframes-rule))

(defun make-keyframe (selectors declarations)
  (declare (ignore selectors declarations))
  (%css0-pending 'make-keyframe))

(defun make-import-rule (url &key layer supports media)
  (declare (ignore url layer supports media))
  (%css0-pending 'make-import-rule))

(defun make-scope-rule (start rules &key end)
  (declare (ignore start rules end))
  (%css0-pending 'make-scope-rule))
