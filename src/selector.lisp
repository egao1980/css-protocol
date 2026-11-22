;;;; Selectors. Classes and readers GIVEN; parsing and text are STUBS.
;;;;
;;;; PARSE-SELECTOR-LIST string -> list of SELECTOR, one per top-level comma.
;;;; Grammar (0.1.0 subset; brief "Selector subset"):
;;;;
;;;;   list      := complex ( "," complex )*        each complex non-empty
;;;;   complex   := compound ( combinator compound )*
;;;;   combinator:= whitespace (:descendant) | ">" (:child) | "+" (:next-sibling)
;;;;              | "~" (:subsequent-sibling), optional whitespace around
;;;;   compound  := [ type | "*" ] simple*            at least one part
;;;;   simple    := "#" ident | "." ident | "[" ... "]"
;;;;              | ":" ident [ "(" ... ")" ] | "::" ident [ "(" ... ")" ]
;;;;   ident     := CSS identifier with escapes ("\" 1-6 hex digits + one
;;;;                optional whitespace, or "\" + any non-newline char).
;;;;                Starts with a letter, "_", a char >= U+0080, an escape, or
;;;;                "-" followed by one of those or "-".
;;;;
;;;; Bracket / paren contents must be non-empty for [], balanced, with
;;;; strings closed; they are kept verbatim with whitespace runs collapsed.
;;;; Refused with CSS-INVALID-SELECTOR (input = the whole string): empty or
;;;; blank text, empty list members, leading / trailing / doubled combinators,
;;;; "|" (namespaces), "&", "{", "}", ";", "<" outside strings, "</"
;;;; anywhere, unterminated strings / brackets, a class or id without a valid
;;;; identifier (".1x", "#", ".", "a..b"). A non-string is a TYPE-ERROR.
;;;; Restarts: USE-VALUE (replacement text, parsed again), SKIP-INVALID (NIL).
;;;;
;;;; SELECTOR-COMPOUNDS: (compound combinator compound ...), combinators as the
;;;; keywords above. COMPOUND-TYPE: lowercased type, "*" or NIL.
;;;; COMPOUND-IDS / COMPOUND-CLASSES: unescaped names in source order.
;;;; COMPOUND-OTHERS: verbatim attribute / pseudo parts in source order
;;;; (":hover", "::before", ":not(.x)", "[type=\"text\"]").
;;;;
;;;; SELECTOR-TEXT selector &key (pretty t): type, then ids / classes / others
;;;; in their source order (ids and classes re-escaped with CSS-IDENT).
;;;; Combinators " > " " + " " ~ " " " pretty; ">" "+" "~" " " minified.

(in-package #:css-protocol)

(defclass compound-selector ()
  ((type :initarg :type :reader compound-type :initform nil)
   (ids :initarg :ids :reader compound-ids :initform nil)
   (classes :initarg :classes :reader compound-classes :initform nil)
   (others :initarg :others :reader compound-others :initform nil)
   (parts :initarg :parts :reader compound-parts :initform nil
          :documentation "Source order of the non-type parts: (:id name),
(:class name) or (:other text). Used by SELECTOR-TEXT."))
  (:documentation "One compound selector: div.a#b:hover."))

(defclass selector ()
  ((compounds :initarg :compounds :reader selector-compounds :initform nil))
  (:documentation "One complex selector of a selector list."))

(defun parse-selector-list (string)
  (declare (ignore string))
  (%css0-pending 'parse-selector-list))

(defun selector-text (selector &key (pretty t))
  (declare (ignore selector pretty))
  (%css0-pending 'selector-text))
