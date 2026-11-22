;;;; Value helpers. STUBS. Brief "Escaping".
;;;;
;;;; CSS-IDENT string -> CSSOM "serialize an identifier":
;;;;   U+0000 -> U+FFFD (the character itself);
;;;;   U+0001-U+001F, U+007F -> "\" + lowercase hex + " ";
;;;;   a digit at index 0, or at index 1 when index 0 is "-" -> "\" + hex + " ";
;;;;   the string "-" alone -> "\-";
;;;;   "-", "_", [0-9A-Za-z], chars >= U+0080 -> as is;
;;;;   any other char -> "\" + the char.
;;;;   "" -> CSS-INVALID-IDENTIFIER (USE-VALUE: escape the replacement instead;
;;;;   SKIP-INVALID: return NIL). Non-string -> TYPE-ERROR.
;;;;
;;;; CSS-STRING string -> double-quoted CSSOM "serialize a string":
;;;;   U+0000 -> U+FFFD; U+0001-U+001F, U+007F -> "\" + hex + " ";
;;;;   "\"" and "\\" -> backslash + char; "<" -> "\3c "; others as is.
;;;;
;;;; CSS-URL string -> "url(" + (css-string string) + ")".
;;;;
;;;; CSS-VAR name &optional fallback -> "var(--NAME)" or "var(--NAME, FALLBACK)".
;;;;   A leading "--" in NAME is accepted; the rest goes through CSS-IDENT.
;;;;   FALLBACK is a string or real, normalised like a declaration value.

(in-package #:css-protocol)

(defun css-ident (string)
  (declare (ignore string))
  (%css0-pending 'css-ident))

(defun css-string (string)
  (declare (ignore string))
  (%css0-pending 'css-string))

(defun css-url (string)
  (declare (ignore string))
  (%css0-pending 'css-url))

(defun css-var (name &optional fallback)
  (declare (ignore name fallback))
  (%css0-pending 'css-var))
