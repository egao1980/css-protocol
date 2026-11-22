;;;; Custom-property themes. Class GIVEN; constructor and stylesheet STUBS.
;;;; Brief "Themes".
;;;;
;;;; MAKE-CSS-THEME &key light dark (selector ":root") toggle-attribute
;;;;   LIGHT / DARK: alists or plists of key -> value (a list whose first
;;;;   element is a cons is an alist, else a plist). Keys: strings with or
;;;;   without the "--" prefix, or symbols (name downcased). Stored normalised
;;;;   as alists of ("--name" . value-text) in the given order, values
;;;;   normalised as declaration values (MAKE-DECLARATION rules + restarts).
;;;;   SELECTOR: replaces ":root" in every rule below.
;;;;   TOGGLE-ATTRIBUTE: NIL or an attribute name such as "data-theme".
;;;;
;;;; CSS-THEME-STYLESHEET theme -> STYLESHEET with, in order:
;;;;   1. SEL { [color-scheme: light dark;  -- only when DARK is non-empty]
;;;;            light custom properties }
;;;;   2. when DARK: @media (prefers-color-scheme: dark) {
;;;;        SEL { dark custom properties } }   where SEL is
;;;;        SEL:not([ATTR="light"]) when TOGGLE-ATTRIBUTE is set;
;;;;   3. when DARK and TOGGLE-ATTRIBUTE:
;;;;        SEL[ATTR="dark"] { color-scheme: dark; dark custom properties }
;;;;        SEL[ATTR="light"] { color-scheme: light; }

(in-package #:css-protocol)

(defclass css-theme ()
  ((light :initarg :light :reader css-theme-light :initform nil)
   (dark :initarg :dark :reader css-theme-dark :initform nil)
   (selector :initarg :selector :reader css-theme-selector :initform ":root")
   (toggle-attribute :initarg :toggle-attribute :reader css-theme-toggle-attribute
                     :initform nil)))

(defun make-css-theme (&key light dark (selector ":root") toggle-attribute)
  (declare (ignore light dark selector toggle-attribute))
  (%css0-pending 'make-css-theme))

(defun css-theme-stylesheet (theme)
  (declare (ignore theme))
  (%css0-pending 'css-theme-stylesheet))
