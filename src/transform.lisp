;;;; Transforms. STUBS. Brief "scope-stylesheet" and "used-rules".
;;;; Neither mutates its input; both return a new STYLESHEET.
;;;;
;;;; SCOPE-STYLESHEET stylesheet scope &key (method :prefix)
;;;;   SCOPE: selector text (one complex selector or a list), validated with
;;;;   PARSE-SELECTOR-LIST (CSS-INVALID-SELECTOR + restarts).
;;;;   :prefix - each complex selector S of each style rule becomes "SCOPE S",
;;;;     except when S's first compound has type html or body or contains
;;;;     ":root": that type / ":root" is removed and SCOPE is put in its place,
;;;;     the rest of the compound kept (":root:not([x])" -> "#c:not([x])",
;;;;     "body > p" -> "#c > p", "html" -> "#c"). Recurses into @media,
;;;;     @supports and @layer blocks. A @scope rule's start selectors are
;;;;     prefixed the same way; its children are left alone. @font-face,
;;;;     @keyframes, @import and @layer statements are copied unchanged.
;;;;   :scope-rule - each maximal run of consecutive style rules, @media,
;;;;     @supports and @scope rules becomes one @scope (SCOPE) { ... }, in
;;;;     which a leading html / body / :root is replaced by ":scope" the same
;;;;     way. @import, @font-face, @keyframes, @layer statements stay in place;
;;;;     a @layer block stays outside and its children get the same treatment.
;;;;   Any other METHOD -> TYPE-ERROR.
;;;;
;;;; USED-RULES stylesheet tree
;;;;   TREE: an html-protocol HTML-DOCUMENT, HTML-ELEMENT, or a list of nodes.
;;;;   A complex selector may match when every compound is satisfied by some
;;;;   single element: type equal ignoring case (NIL / "*" = any), every id
;;;;   equal to the element's id attribute, every class among the element's
;;;;   class attribute tokens (split on ASCII whitespace; case-sensitive).
;;;;   COMPOUND-OTHERS are ignored (satisfied) and combinators are ignored.
;;;;   A compound whose type is html or body, or whose others contain ":root",
;;;;   is always satisfied.
;;;;   Style rule: kept unchanged (EQ) when any of its selectors may match.
;;;;   @media / @supports / @layer block / @scope: rebuilt with the kept
;;;;   children, dropped when none remain. @font-face, @keyframes, @import and
;;;;   @layer statements: always kept.

(in-package #:css-protocol)

(defun scope-stylesheet (stylesheet scope &key (method :prefix))
  (declare (ignore stylesheet scope method))
  (%css0-pending 'scope-stylesheet))

(defun used-rules (stylesheet tree)
  (declare (ignore stylesheet tree))
  (%css0-pending 'used-rules))
