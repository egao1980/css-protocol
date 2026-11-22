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

;;; Shared: rebuild a grouping rule with new children (inputs are already
;;; validated, so no constructor runs again; nothing is mutated).

(defgeneric %with-rules (node rules)
  (:documentation "A fresh copy of grouping rule NODE holding RULES."))

(defmethod %with-rules ((node media-rule) rules)
  (make-instance 'media-rule :query (media-rule-query node) :rules rules))

(defmethod %with-rules ((node supports-rule) rules)
  (make-instance 'supports-rule :condition (supports-rule-condition node) :rules rules))

(defmethod %with-rules ((node layer-rule) rules)
  (make-instance 'layer-rule :names (layer-rule-names node) :statement-p nil :rules rules))

(defmethod %with-rules ((node scope-rule) rules)
  (make-instance 'scope-rule :start (scope-rule-start node) :end (scope-rule-end node)
                             :rules rules))

(defun %root-type-p (type)
  (member type '("html" "body") :test #'equal))

(defun %root-compound-p (compound)
  "Type html or body, or a :root part."
  (or (%root-type-p (compound-type compound))
      (member ":root" (compound-others compound) :test #'string-equal)))

(defun %substitute-root (selector replacement)
  "When SELECTOR's first compound is root-like: a new selector in which that
compound's html / body / :root is replaced by REPLACEMENT (its last compound
merged with the rest of the first compound). Else NIL."
  (destructuring-bind (first &rest rest) (selector-compounds selector)
    (when (%root-compound-p first)
      (let* ((compounds (selector-compounds replacement))
             (last (car (last compounds)))
             (kept (remove-if (lambda (part)
                                (and (eq (first part) :other)
                                     (string-equal (second part) ":root")))
                              (compound-parts first)))
             (type (or (compound-type last)
                       (unless (%root-type-p (compound-type first))
                         (compound-type first)))))
        (make-instance 'selector
                       :compounds (append (butlast compounds)
                                          (list (%make-compound
                                                 type (append (compound-parts last) kept)))
                                          rest))))))

;;; :prefix

(defun %prefix-selectors (selectors scopes)
  (loop for selector in selectors
        nconc (loop for scope in scopes
                    collect (or (%substitute-root selector scope)
                                (make-instance 'selector
                                               :compounds (append (selector-compounds scope)
                                                                  (list :descendant)
                                                                  (selector-compounds selector)))))))

(defgeneric %prefix-node (node scopes)
  (:documentation "NODE with every style selector prefixed by SCOPES (a new node)."))

(defmethod %prefix-node ((node css-node) scopes)
  (declare (ignore scopes))
  node)

(defmethod %prefix-node ((node rule) scopes)
  (make-instance 'rule :selectors (%prefix-selectors (rule-selectors node) scopes)
                       :declarations (rule-declarations node)))

(defmethod %prefix-node ((node grouping-rule) scopes)
  (%with-rules node (mapcar (lambda (child) (%prefix-node child scopes))
                            (at-rule-rules node))))

(defmethod %prefix-node ((node layer-rule) scopes)
  (if (layer-rule-statement-p node) node (call-next-method)))

(defmethod %prefix-node ((node scope-rule) scopes)
  (make-instance 'scope-rule :start (%prefix-selectors (scope-rule-start node) scopes)
                             :end (scope-rule-end node)
                             :rules (at-rule-rules node)))

;;; :scope-rule

(defun %scope-selector ()
  (make-instance 'selector :compounds (list (%make-compound nil '((:other ":scope"))))))

(defgeneric %rootify-node (node scope)
  (:documentation "NODE with leading html / body / :root replaced by SCOPE (:scope)."))

(defmethod %rootify-node ((node css-node) scope)
  (declare (ignore scope))
  node)

(defmethod %rootify-node ((node rule) scope)
  (make-instance 'rule :selectors (mapcar (lambda (s) (or (%substitute-root s scope) s))
                                          (rule-selectors node))
                       :declarations (rule-declarations node)))

(defmethod %rootify-node ((node media-rule) scope)
  (%with-rules node (mapcar (lambda (child) (%rootify-node child scope)) (at-rule-rules node))))

(defmethod %rootify-node ((node supports-rule) scope)
  (%with-rules node (mapcar (lambda (child) (%rootify-node child scope)) (at-rule-rules node))))

(defun %wrap-runs (nodes scopes scope)
  "Wrap each maximal run of style / @media / @supports / @scope nodes in
@scope (SCOPES); @layer blocks stay outside with their children wrapped."
  (let ((out '())
        (run '()))
    (flet ((flush ()
             (when run
               (push (make-instance 'scope-rule :start scopes :rules (reverse run)) out)
               (setf run '()))))
      (dolist (node nodes)
        (typecase node
          ((or rule media-rule supports-rule scope-rule)
           (push (%rootify-node node scope) run))
          (layer-rule
           (flush)
           (push (if (layer-rule-statement-p node)
                     node
                     (%with-rules node (%wrap-runs (at-rule-rules node) scopes scope)))
                 out))
          (t (flush) (push node out))))
      (flush))
    (nreverse out)))

(defun scope-stylesheet (stylesheet scope &key (method :prefix))
  (%require-type stylesheet 'stylesheet)
  (%require-type method '(member :prefix :scope-rule))
  (multiple-value-bind (scopes ok) (%selector-argument scope)
    (when ok
      (make-instance 'stylesheet
                     :rules (ecase method
                              (:prefix
                               (mapcar (lambda (node) (%prefix-node node scopes))
                                       (stylesheet-rules stylesheet)))
                              (:scope-rule
                               (%wrap-runs (stylesheet-rules stylesheet) scopes
                                           (%scope-selector))))))))

;;; used-rules

(defun %split-ascii-whitespace (string)
  (loop with n = (length string)
        for start = (position-if-not #'%css-whitespace-p string :start 0)
          then (position-if-not #'%css-whitespace-p string :start end)
        for end = (and start (or (position-if #'%css-whitespace-p string :start start) n))
        while start
        collect (subseq string start end)))

(defun %element-facts (tree)
  "List of (name id class-tokens), one per element in TREE."
  (let ((facts '()))
    (labels ((walk (node)
               (typecase node
                 (html-protocol:html-document
                  (mapc #'walk (html-protocol:html-document-children node)))
                 (html-protocol:html-element
                  (let ((class (html-protocol:element-attr node "class")))
                    (push (list (html-protocol:html-element-name node)
                                (html-protocol:element-attr node "id")
                                (and (stringp class) (%split-ascii-whitespace class)))
                          facts))
                  (mapc #'walk (html-protocol:html-element-children node)))
                 (cons (mapc #'walk node)))))
      (walk tree))
    facts))

(defun %compound-satisfied-p (compound facts)
  (or (%root-compound-p compound)
      (let ((type (compound-type compound)))
        (some (lambda (fact)
                (destructuring-bind (name id classes) fact
                  (and (or (null type) (string= type "*") (string-equal type name))
                       (every (lambda (x) (equal x id)) (compound-ids compound))
                       (every (lambda (x) (member x classes :test #'string=))
                              (compound-classes compound)))))
              facts))))

(defun %selector-may-match-p (selector facts)
  (every (lambda (item) (or (keywordp item) (%compound-satisfied-p item facts)))
         (selector-compounds selector)))

(defgeneric %used-node (node facts)
  (:documentation "NODE pruned to what FACTS may use, or NIL."))

(defmethod %used-node ((node css-node) facts)
  (declare (ignore facts))
  node)

(defmethod %used-node ((node rule) facts)
  (when (some (lambda (s) (%selector-may-match-p s facts)) (rule-selectors node))
    node))

(defmethod %used-node ((node grouping-rule) facts)
  (let ((kept (loop for child in (at-rule-rules node)
                    for used = (%used-node child facts)
                    when used collect used)))
    (when kept (%with-rules node kept))))

(defmethod %used-node ((node layer-rule) facts)
  (if (layer-rule-statement-p node) node (call-next-method)))

(defun used-rules (stylesheet tree)
  (%require-type stylesheet 'stylesheet)
  (let ((facts (%element-facts tree)))
    (make-instance 'stylesheet
                   :rules (loop for node in (stylesheet-rules stylesheet)
                                for used = (%used-node node facts)
                                when used collect used))))
