(require 'smie)

(defconst autoit-reserved-word-list
  '("if" "then" "elseif" "else" "endif"
    "while" "wend"
    "for" "in" "to" "step" "next"
    "func" "endfunc"
    "select" "case" "endselect"
    "switch" "endswitch"                ; do not repeat "case" "to" and "else"
    "do" "until"
    "with" "endwith"
))

(defun autoit-massage-identifier (str)
  (let ((result (downcase str)))
    (if (member result autoit-reserved-word-list)
        result
      ";id;")))

(defun autoit-forward-identifier ()
  "Advance point after end of an identifier, returning the identifier."
  (let ((old (point)))
    (skip-chars-forward "A-Za-z_0-9$@")
    (autoit-massage-identifier (buffer-substring-no-properties old (point)))))

(defun autoit-backward-identifier ()
  "Backup point before start of an identifier, returning the identifier."
  (let ((old (point)))
    (skip-chars-backward "A-Za-z_0-9$@")
    (autoit-massage-identifier (buffer-substring-no-properties (point) old))))

(defun autoit-xxxward-string (char-incr char-accessor)
  (let ((old (point)))
    (while
        (progn
          (forward-char char-incr)
          (when (and (equal (funcall char-accessor) ?\")
                     (equal (funcall char-accessor (+ (point) char-incr)) ?\"))
            (forward-char (* 2 char-incr)))
          (not (equal (funcall char-accessor) ?\"))))
    (forward-char char-incr)
    (buffer-substring-no-properties old (point))))

(defun autoit-forward-string ()
  "Advance point after end of a string literal, returning the string."
  (autoit-xxxward-string 1 'char-after))

(defun autoit-backward-string ()
  "Backup point to beginning of a string literal, returning the string."
  (autoit-xxxward-string -1 'char-before))

(defun autoit-forward-operator ()
  "Advance point after end of an operator, returning the operator."
  (let ((old (point))
        (next (char-after))
        (after (if next (char-after (+ (point) 1)) nil)))
    (forward-char)
    (when (or (and (char-equal next ?<) (equal after ?>))
              (equal after ?=))
      (forward-char))
    (buffer-substring-no-properties old (point))))

(defun autoit-backward-operator ()
  "Backup point before end of an operator, returning the operator."
  (let ((old (point))
        (prev (char-before))
        (before (if prev (char-before (- (point) 1)) nil)))
    (forward-char -1)
    (when (or (and (equal prev ?=)
                   (member before autoit-operators-maybe-followed-by-=))
              (and (char-equal prev ?>) (equal before ?<)))
      (forward-char -1))
    (buffer-substring-no-properties old (point))))

(defun autoit-forward-number ()
  "Advance point after end of a number, returning the number."
  (let* ((old (point))
         (next (char-after)))
    (skip-chars-forward "+-" (+ (point) 1))
    (when (> (skip-chars-forward "0-9") 0)
      (when (equal (char-after) ?.)
        (forward-char)
        (skip-chars-forward "0-9"))
      (when (member (char-after) '(?e ?E))
        (forward-char)
        (skip-chars-forward "+-" (+ (point) 1))
        (skip-chars-forward "0-9"))
      (buffer-substring-no-properties old (point)))))

(defun autoit-backward-number ()
  "Backup point before start of a number, returning the number."
  (let* ((old (point))
         (next (char-after))
         (scientific-notation nil)
         (float nil))
    (skip-chars-backward "0-9")
    (skip-chars-backward "+-" (- (point) 1))
    (when (member (char-before) '(?e ?E))
      (forward-char -1)
      (setq scientific-notation (skip-chars-backward "0-9")))
    (when (equal (char-before) ?.)
      (forward-char -1)
      (setq float (skip-chars-backward "0-9")))
    (if (let ((before (or (char-before) ?\ )))
          (or (equal float 0)
              (equal scientific-notation 0)
              (and (<= ?a before) (<= before ?z))
              (and (<= ?A before) (<= before ?Z))
              (equal before ?_)))
        (progn
          (goto-char old)
          nil)
      (buffer-substring-no-properties (point) old))))

(defconst autoit-single-char-tokens
  '(?\( ?\) ?\[ ?\] ?,))

(defconst autoit-operators-maybe-followed-by-=
  '(?> ?< ?+ ?- ?* ?/ ?= ?&
       ?^                      ; actually ^= is not allowed, but who cares ATM
       ))

(defvar *autoit-smie-forward-eob* nil)
(defvar *autoit-smie-forward-bob* nil)

(defun autoit-smie-forward-token ()
  (let ((old (point))
        (result nil))
    (skip-chars-forward " \t")
    (if (or (eolp) (char-equal (char-after) ?\;))
        (progn (autoit-skip-to-next-token) (setq result ";\\n;"))
      (autoit-skip-to-next-token)
      (let ((next (char-after)))
        (cond ((not next) (setq result ""))         ; end of buffer
              ((member next autoit-single-char-tokens)
               (forward-char)
               (setq result (string next)))
              ((char-equal next ?\")
               (autoit-forward-string)
               (setq result ";string;"))
              ((or (char-equal next ?$) (char-equal next ?@))
               (autoit-forward-identifier)
               (setq result ";$id;"))
              ((or (char-equal next ?-) (char-equal next ?+)
                   (and (<= ?0 next) (<= next ?9)))
               (if (autoit-forward-number)
                   (setq result ";number;")
                 (setq result (concat (string next) (autoit-forward-operator)))))
              ((member next autoit-operators-maybe-followed-by-=)
               (setq result (autoit-forward-operator)))
              ;; Note that this case also will return "and" "or" and "not"
              ;; that are technically operators.  This distinction is taken
              ;; care of by the grammar.
              (t (let ((id (autoit-forward-identifier)))
                   ;; special case "Case Else" construct in Switch
                   ;; ... EndSwitch to placate the grammar
                   (if (equal id "case")
                       (let* ((old (point))
                              (next (autoit-smie-forward-token)))
                         (if (equal next "else")
                             (setq result ";caseelse;")
                           (goto-char old)
                           (setq result id)))
                     (setq result id)))))))
    (when (or (> (point) old) (not *autoit-smie-forward-eob*))
      ;(message "forward  from %d to %d, return %s" old (point) result)
      (setq *autoit-smie-forward-eob* (= (point) old)
            *autoit-smie-backward-bob* nil)
      result)))

(defun autoit-smie-backward-token ()
  (let ((old (point))
        (result nil))
    (skip-chars-backward " \t")
    (when (bolp)
      (forward-comment (- (point)))
      (if (equal (char-before) ?_)
          (autoit-skip-to-previous-token)
        (setq result ";\\n;")))
    (unless result
      (forward-comment (- (point)))
      (let ((prev (char-before)))
        (cond ((not prev)
               (setq result ""))
              ((member prev autoit-single-char-tokens)
               (forward-char -1)
               (setq result (string prev)))
              ((char-equal prev ?\")
               (autoit-backward-string)
               (setq result ";string;"))
              ((member prev autoit-operators-maybe-followed-by-=)
               (setq result (autoit-backward-operator)))
              ((member prev '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
               (if (autoit-backward-number)
                   (setq result ";number;")
                 (progn
                   (autoit-backward-identifier)
                   (setq result ";$id;"))))
              ;; Note that this case also will return "and" "or" and "not"
              ;; that are technically operators.  This distinction is taken
              ;; care of by the grammar.
              (t (let ((id (autoit-backward-identifier)))
                   (if (and (stringp id)
                            (> (length id) 1)
                            (member (elt id 0) '(?@ ?$)))
                       (setq result ";$id;")
                     ;; special case "Case Else" construct in Switch
                     ;; ... EndSwitch to placate the grammar
                     (if (equal id "else")
                         (let* ((old (point))
                                (prev (autoit-smie-backward-token)))
                           (if (equal prev "case")
                               (setq result ";caseelse;")
                             (goto-char old)
                             (setq result id)))
                       (setq result id))))))))
    (when (or (< (point) old) (not *autoit-smie-backward-bob*))
      ;(message "backward from %d to %d, return %s" old (point) result)
      (setq *autoit-smie-backward-bob* (= (point) old)
            *autoit-smie-forward-eob* nil)
      result)))

(defun autoit-skip-to-next-token ()
  (while
      (progn
        (forward-comment (point-max))
        (when (and (eql (char-after) ?_)
                   (let ((next (char-after (+ (point) 1))))
                     (or (eql next nil) (eql next 32) (eql next 10) (eql next 13)
                         (eql next ?\;))))
          (forward-char)
          t))))

(defun autoit-skip-to-previous-token ()
  (while
      (progn
        (forward-comment (- (point)))
        (when (and (eql (char-before) ?_)
                   (let ((next (char-after)))
                     (or (eql next nil) (eql next 32) (eql next 10) (eql next 13)
                         (eql next ?\;))))
          (backward-char)
          t))))

(defvar autoit-block-offset 4)

(defvar *rules-level* 0)

(defun autoit-smie-rules-2 (kind token)
  (let ((*rules-level* (1+ *rules-level*)))
    ;; (if (member kind '(:before :after))
    ;;     (message "%d kind=%S %d token=%S imm=%S parent=%S"
    ;;              *rules-level*
    ;;              kind
    ;;              (point)
    ;;              (save-excursion (autoit-smie-forward-token))
    ;;              (buffer-substring-no-properties (point) (min (point-max) (+ (point) 10)))
    ;;              (smie-indent--parent))
    ;;   (message "%d kind=%S token=%S" *rules-level* kind token))
    (let ((result
           (cond
            ;; We could set smie-indent-basic instead, but that would have two
            ;; disadvantages:
            ;; - changes to octave-block-offset wouldn't take effect immediately.
            ;; - edebug wouldn't show the use of this variable.
            ((and (eq kind :elem) (eq token 'basic)) autoit-block-offset)
            ((and (eq kind :after) (equal token ";\\n;"))
             (if (smie-rule-parent-p "in")
                 autoit-block-offset
               0))
            ((and (eq kind :before) (stringp token) (equal token ";\\n;"))
             (when (smie-rule-parent-p "then" "while" "for" "func" "select" "case" "switch" "do")
               (smie-rule-parent autoit-block-offset)))
            ((and (eq kind :before) (member token '("case" ";caseelse;")))
             (when (smie-rule-parent-p ";\\n;")
               (smie-rule-parent (- autoit-block-offset))))
            ((and (eq kind :before) (member token '("wend" "endfunc" "until" "next")))
             (- autoit-block-offset))
            ((and (eq kind :before) (member token '("endswitch")))
             (* -2 autoit-block-offset))
            )))
      ;(message "%d -> %S" *rules-level* result)
      result)))

(defun autoit-smie-rules (kind token)
  ;; this one is where I experiment
  (let ((*rules-level* (1+ *rules-level*))
        (keywords-followed-by-indent
         '("for" "do" "while" "func" "switch" "select" "case" "with" "if")))
    ;; (if (member kind '(:before :after))
    ;;     (message "%d kind=%S %d token=%S imm=%S parent=%S"
    ;;              *rules-level*
    ;;              kind
    ;;              (point)
    ;;              (save-excursion (autoit-smie-forward-token))
    ;;              (buffer-substring-no-properties (point) (min (point-max) (+ (point) 10)))
    ;;              (smie-indent--parent))
    ;;   (message "%d kind=%S token=%S" *rules-level* kind token))
    (let ((result
           (cond
            ;; We could set smie-indent-basic instead, but that would have two
            ;; disadvantages:
            ;; - changes to octave-block-offset wouldn't take effect immediately.
            ;; - edebug wouldn't show the use of this variable.
            ((and (eq kind :elem) (eq token 'basic)) autoit-block-offset)
            ((and (eq kind :after) (equal token ";\\n;"))
             (if (apply 'smie-rule-parent-p keywords-followed-by-indent)
                 (smie-rule-parent autoit-block-offset)
               0))
            ((and (eq kind :before)
                  (or (member token '("else" "elseif" "endif"))
                      (smie-rule-parent-p "then")))
             (smie-rule-parent))
            ((and (eq kind :before) (member token '("case" ";caseelse;")))
             (cond ((smie-rule-parent-p ";\\n;")
                    (smie-rule-parent (- autoit-block-offset)))
                   ((smie-rule-parent-p "case")
                    (smie-rule-parent))))
            ((and (eq kind :before) (member token '("endselect")))
             (smie-rule-parent))
            ((and (eq kind :before) (member token '("wend" "endfunc" "until" "next" "endwith")))
             (- autoit-block-offset))
            ((and (eq kind :before) (member token '("endswitch")))
             (* -2 autoit-block-offset))
            ((and (eq kind :before)
                  (apply 'smie-rule-parent-p keywords-followed-by-indent))
             (smie-rule-parent autoit-block-offset)))))
      ;(message "%d -> %S" *rules-level* result)
      result)))

(defun autoit-smie-setup ()
  "Setup SMIE for current buffer for AutoIt"
  (smie-setup
   (smie-prec2->grammar
    (smie-bnf->prec2
     '((id (";$id;"))
       (assignment-op ("=") ("+=") ("-=") ("*=") ("/=") ("&="))
       (mathematical-op
        ("<") (">") ("<>") ("==")("+") ("-") ("*") ("/") ("^") ("=") ("and") ("or") ("not"))
       (inst (inst ";\\n;" inst)
             ;; doesn't handle "if" exp "then" inst ";\\n;" !
             ("if" exp "then" inst "endif")
             ("if" exp "then" inst "else" inst "endif")
             ("if" exp "then" inst "elseif" exp "then" inst "endif")
             ("if" exp "then" inst "elseif" exp "then" inst "else" inst "endif")
             ("if" exp "then" inst "elseif" exp "then" inst "elseif" exp "then" inst "endif")
             ("if" exp "then" inst "elseif" exp "then" inst "elseif" exp "then" inst "else" inst "endif")
             ("while" exp ";\\n;" inst "wend")
             ("for" for-loop-spec ";\\n;" inst "next")
             ("for" id "in" exp ";\\n;" inst "next")
             ("select" select-case-list "endselect")
             ("switch" exp ";\\n;" switch-case-list "endswitch")
             ("switch" exp ";\\n;" switch-case-list ";caseelse;" ";\\n;" inst "endswitch")
             ("func" exp ";\\n;" inst "endfunc")
             ("do" ";\\n;" inst "until" exp)
             ("with" exp ";\\n;" inst "endwith")
             ;; assignment operators
             (id "=" exp)
             (id "+=" exp)
             (id "-=" exp)
             (id "*=" exp)
             (id "/=" exp)
             (id "&=" exp)
             )
       (for-loop-spec (id "=" exp "to" exp)
                      (id "=" exp "to" exp "step" exp))
       (select-case-list (select-case-elt ";\\n;" select-case-elt) (select-case-elt))
       (select-case-elt ("case" exp ";\\n;" inst))
       (switch-case-list (switch-case-elt ";\\n;" switch-case-elt) (switch-case-elt))
       (switch-case-elt ("case" exp ";\\n;" inst))
       (exp (";number;")
            (";string;")
            (id)
            ("(" exps ")")
            (";id;" exp)
            ;; mathematical operators
            (exp "or" exp)
            (exp "and" exp)
                                        ;          ("not" exp)
            (exp "<" exp)
            (exp ">" exp)
            (exp "<>" exp)
            (exp "==" exp)
            (exp "+" exp)
            (exp "-" exp)
            (exp "*" exp)
            (exp "/" exp)
            (exp "^" exp)
            (exp "=" exp))
       (exps (exps "," exps) (exp)))
     '((assoc ";\\n;"))
     '((assoc ","))
     '((assoc ";\\n;") (assoc "wend"))
     '((assoc ";\\n;") (assoc "endwith"))
     '((assoc ";\\n;") (assoc "until"))
     '((assoc ";\\n;") (assoc "next"))
     '((assoc ";\\n;") (assoc "case" ";caseelse;") (assoc "endswitch"))
     '((assoc ";\\n;") (assoc "case") (assoc "endselect"))
     '((assoc ";\\n;") (assoc "endfunc"))
     '((assoc ";$id;" "to" "step" "=" "in"))
     '((assoc "=" "+=" "-=" "*=" "&=" "/=")
       (assoc "<" ">" "<>" "==")
       (assoc "or")
       (assoc "and")
       (assoc "+" "-")
       (assoc "*" "/")
       (assoc "^")
       (assoc ";id;"))))
   'autoit-smie-rules
   :forward-token 'autoit-smie-forward-token
   :backward-token 'autoit-smie-backward-token))

(provide 'autoit-smie)

;; exp ...
;; param-list exp
;;            param-list "," param-list
;; param-list-w/-paren "(" param-list ")"
;; if-tail inst ";\\n;" "else" ";\\n;" inst
;;         inst ";\\n;" "elseif" exp "then" ";\\n;" if-tail
;; for-loop-spec ";$id;" "=" exp "to" exp
;;               ";$id;" "=" exp "to" exp "step" exp
;;               ";$id;" "in" exp
;; switch-case-elt "case" exp "to" exp ";\\n;" inst
;; switch-case-list switch-case-elt
;;                  switch-case-list ";\\n;" switch-case-list
;;                  switch-case-list ";\\n;" "case" "else" ";\\n;" inst
;; select-case-elt "case" exp ";\\n;" inst
;; select-case-list select-case-elt
;;                  select-case-list ";\\n;" select-case-list
;; single-inst ";id;"
;;             ";$id;" assignement-op exp
;;             ";id;" param-list-w/-paren
;;             "if" exp "then" exp
;;             "local" param-list
;;             "global" param-list
;;             "dim" param-list
;; inst single-inst
;;      inst ";\\n;" inst
;;      "if" exp "then" ";\\n;" if-tail ";\\n;" "endif"
;;      "for" for-loop-spec ";\\n;" inst ";\\n;" "next"
;;      "while" exp ";\\n" inst ";\\n" "wend"
;;      "do" ";\\n" inst ";\\n" "until" exp
;;      "switch" exp ";\\n;" switch-case-list ";\\n;" "endswitch"
;;      "select" ";\\n;" select-case-list ";\\n;" "endselect"
;;      "with" exp ";\\n;" inst ";\\n;" "endwith"
;;      "func" ";id;" param-list-w/-paren ";\\n;" inst ";\\n;" "endfunc"
