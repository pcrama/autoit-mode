;;; autoit-smie.el --- AutoIt navigation and indentation

;; Version: 2017-07-28

(require 'smie)

(defconst autoit-reserved-word-list
  '("if" "then" "elseif" "else" "endif"
    "do" "until"
    "while" "wend"
    "with" "endwith"
    "select" "case" "endselect"
    "switch" "to" "endswitch"           ; don't repeat "case" and "else"
    "for" "in" "step" "next"            ; don't repeat "to"
    "func" "endfunc"
    ;; otherwise, these operatros get mistaken as identifiers->";id;".  Do NOT
    ;; add "not" as we want it to be mistaken as an ";id;"
    "or" "and"
    ;; do not add "local", "global", "dim", "byref", "const", so they will be
    ;; seen as identifiers and fall in the rule ";id;" exp
    ))

(defvar autoit-last-token-start nil)
(defvar autoit-last-token-end nil)

(defun autoit-smie-last-token-literally ()
  (buffer-substring-no-properties autoit-last-token-start
                                  autoit-last-token-end))

(defun autoit-massage-identifier (str)
  (if (and (stringp str)
           (> (length str) 1)
           (member (elt str 0) '(?@ ?$)))
      ";$id;"
    (let ((result (downcase str)))
      (cond ((member result autoit-reserved-word-list)
             result)
            ((and (plusp (length result)) (char-equal (elt result 0) ?\#))
             ";preprocessor;")
            (t ";id;")))))

(defun autoit-forward-identifier ()
  "Advance point after end of an identifier, returning the identifier."
  (let ((old (point)))
    (when (member (char-after) '(?\$ ?\@ ?\# ?\.))
      (forward-char))
    (skip-chars-forward "A-Za-z_0-9")
    (autoit-massage-identifier (buffer-substring-no-properties old (point)))))

(defun autoit-backward-identifier ()
  "Backup point before start of an identifier, returning the identifier."
  (let ((old (point)))
    (skip-chars-backward "A-Za-z_0-9")
    (when (member (char-before) '(?\$ ?\@ ?\# ?\.))
      (backward-char))
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
    (when (member next '(?+ ?- ?* ?/ ?= ?< ?> ?&))
      (forward-char)
      (when (or (and (char-equal next ?<) (equal after ?>))
                (equal after ?=))
        (forward-char))
      (buffer-substring-no-properties old (point)))))

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
         (next (char-after))
         (result (progn
                   (skip-chars-forward "+-" (+ (point) 1))
                   (when (> (skip-chars-forward "0-9") 0)
                     (when (equal (char-after) ?.)
                       (forward-char)
                       (skip-chars-forward "0-9"))
                     (when (member (char-after) '(?e ?E))
                       (forward-char)
                       (skip-chars-forward "+-" (+ (point) 1))
                       (skip-chars-forward "0-9"))
                     (buffer-substring-no-properties old (point))))))
    (unless result (goto-char old))
    result))

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

(defconst autoit-multiline-comment-start-regexp
  "^#c\\(s\\|omments-start\\)")

(defconst autoit-multiline-comment-end-regexp
  "^#c\\(e\\|omments-end\\)")

(defun autoit-forward-comment (count)
  (if (> count 0)
      (save-match-data
        (let ((old (point)))
          (while (progn
                   (forward-comment (point-max))
                   (when (looking-at autoit-multiline-comment-start-regexp)
                     (re-search-forward autoit-multiline-comment-end-regexp))
                   (prog1
                       (> (point) old)
                     (setq old (point)))))))
    (save-match-data
      (let ((old (point)))
        (while (progn
                 (forward-comment (- (point)))
                 (when (save-excursion
                         (beginning-of-line)
                         (looking-at autoit-multiline-comment-end-regexp))
                   (re-search-backward autoit-multiline-comment-start-regexp))
                 (prog1
                     (< (point) old)
                   (setq old (point)))))))))

(defun autoit-skip-to-next-token ()
  (while
      (progn
        (autoit-forward-comment (point-max))
        (when (and (eql (char-after) ?_)
                   (let ((next (char-after (+ (point) 1))))
                     (or (eql next nil) (eql next 32) (eql next 10) (eql next 13)
                         (eql next ?\;))))
          (forward-char)
          t))))

(defun autoit-skip-to-previous-token ()
  (while
      (progn
        (autoit-forward-comment (- (point)))
        (when (and (eql (char-before) ?_)
                   (let ((next (char-after)))
                     (or (eql next nil) (eql next 32) (eql next 10) (eql next 13)
                         (eql next ?\;))))
          (backward-char)
          t))))

(defconst autoit-single-char-tokens
  '(?\( ?\) ?\[ ?\] ?,))

(defconst autoit-operators-maybe-followed-by-=
  '(?> ?< ?+ ?- ?* ?/ ?= ?&
       ?^                      ; actually ^= is not allowed, but who cares ATM
       ))

(defvar *autoit-smie-forward-eob* nil)
(defvar *autoit-smie-forward-bob* nil)

(defun autoit-smie-token-internal-at-bol ()
  ;; rebind eob and bob flags to avoid interfering with other routines because
  ;; ot the repeated calls to autoit-smie-{for,back}ward-token
  (let (*autoit-smie-forward-eob*
        *autoit-smie-forward-bob*)
    (save-excursion
      (do ((prev nil current)
           (current (save-excursion (autoit-smie-backward-token-internal (- (point))))
                    (autoit-smie-backward-token-internal (point))))
          ((or (null current) (equal current ";lf;"))
           (or prev
               (save-excursion (autoit-smie-forward-token-internal (point)))))))))

(defun autoit-smie-token-internal-at-eol ()
  ;; rebind eob and bob flags to avoid interfering with other routines because
  ;; ot the repeated calls to autoit-smie-{for,back}ward-token
  (let (*autoit-smie-forward-eob*
        *autoit-smie-forward-bob*)
    (save-excursion
      (do ((prev nil current)
           (current (save-excursion (autoit-smie-forward-token-internal (- (point))))
                    (autoit-smie-forward-token-internal (point))))
          ((or (null current) (equal current ";lf;"))
           (or prev
               (save-excursion (autoit-smie-backward-token-internal (point)))))))))

(defun autoit-smie-forward-token-internal (&optional dummy)
  (let ((old (point))
        (result nil))
    (skip-chars-forward " \t")
    (if (or (eolp) (char-equal (char-after) ?\;))
        (progn (autoit-skip-to-next-token) (setq result ";lf;"))
      (autoit-skip-to-next-token)
      (let ((next (char-after)))
        (setq autoit-last-token-start (point))
        (cond ((not next) (setq result ""))         ; end of buffer
              ((member next autoit-single-char-tokens)
               (forward-char)
               (setq result (string next)))
              ((char-equal next ?\")
               (autoit-forward-string)
               (setq result ";string;"))
              ((or (char-equal next ?-) (char-equal next ?+)
                   (and (<= ?0 next) (<= next ?9)))
               (if (autoit-forward-number)
                   (setq result ";number;")
                 (setq result (autoit-forward-operator))))
              ((member next autoit-operators-maybe-followed-by-=)
               (setq result (autoit-forward-operator)))
              ;; Note that this case also will return "and" "or" and "not"
              ;; that are technically operators.  This distinction is taken
              ;; care of by the grammar.
              (t (setq result (autoit-forward-identifier))))
        (setq autoit-last-token-end (point))))
    (when (or (> (point) old) (not *autoit-smie-forward-eob*))
      (setq *autoit-smie-forward-eob* (= (point) old)
            *autoit-smie-backward-bob* nil)
      result)))

(defun autoit-smie-forward-token ()
  (let* ((starting-point (point))
         (result (autoit-smie-forward-token-internal (point))))
    (cond ((equal result "if")
           (if (equal (autoit-smie-token-internal-at-eol) "then")
               result
             (setq result ";single-line-if;")))
          ((equal result "then")
           (if (equal (save-excursion (autoit-smie-forward-token-internal))
                      ";lf;")
               result
             (setq result ";single-line-then;")))
          ((equal result ";lf;")
           (let (bol-token-cache)
             (cond ((equal (setq bol-token-cache
                                 (save-excursion
                                   (goto-char starting-point)
                                   (autoit-smie-token-internal-at-bol)))
                           "case")
                    (setq result ";lf-after-case;"))
                   ((equal bol-token-cache "switch")
                    (setq result ";lf-after-switch;"))
                   ((equal bol-token-cache "for")
                    (setq result ";lf-after-for;"))
                   ((equal bol-token-cache "func")
                    (setq result ";lf-after-func;"))
                   ((equal (save-excursion (autoit-smie-forward-token-internal))
                           "case")
                    (setq result ";lf-before-case;"))
                   (t result))))
          ((equal result "else")
           (if (equal (save-excursion
                        (goto-char (- (point) (length result)))
                        (autoit-smie-backward-token-internal))
                      "case")
               (setq result ";case-else;")
             result))
          ((equal result "case")
           (let ((old (point)))
             (if (equal (autoit-smie-forward-token-internal)
                        "else")
                 (setq result ";case-else;")
               (goto-char old)
               result)))
          (t result))))

(defun autoit-smie-backward-token-internal (&optional dummy)
  (let ((old (point))
        (result nil))
    (skip-chars-backward " \t")
    (when (bolp)
      (autoit-forward-comment (- (point)))
      (if (equal (char-before) ?_)
          (autoit-skip-to-previous-token)
        (setq result ";lf;")))
    (unless result
      (autoit-forward-comment (- (point)))
      (setq autoit-last-token-end (point))
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
              ((and (<= ?0 prev) (<= prev ?9))
               (if (autoit-backward-number)
                   (setq result ";number;")
                 (setq result (autoit-backward-identifier))))
              ;; Note that this case also will return "and" "or" and "not"
              ;; that are technically operators.  This distinction is taken
              ;; care of by the grammar.
              (t (setq result (autoit-backward-identifier)))))
      (setq autoit-last-token-start (point)))
    (when (or (< (point) old) (not *autoit-smie-backward-bob*))
      (setq *autoit-smie-backward-bob* (= (point) old)
            *autoit-smie-forward-eob* nil)
      result)))

(defun autoit-smie-backward-token ()
  (let* ((starting-point (point))
         (result (autoit-smie-backward-token-internal (point))))
    (cond ((equal result "if")
           (if (equal (autoit-smie-token-internal-at-eol) "then")
               result
             (setq result ";single-line-if;")))
          ((equal result "then")
           (if (equal (save-excursion
                        (goto-char (+ (point) (length result)))
                        (autoit-smie-forward-token-internal))
                      ";lf;")
               result
             (setq result ";single-line-then;")))
          ((and (equal result ";lf;") (not (bobp)))
           (let (bol-token-cache)
             (cond ((equal (setq bol-token-cache
                                 (autoit-smie-token-internal-at-bol))
                           "case")
                    (setq result ";lf-after-case;"))
                   ((equal bol-token-cache "switch")
                    (setq result ";lf-after-switch;"))
                   ((equal bol-token-cache "for")
                    (setq result ";lf-after-for;"))
                   ((equal bol-token-cache "func")
                    (setq result ";lf-after-func;"))
                   ((equal (save-excursion
                             (goto-char starting-point)
                             (autoit-smie-forward-token-internal))
                           "case")
                    (setq result ";lf-before-case;"))
                   (t result))))
          ((equal result "else")
           (let ((old (point)))
             (if (equal (autoit-smie-backward-token-internal)
                        "case")
                 (setq result ";case-else;")
               (goto-char old)
               result)))
          ((equal result "case")
           (if (equal (save-excursion
                        (goto-char starting-point)
                        (autoit-smie-forward-token-internal))
                      "else")
               (setq result ";case-else;")
             result))
          (t result))))

(defvar autoit-block-offset 4)

(defvar *rules-level* 0)

(defun autoit-smie-indent-preprocessor-directive ()
  (when (save-excursion
          ;; This is not needed, as smie-indent-calculate moves to first
          ;; non-space on line
          ;; (beginning-of-line)
          ;; (skip-chars-forward " \t" )
          (equal (char-after) ?\#))
    0))

(defun autoit-smie-rules (kind token)
  ;; this one is where I experiment
  (let (;; SMIE-hack why did I have to force recomputing the parent to get the
        ;; right answer at one time?
        ;(smie--parent nil)
        (*rules-level* (1+ *rules-level*))
        (keywords-followed-by-indent
         '("do" "if" "while" "with" "select" "case" "for" "func")))
    ;; (if (member kind '(:before :after))
    ;;     (let* ((parent (smie-indent--parent))
    ;;            (dummy-next (smie-rule-next-p ";lf;"))
    ;;            (line-info (if (and (consp parent)
    ;;                                (numberp (elt parent 1))
    ;;                                (stringp (elt parent 2)))
    ;;                           (save-excursion
    ;;                             (goto-char (elt parent 1))
    ;;                             (format " parent-line=%d" (line-number-at-pos)))
    ;;                         "")))
    ;;       (message "%d kind=%S %d token=%S imm=%S parent=%S%s smie--after=%S smie--token=%S"
    ;;                *rules-level*
    ;;                kind
    ;;                (point)
    ;;                (save-excursion (autoit-smie-forward-token))
    ;;                (buffer-substring-no-properties (point) (min (point-max) (+ (point) 10)))
    ;;                parent line-info
    ;;                smie--after smie--token))
    ;;   (message "%d kind=%S token=%S" *rules-level* kind token))
    (let ((result
           (cond
            ;; We could set smie-indent-basic instead, but that would have two
            ;; disadvantages:
            ;; - changes to octave-block-offset wouldn't take effect immediately.
            ;; - edebug wouldn't show the use of this variable.
            ((and (eq kind :elem) (eq token 'basic)) autoit-block-offset)
            ((and (member kind '(:before :after))
                  (equal token ";lf;")
                  (smie-rule-parent-p ";lf;"))
             0)
            ((and (eq kind :after) (member token '(";lf;" ";lf-after-case;")))
             (if (apply 'smie-rule-parent-p keywords-followed-by-indent)
                 (smie-rule-parent autoit-block-offset)
               0))
            ((and (eq kind :after) (equal token "then"))
             (smie-rule-parent autoit-block-offset))
            ((and (eq kind :before)
                  (member token '("else" "then"))
                  (smie-rule-parent-p "then" "if"))
             (smie-rule-parent))
            ((and (eq kind :before)
                  (member token '("else" "elseif" "endif")))
             (smie-rule-parent))
            ((and (eq kind :before)
                  (or (smie-rule-parent-p "then" "else")))
             (smie-rule-parent autoit-block-offset))
            ((and (eq kind :before) (member token '("else")))
             (cond ((smie-rule-parent-p ";lf;")
                    (smie-rule-parent (- autoit-block-offset)))))
            ((and (eq kind :before)
                  (member token '("until" "endselect" "endwith" "wend" "next" "endfunc")))
             (smie-rule-parent))
            ((and (eq kind :before)
                  (apply 'smie-rule-parent-p keywords-followed-by-indent))
             (smie-rule-parent autoit-block-offset)))))
      ;; (message "%d -> %S" *rules-level* result)
      result)))

(defun autoit-smie-blink-matching-open ()
  (if (save-excursion
        (skip-chars-backward " \t")
        (eq (syntax-class (syntax-after (1- (point)))) 5))
      (smie-blink-matching-open)
    (let (show-paren-mode)
      (smie-blink-matching-open))))

(defun autoit-smie-setup ()
  "Setup SMIE for current buffer for AutoIt"
  (smie-setup
   (smie-prec2->grammar
    (smie-bnf->prec2
     '((param-list (exp)
                   (param-list "," param-list))
       (param-list-w/-paren ("(" ")")
                            ("(" param-list ")"))
       (exp (";number;")
            (";string;")
            ($id)
            (id-w/-param-list)
            ;(";id;" exp)
            ("[" param-list "]")
            ;; mathematical operators
            (exp "or" exp)
            (exp "and" exp)
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
       (select-case-list ("case" exp ";lf-after-case;" inst)
                         (select-case-list ";lf-before-case;" select-case-list))
       (switch-case-list ("case" exp "to" exp ";lf-after-case;" inst)
                         (switch-case-list ";lf-before-case;" switch-case-list)
                         (switch-case-list ";lf-before-case;"
                                           ";case-else;" ";lf-after-case; inst"))
       (for-clause ($id "=" exp "to" exp)
                   ($id "=" exp "to" exp "step" exp)
                   ($id "in" exp))
       (for-body (for-clause ";lf-after-for;" inst))
       (func-def (id-w/-param-list ";lf-after-func;" inst))
       (if-body (exp "then" if-tail))
       (if-tail (inst "else" inst)
                (inst "elseif" exp "then" if-tail))
       ($id (";$id;"))
       (id-w/-param-list (";id;" param-list-w/-paren))
       (single-inst ;(exp)
                    ;; assignment operators
                    ($id "=" exp)
                    ($id "+=" exp)
                    ($id "-=" exp)
                    ($id "*=" exp)
                    ($id "/=" exp)
                    ($id "&=" exp)
                    (";single-line-if;" exp ";single-line-then;" single-inst))
       (inst (single-inst)
             (inst ";lf;" inst)
             ("if" if-body "endif")
             ("do" inst "until" exp)
             ;; actually, it should be "while" exp ";lf;" inst "wend" but the
             ;; parser won't see the difference anyway
             ("while" inst "wend")
             ("with" inst "endwith")
             ("select" select-case-list "endselect")
             ("switch" exp ";lf-after-switch;" switch-case-list "endswitch")
             ("for" for-body "next")
             ("func" func-def "endfunc")
             ))
     '((assoc ";lf;"))
     '((assoc ";lf;")
       (assoc ";lf-before-case;") (assoc ";lf-after-case;")
       (assoc "case" "case-else")
       (assoc "to"))
     '((assoc "else" "elseif") (assoc "then") (assoc ";lf;"))
     ;'((nonassoc "do;lf;") (assoc ";lf;until") (assoc ";lf;"))
     '((assoc ","))
     '((assoc ";lf;")
       (assoc "to")
       (assoc "=" "+=" "-=" "*=" "&=" "/=")
       (assoc "<" ">" "<>" "==")
       (assoc "or")
       (assoc "and")
       (assoc "+" "-")
       (assoc "*" "/")
       (assoc "^")
       (nonassoc ";id;"))))
   'autoit-smie-rules
   :forward-token 'autoit-smie-forward-token
   :backward-token 'autoit-smie-backward-token)
  (let ((hook-to-prepend
         'autoit-smie-indent-preprocessor-directive)
        (indent-functions (copy-sequence smie-indent-functions)))
    (set (make-local-variable 'smie-indent-functions)
         (if (eq (car indent-functions) hook-to-prepend)
             indent-functions
           (cons hook-to-prepend indent-functions))))
  ;; override smie-blink-matching-open with our own version (hiding
  ;; show-paren-mode temporarily if it would mean that a keyword ender would
  ;; not make the keyword starter blink).  Our own version is first removed
  ;; from the post-self-insert-hook to make sure it runs last.
  (let* ((to-add 'autoit-smie-blink-matching-open)
         (to-remove (list 'smie-blink-matching-open
                          #'smie-blink-matching-open
                          to-add))
         (filtered-psih
          (nconc (mapcan (lambda (h) (unless (member h to-remove) (list h)))
                         post-self-insert-hook)
                 (list to-add))))
    (setq post-self-insert-hook filtered-psih)))

(provide 'autoit-smie)
;;; autoit-smie.el ends here
