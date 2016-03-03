(defconst au3-mode-+number+ "number")

(defconst au3-mode-+identifier+ "identifier")

(defconst au3-mode-+keyword+ "keyword")

(defconst au3-mode-+newline+ ";lf;")

(defconst au3-mode-+operator+ ";op;")

(defconst au3-mode-+string+ "string")

(defconst au3-mode-+keyword-list+
  '("And" "ByRef" "Case" "Const" "ContinueLoop" "Dim" "Do" "Else"
    "ElseIf" "EndFunc" "EndIf" "EndSelect" "Exit" "ExitLoop" "For"
    "Func" "Global" "If" "In" "Local" "Next" "Not" "Or" "ReDim"
    "Return" "Select" "Step" "Then" "To" "Until" "WEnd" "While"
    "With" "EndWith" "Switch" "EndSwitch"))

(defconst au3-mode-+keyword-regexp+
  (concat (regexp-opt au3-mode-+keyword-list+ t) "\\>"))

(defconst au3-mode-+continuation+ ?_
  "Character signaling that line break is to be ignored.")

(defconst au3-mode-+comment+ ?\;
  "Character signaling that a comment starts.")

(defun au3-mode-skip-to-next-token ()
  (skip-chars-forward " \t")
  (let ((after (if (eobp) nil (char-after))))
    (when (and after (equal after au3-mode-+continuation+))
      (skip-chars-forward "^\n")
      ;; No recursion: after each continuation, a new token must come, there
      ;; may not be a new continuation or comment.
      (unless (eobp)
        (forward-char)
        (skip-chars-forward " \t")))))

(defun au3-mode-next-regexp-token (regexp type)
  (save-match-data
    (when (looking-at regexp)
      (goto-char (match-end 0))
      (cons type (substring-no-properties (match-string 0))))))

(defun au3-mode-next-operator ()
  (au3-mode-next-regexp-token
   "\\([&|^*/+=-]?=\\|[&|^*/+-]\\)"
   au3-mode-+operator+))

(defun au3-mode-next-number ()
  (au3-mode-next-regexp-token
   "[-+]?[0-9]+\\(\\.[0-9]+\\)?\\([eE][-+]?[0-9]+\\)?"
   au3-mode-+number+))

(defun au3-mode-next-identifier ()
  (au3-mode-next-regexp-token "[@$][A-Za-z_][A-Za-z0-9_]*"
                              au3-mode-+identifier+))

(defun au3-mode-next-keyword ()
  (let ((case-fold-search t))
    (let ((token (au3-mode-next-regexp-token au3-mode-+keyword-regexp+
                                             au3-mode-+keyword+)))
      (when token
        (let ((normalized (member-ignore-case (cdr token)
                                              au3-mode-+keyword-list+)))
          (if normalized
              (cons (car token) (car normalized))
            (error "NOTREACHED: keyword list and keyword regexp out of sync")))))))

(defconst au3-mode--+multi-line-comment-start-regexp+
  "[ \t]*#c\\(omments-start\\|s\\)\\>"
  "Regular expression matching the start of a multi-line comment block.")
(defconst au3-mode--+multi-line-comment-end-regexp+
  "[ \t]*#c\\(omments-end\\|e\\)\\>"
  "Regular expression matching the end of a multi-line comment block.")

(make-variable-buffer-local
 (defvar au3-mode--next-newline-already-eobp nil
   "Flag: 1st call of `au3-mode-next-newline' at end of buffer returns
`au3-mode-+newline+', second return nil."))

(defun au3-mode-next-newline (&optional recursed)
  (let ((return-result (lambda ()
                         (if (eobp)
                             (setq au3-mode--next-newline-already-eobp t)
                           (skip-chars-backward "\t "))
                         (cons au3-mode-+newline+ au3-mode-+newline+))))
    (cond ((and recursed (eobp))
           (funcall return-result))
          ((looking-at-p "\n")
           (skip-chars-forward "\n\t\r\l ")
           ;; recurse to handle skipping comments
           (au3-mode-next-newline t))
          ((looking-at-p au3-mode--+multi-line-comment-start-regexp+)
           ;; TODO: handle nested comments
           (search-forward-regexp au3-mode--+multi-line-comment-end-regexp+
                                  (point-max)
                                  'noerror)
           ;; recurse to handle skipping several comments
           (au3-mode-next-newline t))
          ((equal (char-after) au3-mode-+comment+)
           (skip-chars-forward "^\n")
           ;; recurse to handle skipping several comments
           (au3-mode-next-newline t))
          ((and (not au3-mode--next-newline-already-eobp)
                (eobp))
           (funcall return-result))
          (recursed
           (funcall return-result)))))

(defun au3-mode--run-token-matcher (matcher str &optional start stop)
  "Test MATCHER on STR."
  (let ((start (or start "|"))
        (stop (or stop "@"))
        start-pos
        end-pos
        result)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (let ((inhibit-changing-match-data t))
        (search-forward start))
      (delete-backward-char (length start) nil)
      (setq start-pos (point))
      (goto-char (point-min))
      (let ((inhibit-changing-match-data t))
        (search-forward stop))
      (delete-backward-char (length stop) nil)
      (setq end-pos (point))
      (when (< end-pos start-pos)
        (setq start-pos (1- start-pos)))
      (goto-char start-pos)
      (setq result (funcall matcher))
      (if (equal (point) end-pos)
          result
        (error "Landed on %d not at %d" (point) end-pos)))))

(defun au3-mode--collect-tokens (fun &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let ((last-pos (point))
          result token last-token)
      (catch 'done
        (while t
          (setq token (funcall fun))
          (if (or (null token)
                  (and (equal last-pos (point))
                       (equal last-token token)))
              (throw 'done nil)
            (setq last-pos (point)
                  last-token token)
            (push token result))))
      (nreverse result))))

(ert-deftest au3-mode-test-next-number ()
  "Test `au3-mode-next-number'"
  :tags '(token)
  (let ((fut 'au3-mode-next-number))    ; fut=function-under-test
    ;; Test normal numbers
    (should (equal (au3-mode--run-token-matcher fut "|12345@")
                   (cons au3-mode-+number+ "12345")))
    (should (equal (au3-mode--run-token-matcher fut "|-123456@")
                   (cons au3-mode-+number+ "-123456")))
    (should (equal (au3-mode--run-token-matcher fut "|+1234567@")
                   (cons au3-mode-+number+ "+1234567")))
    (should (equal (au3-mode--run-token-matcher fut "|-1.23456E-02@")
                   (cons au3-mode-+number+ "-1.23456E-02")))
    (should (equal (au3-mode--run-token-matcher fut "|1.2E7@")
                   (cons au3-mode-+number+ "1.2E7")))
    (should (equal (au3-mode--run-token-matcher fut "|1.2E+7@")
                   (cons au3-mode-+number+ "1.2E+7")))
    ;; Test normal number followed by other text
    (should (equal (au3-mode--run-token-matcher fut "|12345@+678")
                   (cons au3-mode-+number+ "12345")))
    (should (equal (au3-mode--run-token-matcher fut "|-2@\nabc")
                   (cons au3-mode-+number+ "-2")))
    ;; Exceptional cases: these aren't numbers or cursor is in wrong place
    (should (equal (au3-mode--run-token-matcher fut "|@ 1")
                   nil))
    (should (equal (au3-mode--run-token-matcher fut "|@if 1 > 2 then print \"Hello\"")
                   nil))
    (should (equal (au3-mode--run-token-matcher fut "|@+ 1234567")
                   nil))
    (should (equal (au3-mode--run-token-matcher fut "|@\"1234\"")
                   nil))))

(ert-deftest au3-mode-test-next-identifier ()
  "Test `au3-mode-next-identifier'"
  :tags '(token)
  (let ((fut 'au3-mode-next-identifier)) ; fut=function-under-test
    ;; Test normal identifier
    (should (equal (au3-mode--run-token-matcher fut "|@CRLF>" "|" ">")
                   (cons au3-mode-+identifier+ "@CRLF")))
    (should (equal (au3-mode--run-token-matcher fut "|@ScriptName>" "|" ">")
                   (cons au3-mode-+identifier+ "@ScriptName")))
    (should (equal (au3-mode--run-token-matcher fut "|$var@")
                   (cons au3-mode-+identifier+ "$var")))
    (should (equal (au3-mode--run-token-matcher fut "|$snake_case@")
                   (cons au3-mode-+identifier+ "$snake_case")))
    (should (equal (au3-mode--run-token-matcher fut "|$x1@")
                   (cons au3-mode-+identifier+ "$x1")))
    (should (equal (au3-mode--run-token-matcher fut "|$x@")
                   (cons au3-mode-+identifier+ "$x")))
    ;; Exceptional cases: these aren't identifiers or cursor is in wrong place
    (should (equal (au3-mode--run-token-matcher fut "|@ $id")
                   nil))
    (should (equal (au3-mode--run-token-matcher fut "|@if $id > @HOUR then print \"Hello\"")
                   nil))
    (should (equal (au3-mode--run-token-matcher fut "|@+ $id")
                   nil))
    (should (equal (au3-mode--run-token-matcher fut "|@\"@Hello\"")
                   nil))
    (should (equal (au3-mode--run-token-matcher fut "|>@1" "|" ">")
                   nil))))

(ert-deftest au3-mode-test-next-keyword ()
  "Test `au3-mode-next-keyword'"
  :tags '(token)
  (let ((case-fold-search nil)
        (fut 'au3-mode-next-keyword)) ; fut=function-under-test
    (should (equal (au3-mode--run-token-matcher fut "|if@")
                   (cons au3-mode-+keyword+ "If")))
    (should (equal (au3-mode--run-token-matcher fut "|iF@")
                   (cons au3-mode-+keyword+ "If")))
    (should (equal (au3-mode--run-token-matcher fut "|IF@")
                   (cons au3-mode-+keyword+ "If")))
    ;; Exceptional cases: these aren't identifiers or cursor is in wrong place
    (should (equal (au3-mode--run-token-matcher fut "|@ For")
                   nil))
    (should (equal (au3-mode--run-token-matcher fut "|@$var")
                   nil))
    (should (equal (au3-mode--run-token-matcher fut "|@1234")
                   nil))
    (should (equal (au3-mode--run-token-matcher fut "|@\"@Hello\"")
                   nil))
    (should (equal (au3-mode--run-token-matcher fut "|@If123")
                   nil))))

(ert-deftest au3-mode-test-skip-to-next-token ()
  "Test `au3-mode-skip-to-next-token'"
  :tags '(token)
  (let ((fut 'au3-mode-skip-to-next-token))
    (au3-mode--run-token-matcher fut "$s = 1 + 2|@")
    (au3-mode--run-token-matcher fut "If| @\"Hello\" = $a Then Exit(2)")
    (au3-mode--run-token-matcher fut "1+|@2 ")
    (au3-mode--run-token-matcher fut "MsgBox(64,| _\n       @$TITLE, $MESSAGE)")
    (au3-mode--run-token-matcher fut "MsgBox(64,| _ ; comment\n       @$TITLE, $MESSAGE)")
    (au3-mode--run-token-matcher fut "Local $a = 2 |@\nConst $B = 23")
    (au3-mode--run-token-matcher fut "\tLocal $a = 2|@\n\tConst $B = 23")
    (au3-mode--run-token-matcher fut "\tLocal $a = 2\n\tConst $B = 23| @")
    (au3-mode--run-token-matcher fut "\tLocal $a = 2\n\tConst $B = | \t_ @")
    (au3-mode--run-token-matcher fut "\tLocal $a = 2| @; local definitio@\n\tConst $B = 23")))

(ert-deftest au3-mode-test-next-newline ()
  "Test `au3-mode-next-newline'"
  :tags '(token)
  (let ((fut 'au3-mode-next-newline)
        (exp-result (cons au3-mode-+newline+ au3-mode-+newline+)))
    (should (equal (au3-mode--run-token-matcher fut "|@")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher fut "|\n   @")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher fut "Local $a = 2|\n@Local $b=3")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher fut "Local $a = 2|\n\n@\tLocal $b=3")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher fut "$a = 2|\n@  Local $b=3")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher fut "$a = 2|\n  #cs\n\t#ce\n@$b=3")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher
                    fut
                    "|#cs\n11\n\t#ce\n  ;#cs\n@ \t $b=3")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher
                    fut
                    "$a = 2|\n  #cs\n11\n\t#ce\n  ;#cs\n@ \t $b=3")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher fut "$a = 2|\n  ; c1\n\t; c2\n;c3\n@\t\t$b=3\n")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher fut "$a = |@2\n") nil))
    (should (equal (au3-mode--run-token-matcher fut "$a = |@_\n2\n") nil))
    (should (equal (au3-mode--run-token-matcher fut "$a = 2 |; comment\n@$b=3")
                   exp-result))))

(ert-deftest au3-mode-test-next-operator ()
  "Test `au3-mode-next-operator'"
  :tags '(token)
  (let ((fut 'au3-mode-next-operator))
    (should (equal (au3-mode--run-token-matcher fut "|+@")
                   (cons au3-mode-+operator+ "+")))
    (should (equal (au3-mode--run-token-matcher fut "$a|+=@-2")
                   (cons au3-mode-+operator+ "+=")))
    (should (equal (au3-mode--run-token-matcher fut "$a|=@+-2")
                   (cons au3-mode-+operator+ "=")))
    (should (equal (au3-mode--run-token-matcher fut "$a=|+@-2")
                   (cons au3-mode-+operator+ "+")))
    (should (not (au3-mode--run-token-matcher fut "|@$a=")))))

(defun au3-mode--naive-forward-token ()
  (or (au3-mode-next-newline)
      (au3-mode-next-number)
      (au3-mode-next-string)
      (au3-mode-next-operator)
      (au3-mode-next-identifier)
      (au3-mode-next-keyword)))

(defun au3-mode--forward-token-and-update-cache (beg)
  (au3-mode--naive-forward-token))

(defun au3-mode-forward-token ()
  (au3-mode-skip-to-next-token)
  (let ((here (point)))
    (or (au3-mode--cache-get-token-from-beg here)
        (au3-mode--forward-token-and-update-cache here))))

(ert-deftest au3-mode-test-forward-token ()
  "Test `au3-mode-forward-token'"
  :tags '(token)
  (let ((fut 'au3-mode-forward-token))
    (should (equal (au3-mode--run-token-matcher fut "|2@")
                   (cons au3-mode-+number+ "2")))
    (should (equal (au3-mode--run-token-matcher fut "|-3@")
                   (cons au3-mode-+number+ "-3")))
    (should (equal (au3-mode--run-token-matcher fut "|\"1\"@")
                   (cons au3-mode-+string+ "1")))))

(make-variable-buffer-local
 (defvar au3-mode--token-cache nil
   "Cache for `au3-mode-backward-token'

Cache contains 2 trees, one to map start positions to tokens, the
other to map end positions to the same tokens.  See
`au3-mode--cache-valid-p' and `au3-mode--cache-insert-token'."))

(defun au3-mode--cache-valid-p ()
  (eql (buffer-chars-modified-tick) (car au3-mode--token-cache)))

(defun au3-mode--cache-tick ()
  (car au3-mode--token-cache))

(defun au3-mode--cache-beginning ()
  (car (cdr au3-mode--token-cache)))

(defun au3-mode--cache-end ()
  (cdr (cdr au3-mode--token-cache)))

(defun au3-mode--cache-entry-beg (x)
  (car (cdr x)))

(defun au3-mode--cache-entry-end (x)
  (car x))

(defun au3-mode--cache-entry-token (x)
  (cdr (cdr x)))

(defun au3-mode--cache-compare-beg (a b)
  (cond ((and (consp a) (consp b))
         (< (au3-mode--cache-entry-beg a) (au3-mode--cache-entry-beg b)))
        ((and (consp a) (numberp b))
         (< (au3-mode--cache-entry-beg a) b))
        ((and (numberp a) (consp b))
         (< a (au3-mode--cache-entry-beg b)))
        (t (error "Not reached: a=%s b=%s" a b))))

(defun au3-mode--cache-compare-end (a b)
  (cond ((and (consp a) (consp b))
         (< (au3-mode--cache-entry-end a) (au3-mode--cache-entry-end b)))
        ((and (consp a) (numberp b))
         (< (au3-mode--cache-entry-end a) b))
        ((and (numberp a) (consp b))
         (< a (au3-mode--cache-entry-end b)))
        (t (error "Not reached: a=%s b=%s" a b))))

(defun au3-mode--cache-insert-token (token start end)
  (let ((new-entry (cons end (cons start token))))
    (unless (au3-mode--cache-valid-p)
      (setq au3-mode--token-cache
            (cons (buffer-chars-modified-tick)
                  (cons (avl-tree-create #'au3-mode--cache-compare-beg)
                        (avl-tree-create #'au3-mode--cache-compare-end)))))
    (avl-tree-enter (au3-mode--cache-beginning) new-entry)
    (avl-tree-enter (au3-mode--cache-end) new-entry)
    token))

(defun au3-mode--cache-get-token-from-beg (beg)
  "Sets cursor position after token as side effect"
  (let* ((cache (au3-mode--cache-beginning))
         (result (when cache (avl-tree-member cache beg))))
    (when result
      (goto-char (au3-mode--cache-entry-end result))
      (au3-mode--cache-entry-token result))))

(defun au3-mode--cache-get-token-from-end (end)
  "Sets cursor position before token as side effect"
  (let* ((cache (au3-mode--cache-end))
         (result (when cache (avl-tree-member cache end))))
    (when result
      (goto-char (au3-mode--cache-entry-beg result))
      (au3-mode--cache-entry-token result))))

(defun au3-mode--backward-token-and-update-cache (here)
  ;; Move back to a position known to be a token start and advance by token
  ;; until you find back the current position.
  (let (rev-token-list
        au3-mode--bt-result)
    (save-excursion
      (let ((lbp (line-beginning-position)))
        (if (= (point) lbp)
            (forward-line -1)
          (goto-char lbp)))
      (let* ((state 'normal)
             (comment-nesting-depth 0))
        ;; State machine:
        ;; - normal: if line doesn't match anything related to comments and
        ;;   isn't empty, we're good.
        ;;   STATE EXITS:
        ;;   -> normal (if empty, or line-comment)
        ;;   -> block-comment (#ce or #comments-end, depth=1)
        ;;   -> done otherwise (or if bobp)
        ;; - block-comment: handle nesting
        ;;   STATE EXITS:
        ;;   -> normal (if #cs or #comments-start and depth would drop to 0,
        ;;      otherwise depth--)
        ;;   -> block-comment (#ce or #comments-end, depth++)
        ;;   -> done (if bobp) [this effectively ignores nesting errors]
        (catch 'done
          (while t
            (when (bobp) (throw 'done nil))
            (cond
             ((eql state 'normal)
              (cond ((looking-at-p au3-mode--+multi-line-comment-end-regexp+)
                     (setq state 'block-comment
                           comment-nesting-depth 1))
                    ((looking-at-p "^[ \t]*_?[ \t]*\\(;.*\\|\\)$")
                     ;; nothing to do.
                     )
                    (t
                     (throw 'done nil))))
             ((eql state 'block-comment)
              (cond ((looking-at-p au3-mode--+multi-line-comment-end-regexp+)
                     (setq state 'block-comment
                           comment-nesting-depth (1+ comment-nesting-depth)))
                    ((looking-at-p au3-mode--+multi-line-comment-start-regexp+)
                     (if (eql comment-nesting-depth 1)
                         (setq state 'normal)
                       (setq comment-nesting-depth
                             (1- comment-nesting-depth)))))))
            (forward-line -1))))
      ;; Now we're at a point where we know a token starts -> scan forward
      ;; until we get to original point.
      (let (temp)
        (while (< (setq temp (point)) here)
          (let ((token (au3-mode-forward-token)))
            (if token
                (push (cons temp (au3-mode--cache-insert-token
                                  token temp (point)))
                      rev-token-list)
              (error "Token not recognized at %d-%d" temp (point)))
            (when (< (point) here)
              (au3-mode-skip-to-next-token)
              (when (> (point) here)
                ;; we started with (point) before the target, then skipped
                ;; only over unimportant stuff and shot past the target -> the
                ;; last token we saw is the one we wanted: fake that we found
                ;; it.
                (goto-char here))))))
      (if (= (point) here)
          (setq au3-mode--bt-result (car rev-token-list))
        (error "Not started from token boundary: point=%d here=%d\n\t%s"
               (point) here rev-token-list)))
    (when au3-mode--bt-result
      (goto-char (car au3-mode--bt-result))
      (cdr au3-mode--bt-result))))

(defun au3-mode-backward-token ()
  (skip-chars-backward " \t")
  (let ((here (point)))
   (or (au3-mode--cache-get-token-from-end here)
       (au3-mode--backward-token-and-update-cache here))))

(ert-deftest au3-mode-test-token-cache ()
  "Test `au3-mode--cache-get-token-from-end',
`au3-mode--cache-get-token-from-beg',
`au3-mode--cache-insert-token'"
  :tags '(utils)
  (with-temp-buffer
    (insert "A B C D")
    (setq au3-mode--token-cache nil)
    (should (not (au3-mode--cache-get-token-from-end 0)))
    (au3-mode--cache-insert-token "A" 1 2)
    (let ((beg-entry (avl-tree-member (au3-mode--cache-beginning) 1)))
      (should (eq beg-entry
                  (avl-tree-member (au3-mode--cache-end) 2)))
      (should (equal beg-entry (cons 2 (cons 1 "A")))))
    (should (equal (au3-mode--cache-get-token-from-end 2)
                   "A"))
    (should (equal (point) 1))
    (should (not (au3-mode--cache-get-token-from-end 0)))
    (should (not (au3-mode--cache-get-token-from-end 100)))
    (au3-mode--cache-insert-token "B" 3 4)
    (should (equal (au3-mode--cache-get-token-from-end 4)
                   "B"))
    (should (equal (point) 3))
    (should (equal (au3-mode--cache-get-token-from-end 2)
                   "A"))
    (should (equal (point) 1))))

(ert-deftest au3-mode-test-backward-token ()
  "Test `au3-mode-backward-token'"
  :tags '(token)
  (let ((fut 'au3-mode-backward-token))
    (should (equal (au3-mode--run-token-matcher fut "@2|")
                   (cons au3-mode-+number+ "2")))
    (should (equal (au3-mode--run-token-matcher fut "@-3|")
                   (cons au3-mode-+number+ "-3")))
    (should (equal (au3-mode--run-token-matcher fut "$a=@2|")
                   (cons au3-mode-+number+ "2")))
    (should (equal (au3-mode--run-token-matcher fut "$a=@'1'|")
                   (cons au3-mode-+string+ "1")))
    (dolist (bufstr_fwlist
             (list (cons "$b _ \n =2;Hello"
                         (list (cons au3-mode-+identifier+ "$b")
                               (cons au3-mode-+operator+ "=")
                               (cons au3-mode-+number+ "2")
                               (cons au3-mode-+newline+ au3-mode-+newline+)))
                   (cons "$b = 3 ; Hello\n ; other comment"
                         (list (cons au3-mode-+identifier+ "$b")
                               (cons au3-mode-+operator+ "=")
                               (cons au3-mode-+number+ "3")
                               (cons au3-mode-+newline+ au3-mode-+newline+)))
                   (cons "Const $b=4 + 5\nLocal $x\n"
                         (list (cons au3-mode-+keyword+ "Const")
                               (cons au3-mode-+identifier+ "$b")
                               (cons au3-mode-+operator+ "=")
                               (cons au3-mode-+number+ "4")
                               (cons au3-mode-+operator+ "+")
                               (cons au3-mode-+number+ "5")
                               (cons au3-mode-+newline+ au3-mode-+newline+)
                               (cons au3-mode-+keyword+ "Local")
                               (cons au3-mode-+identifier+ "$x")
                               (cons au3-mode-+newline+ au3-mode-+newline+)))
                   (cons "Const _\n$b=\"6\"\n"
                         (list (cons au3-mode-+keyword+ "Const")
                               (cons au3-mode-+identifier+ "$b")
                               (cons au3-mode-+operator+ "=")
                               (cons au3-mode-+string+ "6")
                               (cons au3-mode-+newline+ au3-mode-+newline+)))
                   (cons "#cs\n1\n#ce"
                         (list (cons au3-mode-+newline+ au3-mode-+newline+)))
                   (cons "#cs\n1\n#ce\n  \n\t\n"
                         (list (cons au3-mode-+newline+ au3-mode-+newline+)))
                   (cons "#cs\n1\n#ce\n  \n\t\n #comments-start\n#ce"
                         (list (cons au3-mode-+newline+ au3-mode-+newline+)))
                   ;; can't test nested comments: forward-token doesn't
                   ;; support it
                   ;; (cons "#cs\n#cs\n; 1\n#ce\n  \n\t#ce"
                   ;;       (list (cons au3-mode-+newline+ au3-mode-+newline+)))
                   ))
      (with-temp-buffer
        (insert (car bufstr_fwlist))
        (goto-char (point-min))
        (let ((forward-list (au3-mode--collect-tokens 'au3-mode-forward-token)))
          (should (equal forward-list (cdr bufstr_fwlist)))
          (should (equal (point) (point-max)))
          (let ((back-list (au3-mode--collect-tokens 'au3-mode-backward-token)))
            (should (equal back-list
                           (reverse (cdr bufstr_fwlist))))
            (should (equal (point) (point-min)))))))
    (with-temp-buffer
      (insert "Local $a = 3 ; initial value
               If $a == 2 - 4 Then _
                     $b = 2
               #cs
               Long comment with code in it:
If $this Then $that
               #ce")
      (goto-char (point-min))
      (let ((forward-list (au3-mode--collect-tokens 'au3-mode-forward-token)))
        (should (equal (point) (point-max)))
        (let ((back-list (au3-mode--collect-tokens 'au3-mode-backward-token)))
          (should (equal (point) (point-min)))
          (should (equal forward-list
                         (reverse back-list)))))
      ;; (let ((back-list (au3-mode--collect-tokens 'au3-mode-backward-token)))
      ;;   (should (equal back-list
      ;;                  (list (cons au3-mode-+newline+ au3-mode-+newline+)
      ;;                        (cons au3-mode-+number+ "2")
      ;;                        (cons au3-mode-+operator+ "=")
      ;;                        (cons au3-mode-+identifier+ "$b"))))
      ;;   (should (equal (point) (point-min)))
      ;;   (let ((forward-list (au3-mode--collect-tokens 'au3-mode-forward-token)))
      ;;     (should (equal (point) (point-max)))
      ;;     (should (equal forward-list
      ;;                    (reverse back-list)))))
      )))

(defun au3-mode-next-string ()
  (let ((qot (char-after))
        str
        dest)
    (when (or (eql qot ?\') (eql qot ?\"))
      (let ((start (point))
            ;; parameter for skip-chars-backward: skip anything but newline
            ;; and opening quote (i.e. valid content of the string)
            (not-qot (string ?^ qot ?\n)))
        (save-excursion
          (forward-char)                ; skip opening quote
          (setq
           str
           (catch 'done
             (while t
               ;; skip anything different from opening quote but stay on same
               ;; line
               (skip-chars-forward not-qot)
               ;; we're either at end of text or matching quote
               (let ((next (char-after)))
                 (if (not next)
                     (throw 'done nil) ; end of text, unterminated string
                   (forward-char)      ; skip quote
                   (let ((over (char-after)))
                     (if (eql over qot)
                         ;; "" or '' inside string: skip it and let
                         ;; consuming of token continue
                         (forward-char)
                       (throw
                        'done
                        ;; replace double '' (or "") by single to return true
                        ;; value of string
                        (save-match-data
                          (replace-regexp-in-string
                           (string qot qot)
                           (string qot)
                           (buffer-substring-no-properties
                            (1+ start) (1- (setq dest (point)))))))))))))))
        (when str
          (goto-char dest)
          (cons au3-mode-+string+ str))))))

(ert-deftest au3-mode-test-next-string ()
  "Test `au3-mode-next-string'"
  :tags '(token)
  (let ((fut 'au3-mode-next-string))
    (should (equal (au3-mode--run-token-matcher fut "|\"A\"@")
                   (cons au3-mode-+string+ "A")))
    (should (equal (au3-mode--run-token-matcher fut "|'A'@  ")
                   (cons au3-mode-+string+ "A")))
    (should (equal (au3-mode--run-token-matcher fut "|\"\"@ \n")
                   (cons au3-mode-+string+ "")))
    (should (equal (au3-mode--run-token-matcher fut "|''@\t  ")
                   (cons au3-mode-+string+ "")))
    ;; example from documentation
    ;; https://www.autoitscript.com/autoit3/docs/intro/lang_datatypes.htm
    (should (equal (au3-mode--run-token-matcher
                    fut
                    "|\"here is a \"\"double-quote\"\" - ok?\"@")
                   (cons au3-mode-+string+
                         "here is a \"double-quote\" - ok?")))
    (should
     (equal
      (au3-mode--run-token-matcher
       fut
       "|'This \"sentence\" contains \"lots\" of \"double-quotes\" does it not?'@")
      (cons au3-mode-+string+
            "This \"sentence\" contains \"lots\" of \"double-quotes\" does it not?")))
    (should
     (equal
      (au3-mode--run-token-matcher
       fut
       "|\"This \"\"sentence\"\" contains \"\"lots\"\" of \"\"double-quotes\"\" does it not?\"@")
      (cons au3-mode-+string+
            "This \"sentence\" contains \"lots\" of \"double-quotes\" does it not?")))
    ;; unhappy cases
    (dolist (not-a-string-token
             '("'1\"" "\"1'" "'" "\"" "\"1" "'1" "1" "'1\n'" "\"1\n\""))
      (should-not (au3-mode--run-token-matcher
                   fut
                   (concat "|@" not-a-string-token))))))
