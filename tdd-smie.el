;; run test from scratch with
;;
;; runemacs -Q --eval '(progn (load "~/Desktop/autoit-mode/tdd-smie.el") (ert t))'

(require 'avl-tree)
(require 'pcase)

(defconst au3-mode-+newline+ ";lf;")

(defconst au3-mode-+exp-inst-sep+ ";exp-inst-sep;")

(defconst au3-mode-+operator+ ";op;")

(defconst au3-mode-+operator-regexp+
  (regexp-opt '("=" "+=" "-" "*=" "/=" "&" "&=" "+" "-" "*"
                "/" "^" "=" "==" "<>" ">" ">=" "<" "<=" "?" ":")))
(defconst au3-mode-+string+ "string")

(defconst au3-mode-+number-regexp+
  "[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][+-]?[0-9]+\\)?")

(defconst au3-mode-+number+ ";number;")

(defconst au3-mode-+statement-start-keyword-list+
  '("For" "Func" "If" "Return" "Select" "While" "With"))

(defconst au3-mode-+statement-keyword-list+
  (append '("Case" "Const" "ContinueLoop" "Dim" "Do" "Else"
            "ElseIf" "EndFunc" "EndIf" "EndSelect" "Exit" "ExitLoop"
            "Global" "Local" "Next" "ReDim" "Then" "Until" "WEnd"
            "EndWith" "Switch" "EndSwitch")
          au3-mode-+statement-start-keyword-list+))

(defconst au3-mode-+keyword-list+
  (append '("And" "ByRef" "In" "Not" "Or" "Step" "To")
          au3-mode-+statement-keyword-list+))

(defconst au3-mode-+keyword-normalization+
  (let ((h (make-hash-table :test 'equal)))
    (mapc (lambda (x) (puthash (downcase x) x h))
          au3-mode-+keyword-list+)
    h))

(defun au3-mode--normalize-keyword (x &optional default)
  (gethash (downcase x) au3-mode-+keyword-normalization+ default))

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
      ;; leading `.' in regexp to skip over au3-mode-+continuation+ char
      ;; without having to interpolate it into the regular expression
      (when (looking-at ".[ \t]*\\(;[^\n]*\\)?$")
        (skip-chars-forward "^\n")
        ;; No recursion: after each continuation, a new token must come, there
        ;; may not be a new continuation or comment.
        (unless (eobp)
          (forward-char)
          (skip-chars-forward " \t"))))))

(defun au3-mode-next-regexp-token (regexp)
  (save-match-data
    (when (looking-at regexp)
      (goto-char (match-end 0))
      (substring-no-properties (match-string 0)))))

(defun au3-mode-next-keyword ()
  (let ((case-fold-search t))
    (let ((token (au3-mode-next-regexp-token au3-mode-+keyword-regexp+)))
      (when token
        (let ((normalized (member-ignore-case token
                                              au3-mode-+keyword-list+)))
          (if normalized
              (car normalized)
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

(defun au3-mode-next-newline-1 ()
  (let ((start (point)))
    (cond ((or (eolp) (eql (char-after) au3-mode-+comment+))
           (skip-chars-forward "^\n"))
          ((save-excursion
             (goto-char (line-beginning-position))
             (looking-at au3-mode--+multi-line-comment-start-regexp+))
           (goto-char (match-end 0))
           (search-forward-regexp au3-mode--+multi-line-comment-end-regexp+)
           (goto-char (match-end 0))
           (skip-chars-forward " \t")))
    (when (eql (char-after) ?\n)
      (forward-char)
      (prog1
          (if (and (not au3-mode--restrict-recursion)
                   (member (au3-mode--peek-bol-keyword start)
                           '("While" "Func")))
              au3-mode-+exp-inst-sep+
            au3-mode-+newline+)
        (skip-chars-forward " \t")))))

(defun au3-mode-next-newline ()
  (let ((result (au3-mode-next-newline-1))
        last-forward)
    (when result
      (let ((au3-mode--restrict-recursion t))
        (while (au3-mode-next-newline-1)
          (setq last-forward (skip-chars-forward " \t"))))
      (when last-forward
        (backward-char last-forward)))
    result))

(defun au3-mode--run-token-matcher (matcher str &optional start stop post-validation)
  "Test MATCHER on STR.

Optional parameters:
- start (default \"|\") where to put point in `str' before running `matcher'
- stop (default \"@\") where to expect point in `str' after running `matcher'
- post-validation (default (lambda (start-pos token end-pos) token) extra
  validation function"
  (let ((start (or start "|"))
        (stop (or stop "@"))
        (post-validation (or post-validation
                             (lambda (start-pos token end-pos) token)))
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
          (funcall post-validation start-pos result end-pos)
        (error "Landed on %d not at %d" (point) end-pos)))))

(defun au3-mode--collect-tokens (fun &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let ((last-pos (point))
          result token last-token)
      (catch 'done
        (while t
          (setq token (funcall fun))
          (if (or (null token)
                  (equal last-pos (point)))
              (throw 'done nil)
            (message "%s %s %s %s %s" fun buffer last-pos token (point))
            (setq last-pos (point)
                  last-token token)
            (push token result))))
      (nreverse result))))

(ert-deftest au3-mode-test-next-keyword ()
  "Test `au3-mode-next-keyword'"
  :tags '(token)
  (let ((case-fold-search nil)
        (fut 'au3-mode-next-keyword)) ; fut=function-under-test
    (should (equal (au3-mode--run-token-matcher fut "|if@") "If"))
    (should (equal (au3-mode--run-token-matcher fut "|iF@") "If"))
    (should (equal (au3-mode--run-token-matcher fut "|IF@") "If"))
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
    (au3-mode--run-token-matcher fut "\tLocal $a = 2| @; local definition\n\tConst $B = 23")))

(ert-deftest au3-mode-test-next-newline ()
  "Test `au3-mode-next-newline'"
  :tags '(token)
  (let ((fut 'au3-mode-next-newline)
        (exp-result au3-mode-+newline+))
    (should-not (au3-mode--run-token-matcher fut "|@"))
    (should (equal (au3-mode--run-token-matcher fut "|\n   @")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher fut "Local $a = 2|\n@Local $b=3")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher fut "Local $a = 2|\n\n\t@Local $b=4")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher fut "$a = 2|\n  @Local $b=5")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher fut "$a = 2\n | #cs\n\t#ce\n@$b=6")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher
                    fut
                    "|#cs\n11\n\t#ce\n \n  ;#cs\n \t @$b=7")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher
                    fut
                    "$a = 2\n  #cs\n11\n\t#ce\n  |;#cs\n \t @$b=8")
                   exp-result))
    (should (equal (au3-mode--run-token-matcher fut "$a = 2|\n  ; c1\n\t; c2\n;c3\n\t\t@$b=9\n")
                   exp-result))
    (should-not (au3-mode--run-token-matcher fut "$a = |@2\n"))
    (should-not (au3-mode--run-token-matcher fut "$a = |@_\n2\n"))
    (should (equal (au3-mode--run-token-matcher fut "$a = 2 |; comment\n@$b=3")
                   exp-result))))

(defun au3-mode--peek-token (skip movement)
  (let (next-token last-pos)
    (save-excursion
      (funcall skip)
      (setq next-token (funcall movement))
      (setq last-pos (point)))
    (cons next-token last-pos)))

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

(defun au3-mode--cache-make-entry (token start end)
  (cons end (cons start token)))

(defun au3-mode--cache-compare-beg (a b)
  "AVL tree comparison function (used in forward direction)

The AVL tree must map ranges to tokens and is used to find back
tokens based on the point being just before or inside the token.

This function has two uses:
1. For the AVL library to order things so it can build the tree:
   2 ranges must compare `<' if their beginning offsets are `<'.
2. For autoit-mode to find a token by giving a point just before
   or inside the token (e.g. for range (2, 4) and 1<=point<=7):
   1234567: point
   ffftttt: (au3-mode--cache-compare-beg (range|start=2 end=4) point)
   tffffff: (au3-mode--cache-compare-beg point (range|start=2 end=4))
   See how the overlap of `f' (nil) for 2 and 3 means that the
   AVL tree will return the token that starts after point=2 and
   ends just before point=4 when queried with point=2 or
   point=3."
  (cond ((and (consp a) (consp b))
         (< (au3-mode--cache-entry-beg a) (au3-mode--cache-entry-beg b)))
        ((and (consp a) (numberp b))
         (<= (au3-mode--cache-entry-end a) b))
        ((and (numberp a) (consp b))
         (< a (au3-mode--cache-entry-beg b)))
        (t (error "Not reached: a=%s b=%s" a b))))

(ert-deftest test-au3-mode--cache-compare-beg ()
  "Test `au3-mode--cache-compare-beg'"
  :tags '(util)
  (let ((long-entry (au3-mode--cache-make-entry "make" 11 15))
        ;; relative position of entries matters for tests
        (short-entry (au3-mode--cache-make-entry "(" 21 22)))
    (dolist (entry (list long-entry short-entry))
      ;; point in front of token (or inside token) compares equal to the token's
      ;; range (i.e. both < comparisons are nil)
      (dotimes (idx (- (au3-mode--cache-entry-end entry)
                       (au3-mode--cache-entry-beg entry)))
        (should-not (au3-mode--cache-compare-beg entry (+ idx (au3-mode--cache-entry-beg entry))))
        (should-not (au3-mode--cache-compare-beg (+ idx (au3-mode--cache-entry-beg entry)) entry)))
      ;; point just after token compares as bigger than token's range
      (should (au3-mode--cache-compare-beg entry (au3-mode--cache-entry-end entry)))
      (should-not (au3-mode--cache-compare-beg (au3-mode--cache-entry-end entry) entry)))
    ;; comparing ranges
    (should (au3-mode--cache-compare-beg long-entry short-entry))
    (should-not (au3-mode--cache-compare-beg short-entry long-entry))))

(defun au3-mode--cache-compare-end (a b)
  "AVL tree comparison function (used in backward direction)

The AVL tree must map ranges to tokens and is used to find back
tokens based on the point being just after or inside the token.

This function has two uses:
1. For the AVL library to order things so it can build the tree:
   2 ranges must compare `<' if their end offsets are `<'.
2. For autoit-mode to find a token by giving a point just after
   or inside the token (e.g. for range (2, 4) and 1<=point<=7):
   1234567: point
   ffffttt: (au3-mode--cache-compare-end (range|start=2 end=4) point)
   ttfffff: (au3-mode--cache-compare-end point (range|start=2 end=4))
   See how the overlap of `f' (nil) for 3 and 4 means that the
   AVL tree will return the token that starts after point=2 and
   ends just before point=4 when queried with point=3 or
   point=4."
  (cond ((and (consp a) (consp b))
         (< (au3-mode--cache-entry-end a) (au3-mode--cache-entry-end b)))
        ((and (consp a) (numberp b))
         (< (au3-mode--cache-entry-end a) b))
        ((and (numberp a) (consp b))
         (<= a (au3-mode--cache-entry-beg b)))
        (t (error "Not reached: a=%s b=%s" a b))))

(ert-deftest test-au3-mode--cache-compare-end ()
  "Test `au3-mode--cache-compare-end'"
  :tags '(util)
  (let ((long-entry (au3-mode--cache-make-entry "make" 11 15))
        ;; relative position of entries matters for tests
        (short-entry (au3-mode--cache-make-entry "(" 21 22)))
    (dolist (entry (list long-entry short-entry))
      ;; point after token (or inside token) compares equal to the token's
      ;; range (i.e. both < comparisons are nil)
      (dotimes (idx (- (au3-mode--cache-entry-end entry)
                       (au3-mode--cache-entry-beg entry)))
        (should-not (au3-mode--cache-compare-end entry (+ 1 idx (au3-mode--cache-entry-beg entry))))
        (should-not (au3-mode--cache-compare-end (+ 1 idx (au3-mode--cache-entry-beg entry)) entry)))
      ;; point just before token compares as smaller than token's range
      (should-not (au3-mode--cache-compare-end entry (au3-mode--cache-entry-beg entry)))
      (should (au3-mode--cache-compare-end (au3-mode--cache-entry-beg entry) entry)))
    ;; comparing ranges
    (should (au3-mode--cache-compare-end long-entry short-entry))
    (should-not (au3-mode--cache-compare-end short-entry long-entry))))

(defun au3-mode--cache-insert-token (token start end)
  (let ((new-entry (au3-mode--cache-make-entry token start end)))
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

(defun au3-mode--skip-backward-over-complete-newline-token ()
  "Moves point back to beginning of line that isn't inside a
au3-mode-+newline+ token. Assumes it is called from outside a
multi-line comment."
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
    ;;   -> normal (if empty, or line-comment, or #cs/#comments-start [ignore
    ;;      nesting error, increases number of places in middle of
    ;;      au3-mode-+newline+ token where token is skipped correctly])
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
          (cond ((looking-at au3-mode--+multi-line-comment-end-regexp+)
                 (setq state 'block-comment
                       comment-nesting-depth 1))
                ((or (looking-at "^[ \t]*_?[ \t]*\\(;.*\\|\\)$")
                     (looking-at au3-mode--+multi-line-comment-start-regexp+))
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
        (forward-line -1)))))

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
    (au3-mode--cache-insert-token "BCD" 3 6)
    (should (equal (au3-mode--cache-get-token-from-end 6) "BCD"))
    (should (equal (point) 3))
    (should (equal (au3-mode--cache-get-token-from-end 2) "A"))
    (should (equal (point) 1))
    ;; Should work from the middle of a token, too
    (should (equal (au3-mode--cache-get-token-from-end 4) "BCD"))
    (should (equal (point) 3))
    (should (equal (au3-mode--cache-get-token-from-beg 4) "BCD"))
    (should (equal (point) 6))))

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
          str)))))

(ert-deftest au3-mode-test-next-string ()
  "Test `au3-mode-next-string'"
  :tags '(token)
  (let ((fut 'au3-mode-next-string))
    (should (equal (au3-mode--run-token-matcher fut "|\"A\"@")
                   "A"))
    (should (equal (au3-mode--run-token-matcher fut "|'A'@  ")
                   "A"))
    (should (equal (au3-mode--run-token-matcher fut "|\"\"@ \n")
                   ""))
    (should (equal (au3-mode--run-token-matcher fut "|''@\t  ")
                   ""))
    ;; example from documentation
    ;; https://www.autoitscript.com/autoit3/docs/intro/lang_datatypes.htm
    (should (equal (au3-mode--run-token-matcher
                    fut
                    "|\"here is a \"\"double-quote\"\" - ok?\"@")
                   "here is a \"double-quote\" - ok?"))
    (should
     (equal
      (au3-mode--run-token-matcher
       fut
       "|'This \"sentence\" contains \"lots\" of \"double-quotes\" does it not?'@")
      "This \"sentence\" contains \"lots\" of \"double-quotes\" does it not?"))
    (should
     (equal
      (au3-mode--run-token-matcher
       fut
       "|\"This \"\"sentence\"\" contains \"\"lots\"\" of \"\"double-quotes\"\" does it not?\"@")
      "This \"sentence\" contains \"lots\" of \"double-quotes\" does it not?"))
    ;; unhappy cases
    (dolist (not-a-string-token
             '("'1\"" "\"1'" "'" "\"" "\"1" "'1" "1" "'1\n'" "\"1\n\""))
      (should-not (au3-mode--run-token-matcher
                   fut
                   (concat "|@" not-a-string-token))))))

(require 'smie)

;; Minimal language knowing only
;;
;; - Assignment
;;
;; - Function definition (don't care that they can be nested while AutoIt
;;   normally doesn't support it
;;
;; - Expression as a statement (presumably a function call)
(defvar sample-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    `((id)
      (inst
       ;; (nonassoc "+=" "-=" "*=" "/=" "&=") ; no "^=" according to docs
       (exp)
       ("Func" exp ,au3-mode-+exp-inst-sep+ inst-list "EndFunc")
       )
      (inst-list (inst-list ,au3-mode-+newline+ inst-list) (inst))
      (exp (exp "=" exp)
           (exp "<" exp)
           (exp ">" exp)
           (exp "=" exp)
           (exp "<=" exp)
           (exp ">=" exp)
           (exp "==" exp)
           (exp "<>" exp)
           (exp "+" exp)
           (exp "-" exp)
           (exp "*" exp)
           (exp "/" exp)
           (exp "^" exp)
           (exp "Or" exp)
           (exp "And" exp)
           ;; ("Not" exp)
           ;; ("?" exp ":" exp)
           ("(" exps ")")
           )
      (exps (exps "," exps) (exp))
      )
    `((assoc ,au3-mode-+newline+ ,au3-mode-+exp-inst-sep+))
    '((assoc ",")
      (assoc "Or")
      (assoc "And")
      ;; (nonassoc ":")
      ;; (nonassoc "?")
      (assoc "=" "<" ">" "<=" ">=" "<>" "==")
      (assoc "&")
      (assoc "+" "-")
      (assoc "*" "/")
      (assoc "^")
      ;; (nonassoc "Not")
      ))))

(defun au3-mode--test-xxxward-sexp-jump (txt fun from to)
  (with-current-buffer (get-buffer-create "*yy*") ;with-temp-buffer
    (let ((comment-use-syntax t))
      (delete-region (point-min) (point-max))
      (insert txt)
      (au3-mode--smie-setup)
      (goto-char (point-min))
      (search-forward from)
      (when (eql fun 'smie-backward-sexp-command)
        (goto-char (match-beginning 0)))
      (call-interactively fun)
      (when (eql fun 'smie-backward-sexp-command)
        (if (string-suffix-p "\n" to)
            (forward-line -1)
          (goto-char (line-beginning-position)))
        (skip-chars-forward " \t"))
      (looking-at to))))

(ert-deftest test-au3-mode--jumps ()
  "When changing the tokens or the grammar it's hard to keep
  track if the grammar still matches the language.  This tests
  the validity of the grammar by using
  `smie-backward-sexp-command' and `smie-forward-sexp-command'."
  (let ((txt "
;<3
While $a
  ;<2
  If 1 Then
;<1
While 2
    f()
WEnd;>1
    unique_func_name(1, 2, (3 - 4), 5 + 6 * 3);unique
  EndIf;>2
WEnd;>3
If $a < (2 + 4) Then MsgBox(1, 2, 3);>!3
;<4
Func x($z)
  ;<5
  If _
     dsfg(_
       1 + _
       2, g())_ ; some comment
     Then _     ; comment again
       h();>5
  While ($z > 123)
    Return 1
  WEnd
EndFunc;>4"))
    (pcase-dolist (`(,from ,to . ,only)
                   '((";<1\n" ";>1")
                     (";<2\n" ";>2")
                     (";<3\n" ";>3")
                     (";>3\n" ";>!3" forward)
                     (";<4\n" ";>4")
                     (";<5\n" ";>5" forward)
                     (";<5\n" "While ($z > 123)" backward)
                     ("dsfg" "_ ; some comment")
                     ("unique_func_name" ";unique")
                     (";<5\n" "While ($z > 123)" backward)
                     ))
      (when (or (null only) (member 'backward only))
        (should (au3-mode--test-xxxward-sexp-jump
                 txt
                 'smie-backward-sexp-command
                 to
                 from)))
      (when (or (null only) (member 'forward only))
        (should (au3-mode--test-xxxward-sexp-jump
                 txt
                 'smie-forward-sexp-command
                 from
                 to)))
      )))

;; -- From SMIE documentation ---------------------------------------------
;; Another important concept is the notion of parent: The parent of a token,
;; is the head token of the nearest enclosing syntactic construct. For
;; example, the parent of an else is the if to which it belongs, and the
;; parent of an if, in turn, is the lead token of the surrounding
;; construct. The command backward-sexp jumps from a token to its parent, but
;; there are some caveats: for openers (tokens which start a construct, like
;; if), you need to start with point before the token, while for others you
;; need to start with point after the token. backward-sexp stops with point
;; before the parent token if that is the opener of the token of interest, and
;; otherwise it stops with point after the parent token.
;;
;; SMIE indentation rules are specified using a function that takes two
;; arguments method and arg where the meaning of arg and the expected return
;; value depend on method.
;;
;; method can be:
;; - :after, in which case arg is a token and the function should return the
;;   offset to use for indentation after arg.
;; - :before, in which case arg is a token and the function should return the
;;   offset to use to indent arg itself.
;; - :elem, in which case the function should return either the offset to use
;;   to indent function arguments (if arg is the symbol arg) or the basic
;;   indentation step (if arg is the symbol basic).
;; - :list-intro, in which case arg is a token and the function should return
;;   non-nil if the token is followed by a list of expressions (not separated
;;   by any token) rather than an expression.
;;
;; When arg is a token, the function is called with point just before that
;; token. A return value of nil always means to fallback on the default
;; behavior, so the function should return nil for arguments it does not
;; expect.
;; offset can be:
;; - nil: use the default indentation rule.
;; - (column . column): indent to column column.
;; - number: offset by number, relative to a base token which is the current
;;   token for :after and its parent for :before.

(defvar au3-mode-indent-basic 4)

(defvar au3-smie-rule-parent-while-if nil)

(defvar au3-smie-rule-sibling-while-if nil)

(defun au3-mode--smie-hanging-p (token)
  (and (not (smie-rule-bolp))
       (save-excursion
         (forward-char (length token))
         (looking-at "[ \t]*_?\\(;.*\\)?$"))))

(defun au3-mode-indent-comment ()
  "A function for `smie-indent-functions' (which see)."
  ;; copied/modifed from octave-indent-comment
  (save-excursion
    (back-to-indentation)
    (cond
     ;; TODO: handle multiline comments
     ;; ((octave-in-string-or-comment-p) nil)
     ;; TODO: what's this regex?
     ;; ((looking-at-p "\\(\\s<\\)\\1\\{2,\\}")
     ;;  0)
     ((eql (char-after) au3-mode-+comment+)
      (if (eql (char-after (1+ (point))) au3-mode-+comment+)
          (if (and (eql (char-after (+ 2 (point))) au3-mode-+comment+)
                   (save-excursion (skip-chars-forward (string au3-mode-+comment+))
                                   (not (= (point) (line-end-position)))))
              ;; triple or more comment markers `;;;' are treated as section
              ;; headings if they aren't followed by newline, otherwise, they
              ;; are treated as a comment box
              0
            ;; Double comment marker `;;' get indented like surrounding code ->
            ;; let another SMIE function handle it
            nil)
        ;; single comment marker `;'
        (comment-choose-indent))))))

(defun au3-mode--smie-rule (kind token)
  (ignore-errors (setq au3-smie-rule-sibling-while-if (smie-rule-sibling-p)))
  (ignore-errors (setq au3-smie-rule-parent-while-if (smie-rule-parent-p "+" "$hello" ";number;" "=" "While" "If" "Func" "Then" au3-mode-+newline+ au3-mode-+exp-inst-sep+)))
  (cond ((and (eql kind :elem)
              (or (eql token 'basic)
                  (eql token 'args)))
         au3-mode-indent-basic)
        ;; ((and (eql kind :list-intro)
        ;;       (or (equal token "If")
        ;;           (equal token "While")))
        ;;  t)
        ;; ((and (eql kind :after)
        ;;       (equal token au3-mode-+newline+))
        ;;  (if (smie-rule-parent-p "While" "If" "Func" "Then")
        ;;      (smie-rule-parent au3-mode-indent-basic)
        ;;    (if (smie-rule-parent-p au3-mode-+newline+)
        ;;        (smie-rule-parent)
        ;;      (smie-rule-separator kind))))
        ;; ((and (eql kind :after)
        ;;       (equal token "Then"))
        ;;  nil ;au3-mode-indent-basic
        ;;  )
        ;; ((and (eql kind :list-intro)
        ;;       (or (and (null token) (bobp))
        ;;           (equal token au3-mode-+newline+)
        ;;           ;; It indents like this
        ;;           ;; Local $a = "A" & @CRLF & _
        ;;           ;; "bc"
        ;;           (equal token "Then")
        ;;           ;; NOT (equal token "If")
        ;;           ;; It indents like this
        ;;           ;; If    f() Then
        ;;           ;;       f() ; if (rule :elem 'basic) returns 0
        ;;           ;; EndIf
        ;;           ))
        ;;     t)
        ((or (equal token au3-mode-+newline+)
             (equal token ","))
         (if (save-excursion
               (goto-char (line-beginning-position))
               (looking-at-p "[ \t]*$"))
             3
           (smie-rule-separator kind)))
        ((and (eql kind :before)
              (not (smie-rule-sibling-p)))
         (save-excursion
           (when (au3-mode-simplest-backward-token)
             (cons 'column (current-column)))))
        ((and (eql kind :after)
              (equal token "(")
              (looking-at-p "([ \t]*_[ \t]*\\(;.*\\|\\)$"))
         ;; Hanging open paren
         (smie-rule-parent au3-mode-indent-basic))))

(defmacro au3-mode--should-indent (before after)
  `(let ((au3-mode-indent-basic 4))
     (with-current-buffer (get-buffer-create "*zz*")
       (setq indent-tabs-mode nil
             comment-use-syntax t
             comment-column 5)
       (delete-region (point-min) (point-max))
       (au3-mode--smie-setup)
       (insert ,before)
       (indent-region (point-min) (point-max))
       (should (string-equal (buffer-substring (point-min) (point-max))
                             ,after)))))

(ert-deftest test-au3-mode-indent-statement-list ()
  "Test indentation of list of statements"
  :tags '(indent)
  (au3-mode--should-indent "f()\ngg()\nh()" "f()\ngg()\nh()")
  (au3-mode--should-indent "fff()\n\t; g()\n\thh()\n; i()\nj()"
                           "fff()\n; g()\nhh()\n; i()\nj()")
  (au3-mode--should-indent "; comment\n\tf()\n\t; g()\n\th()"
                           "; comment\nf()\n; g()\nh()")
  (au3-mode--should-indent "$a = 3\n\n$b=4\n\n;; c\n\n$c=5"
                           "$a = 3\n\n$b=4\n\n;; c\n\n$c=5")
  (au3-mode--should-indent "$a = 3\n\n $b=4\n\n  ; c\n\t$c=5"
                           "$a = 3\n\n$b=4\n\n; c\n$c=5"))

(ert-deftest test-au3-mode-indent1 ()
  ""
  :tags '(indent)
  (au3-mode--should-indent
   "\n$a = 1 + 2 * _\n3\n$b = 4 + _ ; er\n5\n"
   "\n$a = 1 + 2 * _\n         3\n\n$b = 4 + _ ; er\n     5\n"))

(ert-deftest test-au3-mode-indent2 ()
  ""
  :tags '(indent)
  (au3-mode--should-indent "
;<1
While $a
If 1 Then
f(1, g(), _
2)
EndIf;>2
WEnd;>1
" "
;<1
While $a
    If 1 Then
        f(1, g(), _
          2)
    EndIf;>2
WEnd;>1
")
  (au3-mode--should-indent "Func abcd($x, $y)
; this is a test
Return 123
EndFunc
Func efghijklmn($z, _
$a = 2)
If $a > $z Then
While $z <> 0
abcd($a, $x)
WEnd
EndiF
EndFunc"
                           "Func abcd($x, $y)
    ; this is a test
    Return 123
EndFunc
Func efghijklmn($z, _
                $a = 2)
    If $a > $z Then
        While $z <> 0
            abcd($a, $x)
        WEnd
    EndiF
EndFunc")
  (au3-mode--should-indent "if $long_var_name > long_func_name(_ ; comment
8, _
9) Then test()
if $a > long_func_name(_ ; comment
8, _
    9) Then _
          test()
          g()
        if $a > long_func_name(_ ; comment
8, _
9) Then
test()
f()
g()
EndIf"
                           "if $long_var_name > long_func_name(_ ; comment
        8, _
        9) Then test()
if $a > long_func_name(_ ; comment
        8, _
        9) Then _
    test()
g()
if $a > long_func_name(_ ; comment
        8, _
        9) Then
    test()
    f()
    g()
EndIf"))
;; (defun au3-mode--smie-rule (method arg)
;;   (pcase (cons method arg)
;;     (`(:before . "Func")
;;      `(column . 0))
;;     (`(:elem . basic) 4)
;;     ;; (`(:after . "case") (or sh-indentation smie-indent-basic))
;;     (`(_ . (or "Else" ";" ,au3-mode-+newline+))
;;      (smie-rule-separator method))
;;     (`(:before . "{")
;;      (save-excursion
;;        (when (sh-smie--rc-after-special-arg-p)
;;          `(column . ,(current-column)))))
;;     (`(:before . ,(or `"(" `"{" `"["))
;;      (if (smie-rule-hanging-p) (smie-rule-parent)))
;;     ;; FIXME: SMIE parses "if (exp) cmd" as "(if ((exp) cmd))" so "cmd" is
;;     ;; treated as an arg to (exp) by default, which indents it all wrong.
;;     ;; To handle it right, we should extend smie-indent-exps so that the
;;     ;; preceding keyword can give special rules.  Currently the only special
;;     ;; rule we have is the :list-intro hack, which we use here to align "cmd"
;;     ;; with "(exp)", which is rarely the right thing to do, but is better
;;     ;; than nothing.
;;     (`(:list-intro . ,(or `"for" `"if" `"while")) t)
;;     ;; sh-indent-after-switch: handled implicitly by the default { rule.
;;     )
;;   ;; (if (and (member method '(:after :before))
;;   ;;       (member arg `(,au3-mode-+newline+ ",")))
;;   ;;  (smie-rule-separator method))
;;   )

(defun au3-mode--smie-setup (&optional rule)
  (set-syntax-table autoit-mode-syntax-table)
  (set (make-variable-buffer-local 'comment-use-syntax) t)
  (set (make-variable-buffer-local 'smie-rule-separator-outdent) nil)
  (smie-setup
   sample-smie-grammar
   (or rule 'au3-mode--smie-rule)
   ;; :forward-token 'au3-mode--smie-forward-token
   ;; :backward-token 'au3-mode--smie-backward-token
   :forward-token 'au3-mode-simplest-forward-token
   :backward-token 'au3-mode-simplest-backward-token)
  ;; (smie-setup sample-smie-grammar
  ;;             'au3-mode--smie-rule
  ;;             ;; :forward-token 'au3-mode--smie-forward-token
  ;;             ;; :backward-token 'au3-mode--smie-backward-token
  ;;             :forward-token 'au3-mode-simplest-forward-token
  ;;             :backward-token 'au3-mode-simplest-backward-token)
  ;; (add-hook (make-variable-buffer-local 'smie-indent-functions)
  ;;           'au3-mode-indent-comment
  ;;           nil
  ;;           t)
  )

(defvar au3-mode--restrict-recursion nil
  "Flag bound to t by some internals: trade recursion depth for less precise answers

Functions like `au3-mode--peek-bol-keyword' call upon higher
level functions which in turn could call
`au3-mode--peek-bol-keyword' again.  This can cause the recursion
to exceed the Lisp stack depth.  When this flag is temporarily
bound to a true value, less precise answers may be given (but not
stored in the cache) because no further recursion will take
place.")

(defun au3-mode--peek-bol-keyword (start)
  "Return keyword at (logical) beginning of line

This function isn't completely implemented: the indentation
relies on differentiating `au3-mode-+newline+' and
`au3-mode-+exp-inst-sep+' by going back to the beginning of the
statement, but the current implementation only goes back to BOL,
doesn't skip back over line continuation characters."
  (if au3-mode--restrict-recursion
      au3-mode-+newline+
    (save-excursion
      (goto-char start)
      (catch 'done
        (while t
          (let* ((lbp (line-beginning-position))
                 bobp)
            (goto-char lbp)
            (setq bobp (bobp))
            (skip-chars-forward " \t")
            (let* ((from (point))
                   (keyword-token (au3-mode-next-keyword))
                   (token (lambda ()
                            (or keyword-token
                                (progn
                                  (goto-char from)
                                  (skip-chars-forward "^; \t\n")
                                  (buffer-substring-no-properties from (point)))))))
              (if (or bobp
                      (member keyword-token au3-mode-+statement-start-keyword-list+))
                  (throw 'done (funcall token))
                (goto-char lbp)
                (let* ((au3-mode--restrict-recursion t)
                       (back-token (au3-mode-simplest-backward-token t)))
                  (unless (or bobp (< (point) lbp))
                    (error "This can't happen: back-token=%s lbp=%d point=%d" back-token lbp (point)))
                  (when (member back-token (list au3-mode-+newline+
                                                 au3-mode-+exp-inst-sep+
                                                 "Then"))
                    (throw 'done (funcall token))))))))))))

(ert-deftest test-au3-mode--peek-bol-keyword ()
  "Test `au3-mode--peek-bol-keyword'"
  :tags '(token)
  (pcase-dolist (`(,str ,exp-token)
                 '(;; trivial cases
                   ("Local $a=1\nIf $a Then|\nf()\nEndIf" "If")
                   ("If $a Then|\nf()\nEndIf" "If") ; at beginning of buffer
                   ;; ... beginning of buffer.  When not one of the keywords,
                   ;;     exact token is not realy important
                   ("f()|\ng()\n" "f()")
                   ;; Normalizing case
                   ("while $a|\nf()\nWEnd" "While")
                   ;; Harder cases (spread over several lines)
                   ("Local $a = _\n1\nIf (_\n$a > _\n$b)_\nThen _\nf()|\ng()\n" "If")
                   ;; ... contain a Then token.  Return value is an
                   ;;     example of malformed token returned by
                   ;;     `au3-mode--peek-bol-keyword', but it doesn't matter
                   ;;     as long as it can't be mistaken with a keyword
                   ("If $a = _\n1 _\nThen\nf((_\n$a > _\n$b)_\n, _\nf() _|\ng())\n" "f((_")
                   ("While $a = _\n1 _\n+ \"Then\"\nf((_\n$a > _\n$b)_\n, _\nf() _|\ng())\n" "f((_")
                   ;; initial use cases for `au3-mode--peek-bol-keyword'
                   ("fgh()\nIf $a = 1 Then\nFor $b = _\n1 _\n\tto\t_\n\t3 _\nStep 4|\n\t\tf()\nNext"
                    "For")
                   ("fgh()\nIf $a = 1 Then|\nFor $b = _\n1 _\n\tto\t_\n\t3 _\nStep 4\t\tf()\nNext"
                    "If")
                   ("If $a _\n = 2 _\nThen _\nIf $b _\n=_\n3 _\nThen _|\nf()" "If")
                   ("f()\nWhile f(_\n)=_\ng(_\n)|\nIf $a Then\nh()\nEndIf" "While")
                   ("f()\nWhile f(_\n)=_\ng(_\n)\nIf $a Then|\nh()\nEndIf" "If")
                   ("If f() Then g()\n\tfunc a(_\n$b,_\n) ; func def|\n\th()\nEndFunc" "Func")
                   ))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (search-forward "|")
      (delete-char -1 nil)
      (should (equal exp-token
                     (au3-mode--peek-bol-keyword (point)))))))

(defun au3-mode--simplest-forward-token-internal ()
  "Advance point and return token after point

Assumes it is called at a token boundary."
  (let ((start (point))
        (after (char-after)))
    (cond ((eobp) nil)
          ((au3-mode-next-newline))
          ((looking-at au3-mode-+number-regexp+)
           (goto-char (match-end 0))
           au3-mode-+number+)
          ((looking-at au3-mode-+operator-regexp+)
           (goto-char (match-end 0))
           (match-string-no-properties 0))
          ((member after '(?' ?\"))
           (au3-mode-next-string))
          ((or (<= ?a after ?z)
               (<= ?A after ?Z)
               (member after '(?_ ?@ ?$)))
           (forward-char)
           (skip-chars-forward "a-zA-Z0-9_")
           (let* ((tok (buffer-substring-no-properties start (point)))
                  (norm-tok (au3-mode--normalize-keyword tok)))
             (cond ((equal norm-tok "If")
                    (let ((next&pos (au3-mode--peek-token
                                     'au3-mode-skip-to-next-token
                                     'au3-mode--simplest-forward-token-internal)))
                      (if (and next&pos (equal (car next&pos) "("))
                          (concat norm-tok ";1;")
                        norm-tok)))
                   ((equal norm-tok "Then")
                    (let ((next&pos (au3-mode--peek-token
                                     'au3-mode-skip-to-next-token
                                     'au3-mode--simplest-forward-token-internal)))
                      (if (and next&pos (equal (car next&pos) au3-mode-+newline+))
                          norm-tok
                        (concat norm-tok ";1;"))))
                   ((not norm-tok)      ; i.e. not a keyword
                    tok)
                   (t norm-tok))))
          (t (forward-char)
             (buffer-substring-no-properties start (point))))))

(defun au3-mode-simplest-forward-token (&optional internal-recurse no-caching)
  (let ((orig-start (point)))
    (au3-mode-skip-to-next-token)
    (or (and (au3-mode--cache-valid-p)
             ;; sets point as side effect:
             (au3-mode--cache-get-token-from-beg (point)))
        (progn
          (if internal-recurse
              ;; this function may back up to a known token starting point,
              ;; then move forward only with au3-mode-skip-to-next-token and
              ;; au3-mode--simplest-forward-token-internal, thus making sure
              ;; we're never inside a token.
              (let* ((start (point))
                     (token (au3-mode--simplest-forward-token-internal))
                     (stop (point)))
                (when (and token (not no-caching))
                  (au3-mode--cache-insert-token token start stop))
                token)
            ;; Always assume we're called from the middle of a token: if point
            ;; is in comment, we must get out first.  Doing some extra parsing
            ;; isn't too bad, since we cache results.
            (au3-mode--skip-backward-over-complete-newline-token)
            (catch 'done
              (while t
                (let ((token (au3-mode-simplest-forward-token t no-caching)))
                  (cond ((> (point) orig-start)
                         (throw 'done token))
                        ((eobp)
                         (throw 'done nil)))))))))))

(ert-deftest test-au3-mode-simplest-forward-token-caching ()
  :tags '(rewind)
  (let ((forward-fun 'au3-mode-simplest-forward-token))
    (with-temp-buffer
      (insert "Func abcd($efg, Const $_1234, _\n\t$z)\n")
      (insert "  ; comment 1\n  ; comment 2\n#comments-start\n")
      (insert "    long comment\n#comments-end\n\n")
      (insert "  Return $efg - $h1234\n")
      (insert "EndFunc")
      (goto-char (point-min))
      (should (null au3-mode--token-cache))
      (dolist (exp-token
               `("Func" "abcd" "(" "$efg" "," "Const" "$_1234" "," "$z" ")"
                 ,au3-mode-+exp-inst-sep+
                 "Return" "$efg" "-" "$h1234"
                 ,au3-mode-+newline+
                 "EndFunc"))
        (let* ((token (funcall forward-fun))
               (stop (point)))
          (should-not (null au3-mode--token-cache))
          (should (equal token exp-token))
          (dotimes (x (if (member token (list au3-mode-+newline+
                                              au3-mode-+exp-inst-sep+))
                          1
                        (length token)))
            (should (equal (au3-mode--cache-get-token-from-beg
                            (- stop x
                               ;; subtract 1 more to cover point just in front
                               ;; of token and all positions inside, but not
                               ;; just after token
                               1
                               ))
                           token))
            (should (equal (point) stop))))))))


(ert-deftest test-au3-mode-simplest-backward-token-caching ()
  "`au3-mode-simplest-backward-token' impicitly caches tokens by
using `au3-mode-simplest-forward-token'."
  :tags '(rewind)
  (let ((backward-fun 'au3-mode-simplest-backward-token))
    (with-temp-buffer
      (insert "Func abcd($efg, Const $_1234, _\n\t$z)\n")
      (insert "  ; comment 1\n  ; comment 2\n#comments-start\n")
      (insert "    long comment\n#comments-end\n\n")
      (insert "  Return $efg - $h1234\n")
      (insert "EndFunc")
      (goto-char (point-min))
      (should (null au3-mode--token-cache))
      (should (equal (au3-mode-simplest-forward-token) "Func"))
      (should (equal (point) 5))
      (goto-char (point-max))
      (dolist (exp-token
               (reverse `("Func" "abcd" "(" "$efg" "," "Const" "$_1234" "," "$z" ")"
                  ,au3-mode-+exp-inst-sep+
                  ;; ,au3-mode-+newline+
                  ;; ,au3-mode-+newline+
                  ;; ,au3-mode-+newline+
                  ;; ,au3-mode-+newline+
                  "Return" "$efg" "-" "$h1234"
                  ,au3-mode-+newline+
                  "EndFunc"
                  )))
        (let* ((token (funcall backward-fun))
               (stop (point)))
          ;; (should-not (null au3-mode--token-cache))
          (should (point))
          (should (equal token exp-token))))
      (should (equal (point) (point-min))))))

(ert-deftest test-au3-mode-simplest-forward-token-from-middle ()
  "Start `au3-mode-simplest-forward-token' from middle of token"
  :tags '(token)
  (let ((fun 'au3-mode-simplest-forward-token))
    (should (equal (au3-mode--run-token-matcher fun "F|unc@") "Func"))
    (should (equal (au3-mode--run-token-matcher fun "Fun|c@") "Func"))
    (should (equal (au3-mode--run-token-matcher fun "If _
a|bcd@ Then") "abcd"))
    (should (equal (au3-mode--run-token-matcher fun "Func a() ; comm|ent
; comment
\t ; more comments
#cs
  Hello
#ce
 @EndFunc") au3-mode-+exp-inst-sep+))
    (should (equal (au3-mode--run-token-matcher fun "Func a() ; comment
; comment
\t ; more comments
#c|s
  Hello
#ce
 @EndFunc") au3-mode-+exp-inst-sep+))
    ;; Disabled: au3-mode--skip-backward-over-complete-newline-token assumes
    ;; it is called outside of a multiline comment.
    ;; (should (equal (au3-mode--run-token-matcher fun "Func a() ; comment
    ;; ; comment
    ;; \t ; more comments
    ;; #cs
    ;;   Hel|lo
    ;; #ce
    ;;  @EndFunc") au3-mode-+newline+))
    (should (equal (au3-mode--run-token-matcher fun "Func a() ; comment
; comment
\t| ; more comments
 #cs
  Hello
#ce
 @EndFunc") au3-mode-+exp-inst-sep+))
    (should (equal (au3-mode--run-token-matcher fun "2+|3E2@") au3-mode-+number+))
    (should (equal (au3-mode--run-token-matcher fun "2+3|E2@") au3-mode-+number+))
    (should (equal (au3-mode--run-token-matcher fun "If\n2+a|bc@ Then") "abc"))
    (let ((post-val-fun
           (lambda (start token stop)
             (let ((entry (avl-tree-member (au3-mode--cache-beginning) start)))
               (when entry
                 ;; caching isn't mandatory, but if `token' was cached, check
                 ;; that the data is correct
                 (should (equal (au3-mode--cache-entry-token entry) token))
                 ;; abuse of dynamic binding!
                 (should (equal (au3-mode--cache-entry-beg entry) exp-beg))
                 (should (equal (au3-mode--cache-entry-end entry) exp-end))))
             token)))
      (let ((exp-beg 2)
            (exp-end 18))
        (should (equal (au3-mode--run-token-matcher fun "\t; c\n#c|s\n#ce\n\t; d@" nil nil post-val-fun)
                       au3-mode-+newline+))
        (should (equal (au3-mode--run-token-matcher fun "\t; z\n#cs\n#ce|\n\t; d@" nil nil post-val-fun)
                       au3-mode-+newline+))
        (should (equal (au3-mode--run-token-matcher fun "\t; x\n|#cs\n#ce\n\t; d@" nil nil post-val-fun)
                       au3-mode-+newline+))
        (should (equal (au3-mode--run-token-matcher fun "\t; d\n#cs\n#ce\n\t;| d@" nil nil post-val-fun)
                       au3-mode-+newline+))))))

(ert-deftest test-au3-mode-simplest-forward-token ()
  :tags '(rewind)
  (let ((forward-fun 'au3-mode-simplest-forward-token)
        (backward-fun 'au3-mode-simplest-backward-token))
    (dolist
        (str_exp
         `(("Func\nEndFunc" ("Func" ,au3-mode-+exp-inst-sep+ "EndFunc"))
           ;; keywords are normalized
           ("if $a thEN" ("If" "$a" "Then;1;"))
           ("\t; c\n\t; d\nWinWait(;\n; handle\n$h\t;t\n)"
            (,au3-mode-+newline+
             "WinWait" "("
             ,au3-mode-+newline+
             "$h"
             ,au3-mode-+newline+ ")"))
           ("Func _ \n\ta($x)\nReturn $x + 22\nEndFunc"
            ("Func" "a" "(" "$x" ")" ,au3-mode-+exp-inst-sep+
             "Return" "$x" "+" ,au3-mode-+number+ ,au3-mode-+newline+
             "EndFunc"))
           ("While $a\nf()\nWEnd"
            ("While" "$a" ,au3-mode-+exp-inst-sep+
             "f" "(" ")" ,au3-mode-+newline+
             "WEnd"))))
      (with-temp-buffer
        (insert (car str_exp))
        (goto-char (point-min))
        (should (equal (au3-mode--collect-tokens forward-fun)
                       (cadr str_exp)))
        (should (equal (point) (point-max)))
        (should (equal (reverse (au3-mode--collect-tokens backward-fun))
                       (cadr str_exp)))
        (should (equal (point) (point-min)))))))

(ert-deftest test-au3-mode-simplest-forward-token-special-tokens ()
  "`au3-mode-simplest-forward-token' must handle certain language
constructs specially to make up for the weakness of the operator
precedence grammar (see SMIE documentation)."
  :tags '(token)
  (let ((fun 'au3-mode-simplest-forward-token))
    ;; Single-line-if "Then" is different from multi-line-if "Then"
    (should (equal (au3-mode--run-token-matcher fun "If $a| then@ $b\n$c")
                   "Then;1;"))
    ;; ... from the middle of the token
    (should (equal (au3-mode--run-token-matcher fun "If $a th|en@ $b\n$c")
                   "Then;1;"))
    (should (equal (au3-mode--run-token-matcher fun "If $a| then@\n $b\nEndIf")
                   "Then"))
    ;; ... from the middle of the token
    (should (equal (au3-mode--run-token-matcher fun "If $a t|hen@\n $b\nEndIf")
                   "Then"))
    (should (equal (au3-mode--run-token-matcher fun "If $a then|\n @$b\nEndIf")
                   au3-mode-+newline+))
    (should (equal (au3-mode--run-token-matcher fun "If $a th|en@ ;c\n\t#cs\n#ce\n;d\n $b\nEndIf")
                   "Then"))
    (should (equal (au3-mode--run-token-matcher fun "If $a _\n then ;|c\n\t#cs\n#ce\n;d\n @$b\nEndIf")
                   au3-mode-+newline+))
    ;; The EndXXX type tokens shouldn't contain the preceding
    ;; au3-mode-+newline+ token
    (should (equal (au3-mode--run-token-matcher fun "If $a then\n$b|\n@EndIf")
                   au3-mode-+newline+))
    ;; ... from the middle of the token
    (should (equal (au3-mode--run-token-matcher fun "Func a()\n$b;c|d\n@EndFunc")
                   au3-mode-+newline+))
    (should (equal (au3-mode--run-token-matcher fun "While $a\n$b;cd\nW|End@")
                   "WEnd"))
    (should (equal (au3-mode--run-token-matcher fun "While $a|\n@$b;cd\nWEnd")
                   au3-mode-+exp-inst-sep+))
    (should (equal (au3-mode--run-token-matcher fun "Func x($a)|\n@Return $d\nEndFunc")
                   au3-mode-+exp-inst-sep+))
    (should (equal (au3-mode--run-token-matcher fun "func _\nxyz(_\n$a,_\n$if _)|\n@Return $d\nEndFunc")
                   au3-mode-+exp-inst-sep+))
    ))

(defun au3-mode-simplest-backward-token (&optional no-caching)
  (skip-chars-backward " \t")
  (if (bobp)
      nil
    (or (and (au3-mode--cache-valid-p)
             (au3-mode--cache-get-token-from-end (point)))
        ;; if we get here the cache didn't contain the token (or wasn't up to
        ;; date)
        (let* ((orig-start (point))
               token-list)
          (au3-mode--skip-backward-over-complete-newline-token)
          (catch 'done
            (while t
              (let* ((start (point))
                     (token (au3-mode-simplest-forward-token t no-caching))
                     (stop (point)))
                (if (null token)
                    (progn
                      (goto-char (point-min))
                      (throw 'done nil))
                  (push (au3-mode--cache-make-entry token start stop) token-list)
                  (when (>= (point) orig-start)
                    (dolist (entry token-list)
                      (when (<= (au3-mode--cache-entry-end entry) orig-start)
                        ;; Pity I can't use
                        ;; (au3-mode--cache-get-token-from-end
                        ;; (au3-mode--cache-entry-end entry)) here as it would
                        ;; set the point correctly.  Unfortunately when
                        ;; no-caching is t, the newly found entry may very
                        ;; well not be in the cache and NIL gets returned and
                        ;; the point isn't updated either.
                        (goto-char (au3-mode--cache-entry-beg entry))
                        (throw 'done (au3-mode--cache-entry-token entry))))
                    ;; something went wrong: we moved past orig-start but
                    ;; no token was found -> stop search and pretend we got
                    ;; to beginning of buffer
                                        ; (message "NOT REACHED")
                    (goto-char (point-min))
                    (throw 'done nil))))))))))

(ert-deftest test-au3-mode-simplest-backward-token-no-caching ()
  "Test `au3-mode-simplest-backward-token' with no-caching=t"
  :tags '(token)
  (let ((fun (lambda () (au3-mode-simplest-backward-token t))))
    ;; (should (equal (au3-mode--run-token-matcher fun "f(_,\n@$a|)") "$a"))
    ;; (should-not (au3-mode--run-token-matcher fun "|@f(_,\n@$a|)"))
    (should (equal (au3-mode--run-token-matcher fun "@\n\n\n\t|f(_,\n@$a|)")
                   au3-mode-+newline+))))

(defun revert-autoit ()
  (interactive)
  (revert-buffer)
  (dolist (x '("~/Desktop/autoit-mode/tdd-smie.el" "~/Desktop/autoit-mode/autoit-mode.el")) (load x))
  (autoit-mode))

(defun collect-all-tokens (fun)
  (catch 'done
    (let (result
          (old (point-min)))
      (goto-char old)
      (while t
        (push (funcall fun) result)
        (if (eql (point) old)
            (throw 'done
                   (nreverse result))
          (setq old (point)))))))

(defun au3-mode--do-indentation-test (text-to-indent)
  (let ((buffer-to-indent (get-buffer-create "*to-indent*"))
        (buffer-to-log (get-buffer-create "*log*"))
        (au3-mode-indent-basic 4)
        (comment-use-syntax t)
        ;; (smie--hanging-eolp-function
        ;;  (lambda ()
        ;;    (let ((point (point)))
        ;;      (with-current-buffer buffer-to-log
        ;;        (goto-char (point-max))
        ;;        (insert "* hanging-eol-comment\npoint=%d" point))
        ;;      (skip-chars-forward " \t")
        ;;      (or (eolp)
        ;;          (when (looking-at "_")
        ;;            (forward-char 1)
        ;;            (skip-chars-forward "^\n;")
        ;;            (when (looking-at ";")
        ;;              (skip-chars-forward "^\n"))
        ;;            t)
        ;;          (and ;; (looking-at comment-start-skip) ;(bug#16041).
        ;;           (forward-comment (point-max)))))))
        )
    (with-current-buffer buffer-to-log
      (delete-region (point-min) (point-max))
      (org-mode))
    (with-current-buffer buffer-to-indent
      (delete-region (point-min) (point-max))
      (set-syntax-table autoit-mode-syntax-table)
      (setq comment-use-syntax t)
      (au3-mode--smie-setup
       (lambda (method arg)
         (let ((p (point))
               (bef (buffer-substring-no-properties (point-min) (point-max)))
               (result (au3-mode--smie-rule method arg))
               (back-token (save-excursion (au3-mode-simplest-backward-token))))
           (ignore-errors
             (with-current-buffer buffer-to-log
               (goto-char (point-max))
               (insert
                (format
                 "* %s %s\nPoint=%d, back-token=%s, {parent,sibling}-p While/If=%s,%s\n#+BEGIN_EXAMPLE\n%s|%s\n#+END_EXAMPLE\nResult(%s %s %d)=%s\n"
                 method
                 arg
                 p
                 back-token
                 au3-smie-rule-parent-while-if
                 au3-smie-rule-sibling-while-if
                 (substring bef 0 (1- p))
                 (substring bef (1- p))
                 method arg p result))))
           result)))
      (goto-char (point-min))
      (insert text-to-indent)
      (indent-region (point-min) (point-max)))))

(provide 'tdd-smie)
