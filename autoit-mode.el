;;; autoit-mode.el --- AutoIt (V3) major mode

;; Author: ytrewq1
;; Keywords: autoit
;; Version: 2005-04-25

;; TODO:
;;   look at beginning-of-defun and end-of-defun
;;   implement eldoc properly (look at skeleton autoit-documentation-function)
;;   indentation?
;;   consider redoing font lock levels
;;   usage instructions?
;;     (require 'autoit-mode)
;;     (add-to-list 'auto-mode-alist
;;                  '("\\.au3" . autoit-mode))


;; References for creation:
;;   http://two-wugs.net/emacs/mode-tutorial.html
;;   http://www.emacswiki.org/cgi-bin/wiki?SampleMode

;; copied from http://www.autoitscript.com/forum/topic/10818-emacs-major-mode-for-autoit-v3/ then modified to suit taste

;;; Code:

(defvar autoit-mode-hook nil)

;; TODO: not really using yet
(defvar autoit-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for `autoit-mode'")

(defconst autoit-builtins
  (list "And" "ByRef" "Case" "Const" "ContinueLoop" "Dim" "Do" "Else" "ElseIf" "EndFunc" "EndIf" "EndSelect" "Exit" "ExitLoop" "For" "Func" "Global" "If" "In" "Local" "Next" "Not" "Or" "ReDim" "Return" "Select" "Step" "Then" "To" "Until" "WEnd" "While" "#ce" "#comments-start" "#comments-end" "#cs" "#include" "#include-once" "#NoTrayIcon" "With" "EndWith" "Switch" "EndSwitch")
  "")

(defconst autoit-function-names
  (list "Abs" "ACos" "AdlibDisable" "AdlibEnable" "Asc" "ASin" "Assign" "ATan" "AutoItSetOption" "AutoItWinGetTitle" "AutoItWinSetTitle" "BitAND" "BitNOT" "BitOR" "BitShift" "BitXOR" "BlockInput" "Break" "Call" "CDTray" "Chr" "ClipGet" "ClipPut" "ConsoleWrite" "ControlClick" "ControlCommand" "ControlDisable" "ControlEnable" "ControlFocus" "ControlGetFocus" "ControlGetHandle" "ControlGetPos" "ControlGetText" "ControlHide" "ControlListView" "ControlMove" "ControlSend" "ControlSetText" "ControlShow" "Cos" "Dec" "DirCopy" "DirCreate" "DirGetSize" "DirMove" "DirRemove" "DllCall" "DllClose" "DllOpen" "DriveGetDrive" "DriveGetFileSystem" "DriveGetLabel" "DriveGetSerial" "DriveGetType" "DriveMapAdd" "DriveMapDel" "DriveMapGet" "DriveSetLabel" "DriveSpaceFree" "DriveSpaceTotal" "DriveStatus" "EnvGet" "EnvSet" "EnvUpdate" "Eval" "Exp" "FileChangeDir" "FileClose" "FileCopy" "FileCreateShortcut" "FileDelete" "FileExists" "FileFindFirstFile" "FileFindNextFile" "FileGetAttrib" "FileGetLongName" "FileGetShortcut" "FileGetShortName" "FileGetSize" "FileGetTime" "FileGetVersion" "FileInstall" "FileMove" "FileOpen" "FileOpenDialog" "FileRead" "FileReadLine" "FileRecycle" "FileRecycleEmpty" "FileSaveDialog" "FileSelectFolder" "FileSetAttrib" "FileSetTime" "FileWrite" "FileWriteLine" "FtpSetProxy" "GUICreate" "GUICtrlCreateAvi" "GUICtrlCreateButton" "GUICtrlCreateCheckbox" "GUICtrlCreateCombo" "GUICtrlCreateContextMenu" "GUICtrlCreateDate" "GUICtrlCreateDummy" "GUICtrlCreateEdit" "GUICtrlCreateGroup" "GUICtrlCreateIcon" "GUICtrlCreateInput" "GUICtrlCreateLabel" "GUICtrlCreateList" "GUICtrlCreateListView" "GUICtrlCreateListViewItem" "GUICtrlCreateMenu" "GUICtrlCreateMenuitem" "GUICtrlCreatePic" "GUICtrlCreateProgress" "GUICtrlCreateRadio" "GUICtrlCreateSlider" "GUICtrlCreateTab" "GUICtrlCreateTabItem" "GUICtrlCreateTreeView" "GUICtrlCreateTreeViewItem" "GUICtrlCreateUpdown" "GUICtrlDelete" "GUICtrlGetState" "GUICtrlRead" "GUICtrlRecvMsg" "GUICtrlSendMsg" "GUICtrlSendToDummy" "GUICtrlSetBkColor" "GUICtrlSetColor" "GUICtrlSetCursor" "GUICtrlSetData" "GUICtrlSetFont" "GUICtrlSetImage" "GUICtrlSetLimit" "GUICtrlSetOnEvent" "GUICtrlSetPos" "GUICtrlSetResizing" "GUICtrlSetState" "GUICtrlSetStyle" "GUICtrlSetTip" "GUIDelete" "GUIGetCursorInfo" "GUIGetMsg" "GUISetBkColor" "GUISetCoord" "GUISetCursor" "GUISetFont" "GUISetHelp" "GUISetIcon" "GUISetOnEvent" "GUISetState" "GUIStartGroup" "GUISwitch" "Hex" "HotKeySet" "HttpSetProxy" "InetGet" "InetGetSize" "IniDelete" "IniRead" "IniReadSection" "IniReadSectionNames" "IniWrite" "InputBox" "Int" "IsAdmin" "IsArray" "IsDeclared" "IsFloat" "IsInt" "IsNumber" "IsString" "Log" "MemGetStats" "Mod" "MouseClick" "MouseClickDrag" "MouseDown" "MouseGetCursor" "MouseGetPos" "MouseMove" "MouseUp" "MouseWheel" "MsgBox" "Number" "Ping" "PixelChecksum" "PixelGetColor" "PixelSearch" "ProcessClose" "ProcessExists" "ProcessList" "ProcessSetPriority" "ProcessWait" "ProcessWaitClose" "ProgressOff" "ProgressOn" "ProgressSet" "Random" "RegDelete" "RegEnumKey" "RegEnumVal" "RegRead" "RegWrite" "Round" "Run" "RunAsSet" "RunWait" "Send" "SetError" "SetExtended" "Shutdown" "Sin" "Sleep" "SoundPlay" "SoundSetWaveVolume" "SplashImageOn" "SplashOff" "SplashTextOn" "Sqrt" "StatusbarGetText" "String" "StringAddCR" "StringFormat" "StringInStr" "StringIsAlNum" "StringIsAlpha" "StringIsASCII" "StringIsDigit" "StringIsFloat" "StringIsInt" "StringIsLower" "StringIsSpace" "StringIsUpper" "StringIsXDigit" "StringLeft" "StringLen" "StringLower" "StringMid" "StringReplace" "StringRight" "StringSplit" "StringStripCR" "StringStripWS" "StringTrimLeft" "StringTrimRight" "StringUpper" "Tan" "TimerDiff" "TimerInit" "ToolTip" "TrayTip" "UBound" "WinActivate" "WinActive" "WinClose" "WinExists" "WinGetCaretPos" "WinGetClassList" "WinGetClientSize" "WinGetHandle" "WinGetPos" "WinGetProcess" "WinGetState" "WinGetText" "WinGetTitle" "WinKill" "WinList" "WinMenuSelectItem" "WinMinimizeAll" "WinMinimizeAllUndo" "WinMove" "WinSetOnTop" "WinSetState" "WinSetTitle" "WinSetTrans" "WinWait" "WinWaitActive" "WinWaitClose" "WinWaitNotActive")
  "")

(defconst autoit-macro-names
  (list "@AppDataCommonDir" "@AppDataDir" "@AutoItExe" "@AutoItVersion" "@CommonFilesDir" "@Compiled" "@ComputerName" "@ComSpec" "@CR" "@CRLF" "@DesktopCommonDir" "@DesktopDir" "@DesktopHeight" "@DesktopWidth" "@DesktopDepth" "@DesktopRefresh" "@DocumentsCommonDir" "@error" "@extended" "@FavoritesCommonDir" "@FavoritesDir" "@GUI_CtrlId" "@GUI_CtrlHandle" "@GUI_WinHandle" "@HomeDrive" "@HomePath" "@HomeShare" "@HOUR" "@InetGetActive" "@InetGetBytesRead" "@IPAddress1" "@IPAddress2" "@IPAddress3" "@IPAddress4" "@LF" "@LogonDNSDomain" "@LogonDomain" "@LogonServer" "@MDAY" "@MIN" "@MON" "@MyDocumentsDir" "@NumParams" "@OSBuild" "@OSLang" "@OSServicePack" "@OSTYPE" "@OSVersion" "@ProgramFilesDir" "@ProgramsCommonDir" "@ProgramsDir" "@ScriptDir" "@ScriptFullPath" "@ScriptName" "@SEC" "@StartMenuCommonDir" "@StartMenuDir" "@StartupCommonDir" "@StartupDir" "@SW_DISABLE" "@SW_ENABLE" "@SW_HIDE" "@SW_MAXIMIZE" "@SW_MINIMIZE" "@SW_RESTORE" "@SW_SHOW" "@SW_SHOWDEFAULT" "@SW_SHOWMAXIMIZED" "@SW_SHOWMINIMIZED" "@SW_SHOWMINNOACTIVE" "@SW_SHOWNA" "@SW_SHOWNOACTIVATE" "@SW_SHOWNORMAL" "@SystemDir" "@TAB" "@TempDir" "@UserProfileDir" "@UserName" "@WDAY" "@WindowsDir" "@WorkingDir" "@YDAY" "@YEAR")
  "")

;; TODO:
;;
;;    * Level 1: highlight function declarations, file directives (such as
;;      include or import directives), strings and comments.  The idea is
;;      speed, so only the most important and top-level components are
;;      fontified.
;;
;;    * Level 2: in addition to level 1, highlight all language keywords,
;;      including type names that act like keywords, as well as named
;;      constant values.  The idea is that all keywords (either syntactic
;;      or semantic) should be fontified appropriately.
;;
;;    * Level 3: in addition to level 2, highlight the symbols being
;;      defined in function and variable declarations, and all builtin
;;      function names, wherever they appear.

(require 'cl-lib)

(defconst autoit-font-lock-keywords-1
  (list
   (cons (concat "\\<\\(" (regexp-opt autoit-builtins t) "\\)\\>")
         font-lock-builtin-face)
   (cons "$\\(\\w+\\)" font-lock-variable-name-face))
  "")

(defconst autoit-font-lock-keywords-2
   (append autoit-font-lock-keywords-1
           (list
            (cons (concat "\\<\\(" (regexp-opt autoit-function-names t)
                          "\\)\\>")
                  font-lock-function-name-face)))
   "")

(defconst autoit-font-lock-keywords-3
   (append autoit-font-lock-keywords-2
           (list
            (cons (concat "\\<\\(" (regexp-opt autoit-macro-names t)
                          "\\)\\>")
                  font-lock-constant-face)))
   "")

(defvar autoit-font-lock-keywords autoit-font-lock-keywords-3
  "")

(defvar autoit-mode-syntax-table
   (let ((table (make-syntax-table)))
     (modify-syntax-entry ?_ "w" table)
     (modify-syntax-entry ?\; "<   " table)
     (modify-syntax-entry ?\n ">   " table)
     ;; commented out in lisp-mode.el (modify-syntax-entry ?\^m ">   " table)
     (modify-syntax-entry ?\" "\"    " table)
     (modify-syntax-entry ?\( "()  " table)
     (modify-syntax-entry ?\) ")(  " table)
     (modify-syntax-entry ?\[ "(]  " table)
     (modify-syntax-entry ?\] ")[  " table)
     (modify-syntax-entry ?\\ "." table) ; `\' isn't an escape character!
     table)
  "Syntax table for `autoit-mode'")

(defun autoit-mode-jump-to-include-file (&rest args)
  (find-file (cl-first args) nil))

(defun imenu--sort-by-position (item1 item2)
  (< (if (consp (cdr item1)) (cadr item1) (cdr item1))
     (if (consp (cdr item2)) (cadr item2) (cdr item2))))

(defun imenu--truncate-items (menulist)
  (mapcar (function
	   (lambda (item)
	     (cond
	      ((and (consp (cdr item)) (stringp (cadr item)))
	       (imenu--truncate-items (cdr item)))
	      ;; truncate if necessary
	      ((and (numberp imenu-max-item-length)
		    (> (length (car item)) imenu-max-item-length))
	       (setcar item (substring (car item) 0 imenu-max-item-length))))))
	  menulist))

(defun imenu--in-alist (str alist)
  "Check whether the string STR is contained in multi-level ALIST."
  (let (elt head tail res)
    (setq res nil)
    (while alist
      (setq elt (car alist)
	    tail (cdr elt)
	    alist (cdr alist)
	    head (car elt))
      ;; A nested ALIST element looks like
      ;;   (INDEX-NAME (INDEX-NAME . INDEX-POSITION) ...) or
      ;;   (INDEX-NAME (INDEX-NAME INDEX-POSITION FUNCTION &rest ARGS) ...)
      ;; while a bottom-level element looks like
      ;;   (INDEX-NAME . INDEX-POSITION) or
      ;;   (INDEX-NAME INDEX-POSITION FUNCTION &rest ARGS)
      ;; We are only interested in the bottom-level elements, so we need to
      ;; recurse if TAIL is a list.
      (cond ((and (listp tail) (stringp (car tail)))
	     (if (setq res (imenu--in-alist str tail))
		 (setq alist nil)))
	    ((if imenu-name-lookup-function
                 (funcall imenu-name-lookup-function str head)
               (string= str head))
	     (setq alist nil res elt))))
    res))

(require 'autoit-smie)

(defvar autoit-eldoc-function-name-regexp
  "\\<func[ \n\t]+\\([A-Za-z_0-9]+\\)")

;;;###autoload
(define-derived-mode autoit-mode prog-mode "AutoIt"
  "A major mode for editing AutoIt (V3) scripts."
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-start-skip) ";+[ \t]*")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'indent-line-function) 'autoit-indent-line)
  ;; The following variables become buffer-local automatically
  (setq case-fold-search t
        font-lock-defaults
        '(autoit-font-lock-keywords     ; list of keywords
          nil                           ; keywords-only
          t                             ; case-fold
          )
        imenu-generic-expression
        (list (list nil autoit-eldoc-function-name-regexp 1)
              '(".Vars" "\\<global[ \n\t]+\\(\\$[A-Za-z_0-9]+\\)" 1)
              '("#include" "^#include[ \t]+\"\\([^\"]+\\)\"" 1 autoit-mode-jump-to-include-file))
        imenu-case-fold-search t)
  (autoit-smie-setup))

(defvar autoit-eldoc-predefined-functions-cache
  (concat (file-name-directory load-file-name)
          "autoit-predefined-functions.el")
  "File where function definitions of builtin & UDF functions are stored.
See `autoit-prepare-function-definitions'")

(defun autoit-enable-eldoc ()
  "Setup and enable ElDoc minor mode for AutoIt source files."
  (set (make-local-variable 'eldoc-documentation-function) 'autoit-documentation-function)
  (turn-on-eldoc-mode)
  (with-temp-buffer
    (insert-file-contents autoit-eldoc-predefined-functions-cache)
    (let ((hash (read (current-buffer))))
      (maphash (lambda (key value)
                 (puthash key value autoit-function-definitions))
               hash))))

(defvar autoit-eldoc-include-docstring
  t
  "Eldoc will append a short description to status unless this is nil.")

(defvar autoit-eldoc-max-width
  100
  "Eldoc will display description on same line as function
name/parameters unless this length is exceeded.")

(defvar autoit-function-definitions
  (make-hash-table :test 'equal)
  "Hashtable mapping function names to \(parameter-sequence short-description origin\).")

;; Todo: fill hash-table automatically!!
;; (setf (gethash "werl" autoit-function-definitions)
;;       '(["$arg1" "$arg2" "$arg3"] "Frob a foo" "file.au3"))

(defun autoit-scan-buffer-for-eldoc ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward autoit-eldoc-function-name-regexp () t)
      (let ((name (buffer-substring-no-properties
                   (match-beginning 1) (match-end 1)))
            (end (match-end 1))
            result)
        (goto-char end)
        (when (equal (autoit-smie-forward-token) "(")
          (let (arglist
                tok
                arg)
            (while (and (stringp (setq tok (autoit-smie-forward-token)))
                        (not (member tok '(";lf;" "\(" "\)"))))
              (if (equal tok ",")
                  (when arg
                    (setq arglist (cons arg arglist)
                          arg nil))
                (setq tok (autoit-smie-last-token-literally)
                      arg (if (null arg)
                              tok
                            (concat arg " " tok)))))
            (when arg
              (setq arglist (cons arg arglist)
                    arg nil))
            (puthash name
                     (list (reverse arglist)
                           ""           ; TODO try to find a docstring
                           (or (buffer-file-name)
                               buffer-file-truename
                               (buffer-name)))
                     autoit-function-definitions)))))))

(defun autoit-skip-back-to-function-name ()
  ;; suppress mode-specific forward-sexp-function so that the backward-up-list
  ;; only looks at parentheses tokens
  (let ((forward-sexp-function nil))
    (while (progn (backward-up-list)
                  (and (not (bobp))
                       (not (equal (autoit-smie-backward-token) ";id;"))))))
  (let ((old (point)))
    (prog1
        (when (equal (autoit-smie-forward-token) ";id;")
          (buffer-substring-no-properties old (point)))
      (goto-char old))))

(defun autoit-documentation-function ()
  (save-excursion
    (let* ((old (point))
           (fun (condition-case err (autoit-skip-back-to-function-name)
                  (scan-error nil)))
           (pos 0)
           fun-info)
      (when (and (stringp fun)
                 (setq fun-info (gethash fun autoit-function-definitions nil))
                 (progn (goto-char (+ (point) (length fun))) t)
                 (equal (autoit-smie-forward-token) "\("))
        (while (and (condition-case nil
                        (progn (forward-sexp) t)
                      ((scan-error end-of-buffer) nil))
                    (not (eobp))
                    (< (point) old))
          (let ((comma-old (point)))
            (if (equal (autoit-smie-forward-token) ",")
		(cl-incf pos)
              (goto-char comma-old))))
        (let* ((result (concat fun ": "))
               (par-seq (car fun-info))
               (docstring (and autoit-eldoc-include-docstring (cadr fun-info)))
               start-idx end-idx
               start-doc)
          (dotimes (i (length par-seq))
	    (when (cl-plusp i) (setq result (concat result ", ")))
            (when (= i pos) (setq start-idx (length result)))
            (setq result (concat result (elt par-seq i)))
            (when (= i pos) (setq end-idx (length result))))
          (when docstring
            (let ((sep (if (> (+ (length result) (length docstring))
                              autoit-eldoc-max-width)
                           "\n"
                         " ; ")))
              (setq start-doc (+ (length result) (length sep)))
              (setq result (concat result sep docstring))))
          (add-text-properties 0 (length fun) '(face font-lock-function-name-face) result)
          (when (and (numberp start-idx) (numberp end-idx))
           (add-text-properties start-idx end-idx
                                '(face font-lock-function-name-face) result))
          (when docstring
            (add-text-properties start-doc (length result)
                                 '(face font-lock-comment-face) result))
          result)))))

(defun autoit-prepare-function-definitions ()
  (with-temp-buffer
    (clrhash autoit-function-definitions)
    (autoit-grovel-chm-for-builtin-functions)
    (autoit-grovel-user-defined-functions)
    (prin1 autoit-function-definitions (current-buffer))
    (write-region (point-min)
                  (point-max)
                  autoit-eldoc-predefined-functions-cache)))

(defun autoit-grovel-chm-for-builtin-functions ()
  ;; 7z x AutoIt3.chm html/functions/... and grovel those
  (let* ((autoit-path (file-name-directory (executable-find "AutoIt3.exe")))
         (temp-dest (make-temp-file "unpack-autoit-help" 'directory)))
    (call-process "7z.exe"
                  nil                   ; no STDIN
                  nil                   ; no STDOUT
                  nil                   ; no redisplay
                  "x"
                  (concat "-o" temp-dest)
                  (concat autoit-path "AutoIt3.chm"))
    (dolist (file (directory-files
                   (concat temp-dest "/html/functions/") 'full-name "\.html?$"))
      (message "Looking at %s" file)
      (save-excursion
        (let ((buffer (find-file-noselect file))
              funcdesc
              params)
          (set-buffer buffer)
          (widen)
          (goto-char (point-min))
          (when (re-search-forward
                 "class=\"funcdesc\">\\(.*?\\)<br>[ \t\n\r]*</p>"
                 (point-max)
                 'noerror)
            (setf funcdesc (buffer-substring-no-properties (match-beginning 1)
                                                           (match-end 1))))
          (goto-char (point-min))
          (when (re-search-forward
                 "class=\"codeheader\">[ \t\n\r]*\\(.*?\\)<br>[ \t\n\r]*</p>"
                 (point-max)
                 'noerror)
            (setf params (buffer-substring-no-properties (match-beginning 1)
                                                         (match-end 1)))
            (goto-char (match-beginning 1))
            (autoit-smie-forward-token)
            (setf funcname (autoit-smie-last-token-literally))
            (let* ((pos (point))
                   (next-should-be-paren (autoit-smie-forward-token))
                   reversed-parameters
                   par-start
                   par-end
                   (next-par-start (point))
                   (closers '(";lf;" ";lf-after-func;" "" nil "\)"))
                   done
                   future-optional-par
                   optional-par)
	      (cl-labels ((forward-ignore-square-brackets
                        ()
                        (let (done tok)
                          (while (not done)
                            (setq tok (autoit-smie-forward-token))
                            (cond ((equal tok "[")
                                   (setq future-optional-par t))
                                  ((equal tok "]"))
                                  (t (setq done t))))
                          tok)))
                (when (equal next-should-be-paren "\(")
                  (while
                      (progn
                        (goto-char (setq par-start next-par-start))
                        (setq optional-par future-optional-par)
                        (while
                            (unless done
                              (setq par-end (point))
                              (let ((tok (forward-ignore-square-brackets)))
                                (setq next-par-start (point))
                                (setq done (member tok closers))
                                (not (or done (equal tok ","))))))
                        (< par-start par-end))
                    (let (rev-par-tokens)
                      (goto-char par-start)
                      (while (< (point) par-end)
                        (push (progn (forward-ignore-square-brackets)
                                     (autoit-smie-last-token-literally))
                              rev-par-tokens))
                      (push (apply 'concat
                                   (let ((parts (list (if optional-par "]" "")))
                                         tok)
                                     (while (setq tok (pop rev-par-tokens))
                                       (cond ((equal tok "=")
                                              (push tok parts))
					     ((cl-endp rev-par-tokens)
                                              (push tok parts))
                                             ((equal "=" (car rev-par-tokens))
                                              (push tok parts))
                                             (t (setq parts
						      (cl-list* " " tok parts)))))
                                     (when optional-par
                                       (push "[" parts))
                                     parts))
                            reversed-parameters)))
                  (message "%S" (autoit-eldoc-make-entry
                                 file pos
                                 funcname (reverse reversed-parameters)
                                 funcdesc))
                  (puthash funcname
                           (list (reverse reversed-parameters)
                                 funcdesc
                                 'builtin)
                           autoit-function-definitions)))))
          (kill-buffer buffer))))))

(defun autoit-eldoc-make-entry (file pos
                                func-name parameters
                                descr)
  (list file pos func-name parameters descr))

(defun autoit-grovel-user-defined-functions ()
  (let* ((autoit-path (file-name-directory (executable-find "AutoIt3.exe")))
         (include-path (file-name-as-directory (concat autoit-path "Include")))
         (files (directory-files include-path 'full-name "\.au3$")))
    (dolist (file files)
      (message "Looking at %s" file)
      (save-excursion
        (let ((buffer (find-file-noselect file)))
          (set-buffer buffer)
          (widen)
          (goto-char (point-min))
          (while (re-search-forward ";[ \t;=]*#FUNCTION#" (point-max) 'noerror)
            (let ((func-top (point))
                  (func-bot (if (re-search-forward "[\r\n]endfunc"
                                                   (point-max)
                                                   'noerror)
                                (point)
                              (point-max)))
                  info)
              (dolist (pattern '("; \\(?:Function[ \t]*\\)?Name[. \t]*:[ \t]*\\([A-Za-z_0-9]*\\)"
                                 "; Description[. \t]*:[ \t]*\\([^\r\n]*\\)"
                                 ;; This pattern must be last to use the
                                 ;; side-effect that the cursor is located close
                                 ;; to the parameter list
                                 "[\r\n]Func[ \t]*\\([A-Za-z_0-9]*\\)[ \t\r\n]*"))
                (goto-char func-top)
                (push (when (re-search-forward pattern func-bot 'noerror)
                        (goto-char (match-end 1))
                        (buffer-substring-no-properties (match-beginning 1)
                                                        (match-end 1)))
                      info))
              (let* ((pos (point))
                     (next-should-be-paren (autoit-smie-forward-token))
                     reversed-parameters
                     par-start
                     par-end
                     (next-par-start (point))
                     done)
                (when (equal next-should-be-paren "\(")
                  (while
                      (progn
                        (goto-char (setq par-start next-par-start))
                        (while
                            (unless done
                              (setq par-end (point))
                              (let ((tok (autoit-smie-forward-token)))
                                (setq next-par-start (point))
                                (setq done (member tok '(";lf;" ";lf-after-func;" "" nil "\)")))
                                (not (or done (equal tok ","))))))
                        (< par-start par-end))
                    (let (rev-par-tokens)
                      (goto-char par-start)
                      (while (< (point) par-end)
                        (push (progn (autoit-smie-forward-token)
                                     (autoit-smie-last-token-literally))
                              rev-par-tokens))
                      (push (apply 'concat
                                   (let ((parts '(""))
                                         tok)
                                     (while (setq tok (pop rev-par-tokens))
                                       (cond ((equal tok "=")
                                              (push tok parts))
					     ((cl-endp rev-par-tokens)
                                              (push tok parts))
                                             ((equal "=" (car rev-par-tokens))
                                              (push tok parts))
                                             (t (setq parts
						      (cl-list* " " tok parts)))))
                                     parts))
                            reversed-parameters)))
                  (message "%S" (autoit-eldoc-make-entry
                                 file pos
				 (cl-caddr info) (reverse reversed-parameters)
                                 (cadr info)))
		  (puthash (cl-caddr info)
                           (list (reverse reversed-parameters)
                                 (cadr info)
                                 'udf)
                           autoit-function-definitions)))))
          (kill-buffer buffer))))))

(provide 'autoit-mode)
;;; autoit-mode.el ends here
