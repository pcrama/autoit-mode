;;; autoit-mode.el --- AutoIt (V3) major mode

;; Author: ytrewq1
;; Keywords: autoit
;; Version: 2005-04-25

;; TODO:
;;   indentation?
;;   consider redoing font lock levels
;;   usage instructions?
;;     (require 'autoit-mode)
;;     (add-to-list 'auto-mode-alist
;;                  '("\\.au3" . autoit-mode))


;; References for creation:
;;   http://two-wugs.net/emacs/mode-tutorial.html
;;   http://www.emacswiki.org/cgi-bin/wiki?SampleMode

;; copied from http://www.autoitscript.com/forum/topic/10818-emacs-major-mode-for-autoit-v3/

;;; Code:

(defvar autoit-mode-hook nil)

;; TODO: not really using yet
(defvar autoit-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for `autoit-mode'")

(defconst autoit-builtins
  (list "And" "ByRef" "Case" "Const" "ContinueLoop" "Dim" "Do" "Else" "ElseIf" "EndFunc" "EndIf" "EndSelect" "Exit" "ExitLoop" "For" "Func" "Global" "If" "In" "Local" "Next" "Not" "Or" "ReDim" "Return" "Select" "Step" "Then" "To" "Until" "WEnd" "While" "#ce" "#comments-start" "#comments-end" "#cs" "#include" "#include-once" "#NoTrayIcon")
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
     (modify-syntax-entry ?\^m ">   " table)
     (modify-syntax-entry ?\\ "." table) ; `\' isn't an escape character!
     table)
  "Syntax table for `autoit-mode'")

(defun autoit-mode-jump-to-include-file (&rest args)
  (find-file (first args) nil))

(defun autoit-indent-line ()
  "Indent current line as AutoIt code"
  (interactive)
  (beginning-of-line)

  (if (bobp)  ; Check for rule 1
      (indent-line-to 0)

    (let ((not-indented t) cur-indent)

      (if (looking-at "^[ \t]*\\<\\(Next\\|EndFunc\\|ElseIf\\|Else\\|EndIf\\|EndSelect\\|EndSwitch\\|WEnd\\|EndWith\\)\\>") ; Check for rule 2
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) default-tab-width)))

            (if (< cur-indent 0)
                (setq cur-indent 0))

            (if (looking-at "^[ \t]*\\<\\(ElseIf\\|Else\\)\\>")
                (progn
                  (save-excursion
                    (forward-line -1)
                    (setq cur-indent (- (current-indentation) default-tab-width))))))

        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*\\<\\(Next\\|EndFunc\\|EndIf\\|EndSelect\\|EndSwitch\\|WEnd\\|EndWith\\)\\>") ; Check for rule 3
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
                                        ; Check for rule 4
              (if (looking-at "^[ \t]*\\<\\(For\\|Func\\|If\\|Select\\|Switch\\|While\\|With\\)\\>")
                  (progn
                    (setq cur-indent (+ (current-indentation) default-tab-width))
                    (setq not-indented nil))

                (if (bobp) ; Check for rule 5
                    (setq not-indented nil)))))))

      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

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

;;;###autoload
(define-derived-mode autoit-mode prog-mode "AutoIt"
  "A major mode for editing AutoIt (V3) scripts."
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-start-skip) ";+[ \t]*")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'indent-line-function) 'autoit-indent-line)
  ;; The following variables become buffer-local automatically
  (setq imenu-generic-expression
        '((nil "Func[ \n\t]+\\([A-Za-z_0-9]+\\)" 1)
          (".Vars" "Global[ \n\t]+\\(\\$[A-Za-z_0-9]+\\)" 1)
          ("#include" "^#include[ \t]+\"\\([^\"]+\\)\"" 1 autoit-mode-jump-to-include-file))
        case-fold-search t
        font-lock-defaults '(autoit-font-lock-keywords)
        font-lock-keywords-case-fold-search t)
  )

(provide 'autoit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
