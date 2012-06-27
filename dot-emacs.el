;; init.el --- Custom configuration for SXEmacs.

;; Copyright (C) 1998-2010 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; X-CVS: $Id: init-home.el,v 1.13 2008/12/23 13:24:37 lg Exp $

;;; Note:

;; File layout controlled by Emacs folding.el available at:
;; http://www.csd.uu.se/~andersl/emacs.shtml - Latest included in
;; (S)XEmacs

(setq debug-on-error t)
;;; Code:

;;; Mule
(when (featurep 'mule)
  (set-language-environment 'Cyrillic-KOI8)

  (set-charset-registry 'ascii "koi8-r")
  (set-charset-registry 'cyrillic-iso8859-5 "koi8-r")
  (set-charset-ccl-program 'cyrillic-iso8859-5 'ccl-encode-koi8-r-font)
  (put-charset-property 'cyrillic-iso8859-5 'preferred-coding-system 'koi8-r)

  (set-charset-registry 'latin-iso8859-1 "koi8-r")
  (require 'un-define)

  (setq buffer-file-coding-system-for-read 'utf-8)
  (setq coding-system-for-read 'utf-8)
  (setq coding-system-for-write 'utf-8)

;   (require 'quail-dvorak-jcuken
; 	   (expand-file-name "quail-dvorak-jcuken.el" user-init-directory))
;   (setq default-input-method "cyrillic-dvorak")

  (setq keyboard-coding-system 'iso-8859-1)

  (setq mule-retrieval-coding-system 'utf-8)
  )

(load (expand-file-name "local.loadpath.el" user-init-directory))

;;{{{ `-- Presetup

(set-default 'indent-tabs-mode nil)

;; No GC messages please! (SXEmacs)
(setq gc-message nil)

;; Speed-up GC
(setq gc-cons-threshold 50000000)

;;; adjust load-path
(push (expand-file-name "lisp" user-init-directory) load-path)
(push (expand-file-name "lisp/myemacs" user-init-directory) load-path)
(push (expand-file-name "lisp/thirdpart" user-init-directory) load-path)
(push (expand-file-name "~/dev/emacs") load-path)

(push (expand-file-name "~/bin/") exec-path)

(unless (fboundp 'get-coding-system)
  (defalias 'get-coding-system 'ignore))
(unless (fboundp 'set-buffer-file-coding-system)
  (defalias 'set-buffer-file-coding-system 'ignore))

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable local variables
(setq enable-local-variables nil)

;; SXEmacs audio
(setq default-audio-device (make-audio-device 'ao))

;;}}}

;;{{{ `-- Useful interactive functions

;;; Some useful functions
(defun lg-maybeinsert (str &optional arg)
  "As identity, but inserts to selected buffer if ARG is non-nil."
  (if arg (insert str) str))

(defun alphabet (&optional arg)
  "Return latin alphabet as string or insert to selected buffer."
  (interactive "P")
  (lg-maybeinsert "abcdefghijklmnopqrstuvwxyz" arg))

(defun digits (&optional arg)
  "Return digits list as string or insert to selected buffer."
  (interactive "P")
  (lg-maybeinsert "1234567890" arg))

(defun true (&rest args)
  "Always return `t'."
  t)

(defun false (&rest args)
  "Always return `nil'."
  nil)

(defun debug-on-error (arg)
  "Toggle debug on error.
With negative ARG turn it off, with positive turn it on.
Otherwise toggle."
  (interactive "_P")
  (setq debug-on-error
        (or (and (null arg)
                 (not debug-on-error))
            (and (not (null arg))
                 (> (prefix-numeric-value arg) 0))))
  (message "Debug on error is %s" (if debug-on-error "ON" "OFF")))

(defvar lg-scratch-file (expand-file-name "*scratch-file*" user-init-directory))

(defun lg-switch-to-scratch (arg)
  "Switch to \\*scratch\\* buffer.
If prefix ARG is specified, switch in other window."
  (interactive "P")
  (let ((scbuf (find-file lg-scratch-file)))
    (if arg
        (switch-to-buffer-other-window scbuf)
      (switch-to-buffer scbuf))))

(define-key global-map (kbd "M-<f3>") 'lg-switch-to-scratch)
(define-key global-map (kbd "C-<f3>") 'lg-switch-to-scratch)

(defvar lg-long-line-column 80
  "*Lines longer than this supposed to be long.")

(defun lg-fix-long-lines (arg)
  "Fix lines that are longer than `lg-long-line-column'.
If prefix ARG is specified than fill long lines automatically."
  (interactive "P")

  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (> (- (point-at-eol) (point-at-bol))
               (if arg fill-column lg-long-line-column))
        (if arg
            (save-excursion
              (fill-region (point-at-bol) (1+ (point-at-eol))))

          (move-to-column lg-long-line-column)
          (message (substitute-command-keys
                    (concat "Too long line (%d chars) - fix it."
                            " \\[exit-recursive-edit] to continue,"
                            " \\[abort-recursive-edit] to abort."))
                   (- (point-at-eol) (point-at-bol)))
          (recursive-edit)))
      (forward-line)))
  (message "Done fixing long lines."))

(defun lg-do-cmd-under-save-excursion (keys key)
  "Execute KEY command under `save-excursion'.
This command can be bound only to single key(not keysequence).
Useful to performe commands which changes point/mark or stuff.

For example if `lg-do-cmd-under-save-excursion' is binded to `M-o',
`M-1 7 M-o <SPC>' will insert 17 spaces keeping cursor at point."
  (interactive
   (list (this-command-keys)
         (read-key-sequence
          (concat (key-description (this-command-keys)) " -"))))

  (let ((nkeys (vconcat (butlast (append keys nil)) key)))
    (save-excursion
      (command-execute nkeys))))

(defun lg-fixup-whitespace (arg)
  "Without prefix ARG run `fixup-whitespace'.
With prefix ARG run `just-one-space'."
  (interactive "P")
  (if arg
      (just-one-space)
    (fixup-whitespace)))

(defun lg-delete-horizontal-space (arg)
  "Delete all spaces and tabs around point.
If ARG is given then delete spaces only from right."
  (interactive "*P")
  (unless arg
    (skip-chars-backward " \t"))
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(autoload 'w3m-url-encode-string "w3m")
(autoload 'url-insert-file-contents "url")

;; For G-Client
(push (expand-file-name "lisp/thirdpart/g-client" user-init-directory)
      load-path)

(defun google-calc (string &optional ret)
  "Query google calc to calculate STRING."
  (interactive "sGoogle Calc: ")
  (with-temp-buffer
    (url-insert-file-contents
     (concat "http://www.google.ru/search?q="
             (w3m-url-encode-string (encode-coding-string string 'utf-8))
             "&ie=UTF-8"))
    (if (and (re-search-forward "/images/calc_img\\.gif" nil t)
             (re-search-forward "<b>\\(.*\\)</b></h2>" nil t))
        (setq res (replace-in-string
                   (match-string 1) "<[^>]*>[^<]*</[^>]*>" ""))
      (setq res (format "No result (%s)" string)))
    (set-buffer-modified-p nil))
  (if ret
      res
    (message "Google Calc: %s" res)))

(autoload 'w3-form-encode-xwfu "w3-forms")

(defun lg-detect-lang (string)
  "Return language to use in `google-translate' according to STRING."
  (if (memq 'cyrillic-iso8859-5 (charsets-in-string string))
      "ru|en"
    "en|ru"))
        
(defun google-translate-1 (string lang &optional ret)
  "Translate STRING to russian.
If RET is specified, then only return the result."
  (with-current-buffer (get-buffer-create "TTT") ;with-temp-buffer
    (let* ((mule-retrieval-coding-system 'utf-8)
           (pairs `(("ie" . "UTF8")
                    ("langpair" . ,lang)
                    ("text" . ,string)))
           (url-request-data
            (mapconcat (lambda (p)
                         (concat (w3-form-encode-xwfu (car p))
                                 "="
                                 (w3-form-encode-xwfu (cdr p))))
                       pairs "&"))
           (url-request-method "POST")
           (url-request-extra-headers
            '(("Content-type" . "application/x-www-form-urlencoded"))))
      (url-insert-file-contents
       "http://www.google.com/translate_t"))
    (goto-char (point-min))

    ;; Find translated text
    (re-search-forward "<\\(div\\|span\\) id=result_box[^>]*>")
    (delete-region (point-min) (point))
    (re-search-forward "</\\(div\\|span\\)>")
    (delete-region (match-beginning 0) (point-max))

    (goto-char (point-min))
    (save-excursion
      (while (re-search-forward " <br>\\|<br> " nil t)
        (replace-match "\n" nil nil)))
    (save-excursion
      (while (search-forward "&quot;" nil t)
        (replace-match "\"" nil nil)))

    ;; Nuke all other tags
    (save-excursion
      (while (re-search-forward "</?[^>]*>" nil t)
        (replace-match "" nil nil)))

    (set-buffer-modified-p nil)
    (buffer-substring (point-min) (point-max))
    ))

(defun google-translate (string)
  ;; chop trailing newline if necesary
  (when (char-equal (aref string (1- (length string))) ?\n)
    (setq string (substring string 0 (1- (length string)))))
  (with-current-buffer (get-buffer-create "*Google Translate*")
    (let ((buffer-read-only nil)
          (lang (lg-detect-lang string)))
      (erase-buffer)
      (insert string)
      (insert "\n" (make-string 70 ?=) "\n")
      (insert (google-translate-1 string lang))
;      (insert "\n")
      (fill-region (point-min) (point-max))
      (set-buffer-modified-p nil)

      (let ((cwc (current-window-configuration)))
        (view-mode nil `(lambda (&rest args)
                          (interactive)
                          (kill-buffer (current-buffer))
                          (set-window-configuration ,cwc))))
      (switch-to-buffer-other-window (current-buffer))

      (set-window-text-height
       (selected-window) (1+ (count-lines (point-min) (point-max))))
    
      (run-hooks 'google-translate-mode)
      )))

(defun google-translate-region (s e &optional arg)
  "Translate region using google.
If optional ARG is given then substitute region with the translation."
  (interactive "r\nP")
  (if arg
      (let* ((bs (buffer-substring s e))
             (lang (lg-detect-lang bs))
             (tr (google-translate-1 bs lang)))
        (delete-region s e)
        (insert tr))
    (google-translate (buffer-substring s e))))

(defun google-translate-buffer ()
  (google-translate-region (point-min) (point-max)))

(add-hook 'google-translate-mode 'flyspell-buffer)

;; NOTE: `google-license-key' is in ~/.sxemacs/local.password.el

;;; 4 windows split
(defun lg-initial-split ()
  "Make initial splits"
  (interactive)

  (delete-other-windows)                ; make sure we have 1 window at frame
  (split-window-vertically)
  (split-window-horizontally)
  (other-window 2)
  (split-window-horizontally)
  (other-window 2))                     ; jump to initial window

;;; Make new line without split
(defun lg-insert-nl-at-eol (arg)
  "Insert new line at the end of line.
If prefix ARG is supplied, do not move point."
  (interactive "P")
  (eval (list (if arg 'save-excursion 'progn)
              '(end-of-line)
              '(newline-and-indent))))

(defun lg-manual-entry (arg)
  "Show manual entry in other window.
If used with prefix arg switch to it.
If used with double prefix arg act as `manual-entry'."
  (interactive "P")
  (let ((obuf (current-buffer))
        (mbuf (call-interactively 'manual-entry)))
    (cond ((equal arg '(4))
           (switch-to-buffer obuf)
           (switch-to-buffer-other-window mbuf))

          ((equal arg '(16)) nil)
          (t
           (save-excursion
             (switch-to-buffer-other-window mbuf))
           (switch-to-buffer obuf)))))

(defun indent-or-complete (arg)
  "Complete if point is at end of a word, otherwise indent line"
  (interactive "P")
  (if (looking-at "\\>")
      (dabbrev-expand arg)
    (indent-for-tab-command)))

;;; IGREP configuration and Advanced `find-tag'
(setq igrep-options "--colour=never")
(setq igrep-save-buffers nil)
(setq igrep-find-use-xargs nil)

(defun lg-find-tag-regex (tagname)
  "Use `igrep-find' command to find all occurances of tag with TAGNAME."
  (interactive (if current-prefix-arg (list (current-word))
                 (list (find-tag-tag "Find tag: "))))
  (let ((dir (file-name-directory tags-file-name)))
    (igrep-find "grep" tagname (concat dir "/*"))))

(defun lg-highlight-text (regexp &optional faces)
  "Highlight text matched with REGEXP with FACES in buffer."
  (while (re-search-forward regexp nil t)
    (let ((ext (make-extent (match-beginning 0) (match-end 0))))
      (set-extent-face ext (or faces '(red bold))))))

(defun lg-compile-finish-function (buffer exit-status)
  "In case of `igrep' command, highlight search string."
  (let ((cmd (nth 0 command-history)))
    (when (eq (first cmd) 'igrep)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (lg-highlight-text (third cmd)))))))

(setq compilation-finish-function #'lg-compile-finish-function)

;;; Occur tweak
(defadvice list-matching-lines (after highlight-regexp activate)
  (with-current-buffer "*Occur*"
    (lg-highlight-text regexp)))

;;; Yanking
(defun lg-mouse-yank ()
  "As `mouse-yank', but does not require to be bound to mouse."
  (interactive)
  (mouse-yank nil))

;; for slippery fingers
(defun lg-ask-exit-emacs (arg)
  "Ask for confirmation before exit.
If used with prefix ARG, force Emacs to exit, skiping `kill-emacs-hook'."
  (interactive "P")
  (if arg
      (let (kill-emacs-hook)
        (kill-emacs))

    (when (yes-or-no-p "Exit SXEmacs?")
      (lg-save-lsf-buffer)
      (save-buffers-kill-emacs))))

;;; Some enhacement to `kill-line'
;; NOTE: With negative prefix arg `kill-line' will kill lines backward!
(defun lg-kill-line (&optional arg)
  "Deletes to the end of current line.
If ARG is given `lg-kill-line' deletes to the beginning of line."
  (interactive "P")
  (if (and arg (listp arg))
      (delete-region
       (save-excursion
         (command-execute (key-binding (kbd "C-a")))
         (point)) (point))
    (kill-line arg)))

(defun lg-kill-region (beg end &optional arg)
  "Delete or kill region at BEG END according to ARG.
If ARG is non-nil delete region, otherwise kill."
  (interactive "*r\nP")
  (if arg
      (delete-region beg end)
    (kill-region beg end)))

;;; Switching to other buffer in other window
(defun lg-switch-to-other-other-window (arg)
  "Like \\[switch-to-other-buffer] but in other window."
  (interactive "p")

  (other-window 1)
  (switch-to-other-buffer arg)
  (other-window -1))

(defun lg-kill-current-buffer (buffer)
  "Kill current buffer without confirmation."
  (interactive (list (current-buffer)))
  (kill-buffer buffer))

(defun lg-kill-buffer-and-window (&rest args)
  "Kill current buffer and selected window withot confirmation."
  (interactive)
  (let ((buffer (current-buffer)))
    (delete-window (selected-window))
    (kill-buffer buffer)))

(defun lg-kill-buffer-in-other-window (arg)
  "Kill buffer in other window.
If prefix ARG is supplied, move point to other window."
  (interactive "P")

  (when (> (count-windows) 1)
    (other-window 1)
    (kill-buffer (current-buffer))

    (unless arg
      ;; Switch back
      (other-window -1))))

;; Gently modify `call-last-kbd-macro' to support applying macro to
;; region.
(fset 'orig-call-last-kbd-macro (symbol-function 'call-last-kbd-macro))

(defun call-last-kbd-macro (arg)
  "Run `apply-macro-to-region-lines' if region is active.
Or run `call-last-kbd-macro' otherwise."
  (interactive "P")
  (if (region-active-p)
      (call-interactively #'apply-macro-to-region-lines)
    (orig-call-last-kbd-macro arg)))

;;; Finction which do nothing
(defun lg-nil-func ()
  "Nil function."
  (interactive)
  (when isearch-mode (isearch-message)))
(put 'lg-nil-func 'isearch-command t)

;; Filling
(defun lg-fill-paragraph-or-region (arg)
  "Temporary set fill column to ARG and `fill-paragraph-or-region'.
If ARG is integer - `fill-paragraph-or-region' with ARG fill column.
Otherwise call `fill-paragraph-or-region' as is."
  (interactive "P")
  (if (integerp arg)
      (let ((fill-column arg))
	(fill-paragraph-or-region nil))
    (fill-paragraph-or-region arg)))

;;; Isearch
(defun lg-isearch-yank-symbol-near-point ()
  "Yank symbol at point to isearch."
  (interactive)
  (isearch-yank (symbol-near-point)))
(put 'lg-isearch-yank-symbol-near-point 'isearch-command t)

(define-key isearch-mode-map (kbd "M-s") 'lg-isearch-yank-symbol-near-point)
(define-key isearch-mode-map (kbd "C-M-r") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-M-s") 'isearch-repeat-forward)

;;; Useful mini calculator
(autoload 'calc-radix "calc-bin")
(autoload 'calc-hex-radix "calc-bin")
(autoload 'calc-decimal-radix "calc-bin")
(autoload 'calc-bin-radix "calc-bin")
(autoload 'calc-octal-radix "calc-bin")

(defun lg-mini-calc (expr &optional arg)
  "Calculate expression EXPR.
If ARG is given, then insert the result to current-buffer"
  (interactive
   (list (read-from-minibuffer "Enter expression: ")
         current-prefix-arg))

  (let ((result (calc-eval expr)))
    (if arg
        (insert result)
      (message "Result: [%s] = %s" expr result))))

(defun lg-calc-register (r cstr)
  "Recalculate value for register R.
CSTR specifies format string for `calc-eval' to be evaluated.
CSTR can contain special escape sequences:
 ~_  - for value of the current register
 ~n  - for value of the register n"
  (interactive "cCalc for register: \nsCalc string: ")
  (let* ((ss (split-string cstr "~"))
         (es (concat (car ss)
                     (mapconcat #'(lambda (s)
                                    (let* ((c (aref s 0))
                                           (cv (get-register (if (= c ?_) r c)))
                                           (os (substring s 1)))
                                      (format "%s %s" cv os)))
                                (cdr ss) ""))))
    (set-register r (string-to-int (calc-eval es)))
    (message "Register %c set to: %s" r (get-register r))))

;; Momentary ruler
(defun lg-column-ruler (width)
  "Display temp ruler at point."
  (interactive (list (+ (window-hscroll) (window-width) -1)))

  (momentary-string-display
   (if (< width 10)
       "1   5   10\n|...|....|\n"
     (let* ((iterations (/ width 10))
	    (short (- width (* 10 iterations)))
	    (result1 "|...|....|")
	    (result2 "1   5   10")
	    (inc1 "....|....|")
	    (inc2 "        %d0")
	    (i 1))
       (while  (< i iterations)
	 (setq i (1+ i))
	 (setq result1 (concat result1 inc1))
	 (setq result2 (concat result2 (substring (format inc2 i) -10))))
       (concat result2 "\n" result1 (substring inc1 0 short) "\n")))
   (point-at-bol)
   nil "[space] Clears ruler"))

;;; Useful momentary help mode
(defvar lg-momentary-help-saved-with-displaying-help-buffer nil
  "Saved value of `with-displaying-help-buffer'")

(defvar lg-momentary-help-mode nil
  "*Non-nil mean momentary help mode is enabled.")

(defun lg-momentary-help-display-buffer (thunk &optional name)
  "Momentary display buffer."
  (let ((hstr (with-output-to-string
                (funcall thunk)
                (beginning-of-buffer)
                (insert (format ".-------- [ %s ] --------\n" name))
                (while (not (eobp))
                  (goto-char (point-at-bol))
                  (insert "| ")
                  (forward-line))
                (end-of-buffer)
                (insert "`---------------\n"))))
    (momentary-string-display
     hstr (point-at-bol)
     nil "[space] Exit momentary help")))

(defun lg-momentary-help-mode (arg)
  "Toggle momentary help mode.
With positive prefix ARG - turn on.
With negative prefix ARG - turn off.
Otherwise - toggle."
  (interactive "P")

  (if (or (and (numberp arg) (< 0 arg))
          (null lg-momentary-help-mode))
      ;; Turn it on
      (progn
        (setq lg-momentary-help-saved-with-displaying-help-buffer
              (symbol-function 'with-displaying-help-buffer))
        (fset 'with-displaying-help-buffer
              (symbol-function 'lg-momentary-help-display-buffer))
        (setq lg-momentary-help-mode t))
    ;; Turn it off
    (when lg-momentary-help-saved-with-displaying-help-buffer
      (fset 'with-displaying-help-buffer
            lg-momentary-help-saved-with-displaying-help-buffer))
    (setq lg-momentary-help-mode nil)))

;; Do M-x lg-try-luck RET before serious work
(defun lg-try-luck (&optional luck-arg)
  "Try your luck.
Prefix arg LUCK-ARG specifies luck parameter, default is 4."
  (interactive (list (prefix-numeric-value (or prefix-arg 4))))

  (if (= (truncate (* luck-arg (/ (random most-positive-fixnum)
                                  (float most-positive-fixnum))))
         (1- luck-arg))
      (progn (message "No luck for today :(") nil)
    (progn (message "You are lucky!") t)))

(defun lg-set-save-buffers-skip ()
  "Set `save-buffers-skip' to non-nil for current buffer."
  (interactive)
  (setq save-buffers-skip t))

(defun lg-count-lines ()
  "Count lines in region or buffer."
  (interactive)
  (if (region-active-p)
      (call-interactively #'count-lines-region)
    (call-interactively #'count-lines-buffer)))

(defun lg-count-words ()
  "Count words in region or buffer."
  (interactive)
  (if (region-active-p)
      (call-interactively #'count-words-region)
    (call-interactively #'count-words-buffer)))

(defun lg-count-matches (regexp)
  "Print number of matches for REGEXP in region or buffer."
  (interactive (list (read-from-minibuffer
		      "How many matches for (regexp): "
		      nil nil nil 'regexp-history)))
  (unwind-protect
      (save-excursion
        (when (region-active-p)
          (narrow-to-region (region-beginning) (region-end)))
        (goto-char (point-min))
        (count-matches regexp))
    (widen)))

;;}}}
;;{{{ `-- Emacs variables customization

;;; Misc
(icomplete-mode)
(add-hook 'after-init-hook
          (lambda ()
            (mouse-avoidance-mode 'none)))

;; No limit
(setq minibuffer-max-depth nil)

;; Recursive minibuffers is powerfull, but confusing for newbies!  Am
;; I newbie?
(setq enable-recursive-minibuffers t)

;; Default behaviour is a little strange
(setq next-line-add-newlines nil)

;; Display current line/column in modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; No jumping scrolling
(setq scroll-step 1)

(setq lg-scroll-lines 2)
(defun lg-scroll-up ()
  (interactive)
  (scroll-up lg-scroll-lines))
(defun lg-scroll-down ()
  (interactive)
  (scroll-down lg-scroll-lines))

;;; Yank text to the cursor position instead of mouse clicked point
(setq mouse-yank-at-point t)

;; No keys mapping warnings please
(setq display-warning-suppressed-classes '(key-mapping))

;; And do not popup to warning buffer
(setq inhibit-warning-display nil)

;; Display keys pressed immediatly
(setq echo-keystrokes 0.1)

;; Get rid of splash-screen
(setq inhibit-startup-message t)

;; DEL key deletes character forward
(setq delete-key-deletes-forward t)

;;; Major mode for *scratch*
(setq initial-major-mode 'lisp-interaction-mode)
(push '("\\*scratch-file\\*$" . lisp-interaction-mode) auto-mode-alist)

;;; Powerfull mode to handle tar.gz or even bzip2 archives
(auto-compression-mode 1)

;; for M-( - don't insert space before (
(setq parens-require-spaces nil)

;; Buffername state(*-modified,%-readonly,--otherwise) filename
(setq frame-title-format "Emacs: %b %+%+ %f")

;; Kill \n also if killing from the begining of line
(setq kill-whole-line t)

;;; Install auto-insertation on opening new file
(setq auto-insert t)
(setq auto-insert-query 'function)
(add-hook 'find-file-hooks 'auto-insert)

;;; Show more keys
(set-recent-keys-ring-size 600)
(setq view-lossage-key-count 600)

;; Configuration for calc that is on M-#
(setq calc-display-trail nil)
(setq calc-window-height 12)

;; Default 32 is too little, so set it to larger
(setq list-command-history-max 200)

;; Paren highlight mode
(defface paren-match
  '((((class color) (background dark))
     (:background "#223322"))
    (((class color) (background light))
     (:background "#CCDDCC"))
    (t (:background "#CCDDCC")))
  "Face to highlight for use by paren-mode.")

(paren-set-mode 'sexp t)
(make-variable-buffer-local 'paren-mode)

;; Set it
(setq mail-host-address "localhost")

;; Fix
(setq custom-face-default-form 'all)

;; Buffer local variables
(custom-set-variables
 '(progress-feedback-use-echo-area t)   ; use echo erea
 '(gutter-buffers-tab-visible-p nil)
 '(load-home-init-file t t)
 '(font-lock-mode t nil (font-lock)))

(set-specifier default-toolbar-visible-p nil)
(set-specifier menubar-visible-p nil)
(set-specifier vertical-scrollbar-visible-p nil)
(set-specifier horizontal-scrollbar-visible-p nil)
(set-specifier modeline-shadow-thickness 0)

;;; Enable some useful commands
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; set it for GNU Emacs
(unless (string-match "XEmacs" emacs-version)
  (set-language-environment "Cyrillic-KOI8"))

;; Try to silientify debugger
(add-to-list 'debug-ignored-errors "dynamic expansion")
(add-to-list 'debug-ignored-errors "Unbalanced parentheses")

;; Initialize one file updater
(require 'ofupd)

;;}}}
;;{{{ `-- Compilation customization

;;; compilation
(setq compile-command "make -k")
(setq compilation-scroll-output nil)
(setq compilation-window-height 10)
(setq compilation-ask-about-save nil)   ; do not ask about saving

;;; Use wonderfull mode-compile package
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)

(setq mode-compile-expert-p t)

(define-key global-map (kbd "C-c c c") 'mode-compile)
(define-key global-map (kbd "C-c c k") 'mode-compile-kill)

;;}}}
;;{{{ `-- Misc modes customization

;;; Pabbrev
;; <X-URL:http://homepages.cs.ncl.ac.uk/phillip.lord/download/emacs/pabbrev.el>
(ofupd-register
 "pabbrev" "~/.sxemacs/lisp/thirdpart/pabbrev.el"
 "http://homepages.cs.ncl.ac.uk/phillip.lord/download/emacs/pabbrev.el"
 #'ofupd-ve-version-colon)

(autoload 'pabbrev-mode "pabbrev" "Enable pabbrev mode.")

;;; Stripes mode
;; <X-URL:http://www.emacswiki.org/elisp/stripes.el>
(ofupd-register "stripes" "~/.sxemacs/lisp/thirdpart/stripes.el"
                "http://www.emacswiki.org/cgi-bin/wiki/download/stripes.el"
                #'ofupd-ve-version-colon)

(defface stripes-face nil "Face for stripes mode.")
(set-face-background 'stripes-face "gray76")

(autoload 'stripes-mode "stripes" nil t)
(autoload 'turn-on-stripes-mode "stripes" nil t)

;;;; --- Miscelance modes
(require 'comint)
(define-key comint-mode-map (kbd "C-M-l") 'switch-to-other-buffer)
(set-default 'comment-column 40)

(setq dabbrev-ignored-buffer-names
      '(" *Message-Log*" "*Messages*" "*Buffer List*" "*Ibuffer*"))

;;; Diff-mode
(require 'diff-mode)
(setq-default diff-switches "-u --show-c-function")
(set-face-foreground 'diff-added-face "green4")
(set-face-foreground 'diff-removed-face "red4")
(set-face-foreground 'diff-changed-face "yellow4")
(add-hook 'diff-mode-hook 'turn-on-font-lock)

;;; Matlab mode
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.ml\\'" . matlab-mode) auto-mode-alist))

(autoload 'rfc-view "rfc-view" "RFC mode" t)

;; List functions in popup menu, also display function at point in
;; modeline.
(autoload 'function-menu "func-menu" nil t)

;;; Info mode
;(custom-set-faces)

;(ad(custom-set-faces)rectory-list "/usr/share/info")

;; Load the auto-save.el package, which lets you put all of your autosave
;; files in one place, instead of scattering them around the file system.
(setq auto-save-directory (expand-file-name "~/.autosave/")
      auto-save-directory-fallback auto-save-directory
      auto-save-list-file-prefix "~/.autosave/saves/saves-"
      auto-save-hash-p nil
      auto-save-timeout 60              ; every minute
      auto-save-interval 200)

;; Powerfull re-builder package
(autoload 're-builder "re-builder" "Regexps Builder mode" t)

;; powerfull completion tool
(load-library "completer")

;; words completion "Meta-Shift-Tab"
;; Hmm, aspell does not find russian dictionary
;(when (executable-find "aspell")
;  (setq-default ispell-program-name "aspell"))

(setq ispell-personal-dictionary nil
      ispell-silently-savep t
      ispell-local-dictionary "russian")

;;; time-stamp files
(autoload 'time-stamp "time-stamp" "Update the time stamp in a buffer." t)

(setq time-stamp-format "%d/%m/%y %02H:%02M:%02S %u@%s")
(setq time-stamp-line-limit 12)         ; 8 -> 12
(add-hook 'write-file-hooks 'time-stamp)

;;; Filladapt mode just cool.
;; Comment part of filladapt says that it can't be autoloaded since it
;; substitutes existing Emacs functions, but it is not quite true, we
;; can unbound function and mark symbol as autoload.
(defvar filladapt-function-table
  (let ((assoc-list
         (list (cons 'fill-paragraph (symbol-function 'fill-paragraph))
               (cons 'fill-region (symbol-function 'fill-region))
               (cons 'fill-region-as-paragraph
                     (symbol-function 'fill-region-as-paragraph))
               (cons 'do-auto-fill (symbol-function 'do-auto-fill)))))
    ;; v18 Emacs doesn't have lisp-fill-paragraph
    (if (fboundp 'lisp-fill-paragraph)
        (nconc assoc-list
               (list (cons 'lisp-fill-paragraph
 			   (symbol-function 'lisp-fill-paragraph)))))
    assoc-list )
  "Table containing the old function definitions that filladapt usurps.")

(fmakunbound 'do-auto-fill)
(fmakunbound 'fill-paragraph)
(fmakunbound 'lisp-fill-paragraph)
(fmakunbound 'fill-region-as-paragraph)
(fmakunbound 'fill-region)

(autoload 'do-auto-fill "filladapt"
  "filladapt variant of `do-auto-fill'." t)
(autoload 'fill-paragraph "filladapt"
  "filladapt variant of `fill-paragraph'." t)
(autoload 'lisp-fill-paragraph "filladapt"
  "filladapt variant of `lisp-fill-paragraph'." t)
(autoload 'fill-region-as-paragraph "filladapt"
  "filladapt variant of `fill-region-as-paragraph'." t)
(autoload 'fill-region "filladapt"
  "filladapt variant of `fill-region'." t)
(autoload 'filladapt-mode "filladapt" "Filladapt mode" t)

;;; Visible Vertical Bar
;; <X-URL:http://www.geocities.com/gchen275/xemacs/>
(ofupd-register
 "vvb" "~/.sxemacs/lisp/vvb-mode.el"
 "http://www.gmdsoft.de/mitsch/emacs/emacs/fundamental/vvb-mode.el"
 #'ofupd-ve-version-colon)

(setq vvb-sticky-p t)
(autoload 'vvb-popup-menu "vvb-mode" nil t)
(autoload 'vvb-mode "vvb-mode" nil t)

;;; Make info looking nicer
(load "gc-info.el")

;;; Tramp configuration
(require 'tramp)
(setq tramp-terminal-type "emacs")
(setq tramp-default-method "scpx")
(add-to-list 'tramp-default-method-alist '("10.1.1.249" "root" "ssh"))

;;; Don't rescan
(setq imenu-auto-rescan nil)

;;; q - buries buffer in view mode
;(define-key view-mode-map [?q] 'bury-buffer)

;;; Use `iswitchb' instead of ugly `switch-to-buffer'.
(iswitchb-default-keybindings)
;; Pop to buffer in selected-window even if it is viewable in other one
(setq iswitchb-default-method 'samewindow)
(setq iswitchb-prompt-newbuffer nil)

;;; Checkdoc mode customization
(setq checkdoc-force-history-flag nil)  ; do not check for history or
					; changelog file

;;; Incredible folding packege
(setq folding-folding-on-startup nil)

(autoload 'folding-mode "folding" nil t)
(autoload 'folding-mode-find-file "folding")

(add-hook 'folding-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c 2") folding-mode-prefix-map)
            (local-set-key (kbd "C-c C-2") folding-mode-prefix-map)))

(add-hook 'find-file-hooks 'folding-mode-find-file t)

;;; ASCII code display
;; <X-URL:http://www.cpqd.com.br/~vinicius/emacs/Emacs.html>
(ofupd-register "ascii" "~/.sxemacs/lisp/thirdpart/ascii.el"
                "http://www.emacswiki.org/cgi-bin/emacs/download/ascii.el"
                #'ofupd-ve-version-colon)

(setq ascii-show-nonascii nil)
(autoload 'ascii-on "ascii" "Turn on ASCII code display." t)
(autoload 'ascii-off "ascii" "Turn off ASCII code display." t)
(autoload 'ascii-display "ascii" "Toggle ASCII code display." t)
(autoload 'ascii-customize "ascii" "Customize ASCII code display." t)

(unless (fboundp 'set-buffer-multibyte)
  (defalias 'set-buffer-multibyte 'ignore))

;;; Web interface to DICT
(autoload 'dict "dict-web" nil t)

(defadvice dict (before lg-dict-word activate)
  "Lookup word at point if not given."
  (interactive (list (let* ((w (funcall dictweb-guess-word-function))
                            (r (read-string
                                (format "Dict lookup%s: "
                                        (if w (concat " [" w "]") "")))))
                       (or (and w (string= r "") w) r)))))

;;; Interface to Online Lingvo dictionary
;; <X-URL:http://lgarc.narod.ru/xemacs/rdict.el>
;; <X-URL:http://emacswiki.org/cgi-bin/wiki/download/wordfreq.el>
(autoload 'rdict "rdict" nil t)
(autoload 'wordfreq-find "wordfreq")

(setq rdict-fill-column 72)
(defun lg-rdict-insert-wordfreq ()
  "Insert info about frequency of usage."
  (let ((wfreq (and rdict-current-word
                    (wordfreq-find rdict-current-word)))
        (buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (rdict-ins-faces
       (point)
       (progn (insert (or (and wfreq (format ", Frequency: %S" (cdr wfreq)))
                          ", Frequency: not in first 5000 words"))
              (point)) '(header-line)))
    (set-buffer-modified-p nil)))

(add-hook 'rdict-mode-hook 'lg-rdict-insert-wordfreq)

;;; List mode
;(setq display-fixed-width 85)
;(load "list-mode")

;;; Plain text-mode
(defun lg-ispell-region-or-buffer ()
  "Either spellcheck region or buffer."
  (interactive)
  (if (region-active-p)
      (ispell-region (region-beginning) (region-end))
    (ispell-buffer)))

(add-to-list 'auto-mode-alist '("TODO" . text-mode))
(add-to-list 'auto-mode-alist '("README" . text-mode))

(add-hook 'text-mode-hook 'filladapt-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

;; read my abbrevs
(add-hook 'text-mode-hook '(lambda () (abbrev-mode 1)))
(add-hook 'text-mode-hook
          (lambda ()
            (define-key text-mode-map
              (kbd "C-c e i") 'lg-ispell-region-or-buffer)))
(ignore-errors
  (quietly-read-abbrev-file))

;;; WYSIWYG Tabble mode
;; <X-URL:http://table.sourceforge.net/>
(autoload 'table-insert "table" nil t)
;; Disable warning message for (S)XEmacs
(put 'table-disable-incompatibility-warning 'xemacs t)

;;; reStructuredText mode
;; <X-URL:http://svn.berlios.de/viewcvs/*checkout*/docutils/trunk/docutils/tools/editors/emacs/rst.el>

;;Note: rst.el does not have proper Version identificator
;(ofupd-register "rst" "~/.sxemacs/lisp/thirdpart/rst.el"
;                "http://svn.berlios.de/viewcvs/*checkout*/docutils/trunk/docutils/tools/editors/emacs/rst.el"
;                #'ofupd-ve-version-colon)

(require 'rst)

(put-alist 'html '("rst2html" ".html" nil) rst-compile-toolsets)
(put-alist 'latex '("rst2latex" ".tex" nil) rst-compile-toolsets)
(put-alist 'oo '("rst2odt.py" ".odt" nil) rst-compile-toolsets)

;; Add support for sections in russian
(setq russian-letters
      (mapconcat
       #'identity
       (mapcar 'char-to-string
               (mapcar 'int-to-char
                       '(1456 1457 1458 1459 1460 1461 1441 1462 1463 1464
                              1465 1466 1467 1468 1469 1470 1471 1472 1473
                              1474 1475 1476 1479 1480 1478 1484 1483 1482
                              1485 1486 1487 1488 1489 1490 1491 1492 1493
                              1521 1494 1495 1496 1497 1498 1499 1500 1501
                              1502 1503 1504 1505 1506 1507 1508 1511 1512
                              1510 1516 1515 1514 1517 1518 1519)))
       ""))

(setq rst-section-text-regexp
      (concat "^[ \t]*\\S-*[a-zA-Z0-9" russian-letters "]\\S-*"))
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

(add-hook 'text-mode-hook 'rst-text-mode-bindings)
(add-hook 'text-mode-hook 'rst-set-paragraph-separation)

(add-hook 'rst-mode-hook 'turn-on-font-lock)
(add-hook 'rst-mode-hook
          (lambda ()
            (set (make-local-variable 'comment-start-skip) "\.\. ")
            (define-key rst-mode-map
              (kbd "C-c e i") 'lg-ispell-region-or-buffer)
            (define-key rst-mode-map
              (kbd "C-c ! a") 'lg-rst-insert-footnote)
            ))

(defun lg-rst-insert-footnote ()
  "Insert footnote at point."
  (interactive)
  (insert "[#]_")
  (goto-char (point-max))
  (unless (save-excursion (re-search-backward "^-----$" nil t))
    (insert "\n-----\n\n"))
  (insert ".. [#] "))

;; add `mode-compile' support
(require 'mode-compile)
(push '(rst-mode rst-compile kill-compilation) mode-compile-modes-alist)

(defun lg-sxemacsen-browse ()
  (interactive)
  (lg-browse-in-firefox-with-as
   (format "file:///mnt/ftp/%s.xhtml"
           (file-name-sans-extension
            (file-name-nondirectory buffer-file-name)))))

;; SXEmacsen blog special compiler
(defun lg-sxemacsen-setup ()
  "In case of sxemacsen blog, setup compilation parameters."
  (when (string-match "blog-sxemacsen"
                      (file-name-directory (buffer-file-name)))
    ;; Setup compilation parameters
    (make-variable-buffer-local 'rst-compile-toolsets)
    (make-variable-buffer-local 'rst-compile-primary-toolset)
    (setq rst-compile-primary-toolset 'xhtml)
    (setq rst-compile-toolsets '((xhtml "./rst2sxe.py" ".xhtml" nil)))

    (let ((fn (file-name-nondirectory buffer-file-name)))
      (set (make-local-variable 'compile-command)
           (format "./rst2sxe.py %s /mnt/ftp/%s.xhtml"
                   fn (file-name-sans-extension fn))))

    ;; Set special keys
    (local-set-key (kbd "C-c C-w") 'lg-sxemacsen-browse)
    (local-set-key (kbd "C-c c b") 'lg-sxemacsen-browse)
    (local-set-key (kbd "C-c c p") 'lg-sxemacsen-post)

    ;; Insert some comments in empty file
    (when (string= (buffer-substring) "")
      (insert (format ".. Copyright (C) %s Zajcev Evgeny\n"
                      (substring (current-time-string) -4)))
      (insert ".. $Id$\n"))
    ))

(add-hook 'rst-mode-hook 'lg-sxemacsen-setup)

(defvar sxemacsen-blogs
  '(("en SXEmacs-en" . "http://www.blogger.com/feeds/517942420965585111/posts/default")
    ("ru SXEmacsen" . "http://www.blogger.com/feeds/4022155443158713503/posts/default")
    ("sandbox" . "http://www.blogger.com/feeds/1458735071059128411/posts/default"))
  "My blogs to post in.")

(defvar lg-sxemacsen-generator "http://sxemacsen.blogspot.com")

(defvar lg-sxemacsen-template
  "<entry xmlns='http://www.w3.org/2005/Atom'>
  <generator url=\"%s\">%s</generator>
  <author> <name>%s</name> </author>
  <title mode=\"escaped\" type=\"text/html\">%s</title>
%s
  <content type='xhtml'>
    <div xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:m=\"http://www.w3.org/1999/xhtml\">
%s
    </div>
  </content>
</entry>")

(defun lg-sxemacsen-format-tags (tags)
  (mapconcat #'(lambda (tag)
                 (concat "<category scheme="
                         "'http://www.blogger.com/atom/ns#' term='"
                         tag "'/>"))
             (split-string tags " " t)
             ""))

;; Depends on g-client
(defun lg-sxemacsen-post (blog)
  "Create a new Blog post."
  (interactive
   (list (completing-read "Post to: "
                          (mapcar #'list (mapcar #'car sxemacsen-blogs))
                          nil t)))
  (require 'gblogger)
  (g-auth-ensure-token gblogger-auth-handle)
  (let ((ss (shell-command-to-string
             (format "./rst2sxe.py %s xxx release"
                     (file-name-nondirectory buffer-file-name))))
        title tags body)
    (with-temp-buffer
      (insert ss)
      (goto-char (point-min))
      (setq title (buffer-substring (point-at-bol) (point-at-eol)))
      (kill-line)
      (setq tags (buffer-substring (point-at-bol) (point-at-eol)))
      (kill-line 2)                     ; nuke title as well
      (setq body (buffer-substring)))

    (with-current-buffer (get-buffer-create "mmm"); with-temp-buffer
      (setq buffer-file-coding-system 'utf-8)
      (erase-buffer)
      (g-app-mode)
      (setq g-app-this-url (cdr (assoc blog sxemacsen-blogs))
            g-app-auth-handle gblogger-auth-handle)
      (insert
       (format lg-sxemacsen-template
               lg-sxemacsen-generator lg-sxemacsen-generator
               gblogger-author title
               (lg-sxemacsen-format-tags tags) body))

      (message "Publishing to %s..." blog)
;      (debug)
      (g-app-post-entry)

      ;; Check post status
      (if (string= (buffer-substring 1 21)
                   "HTTP/1.0 201 Created")
          (message "Publishing to %s... DONE!" blog)
        (let ((bc (buffer-substring))
              (bn "*Post Debug*"))
          (with-current-buffer (get-buffer-create bn)
            (erase-buffer)
            (insert bc))
          (switch-to-buffer bn)))
      )))

;;; Highlight marks mode
;; <X-URL: http://lgarc.narod.ru/himarks-mode.el>
(autoload 'himarks-mode "himarks-mode" "Toggle mark highlighting." t)
(autoload 'himarks-set-mark-command "himarks-mode"
  "Set mark at point command." t)
(autoload 'himarks-set-mark-no-activate "himarks-mode"
  "Set mark at point command, but do not activates mark region." t)

(defun lg-jump-to-next-mark ()
  (interactive)
  (himarks-set-mark-no-activate 1))

;;; Gnuplot mode
;; <X-URL:http://feff.phys.washington.edu/~ravel/software/gnuplot-mode/>
(add-to-list 'load-path (expand-file-name "~/.sxemacs/lisp/gnuplot-mode/"))

;; gnuplot uses info file when building keywords completion table
;(add-to-list 'Info-directory-list "~/.xemacs/lisp/gnuplot-mode.0.6.0")

(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

(setq auto-mode-alist
      (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

(defun lg-setup-gnuplot-mode ()
  (define-key gnuplot-mode-map (kbd "C-c C-s") 'gnuplot-show-gnuplot-buffer)
  (define-key gnuplot-mode-map (kbd "C-x C-e") 'gnuplot-send-line-to-gnuplot)
  (define-key gnuplot-mode-map (kbd "C-c e r") 'gnuplot-send-region-to-gnuplot)
  (define-key gnuplot-mode-map (kbd "C-c e b") 'gnuplot-send-buffer-to-gnuplot))

(add-hook 'gnuplot-mode-hook 'lg-setup-gnuplot-mode)

;;; GraphViz DOT mode
;; <X-URL:http://users.skynet.be/ppareit/projects/graphviz-dot-mode/graphviz-dot-mode.el>
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))

(defun lg-make-window-fit-displayed (&optional win)
  "Shrink the buffer's B window to fit its contents."
  (interactive)
  (let ((visible-height 0))
    (with-selected-window win
      (save-window-excursion
       (delete-other-windows)
       (unless (window-last-line-visible-height)
         (setq visible-height (max (* (face-height 'default) window-min-height)
                                   (window-displayed-text-pixel-height)))))

      (shrink-window-pixels
       (- (window-pixel-height) (face-height 'default) visible-height)))))

(defun lg-wand-display-file (&optional select)
  "Display current buffer using wand."
  (interactive "P")
  (let ((buf (Wand-display-noselect (buffer-file-name))))
    (lg-make-window-fit-displayed
     (if select (switch-to-buffer buf) (display-buffer buf t)))))

(defun lg-setup-graphviz-dot-mode ()
  "Install `C-c v', `C-c C-s' binding.
And display dot file using wand on save."
  (define-key graphviz-dot-mode-map (kbd "C-c v") 'lg-wand-display-file)
  (define-key graphviz-dot-mode-map (kbd "C-c C-s") 'lg-wand-display-file)
  (add-local-hook 'after-save-hook 'lg-wand-display-file))

(add-hook 'graphviz-dot-mode-hook 'lg-setup-graphviz-dot-mode)

;; Lisp mode
(defun lg-lisp-mode-hook ()
  "Adjust some variables in lisp-mode."
  (paren-set-mode 'sexp-surround t)
  (setq comment-start "; ")
  (setq block-comment-start ";; "))

(defun lg-lisp-comment-region (beg end &optional arg)
  "A little modification to `comment-region'.
BEG, AND and ARG arguments are the same as for `comment-region'."
  (interactive "r\nP")

  (let ((comment-start ";"))
    (comment-region beg end arg)))

(add-hook 'lisp-mode-hook 'lg-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'lg-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'lg-lisp-mode-hook)

(define-key lisp-mode-map (kbd "C-c C-c") 'lg-lisp-comment-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'lg-lisp-comment-region)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") 'lg-lisp-comment-region)

;; Lispdoc functionality
;; <X-URL:http://groups.google.com/group/comp.lang.lisp/msg/3808d9fea8d9e86b>
(defun lispdoc ()
  "Searches lispdoc.com for SYMBOL, which is by default the symbol
currently under the curser"
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
		 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point)
             (not symbol-at-point))
        (message "You didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
			  "full-text (f) or basic (b) search (default b)? ")))
	(browse-url (concat "http://lispdoc.com?q="
			    (if (string= inp "")
				default
			      inp)
			    "&search;="
			    (if (string-equal search-type "f")
				"full+text+search"
			      "basic+search")))))))

;;; Nice slime package to interact with inferiour lisp
;; <X-URL:http://common-lisp.net/project/slime/>
(add-to-list 'load-path (expand-file-name "~/.sxemacs/lisp/slime"))
(add-to-list 'load-path (expand-file-name "~/.sxemacs/lisp/slime/contrib"))
(require 'slime)

(setq slime-setup-contribs
      '(slime-fuzzy slime-repl slime-presentations slime-presentation-streams
        slime-indentation slime-parse slime-highlight-edits
        slime-mdot-fu slime-scratch))

(slime-setup-contribs)

(defun lg-slime-lispworks ()
  "Start using slime with lispworks."
  (interactive)
  (shell-command "~/.sxemacs/slime-lispworks.app")
  (slime-connect slime-lisp-host slime-port))

(setq inferior-lisp-program "sbcl"
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      common-lisp-hyperspec-root
      "file:///usr/local/share/doc/clisp-hyperspec/HyperSpec/")

(add-hook 'lisp-mode-hook #'(lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook #'(lambda () (inferior-slime-mode t)))

(add-hook 'slime-mode-hook
          #'(lambda ()
              (set (make-variable-buffer-local 'lisp-indent-function)
                   'common-lisp-indent-function)))

(add-hook 'slime-connected-hook
          (lambda ()
            (let ((ff (expand-file-name "~/lisp/boot/init.lisp")))
              (when (file-exists-p ff)
                (slime-load-file ff)))))

(define-key slime-mode-map (kbd "C-c C-s") 'slime-switch-to-output-buffer)
(define-key slime-mode-map (kbd "C-c c c") 'slime-compile-file)
(define-key slime-mode-map (kbd "C-c c d") 'slime-compile-defun)
(define-key slime-mode-map (kbd "C-c c r") 'slime-compile-region)
(define-key slime-mode-map (kbd "C-c c n") 'slime-list-compiler-notes)
(define-key slime-mode-map (kbd "C-c c k") 'slime-remove-notes)

(define-key slime-mode-map (kbd "C-c e r") 'slime-eval-region)
(define-key slime-mode-map (kbd "C-c e b")
  (lambda () (interactive) (slime-eval-region (point-min) (point-max))))
(define-key slime-mode-map (kbd "C-c e f") 'slime-eval-defun)
(define-key slime-mode-map (kbd "C-c e s") 'slime-eval-last-expression)
(define-key slime-mode-map (kbd "C-M-n") 'forward-list)

(defun lg-slime-selector ()
  "Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer. The `?' character describes the
available methods.

See `def-slime-selector-method' for defining new methods."
  (interactive)
  (let* ((ch (event-to-character (car (last (append (this-command-keys) nil)))))
         (method (find ch slime-selector-methods :key #'car)))
    (when (null method)
      (error "No method for character: ?\\%c" ch))
    (funcall (third method))))

(defun lg-slime-selector-no-method ()
  (interactive)
  (error (format "No method for character: %S"
                 (event-to-character
                  (car (last (append (this-command-keys) nil)))))))

(defvar lg-slime-selector-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'lg-slime-selector)
    (define-key map (kbd "d") 'lg-slime-selector)
    (define-key map (kbd "e") 'lg-slime-selector)
    (define-key map (kbd "i") 'lg-slime-selector)
    (define-key map (kbd "l") 'lg-slime-selector)
    (define-key map (kbd "r") 'lg-slime-selector)
    (define-key map (kbd "s") 'lg-slime-selector)
    (define-key map (kbd "t") 'lg-slime-selector)
    (define-key map (kbd "v") 'lg-slime-selector)
    (define-key map (kbd "?") 'lg-slime-selector)
    (set-keymap-default-binding map 'lg-slime-selector-no-method)
    (set-keymap-prompt map "Select [?cdeilrstv]: ")
    map))

(define-key global-map (kbd "<f2>") lg-slime-selector-map)

;; Uniquify buffer names in gentle manner
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Popup window with buffer list ; from XEmacs FAQ (2.61)
(defun cw-build-buffers ()
  "Popup buffer menu."
  (interactive "@")
  (run-hooks 'activate-menubar-hook)
  (popup-menu (car (find-menu-item current-menubar '("Buffers")))))

;;; Caml
(require 'caml)

(setq inferior-caml-program "ocaml")

(define-key caml-mode-map "\C-ci" nil)
(define-key caml-mode-map "\C-cb" nil)
(define-key caml-mode-map "\C-cw" nil)
(define-key caml-mode-map "\C-cf" nil)
(define-key caml-mode-map "\C-cl" nil)
(define-key caml-mode-map "\C-cm" nil)
(define-key caml-mode-map "\C-ct" nil)
(define-key caml-mode-map "\C-cib" 'caml-insert-begin-form)
(define-key caml-mode-map "\C-cif" 'caml-insert-for-form)
(define-key caml-mode-map "\C-cii" 'caml-insert-if-form)
(define-key caml-mode-map "\C-cil" 'caml-insert-let-form)
(define-key caml-mode-map "\C-cim" 'caml-insert-match-form)
(define-key caml-mode-map "\C-cit" 'caml-insert-try-form)
(define-key caml-mode-map "\C-ciw" 'caml-insert-while-form)
(define-key caml-mode-map "\C-c`" 'caml-goto-phrase-error)
(define-key caml-mode-map "\C-c\C-a" 'caml-find-alternate-file)
(define-key caml-mode-map "\C-c\C-c" 'comment-region)
(define-key caml-mode-map "\C-c\C-e" 'caml-eval-phrase)
(define-key caml-mode-map "\C-c\C-\[" 'caml-backward-to-less-indent)
(define-key caml-mode-map "\C-c\C-\]" 'caml-forward-to-less-indent)
(define-key caml-mode-map "\C-c\C-q" 'caml-indent-phrase)
(define-key caml-mode-map "\C-c\C-r" 'caml-eval-region)
(define-key caml-mode-map "\C-c\C-s" 'caml-show-subshell)
(define-key caml-mode-map "\M-\C-h" 'caml-mark-phrase)
(define-key caml-mode-map "\M-\C-q" 'caml-indent-phrase)
(define-key caml-mode-map "\M-\C-x" 'caml-eval-phrase)

(define-key caml-mode-map (kbd "C-c e r") 'caml-eval-region)
(define-key caml-mode-map (kbd "C-c e b") 'caml-eval-buffer)
(define-key caml-mode-map (kbd "C-x C-e") 'caml-eval-phrase)

(autoload 'caml-complete "caml-help" nil t)
(autoload 'caml-help "caml-help" nil t)
(define-key caml-mode-map (kbd "M-<tab>") 'caml-complete)
(define-key caml-mode-map (kbd "C-c C-h") 'caml-help)
(define-key caml-mode-map (kbd "C-c <tab>") 'caml-complete)

;; Nice vc package
(require 'vc)
(setq vc-follow-symlinks t)
(setq vc-diff-switches '("-u" "--show-c-function"))

;; Background for C-x v g
(setq vc-annotate-background "gray70")

(make-face 'vc-mode-highlight)
(set-face-background 'vc-mode-highlight "darkseagreen2")
(set-face-font 'vc-mode-highlight (face-font-name 'modeline))
(setq vc-mode-face 'vc-mode-highlight)

;;; Nice multiple regions package
;; <X-URL:http://lgarc.narod.ru/xemacs/multr.el>
(require 'multr)
(define-key global-map (kbd "C-M-m") mr-regions-map)

;;; Ibuffer
(require 'ibuffer)

;; My groups, use `/ R' to switch to group
(setq ibuffer-show-empty-filter-groups nil)

(defvar ibuffer-saved-filter-groups nil)
(ignore-errors
  (load-file (expand-file-name
              "ibuffer-private-filter-groups.el" user-init-directory)))
(add-to-list 'ibuffer-saved-filter-groups
             '("emacs"
               ("XWEM" (mode . emacs-lisp-mode) (filename . "xwem"))
               ("XLIB" (mode . emacs-lisp-mode) (filename . "xlib"))
               ("SLIME" (name . "^\\*slime"))
               ("Elisp" (mode . emacs-lisp-mode))
               ("Lisp" (mode . lisp-mode))
               ("Documentation" (or (mode . Info-mode)
                                    (mode . Manual-mode)
                                    (filename . "\\.txt")))
               ;; Various modes
               ("CVS" (or (mode . cvs-mode) (mode . cvs-edit-mode)
                          (name . "^\\*cvs")))
               ("ChangeLog" (mode . change-log-mode))
               ("Dired" (mode . dired-mode))
	       ("Chat" (or (mode . erc-mode)
                           (mode . jabber-chat)
                           (mode . jabber-roster)))
	       ("W3M" (mode . w3m-mode))
               ))
(add-to-list 'ibuffer-saved-filter-groups
             '("modes"
               ("Wand" (mode . Wand-mode))
               ("reST" (mode . reST))
               ("Dired" (mode . dired-mode))
               ("C" (mode . c-mode))
               ("Makefile" (mode . makefile-mode))
               ("ChangeLog" (mode . change-log-mode))
               ("Lisp" (mode . lisp-mode))
               ("CVS/SVN" (or (mode . cvs-mode) (mode . svn-status-mode)
                              (mode . svn-prop-edit-mode)))
               ("Emacs Lisp" (mode . emacs-lisp-mode))
               ("Python" (mode . python-mode))
               ("Pyrex" (mode . pyrex-mode))
               ("LaTeX" (mode . latex-mode))
               ("Manual" (mode . Manual-mode))
               ("Info" (mode . Info-mode))
               ("Help" (or (mode . help-mode) (mode . hyper-apropos-mode)
                           (mode . hyper-apropos-help-mode)))
               ("Completion List" (mode . completion-list-mode))
               ("Comint" (mode . comint-mode))
               ("Custom" (mode . custom-mode))
               ("Fundamental" (mode . fundamental-mode))
               ("ERC" (mode . erc-mode))
               ("Jabber" (or (mode . jabber-chat) (mode . jabber-roster)))
               ("W3M" (mode . w3m-mode))
               ("Gnus" (or (mode . gnus-group-mode)
                           (mode . gnus-summary-mode)
                           (mode . gnus-article-mode)
                           (mode . message-mode)))
               ))

;; SealHunter development
(add-to-list 'ibuffer-saved-filter-groups
             '("sh"
               ("SealHunter" (mode . python-mode) (filename . "sealhunter"))
               ("Textures" (filename . "textures"))
               ("Misc" (filename . "sealhunter"))))

(setq ibuffer-tmp-hide-regexps
      (list "^\\*\\(Help\\|Customize\\|Compile\\)"))

(add-hook 'ibuffer-mode-hooks
          (lambda ()
            (setq ibuffer-filter-groups
                  (cdar ibuffer-saved-filter-groups))))

;; Add <TAB>, Shift-<TAB> keys to navigate thgrouht groups.
(defvar lg-ibuffer-error-nomove t
  "Non-nil mean issue error if move is not done.")

(defvar lg-ibuffer-nomove-msg "Ibuffer: No move")

(defun lg-ibuffer-next-group (arg)
  "Goto ARG next group."
  (interactive "p")

  (let (pos)
    (save-excursion
      ;; Only in case when moving backward
      (when (and (< arg 0)
                 (re-search-backward "^\\[" nil t))
        (goto-char (match-beginning 0)))

      (while (not (zerop arg))
        (if (funcall (if (< arg 0) 're-search-backward
                       're-search-forward) "^\\[" nil t)
            (progn
              (setq pos (match-beginning 0))
              (if (< arg 0) (incf arg) (decf arg)))

          ;; Not found, so break
          (setq arg 0))))

    (cond (pos (goto-char pos) (ibuffer-forward-line))
          (lg-ibuffer-error-nomove (error lg-ibuffer-nomove-msg)))))

(defun lg-ibuffer-prev-group (arg)
  "Goto ARG previous group."
  (interactive "p")
  (lg-ibuffer-next-group (- arg)))

;; Ignore our error, when debuging
(add-to-list 'debug-ignored-errors lg-ibuffer-nomove-msg)

;; Release C-o key for -other- commands
(define-key ibuffer-mode-map (kbd "C-o") nil)
(define-key ibuffer-mode-map (kbd "C-O")
  'ibuffer-visit-buffer-other-window-noselect)
(define-key ibuffer-mode-map (kbd "S-<RET>")
  'ibuffer-visit-buffer-other-window-noselect)

(define-key ibuffer-mode-map (kbd "<TAB>") 'lg-ibuffer-next-group)
(define-key ibuffer-mode-map (kbd "S-<TAB>") 'lg-ibuffer-prev-group)


;;; List registers
;; <X-URL:http://www.bookshelf.jp/elc/list-register.el>
(ofupd-register "list-register" "~/.sxemacs/lisp/thirdpart/list-register.el"
                "http://www.bookshelf.jp/elc/list-register.el")

(autoload 'list-register "list-register" "List registers." t)

;;; Sudoku mode
;; <X-URL:http://lgarc.narod.ru/xemacs/sudoku.el>
(autoload 'sudoku "sudoku" "Start playing sudoku." t)
(autoload 'sudoku-sdk-file-p "sudoku" "Non-nil if file is sdk." t)

(setq sudoku-level 'evil)
(setq sudoku-download nil)
(setq sudoku-style 'pseudo)
(setq sudoku-font-size 48)

(push '(sudoku-sdk-file-p . sudoku-load-puzzle-noselect)
      find-file-magic-files-alist)

;; Enable autoinserter
(setq sudoku-autoinsert-mode t)
(add-hook 'sudoku-after-change-hook 'sudoku-autoinsert)

(add-hook 'sudoku-mode-hook
          (lambda ()
            (remove-face-property 'default 'background-pixmap
                                  (current-buffer))))

;;; Enable displaying total lines and total columns in modeline
;; <X-URL:http://lgarc.narod.ru/xemacs/cocol.el>
;(require 'cocol) ; MULE
;(cocol-turn-on-mode) ; MULE

;;; Dot-mode (aka vi's .)
;; <X-URL:http://www.emacswiki.org/cgi-bin/emacs/download/dot-mode.el>
(ofupd-register "dot-mode" "~/.sxemacs/lisp/dot-mode.el"
                "http://www.emacswiki.org/cgi-bin/emacs/download/dot-mode.el"
                #'(lambda (buf)
                    (with-current-buffer buf
                      (goto-char (point-min))
                      (when (re-search-forward
                             "dot-mode-version \"\\([^\"]*\\)\"" nil t)
                        (match-string 1)))))

;; Installs bindings: `C-.', `C-M-.' and `C-c .'
(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)

(defun lg-dot-mode-apply-to-region-lines (begin end)
  "Similiar to `apply-macro-to-region-lines', but for `dot-mode-execute'."
  (interactive "r")
  (if (null dot-mode-cmd-buffer)
      (message "Nothing to repeat")

    ;; Don't want execution to kick off infinite recursion
    (remove-hook 'pre-command-hook 'dot-mode-pre-hook t)
    (remove-hook 'post-command-hook 'dot-mode-loop t)
    (remove-hook 'after-change-functions 'dot-mode-after-change t)

    (apply-macro-to-region-lines begin end dot-mode-cmd-buffer)

    ;; Put the hooks back
    (make-local-hook 'pre-command-hook)
    (make-local-hook 'post-command-hook)
    (make-local-hook 'after-change-functions)
    (add-hook 'pre-command-hook 'dot-mode-pre-hook nil t)
    (add-hook 'post-command-hook 'dot-mode-loop nil t)
    (add-hook 'after-change-functions 'dot-mode-after-change nil t)))

(defun lg-dot-mode-execute (n)
  "Execute either `dot-mode-execute' or `lg-dot-mode-apply-to-region-lines'."
  (interactive "p")
  (if (region-active-p)
      (dotimes (i n)
        (lg-dot-mode-apply-to-region-lines
         (region-beginning) (region-end)))
    (dotimes (i n)
      (dot-mode-execute))))

(define-key dot-mode-map (kbd "C-.") 'lg-dot-mode-execute)

;;; Gnuserv
(defun lg-str-with-faces (str face-list)
  "Return STR with applied FACE-LIST."
  (let ((ext (make-extent 0 (length str) str)))

    (set-extent-property ext 'duplicable t)
    (set-extent-property ext 'unique t)
    (set-extent-property ext 'start-open t)
    (set-extent-property ext 'end-open t)
    (set-extent-property ext 'face face-list))
  str)

(defun lg-gnuserv-visit ()
  (set-specifier default-gutter-visible-p t (selected-frame))
  (let* ((gstr (concat
                (lg-str-with-faces
                 (format "Buffer: %s, " (buffer-name (current-buffer)))
                 '(bold))
                (lg-str-with-faces
                 "`C-x #'"
                 '(bold blue))
                (lg-str-with-faces
                 " when done"
                 '(bold))))
         (sp-len -1))
    (when (> sp-len 0)
      (setq gstr (concat gstr (lg-str-with-faces
                               (make-string sp-len ? )
                               '(bold zmacs-region)))))
    (set-specifier top-gutter gstr (selected-frame))
    (set-specifier top-gutter-border-width 2 (selected-frame))))

(add-hook 'gnuserv-visit-hook 'lg-gnuserv-visit)

;;; Browse kill ring
;; <X-URL:http://www.todesschaf.org/files/browse-kill-ring.el>
(ofupd-register "browse-kill-ring" "~/.sxemacs/lisp/browse-kill-ring.el"
                "http://www.todesschaf.org/files/browse-kill-ring.el"
                #'ofupd-ve-version-colon)

;; Installs `M-y' binding to run `browse-kill-ring' command.
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; Puts kills into X cutbuffer
(add-hook 'kill-hooks 'x-store-cutbuffer)

;;; Erlang mode
(autoload 'erlang-mode "erlang")
(add-hook 'erlang-new-file-hook 'tempo-template-erlang-small-header)
(add-hook 'erlang-mode-hook
          (lambda ()
            (define-key erlang-mode-map (kbd "M-<BS>") 'backward-kill-word)))
(push '("\\.erl$"  . erlang-mode) auto-mode-alist)

;; Perl mode
(setq cperl-indent-level 8)

;;; Haskell mode
(push (expand-file-name "~/.cabal/bin") exec-path)
(setenv "PATH" (concat (expand-file-name "~/.cabal/bin")
                       ":" (getenv "PATH")))
(push (expand-file-name "~/.sxemacs/lisp/thirdpart/haskell-mode-2.4")
      load-path)

(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hi$"     . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))

(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
  "Major mode for editing literate Haskell scripts." t)

(autoload 'turn-on-haskell-doc-mode "haskell-doc" nil t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode) ; haskell-mode

(autoload 'turn-on-haskell-ghci "haskell-ghci" nil t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(autoload 'haskell-ghci-load-file "haskell-ghci" nil t)

(autoload 'turn-on-haskell-indent "haskell-indent" nil t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;(add-hook 'haskell-ghci-mode-hook
;          (lambda ()
;            (local-set-key (kbd "C-M-l") 'switch-to-other-buffer)
;            ))

;; HLint compilation
(defvar hs-lint-save-files t
  "Save modified files when run HLint or no (ask user)")

(defun hs-lint-compile (&optional prfxarg)
  "Run compile."
  (interactive "P")
  (save-some-buffers hs-lint-save-files)
  (set (make-local-variable 'compile-command)
       (concat "hlint" " " buffer-file-name))
  (if (or compilation-read-command current-prefix-arg)
      (call-interactively 'compile)
    (compile compile-command)))

(push '(haskell-mode hs-lint-compile kill-compilation)
      mode-compile-modes-alist)

(defun lg-haskell-eval-region (b e)
  (interactive "r")
  (let ((bs (buffer-substring b e)))
    (with-current-buffer "*ghci*"
      (haskell-ghci-send bs)))
  (display-buffer "*ghci*"))

(add-hook 'haskell-mode-hook
   (lambda ()
    (define-key haskell-mode-map (kbd "C-c e r") 'lg-haskell-eval-region)
    (define-key haskell-mode-map (kbd "C-c e b") 'haskell-ghci-load-file)
    (define-key haskell-mode-map (kbd "C-x C-e") 'lg-haskell-eval-phrase)
   ))

;;; Boxquote
;; <X-URL:http://www.davep.org/emacs/boxquote.el>
(ofupd-register "boxquote" "~/.sxemacs/lisp/thirdpart/boxquote.el"
                "http://www.davep.org/emacs/boxquote.el"
                #'ofupd-ve-version-colon)

(autoload 'boxquote-region "boxquote" nil t)
(setq boxquote-top-corner ".")
(setq boxquote-top-and-tail "-----")

;;; Eukleides
;; <X-URL:http://emacswiki.org/cgi-bin/wiki/download/eukleides.el>
(autoload 'eukleides-mode "eukleides" nil t)
(unless (rassq 'eukleides-mode auto-mode-alist)
  (push '("\\.euk$" . eukleides-mode) auto-mode-alist))
(add-hook 'eukleides-mode-hook 'eldoc-mode)

;;; Pov-Ray mode
;; <X-URL:http://www.acc.umu.se/~woormie/povray/>
;; Set autoloading of POV-mode for these file-types.
(autoload 'pov-mode "pov-mode" "PoVray scene file mode" t)
(add-to-list 'auto-mode-alist '("\\.pov$" . pov-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . pov-mode))

;; Jabber
(push (expand-file-name "~/dev/emacs/emacs-jabber") load-path)
(setq jabber-message-alert-same-buffer nil)
(autoload 'jabber-connect "jabber" "Connect to jabber server." t)
(add-hook 'jabber-post-connect-hook 'jabber-keepalive-start)

;;}}}

;;{{{ +-  Various Modes customization

;;{{{   `-- RUSSIAN customization

;(require 'ru-keys)

;(ru-keys-setup)

;; Define M-CyrillicXX keys

(defconst lg-rukeys-dvorak-phonetic-layout
  '((Cyrillic_IO . ?~) (Cyrillic_io . ?`) (Cyrillic_SHORTI . ??)
    (Cyrillic_shorti . ??)
(Cyrillic_TSE . ?C) (Cyrillic_tse . ?c) (Cyrillic_U . ?U) (Cyrillic_u . ?u)
(Cyrillic_KA . ?K) (Cyrillic_ka . ?k) (Cyrillic_IE . ?E) (Cyrillic_ie . ?e)
(Cyrillic_EN . ?N) (Cyrillic_en . ?n) (Cyrillic_GHE . ?G) (Cyrillic_ghe . ?g)
(Cyrillic_SHA . ?{) (Cyrillic_sha . ?[) (Cyrillic_SHCHA . ?})
(Cyrillic_shcha . ?])  (Cyrillic_ZE . ?Z) (Cyrillic_ze . ?z) (Cyrillic_HA . ?H)
 (Cyrillic_ha . ?h) (Cyrillic_HARDSIGN . ? ) (Cyrillic_hardsign . ??)
(Cyrillic_EF . ?F)
(Cyrillic_ef . ?f) (Cyrillic_YERU . ?Y) (Cyrillic_yeru . ?y) (Cyrillic_VE . ?V)
(Cyrillic_ve . ?v) (Cyrillic_A . ?A) (Cyrillic_a . ?a) (Cyrillic_PE . ?P)
(Cyrillic_pe . ?p) (Cyrillic_ER . ?R) (Cyrillic_er . ?r) (Cyrillic_O . ?O)
(Cyrillic_o . ?o) (Cyrillic_EL . ?L) (Cyrillic_el . ?l) (Cyrillic_DE . ?D)
(Cyrillic_de . ?d) (Cyrillic_ZHE . ?V) (Cyrillic_zhe . ?v) (Cyrillic_E . ?|)
(Cyrillic_e . ?\\) (Cyrillic_YA . ?Q) (Cyrillic_ya . ?q) (Cyrillic_CHE . ?+)
(Cyrillic_che . ?=) (Cyrillic_ES . ?S) (Cyrillic_es . ?s) (Cyrillic_EM . ?M)
(Cyrillic_em . ?m) (Cyrillic_I . ?I) (Cyrillic_i . ?i) (Cyrillic_TE . ?T)
(Cyrillic_te . ?t) (Cyrillic_SOFTSIGN . ?X) (Cyrillic_softsign . ?x)
(Cyrillic_BE . ?B) (Cyrillic_be . ?b) (Cyrillic_YU . ?~) (Cyrillic_yu . ?`)))

(defconst lg-rukeys-dvorak-jcuken-layout
  '((Cyrillic_IO . ?~) (Cyrillic_io . ?`)
 (Cyrillic_SHORTI . ?\") (Cyrillic_shorti . ?') (Cyrillic_TSE
. ?<) (Cyrillic_tse . ?,) (Cyrillic_U . ?>) (Cyrillic_u . ?.)
(Cyrillic_KA . ?P) (Cyrillic_ka . ?p) (Cyrillic_IE . ?Y) (Cyrillic_ie
. ?y) (Cyrillic_EN . ?F) (Cyrillic_en . ?f) (Cyrillic_GHE . ?G)
(Cyrillic_ghe . ?g) (Cyrillic_SHA . ?C) (Cyrillic_sha . ?c)
(Cyrillic_SHCHA . ?R)  (Cyrillic_shcha . ?r)  (Cyrillic_ZE . ?L)
(Cyrillic_ze . ?l) (Cyrillic_HA . ??) (Cyrillic_ha . ?/)
(Cyrillic_HARDSIGN . ?+) (Cyrillic_hardsign . ?=) (Cyrillic_EF . ?A)
(Cyrillic_ef . ?a) (Cyrillic_YERU . ?O) (Cyrillic_yeru . ?o)
(Cyrillic_VE . ?E) (Cyrillic_ve . ?e) (Cyrillic_A . ?U) (Cyrillic_a
. ?u) (Cyrillic_PE . ?I) (Cyrillic_pe . ?i) (Cyrillic_ER . ?D)
(Cyrillic_er . ?d) (Cyrillic_O . ?H) (Cyrillic_o . ?h) (Cyrillic_EL
. ?T) (Cyrillic_el . ?t) (Cyrillic_DE . ?N) (Cyrillic_de . ?n)
(Cyrillic_ZHE . ?S) (Cyrillic_zhe . ?s) (Cyrillic_E . ?_) (Cyrillic_e
. ?-) (Cyrillic_YA . ?:) (Cyrillic_ya . ?\;) (Cyrillic_CHE . ?Q)
(Cyrillic_che . ?q) (Cyrillic_ES . ?J) (Cyrillic_es . ?j) (Cyrillic_EM
. ?K) (Cyrillic_em . ?k) (Cyrillic_I . ?X) (Cyrillic_i . ?x)
(Cyrillic_TE . ?B) (Cyrillic_te . ?b) (Cyrillic_SOFTSIGN . ?M)
(Cyrillic_softsign . ?m) (Cyrillic_BE . ?W) (Cyrillic_be . ?w)
(Cyrillic_YU . ?V) (Cyrillic_yu . ?v)))

(defconst lg-rukeys-engkeys-alist lg-rukeys-dvorak-jcuken-layout)

(mapc #'(lambda (cc)
          (global-set-key
              (vector (list 'meta (car cc)))
            (vector (list 'meta (cdr cc)))))
      lg-rukeys-engkeys-alist)

;;}}}
;;{{{   `-- AUCTEX customization

;;; Auctex configuration
(setq tex-dvi-print-command "dvips")    ; for M-x tex-print RET
(setq tex-dvi-view-command "xdvi")      ; for M-x tex-view RET

(require 'tex-site)

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq LaTeX-enable-toolbar nil)

;; Whizzytex
;; <X-URL:http://pauillac.inria.fr/whizzytex>
(push "/usr/local/share/whizzytex/xemacs" load-path)

(setq whizzy-key-bindings 'whizzy-short-bindings)

(autoload 'whizzytex-mode "whizzytex"
  "WhizzyTeX, a minor-mode WYSIWYG environment for LaTeX" t)

;;}}}
;;{{{   `-- XREF customization

;; XRefactory configuration

(defvar xref-current-project nil)       ; can be also "my_project_name"
(defvar xref-key-binding 'global)       ; can be also 'local or 'none

(defun xrefactory ()
  "Load xrefactory."
  (interactive)
  (push "/usr/home/dusr5/down/xref/xemacs" load-path)
  (load "xrefactory"))

;;}}}
;;{{{   `-- SPEEDBAR customization

(autoload 'speedbar "speedbar" "Enable or disable speedbar" t)

(setq speedbar-supported-extension-expressions
      '(".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".f\\(90\\|77\\|or\\)?" ".ada" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".s?html" "[Mm]akefile\\(\\.in\\|\\.am\\)?" "configure\\.in?" ".conf?"))

;; Dont use images
(setq-default speedbar-use-images nil)

;; Dont create sub-lists in tags
(setq speedbar-tag-split-minimum-length 120)

;; Bind button1 to select item
(add-hook 'speedbar-load-hook
          (lambda ()
            (define-key speedbar-key-map [button1] 'dframe-click)))

(defun lg-make-speedbar-frame ()
  "Makes font of speedbar frame to be \"fixed\""
  (interactive)
  (set-face-property 'default 'font "fixed" (speedbar-current-frame)))

;;}}}
;;{{{   `-- FONTLOCK customization

;; always use font-lock-mode because it looks nicer
;; use lazy-lock bacause of speed
;;(add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
;;(turn-on-font-lock)

;; Turn on lazy-shot mode, because of speed
(setq lazy-shot-verbose nil)            ; shut it up
(add-hook 'font-lock-mode-hook 'turn-on-lazy-shot)

;(add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
(add-hook 'lisp-interaction-mode-hook 'turn-on-font-lock)

(set-face-foreground 'font-lock-builtin-face "saddlebrown")

;;}}}
;;{{{   `-- SEMANTIC bovinator configuration

;; XEmacs have no such funcs (it needed by semanticdb when
;; semanticdb-default-save-directory is not nil
;(unless (fboundp 'subst-char-in-string)
;  (defun subst-char-in-string (old-char new-char str)
;    "Replaces any occurance of old-char with new-char in str"
;    (replace-in-string str (char-to-string old-char) (char-to-string new-char))))

;(unless (fboundp 'replace-regexp-in-string)
;  (defun replace-regexp-in-string (regx subst str)
;    "Replaces regx by subst in str"
;    (replace-in-string str regx subst)))

;;; Load semantic
;(load-file "/usr/local/lib/xemacs/site-lisp/cedet/common/cedet.el")
;(semantic-load-enable-code-helpers)

;;; Use semantic package
;(require 'semantic-load)
;(setq semantic-load-turn-everything-on t)
;; remove stupid dirty-mode
;(setq global-semantic-show-dirty-mode nil)

;(setq semantic-auto-parse-working-in-modeline-flag nil)
;(setq semantic-auto-parse-no-working-message nil)
;(setq semantic-auto-parse-max-buffer-size 30720)
;(setq semantic-auto-parse-idle-time 120) ; autoparse once per 2 minutes

;;; use ~/semdb.cache for caching
;(require 'semanticdb)
;(global-semanticdb-minor-mode 1)
;; set directory where to store semantic files
;(setq semanticdb-default-save-directory (expand-file-name "~/semdb.cache"))

;(setq semanticdb-persistent-path (list 'never))
;(setq semanticdb-project-roots
;      (list (expand-file-name "/tmp/iw/src")
;            (expand-file-name "/tmp/old/src")))

;; Emacs Code Browser
;(add-to-list 'load-path "/usr/local/lib/xemacs/site-lisp/ecb")
;(require 'ecb)

;;}}}
;;{{{   `-- EDIFF configuration

(autoload 'ediff "ediff")
;; don't use this ugly frames .. display all in one please
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Some custom configuration to ediff
(defvar lg-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom lg-ediff-bwin-reg ?b
  "*Register to be setuped to hold `lg-ediff-bwin-config'
configuration.")

(defvar lg-ediff-awin-config nil "Window configuration after ediff.")
(defcustom lg-ediff-awin-reg ?e
  "*Register to be used to hold `lg-ediff-awin-config' window
configuration.")

(defun lg-ediff-bsh ()
  "Function to be called before any buffers or window setup for
ediff."
  (setq lg-ediff-bwin-config (current-window-configuration))
  (when (characterp lg-ediff-bwin-reg)
    (set-register lg-ediff-bwin-reg
                  (list lg-ediff-bwin-config (point-marker)))))

(defun lg-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq lg-ediff-awin-config (current-window-configuration))
  (when (characterp lg-ediff-awin-reg)
    (set-register lg-ediff-awin-reg
                  (list lg-ediff-awin-config (point-marker)))))

(defun lg-ediff-qh ()
  "Function to be called when ediff quits."
  (when lg-ediff-bwin-config
    (set-window-configuration lg-ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook 'lg-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'lg-ediff-ash)
(add-hook 'ediff-quit-hook 'lg-ediff-qh)

;;}}}
;;{{{   `-- HIGHLINE customization

(setq highline-priority 1000)

;;; Highlight current line highline.el customization
(autoload 'highline-on "highline" nil t)
(autoload 'highline-off "highline" nil t)
(autoload 'highline-local-mode "highline" nil t)

(defface highlight-line-face
  '((((class color) (background dark))
     (:background "navy"))
    (((class color) (background light))
     (:background "greenyellow"))
    (t (:background "greenyellow")))
  "Face for highline mode.")
(setq highline-face 'highlight-line-face)

;;; Use highline in several major modes by default
(add-hook 'ibuffer-hooks 'highline-on)
(add-hook 'cvs-mode-hook 'highline-on)

(add-hook 'gnus-group-prepare-hook 'highline-on)
(add-hook 'gnus-summary-prepare-hook 'highline-on)

;;}}}
;;{{{   `-- ELDOC customization

;;; Use powerfull eldoc packege
(autoload 'turn-on-eldoc-mode "eldoc" "Turn on eldoc mode." t)

;; Fast refresh
(setq eldoc-idle-delay 0.1)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;}}}
;;{{{   `-- TPUM customization

;;; Use text popup menus
(require 'tpum)
(setq tpum-cstyle 'tpum-style-pseudo)
(setq tpum-menu-type 'inline)
(tpum-global-mode 1)                    ; enable tpum global mode

;;; Emacs events emulation
(define-tpum-key global-map (kbd "M-<f1>") (kbd "C-<button1>"))
(define-tpum-key global-map (kbd "M-<f2>") (kbd "<button3>"))

(define-tpum-key global-map (kbd "M-<f5>") (kbd "S-<button3>"))
(define-tpum-key global-map (kbd "M-<f6>") (kbd "S-<button1>"))

(define-tpum-key global-map (kbd "s-1") (kbd "C-<button1>"))
(define-tpum-key global-map (kbd "s-2") (kbd "S-<button3>"))
(define-tpum-key global-map (kbd "s-3") (kbd "<button3>"))
(define-tpum-key global-map (kbd "s-4") (kbd "C-<button3>"))
(define-tpum-key global-map (kbd "s-!") (kbd "S-<button1>"))

(define-tpum-key global-map (kbd "C-!") (kbd "C-<button1>"))
(define-tpum-key global-map (kbd "C-#") (kbd "C-<button3>"))
(define-tpum-key global-map (kbd "C-$") (kbd "S-<button3>"))
(define-tpum-key global-map (kbd "C-%") (kbd "S-<button1>"))

;;}}}
;;{{{   `-- CALENDAR customization

(autoload 'calendar "calendar" nil t)
(defalias 'cal 'calendar)

(defun lg-cal-load-hook ()
  "Hook to customize calendar when it loaded."
  (custom-set-face-bold 'diary t)
  (set-face-background 'diary "red")
  (custom-set-face-bold 'calendar-today-face t)
  (set-face-background 'holiday-face "DarkSeaGreen3"))

(add-hook 'calendar-load-hook 'lg-cal-load-hook)

;; Customize calendar variables
(setq diary-file
      (expand-file-name "diary" user-init-directory))
(setq calendar-location-name "Moscow")
(setq calendar-week-start-day 1)
(setq european-calendar-style t)
(setq calendar-time-display-form
      '(24-hours ":" minutes
                 (if time-zone " (") time-zone (if time-zone ")")))

(setq mark-diary-entries-in-calendar t)
(setq mark-holidays-in-calendar t)

(setq cal-tex-24 t)
(setq cal-tex-daily-start 7)
(setq cal-tex-daily-end 23)
(setq cal-tex-diary t)

(setq today-visible-calendar-hook 'calendar-mark-today)

(defface lgcal-freeday-face nil nil)
(set-face-background 'lgcal-freeday-face "pink")

(defun lgcal-hightlight-free-day (date &optional face)
  "Highlight DATE with FACE if DATE is free-day.
Default face is `lgcal-freeday-face'."
  (when (and (calendar-date-is-visible-p date)
             (member (calendar-day-of-week date) '(0 6)))
    (save-excursion
      (calendar-goto-date date)
      (set-extent-properties (make-extent (1- (point)) (1+ (point)))
                             (list 'face (or face 'lgcal-freeday-face)
                                   'priority -1)))
    ))

(defun lgcal-mark-freedays ()
  "Scan Calendar buffer and highlight freedays.
Prefix ARG specifies number of weeks to highlight."
  (interactive)

  (save-excursion
    (beginning-of-buffer)

    ;; Process first week
    (calendar-end-of-week 1)
    (let* ((date (calendar-cursor-to-nearest-date))
           (day (car (cdr date))))
      (when (> day 7)
        (calendar-backward-week 1))
      ;; Refresh date and day
      (setq date (calendar-cursor-to-nearest-date))
      (setq day (car (cdr date)))
      (lgcal-hightlight-free-day date)

      (unless (= day 1)
        (calendar-backward-day 1)
        (lgcal-hightlight-free-day (calendar-cursor-to-nearest-date))
        (calendar-end-of-week 1)))

    ;; Process other monthes
    (let* ((date (calendar-cursor-to-nearest-date))
           (absdate (+ 6 (calendar-absolute-from-gregorian date)))
           (gregd (calendar-gregorian-from-absolute absdate)))
      (while (calendar-date-is-visible-p gregd)
        (lgcal-hightlight-free-day gregd)

        (setq gregd (calendar-gregorian-from-absolute (+ absdate 1)))
        (lgcal-hightlight-free-day gregd)

        (setq absdate (+ 7 absdate))
        (setq gregd (calendar-gregorian-from-absolute absdate))))
    ))

;; Advice `mark-calendar-holidays' so it will highlight freedays as
;; well.
(defadvice mark-calendar-holidays (after highlight-free-days activate)
  "Highlight freedays as well."
  (lgcal-mark-freedays))

;; Daily journal notes (`j' in calendar)
;; <X-URL:http://home.midsouth.rr.com/svartulf/emacs/daily-journal/>
(setq daily-journal-default-filename
      (expand-file-name ".daily-journal" user-init-directory))

(autoload 'daily-journal-edit-calendar-entry "daily-journal" nil t)
(add-hook 'calendar-load-hook
          (lambda ()
            (define-key calendar-mode-map
              "j" 'daily-journal-edit-calendar-entry)))

(defun lg-insert-journal-header ()
  (let ((txt (format "%s, %04d %s %02d"
                     (aref calendar-day-name-array
                           (calendar-day-of-week
                            daily-journal:mdy))
                     (caddr daily-journal:mdy)
                     (aref calendar-month-name-array
                           (1- (car daily-journal:mdy)))
                     (cadr daily-journal:mdy))))
    (insert txt "\n" (make-string (length txt) ?=) "\n\n")))

(add-hook 'daily-journal-new-entry-hook 'lg-insert-journal-header)

;;; iCalendar - import/export ics files into/from diary
;; <X-URL:http://de.geocities.com/ulf_jasper/lisp/icalendar.el.txt>
(autoload 'icalendar-import-buffer "icalendar"
  "Import iCalendar data from current buffer" t)
(autoload 'icalendar-import-file "icalendar"
  "Import iCalendar data from a file" t)
(autoload 'icalendar-export-file "icalendar"
  "Export diary data in a file" t)
(autoload 'icalendar-export-region "icalendar"
  "Export diary data in a region" t)

;;}}}
;;{{{   `-- C-MODE configuration

;; Auto-insert for c/h files
(require 'autoinsert)

;; Obj-c config
(require 'objc-c-mode)
;(autoload 'objc-mode "objc-c-mode")

(defconst lg-objc-style
  '("bsd"
    (c-offsets-alist . ((objc-method-call-cont . (c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +))))))

(defun lg-objc-hook ()
  (c-add-style "objc" lg-objc-style)
  )

(add-hook 'objc-mode-hook 'lg-objc-hook)

(setq auto-mode-alist (cons '("\\.[mh]\\'" . objc-mode) auto-mode-alist))

;; autoinserters
(push '(("\\.[Hh]\\'" . "C header")
        (upcase (concat "_" (file-name-nondirectory
                             (substring buffer-file-name 0 (match-beginning 0)))
                        "_"
                        (substring buffer-file-name (1+ (match-beginning 0)))
                        "_"))
        "/*\n"
        " * "
        (file-name-nondirectory buffer-file-name)
        " --- "
        _
        "\n *\n"
        " * $Id: init-home.el,v 1.13 2008/12/23 13:24:37 lg Exp $\n"
        " */\n"
        "#ifndef "
        str
        "\n"
        "#define "
        str
        "\n\n\n\n"
        "#endif /* "
        str
        " */\n")
      auto-insert-alist)

(push '(("\\.[Cc]\\'" . "C file")
        nil
        "/*\n"
        " * "
        (file-name-nondirectory buffer-file-name)
        " --- "
        _
        "\n *\n"
        " * $Id: init-home.el,v 1.13 2008/12/23 13:24:37 lg Exp $\n"
        " */\n"
        (push-mark))
      auto-insert-alist)

(push '(("\\.el\\'" . "Emacs Lisp header")
        "Short description: "
        ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str "
        
;; Copyright (C) "
        (substring (current-time-string) -4)
        " by Zajcev Evegny.
        
;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: " (current-time-string) "
;; Keywords: "
        '(require 'finder)
        ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
        '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
                          finder-known-keywords)
               v2 (mapconcat (lambda (x) (format "%10.0s:  %s" (car x) (cdr x)))
                             finder-known-keywords
                             "\n"))
        ((let ((minibuffer-help-form v2))
           (completing-read "Keyword, C-h: " v1 nil t))
         str ", ") & -2 "
         
;; This file is part of SXEmacs.

;; SXEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; SXEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not in FSF

;;; Commentary:

;; " _ "

;;; Code:

(provide '" (file-name-sans-extension (file-name-nondirectory
(buffer-file-name))) ")

;;; " (file-name-nondirectory (buffer-file-name)) " ends here
")
auto-insert-alist)

;; I prefer BSD mode[8 spaces], because it prevent code to became a
;; mess.
(setq c-default-style "bsd")

;; Align to left when `#' is inserted
(setq c-electric-pound-behavior '(alignleft))

;; Use C support packages, such as c-types, etc.
(defun lg-ctypes-load-hook ()
  (interactive)
  (ctypes-read-file "~/.xemacs/ctypes/ctypes_std_c" nil t t))

(defun lg-ctypes-load-x11 ()
  (interactive)
  (ctypes-read-file "~/.xemacs/ctypes/ctypes_x11_c" nil t t))

(add-hook 'ctypes-load-hook 'lg-ctypes-load-hook)

(setq c-macro-preprocessor "/usr/bin/cpp -C") ; for C-c C-e command

(defun lg-c-mode-comment-region (beg end &optional arg)
  "A little modification of `comment-region' when in `c-mode'.
BEG and END defines region to comment."
  (interactive "r\nP")

  (let (bok eok)
    (save-excursion
      (goto-char beg)
      (setq bok (looking-at "^"))
      (goto-char end)
      (setq eok (looking-at "^")))

    (if (and bok eok)
        (if (consp arg)
            ;; Uncomment
            (save-excursion
              (goto-char end)
              (previous-line 1)
              (let ((kill-whole-line t))
                (kill-line))
              (goto-char beg)
              (let ((kill-whole-line t))
                (kill-line)))

          ;; Comment
          (save-excursion
            (goto-char beg)
            (insert "#if 0\n")
            (goto-char (+ end 6))
            (insert "#endif /* 0 */\n")))

      (comment-region beg end arg))))

(defun lg-c-context-line-break (arg)
  "My variant of `c-context-line-break'.
With prefix ARG, do not move the point."
  (interactive "P")
  (eval (list (if arg 'save-excursion 'progn)
              '(end-of-line)
              '(c-context-line-break))))

(defun lg-c-mode-hook ()
  "My configuration of C mode."
  (ctypes-auto-parse-mode 1)
  (turn-on-font-lock)
  (setq abbrev-mode nil)                ; don't use abbrev mode
  (setq comment-column 40)

  (setq c-backslash-max-column 120)

  (define-key c-mode-map (kbd "C-j") 'lg-c-context-line-break)
  (define-key c-mode-map (kbd "C-c C-s") 'lg-switch-to-scratch)
  (define-key c-mode-map (kbd "C-c C-c") 'lg-c-mode-comment-region)
  (define-key c-mode-map (kbd "s-2") 'lg-includes-popup-menu)
  (define-key c-mode-map (kbd "C-c c e") 'c-comment-edit)
  )

(add-hook 'c-mode-hook 'lg-c-mode-hook)

;; Easy insert some includes
(defvar lg-include-alist
  '((sys/types "sys/types")
    (sys/time "sys/time")
    (sys/param "sys/param")
    (sys/queue "sys/queue")
    (sys/stat "sys/stat")
    (sys/uio "sys/uio")
    (sys/socket "sys/socket")
    (sys/ioctl "sys/ioctl")
    (sys/sysctl "sys/sysctl")
    (--)
    (netinet/in sys/types "netinet/in")
    (arpa/inet netinet/in "arpa/inet")
    (--)
    (errno "errno")
    (fcntl "fcntl")
    (unistd "unistd")
    (ctype "ctype")
    (stdlib "stdlib")
    (string "string")
    (stdio "stdio")
    (stdarg "stdarg")
    (--)
    (open fcntl)
    (close unistd)
    (pipe unistd)
    (write sys/types sys/uio unistd)
    (read write)
    (ioctl sys/ioctl)
    (select sys/types sys/time unistd)
    (fork sys/types unistd)
    (execve unistd)
    (--)
    (getopt unistd)
    (getsubopt stdlib)
    (getopt_long "getopt.h")
    (--)
    (socket sys/types sys/socket)
    (connect socket)
    (accept socket)
    (bind socket)
    (listen socket)
    (recv socket)
    (send socket)
    (sendto send)
    (sendmsg send)
    (getsockname socket)
    (getpeername socket)
    (getsockopt socket)
    (setsockopt socket)
    (socketpair socket)
    (inet_ntoa socket arpa/inet)
    (inet_addr inet_ntoa)
    )
  "Alist of includes.")

(defun lg-insert-include (inc)
  "Insert include specified by INC."
  (let ((ival (assoc inc lg-include-alist)))
    (mapc (lambda (iel)
            (cond ((stringp iel)
                   ;; Check is already inserted
                   (let ((found nil))
                     (save-excursion
                       (goto-char (point-min))
                       (when (re-search-forward
                              (concat "^#include <" iel ".h>$") nil t)
                         (setq found t)))
                     (unless found
                       (insert (concat "#include <" iel ".h>\n")))))
                  (t (lg-insert-include iel))))
          (cdr ival))
    ))

(defun lg-includes-popup-menu ()
  "Popup menu with includes."
  (interactive)

  (let ((menu (list "C Include" :filter
                    (lambda (not-used)
                      (mapcar (lambda (inc)
                                (let ((incn (symbol-name (car inc))))
                                  (if (string= incn "--")
                                      "--" ; delimiter
                                    (vector incn `(lg-insert-include (quote ,(car inc)))
                                            :active (save-excursion
                                                      (goto-char (point-min))
                                                      (not (re-search-forward
                                                            (concat "^#include <" incn ".h>$") nil t)))))))
                              lg-include-alist)))))
    (popup-menu menu)))

;(add-hook 'c-mode-hook (lambda () (c-toggle-auto-hungry-state 1)))
;(modify-syntax-entry ?\_ "w" c-mode-syntax-table) ; make ?\_ to be part of a word in c-mode

;; My abbrevs and skeletons for C-mode
(require 'mcskels "my-cskels")
(define-key c-mode-map (kbd "s-i") mc-mode-map)

;;}}}
;;{{{   `-- GO language mode

;(add-to-list 'load-path "~/down/go-lang/misc/emacs/" t)
;(require 'go-mode-load)

;;}}}
;;{{{   `-- PYTHON & PYREX modes

;;; Python
;; Use IPython as shell
;(require 'ipython)

(require 'python-mode)
(define-key py-shell-map (kbd "M-C-l") 'switch-to-other-buffer)

(defface py-pseudo-keyword-face
  `((((class color) (background light))
     (:foreground "red4" :bold t))
    (((class color) (background dark))
     (:foreground "red" :bold t))
    (t (:foreground "red4")))
  "Face for python's pseudo keywords (False, None, etc).")

(defface py-builtins-face
  `((((class color) (background light))
     (:foreground "Purple"))
    (((class color) (background dark))
     (:foreground "Purple"))
    (t (:foreground "Purple")))
  "Face for Python's built-ins.")

;;; Pyrex
(defface pyrex-keyword-face
  `((((class color) (background light))
     (:foreground "gray30"))
    (((class color) (background dark))
     (:foreground "gray60"))
    (t (:foreground "gray30")))
  "*Face used to highlight pyrex keywords.")

(define-derived-mode pyrex-mode python-mode "Pyrex"
  (font-lock-mode 1)
  (push (cons (concat "\\<\\("
                      "DEF\\|c\\(def\\|pdef\\|har\\|typedef\\)"
                      "\\|e\\(num\\|xtern\\)"
                      "\\|float"
                      "\\|in\\(clude\\|t\\)"
                      "\\|object\\|public\\|struct\\|type\\|union\\|void"
                      "\\)\\>")
              'pyrex-keyword-face)
        font-lock-keywords)
  (push (cons (concat "\\<\\(char\\|int\\|bint\\|float\\|unsigned"
                      "\\|inline"
                      "\\|double\\|void\\|long\\)\\>")
              'font-lock-type-face)
        font-lock-keywords)
  (push (cons (concat "\\<\\(cimport\\|include\\|DEF\\|cdef"
                      "\\|cpdef\\)\\>")
              'font-lock-keyword-face)
        font-lock-keywords)
  (push (cons "\\bNULL\\b" 'py-pseudo-keyword-face)
        font-lock-keywords))

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . pyrex-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . pyrex-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . pyrex-mode))
(push '("python" . pyrex-mode) interpreter-mode-alist)

(defun lg-py-install-keys ()
  (require 'whitespace)
  (push 'pyrex-mode whitespace-modes)
  (local-set-key (kbd "C-c e r") 'py-execute-region)
  (local-set-key (kbd "C-c e b") 'py-execute-buffer)
  (local-set-key (kbd "C-c e f") 'py-execute-def-or-class)
  (local-set-key (kbd "C-c e s") 'py-execute-string)
  (local-set-key (kbd "C-c C-c") 'py-comment-region)
  (local-set-key (kbd "C-j") 'lg-insert-nl-at-eol)
  )

(add-hook 'python-mode-hook 'lg-py-install-keys)
(add-hook 'pyrex-mode-hook 'lg-py-install-keys)

;; For scons
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))

;;; Pymacs
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")

;;}}}
;;{{{   `-- PCL-CVS customization
(when nil
(require 'pcl-cvs)

;; Hack to modify cvs-diff-flags
(setcdr (car (aref cvs-diff-flags 1)) (list "-N" "--show-c-function"))

;; Customize hacked highlighting pcv-cvs part ..
(defvar lg-pcvs-unknown-special "\\.[ch]$")
(defvar lg-pcvs-unknown-special-face
  (copy-face 'cvs-unknown-face 'lg-pcvs-unknown-special-face))
(set-face-bold-p 'lg-pcvs-unknown-special-face t)

(defun lg-pcvs-select-unknown-face (fileinfo)
  "Select face to display files in UNKNOWN state."
  (if (string-match lg-pcvs-unknown-special (cvs-fileinfo->file fileinfo))
      'lg-pcvs-unknown-special-face
    'cvs-unknown-face))

(setq cvs-select-unknown-face-function 'lg-pcvs-select-unknown-face)

;;; TO USE WITH code:
;(defvar cvs-select-unknown-face-function 'cvs-defaut-unknown-face-function)

;(defun cvs-defaut-unknown-face-function (fileinfo)
;  'cvs-unknown-face)

;(defun cvs-fileinfo-pp (fileinfo)
;  "Pretty print FILEINFO.  Insert a printed representation in current buffer.
;For use by the cookie package."
;  (cvs-check-fileinfo fileinfo)
;  (let ((type (cvs-fileinfo->type fileinfo))
;	(subtype (cvs-fileinfo->subtype fileinfo)))
;    (insert
;     (case type
;       (DIRCHANGE (concat "In directory "
;			  (cvs-add-face (cvs-fileinfo->full-path fileinfo)
;					'cvs-header-face cvs-dirname-map)
;			  ":"))
;       (MESSAGE
;	(if (memq (cvs-fileinfo->subtype fileinfo) '(FOOTER HEADER))
;	    (cvs-fileinfo->full-log fileinfo)
;	  (cvs-add-face (format "Message: %s" (cvs-fileinfo->full-log fileinfo))
;			'cvs-msg-face)))
;       (t
;	(let* ((status (if (cvs-fileinfo->marked fileinfo)
;			   (cvs-add-face "*" 'cvs-marked-face)
;			 " "))
;	       (file (cvs-add-face (cvs-fileinfo->pp-name fileinfo)
;				   'cvs-filename-face cvs-filename-map))
;	       (base (or (cvs-fileinfo->base-rev fileinfo) ""))
;	       (head (cvs-fileinfo->head-rev fileinfo))
;	       (type
;		(let ((str (case type
;			     ;;(MOD-CONFLICT "Not Removed")
;			     (DEAD	  "")
;			     (t (capitalize (symbol-name type)))))
;		      (face (case type
;			      (UP-TO-DATE 'cvs-handled-face)
;			      (UNKNOWN (funcall cvs-select-unknown-face-function fileinfo));'cvs-unknown-face)
;			      (t 'cvs-need-action-face))))
;		  (cvs-add-face str face cvs-status-map)))
;	       (side (or
;		      ;; maybe a subtype
;		      (when subtype (downcase (symbol-name subtype)))
;		      ;; or the head-rev
;		      (when (and head (not (string= head base))) head)
;		      ;; or nothing
;		      ""))
;	       ;; (action (cvs-add-face (case (cvs-default-action fileinfo)
;	       ;;			  (commit "com")
;	       ;;			  (update "upd")
;	       ;;			  (undo   "udo")
;	       ;;			  (t      "   "))
;	       ;;			'cvs-action-face
;	       ;;			cvs-action-map))
;	       )
;	  (concat (cvs-string-fill side 11) " "
;		  status " "
;		  (cvs-string-fill type 11) " "
;		  ;; action " "
;		  (cvs-string-fill base 11) " "
;		  file)))))))
;;;        it seems that `format' removes text-properties.  Too bad!
;;;	  (format "%-11s %s %-11s %-11s %s"
;;;		  side status type base file)))))))


;;; pcl-cvs and add-log configuration
(add-hook 'change-log-mode-hook
          #'(lambda ()
              (interactive)
              (filladapt-mode)
              (auto-fill-mode)))

;; Some enhacements to cvs-mode and add-log
(defun lg-cvs-changelog-in-other-window ()
  "Switch to ChangeLog file in other window."
  (interactive)
  (let* ((fi (cvs-mode-marked nil nil :one t))
         (default-directory (cvs-expand-dir-name (cvs-fileinfo->dir fi)))
         (buffer-file-name (expand-file-name (cvs-fileinfo->file fi)))
         change-log-default-name)
    (save-excursion
      (find-file-other-window (find-change-log)))))

(defun lg-cvs-diff-in-other-window ()
  "Switch to cvs-diff buffer in other window."
  (display-buffer (get-buffer "*cvs-diff*") t))

(defun-cvs-mode (cvs-mode-mydiff . DOUBLE) ()
  "Diff the selected files against the repository and
patch-to-change-log it after processing."
  (interactive)
  (cvs-mode-do "diff" '("-u" "-N") 'diff
               :show nil :postproc '((patch-to-change-log default-directory))))

(defun lg-insert-cvs-tag ()
  "Substitutes $TAG: ...$ by CVS tag if any."
  (interactive)
  (let ((tbuf nil)
        (tstr "HEAD"))

    (when (file-exists-p "CVS/Tag")
      (setq tbuf (find-file-noselect "CVS/Tag")))

    (when tbuf
      (with-current-buffer tbuf
        (if (re-search-forward "T\\(.*\\)")
          (setq tstr (match-string 1))
          (setq tstr "unknown"))
        (kill-buffer tbuf)))

    (save-excursion
      (beginning-of-buffer)
      (when (re-search-forward "\\$TAG\\(: .*\\$\\|\\$\\)")
        (replace-match (format "$TAG: %s$" tstr) nil nil)))
    ))

(defun lg-cvs-patch-to-change-log (arg)
  "Make patch of file in working directory and in CVS. Create
ChangeLog entries according to diff.
If used with prefix arg, show cvs-diff buffer also."
  (interactive "P")

  (when arg
    (cvs-mode-do "diff" '("-u" "-N") 'diff
                 :show t :postproc '((lg-cvs-diff-in-other-window))))

  (cvs-mode-mydiff)
  (lg-cvs-changelog-in-other-window))

(define-key 'cvs-mode-map (kbd "C-c A") 'lg-cvs-patch-to-change-log)
)
;;}}}
;;{{{   `-- EGG - Emacs Got Git
(add-to-list 'load-path "~/.sxemacs/lisp/thirdpart/egg" t)

(autoload 'egg-status "~/.sxemacs/lisp/thirdpart/egg/egg.el" nil t)

;;{{{   `-- PSVN - Subversion manager

;; <X-URL:http://www.xsteve.at/prg/emacs/psvn.el>
(ofupd-register "psvn" "~/.sxemacs/lisp/thirdpart/psvn.el"
                "http://www.xsteve.at/prg/emacs/psvn.el")

(autoload 'svn-status "psvn" nil t)
(autoload 'svn-trac-browse-changeset "psvn" nil t)
(autoload 'svn-trac-browse-ticket "psvn" nil t)
(autoload 'svn-trac-browse-timeline "psvn" nil t)

(setq svn-status-display-full-path t)
(setq svn-status-hide-unmodified t)

(defvar svn-status-mode-hook nil
  "Hook to run when entering svn-status-mode.")
(defadvice svn-status-mode (after lg-run-svn-mode-hooks activate)
  "Run `svn-status-mode-hook' when entering svn-status-mode."
  (run-hooks 'svn-status-mode-hook))

(add-hook 'svn-status-mode-hook 'highline-on)

;; NOTE: `svn-trac-project-root' is in ~/.sxemacs/local.settings.el

;;}}}
;;{{{   `-- MIC-PAREN customization

;;; Configure nice mic-paren package
(require 'mic-paren)
(setq paren-display-message 'only)
(add-hook 'c-mode-hook
          (lambda ()
            (paren-toggle-open-paren-context 1)))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (paren-toggle-matching-quoted-paren 1)))
(paren-activate)

;;}}}
;;{{{   `-- W3M customization

(unless (fboundp 'make-char)
  (defun make-char (enc char)
    char))

(autoload 'w3m "w3m" nil t)
(autoload 'w3m-browse-url "w3m" nil t)

(setq w3m-use-cookies t)
(setq w3m-fill-column 75)
(setq w3m-use-favicon nil)

;; Strip ^M in editing textarea
(add-hook 'w3m-form-input-textarea-mode-hook
          '(lambda ()
             (save-excursion
               (while (re-search-forward "\r\n" nil t)
                 (replace-match "\n" nil nil)))))

(add-hook 'w3m-form-input-textarea-set-hook
          '(lambda ()
             (save-excursion
               (while (re-search-forward "\n" nil t)
                 (replace-match "\r\n" nil nil)))))

(autoload 'russian-translate-region "russian")

(defun mym-w3m-translate-chars ()
  "Translate cp1251 to koi8."
  (interactive)
  (let ((buffer-read-only nil))
    (russian-translate-region (point-min) (point-max) t "win-cp1251" "8koi"))
  )

(defun mym-w3m-translate-string (str)
  (with-temp-buffer
    (insert str)
    (mym-w3m-translate-chars)
    (buffer-substring)))

(defun mym-w3m-maybe-translate (url)
  "May be we may automaticaly translate URL."
  (let ((buffer-read-only nil)
        (arct (w3m-content-charset url)))
    (when (and arct (string= arct "windows-1251"))
      (w3m-arrived-put
       url 'title (mym-w3m-translate-string (w3m-arrived-title url)))
      (mym-w3m-translate-chars))))

(defface w3m-form-face
  '((((class color) (background light)) (:foreground "cyan4" :underline t))
    (((class color) (background dark)) (:foreground "red4" :underline t))
    (t (:underline t)))
  "*Face to fontify forms."
  :group 'w3m-face)

(defun lg-w3m-load-hook ()
  "Setup w3m after it loaded."

  ;; unbind it
  (define-key w3m-mode-map (kbd "M-[") nil)
  (define-key w3m-mode-map (kbd "M-]") nil)

  ;; Navigation
  (define-key w3m-mode-map (kbd "p") 'previous-line)
  (define-key w3m-mode-map (kbd "n") 'next-line)
  (define-key w3m-mode-map (kbd "f") 'forward-char)
  (define-key w3m-mode-map (kbd "b") 'backward-char)

  (define-key w3m-mode-map "CT" 'mym-w3m-translate-chars)

  (set-face-font 'w3m-current-anchor-face
                 (font-name (face-property 'default 'font))))

(add-hook 'w3m-load-hook 'lg-w3m-load-hook)
(add-hook 'w3m-display-hook 'mym-w3m-maybe-translate)

(defun lg-browse-in-w3m (url &rest arg)
  "Browse URL in new w3m window.
ARG is unused."
  (w3m-browse-url url t))

(setq browse-url-browser-function 'lg-browse-in-w3m)
(setq browse-url-new-window-flag t)

;;}}}
;;{{{   `-- ACTIVE-MENU customization

;;; Use this fine active-menu
;; <X-URL:http://www.cbrunzema.de/download/active-menu/active-menu.el>
(ofupd-register "active-menu" "~/.sxemacs/lisp/thirdpart/active-menu.el"
                "http://www.skamphausen.de/cgi-bin/ska/download/active-menu.el"
                #'ofupd-ve-version-colon)

(autoload 'active-menu "active-menu" "Show menu only when mouse is at the top of the frame." t)
(autoload 'turn-on-active-menu "active-menu" "Show menu only when mouse is at the top of the frame." t)
(setq active-menu-frame-compensation nil) ; Do not compensate frame
;(turn-on-active-menu)

;;}}}
;;{{{   `-- ESHELL

;; In case SHELL is xinit
(setq shell-file-name "/bin/sh")

;; Workarond 'man problem
(defalias 'man 'manual-entry)

(autoload 'ansi-color-apply-on-region "ansi-color")

;; Workaround non-mule (S)XEmacs
(unless (fboundp 'coding-system-eol-type)
  (defun coding-system-eol-type (arg)
    arg))
(unless (fboundp 'coding-system-change-eol-conversion)
  (defun coding-system-change-eol-conversion (en type)
    en))

(defun lg-eshell-mode-hook ()
  ;; Unset grid pixmap
  (set-face-background-pixmap 'default "" (current-buffer))

  (set-face-foreground 'default "green" (current-buffer))
  (set-face-background 'default "black" (current-buffer))

  (define-key eshell-mode-map (kbd "M-C-l") 'switch-to-other-buffer)
  )
(add-hook 'eshell-mode-hook 'lg-eshell-mode-hook)

(defun lg-eshell-filter-function ()
  "ANSI colourize eshell output."
  (save-excursion
    (ansi-color-apply-on-region eshell-last-output-block-begin (point-marker))))
(add-hook 'eshell-output-filter-functions 'lg-eshell-filter-function)

;;}}}
;;{{{   `-- INFO

(require 'info-look)
(info-lookup-add-help* nil
 :mode 'emacs-lisp-mode
 :regexp "[^()' \t\n]+"
 :doc-spec '(("(xemacs)Command Index")
             ("(xemacs)Variable Index")
             ("(lispref)Index"
	      (lambda (item)
		(let ((sym (intern-soft item)))
		  (cond ((null sym)
			 (if (string-equal item "nil") item))
			((or (boundp sym) (fboundp sym))
			 item))))
	      "^[ \t]+- [^:]+:[ \t]*" "\\b")))

(info-lookup-add-help* nil
 :mode 'c-mode :topic 'symbol
 :regexp "\\(struct \\|union \\|enum \\)?[_a-zA-Z][_a-zA-Z0-9]*"
 :doc-spec '(("(libc)Function Index" nil
	      "^[ \t]+- \\(Function\\|Macro\\): .*\\<" "\\>")
	     ("(libc)Variable Index" nil
	      "^[ \t]+- \\(Variable\\|Macro\\): .*\\<" "\\>")
	     ("(libc)Type Index" nil
	      "^[ \t]+- Data Type: \\<" "\\>")
	     ("(termcap)Var Index" nil
	      "^[ \t]*`" "'"))
 :parse-rule 'info-lookup-guess-c-symbol)

(info-lookup-add-help* nil
 :mode 'python-mode
 :regexp "[_a-zA-Z][_a-zA-Z0-9]*"
 :ignore-case t
 :doc-spec '(("(python-lib)Function-Method-Variable Index" nil
              "^[ \t]*`" "[^']*'")
             ("(python-ref)Function-Method-Variable Index" nil
              "^[ \t]*`" "[^']*'")))

(info-lookup-add-help* nil
 :mode 'pyrex-mode
 :regexp "[_a-zA-Z][_a-zA-Z0-9]*"
 :ignore-case t
 :doc-spec '(("(python-lib)Function-Method-Variable Index" nil
              "^[ \t]*`" "[^']*'")
             ("(python-ref)Function-Method-Variable Index" nil
              "^[ \t]*`" "[^']*'")))

;;}}}
(when nil
;;{{{   `-- ABBREV

(require 'msf-abbrev)
(setq msf-abbrev-root
      (expand-file-name "mode-abbrevs" user-init-directory))
(msf-abbrev-load)

(defun lg-msf-abbrev-xemacs-fix ()
  (when (and abbrev-mode
             (eq this-command 'self-insert-command))
    (when (and (expand-abbrev)
               (memq (char-syntax
                      (event-to-character
                       (car (last (append (this-command-keys) nil)))))
                     '(?\. ?\x20)))
      (setq this-command 'ignore))))

(add-hook 'pre-command-hook 'lg-msf-abbrev-xemacs-fix)

;;}}}
)

;;{{{   `-- EICQ

(require 'emchat)
(autoload 'emchat-xwem-init "emchat-xwem")

(setq emchat-user-alias "lg"
      emchat-port 6667
      emchat-coding-system 'windows-1251
      emchat-buddy-window-width 10
      emchat-log-fill-column 70
      emchat-log-info-flag 'tail
      emchat-log-buddy-status-flag nil
      emchat-log-system-flag nil
      emchat-log-filename (expand-file-name "~/.emchat/emchat-log")
      emchat-auto-response-messages-p nil
      emchat-track-enable t)

(add-hook 'emchat-log-mode-hook 'lg-set-save-buffers-skip)
(add-hook 'emchat-log-mode-hook
          (lambda ()
            (set-buffer-file-coding-system 'koi8-r)))

(setq emchat-xwem-osd-font
      "-*-terminus-bold-r-*-*-*-640-*-*-*-*-koi8-r"
      emchat-xwem-osd-colors
      '(("^\\(kisa\\|shira\\|debil\\|zorg\\)$" . "red3")
        ("^\\(iri\\|Lesik\\|JackalX\\)$" . "green3")
        ("^karma$" . "yellow2")
        (".*" . "magenta3"))
      emchat-xwem-osd-coordinates '(100 . 800))

;;}}}
;;{{{   `-- ERC

(require 'erc)
(require 'erc-pcomplete)
(require 'erc-fill)
(require 'erc-stamp)
(require 'erc-match)
(require 'erc-track)

(setq erc-auto-query 'window-noselect)
(setq erc-timestamp-format "[%H:%M] ")
(setq erc-insert-timestamp-function 'erc-insert-timestamp-left)
(set-face-foreground 'erc-timestamp-face "green4")
(set-face-foreground 'erc-error-face "red")
(setq erc-fill-column 70)

(add-hook 'erc-insert-modify-hook 'erc-fill)
(add-hook 'erc-send-modify-hook 'erc-fill)

(erc-match-enable)
(erc-stamp-enable)
(erc-pcomplete-enable)

(setq erc-server "irc.sxemacs.org")
(setq erc-port 6667)
(setq erc-nick "lg_")
(setq erc-user-full-name "Zajcev Evgeny")
(setq erc-prompt-for-password nil)
(setq erc-paranoid t)                   ;show incoming CTCPs
(setq erc-minibuffer-notice nil)

(erc-track-mode 1)
(setq erc-track-switch-from-erc t)
(setq erc-track-use-faces nil)
(setq erc-track-showcount t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "PING" "PONG"))
;; please highlight my nick
(setq erc-keywords '(("lg_" . bold)))

(defun lg-keyword-here (mt who msg)
  "To be used in `erc-text-matched-hook'."
  (when (and (eq mt 'keyword) (not (string-match "PONG" msg)))
    (let ((bell-volume 100))
      (beep)
      (sleep-for 0.5)
      (beep))))
(add-hook 'erc-text-matched-hook 'lg-keyword-here)

(defun erc-cmd-SHOWOFF (&rest ignore)
  "Show off implementation"
  (let* ((chnl (erc-buffer-list))
         (srvl (erc-buffer-list 'erc-server-buffer-p))
         (memb (apply '+ (mapcar (lambda (chn)
                                   (with-current-buffer chn
                                     (1- (length ;(erc-get-channel-user-list)
                                          (erc-get-channel-members (buffer-name chn))
                                          ))))
                                 chnl)))
         (show (format "is connected to %i networks and talks in %i chans to %i ppl overall :>"
                       (length srvl)
                       (- (length chnl) (length srvl))
                       memb)))
    (erc-send-action (erc-default-target) show)
    t))

(defalias 'erc-cmd-SO 'erc-cmd-SHOWOFF)

(define-key global-map (kbd "C-c <SPC>") 'erc-track-switch-buffer)

;;}}}
;;{{{   `-- DIRED

(when nil
;; Release C-o key for -other- commands
(define-key dired-mode-map (kbd "C-o") nil)
(define-key dired-mode-map (kbd "C-O") 'dired-omit-toggle)
)

;;}}}
;;{{{   `-- GNUS

(unless (find-face 'mode-line-highlight)
  (copy-face 'default 'mode-line-highlight))

(require 'gnus)

(setq gnus-use-toolbar nil)
(setq message-use-toolbar nil)

(setq gnus-logo-color-style 'purp)
(setq gnus-logo-colors
      (cdr (assq gnus-logo-color-style gnus-logo-color-alist)))

(setq gnus-novice-user nil)

(setq user-full-name "Zajcev Evgeny")
(setq user-mail-address "lg.zevlg@gmail.com")
(setq query-user-mail-address nil)

(setq message-default-charset 'koi8-r)
(setq imap-username "lg")

;; NOTE: `gnus-secondary-select-methods' and `gnus-select-method' are
;; in ~/.sxemacs/local.settings.el

;; Allow '/' in group names
(setq gnus-invalid-group-regexp "[: `'\"]\\|^$")

(setq nnimap-split-crosspost nil)
(setq nnimap-split-inbox nil);'("INBOX"))
(setq nnimap-split-rule
      '(
;         ("freesol-lg" "^To:.*lg@freesol\\.ru")
;         ("freesol-info" "^To:.*info@freesol\\.ru")

        ("redmine" "^From: redmine@undev\\.ru")
        ("FreeBSD" "^Sender.*owner-freebsd-hackers@freebsd.org")
        ("edu-sig" "^Sender.*edu-sig[^@]*@python.org")
        ("nnov-security" "^Sender.*listserv@security.nnov.ru")
        ("savannah" "^Sender.*www-data <www-data@subversions.gnu.org>")
        ("fm" "^Sender.*freshmeat-news-bounces@lists.freshmeat.net")
        ("xemacs-design" "^Sender: xemacs-design[^@]*@")
        ("xemacs-beta" "^Sender: xemacs-beta[^@]*@")
        ("xemacs-ru" "^Sender: xemacs-users-ru[^@]*@")
        ("xemacs-patches" "^Sender: xemacs-patches-[^@]*@")
        ("gnu-emacs-sources" "^Sender: gnu-emacs-sources[^@]*@gnu.org")
        ("pyrex" "^Sender.*pyrex[^@]*@lists.copyleft.no")
        ("climacs-devel" "^Sender.*climacs-devel[^@]*@common-lisp.net")
        ("climacs-cvs" "^Sender.*climacs-cvs[^@]*@common-lisp.net")
        ("xwem-devel" "^Sender.*xwem-devel")
        ("xwem-patches" "^Sender.*xwem-patches")
        ("xwem-bugs" "^Sender.*xwem-bugs")
        ("eicq-devel" "^Sender.*eicq-devel[^@]*@lists.sourceforge.net")
        ("eicq-devel" "^Sender.*eicq-devel[^@]*@eicq.org")
        ("eicq-devel" "^Sender:.*emchat-devel[^@]*emchat.org")
        ("ldap-sol" "^Sender:.*ldap-sol[^@]*@googlegroups.com")
        ("conkeror" "^Sender.*conkeror[^@]*@mozdev.org")

        ("brainstorm" "^Sender.*brainstorm[^@]*@youngs.au.com")
        ("SXEmacs-devel" "^Sender.*sxemacs-devel[^@]*@lists.sxemacs.org")
        ("SXEmacs-patches" "^Sender.*sxemacs-patches[^@]*@lists.sxemacs.org")

        ("pereslavl" "^Sender.*pereslavl[^@]*@lists.altlinux.org")
        ("pereslavl2006" "^Sender.*pereslavl2006[^@]*@lists.altlinux.org")
        ("pereslavl2007" "^Sender.*pereslavl2007[^@]*@lists.altlinux.org")
        ("haskell-cafe" "^Sender.*haskell-cafe[^@]*@haskell.org")
        ("haskell" "^Sender.*haskell[^@]*@haskell.org")
        ("maxima" "^Sender:.*maxima-\\(admin\\|bounces\\)[^@]*@math.utexas.edu")
        ("PEG" "^Sender:.*peg[^@]*@lists.csail.mit.edu")
        ("funprog-ru" "^Sender:.*funprog-ru[^@]*@sxemacs.org")
        ("funprog-ru" "^Sender:.*funprog-ru[^@]*@googlegroups.com")
        ("freeschool" "^Sender:.*freeschool[^@]*@lists.linux.ru.net")
        ("pyjamas" "^\\(Sender\\|To\\):.*pyjamas-dev@googlegroups.com")
        ("pyjamas" "^\\(Sender\\|To\\):.*pyjamasdev@pyjs.org")

        ("clutter-dev" "^\\(Sender\\|To\\):.*clutter-devel[^@]*@clutter-project.org")
        ("clutter-app-dev" "^\\(Sender\\|To\\):.*clutter-app-devel[^@]*@clutter-project.org")

        ("SPAM.misc" "^Subject: Top 1000.*")
;        ("SPAM.misc" "^X-Spam-Flag:.*YES")
        ("SPAM.misc" "^From:.*Canadian Doctor.*")

        ;; JS-Kit stuff
        ("jskit-lg" "^\\(To\\|Cc\\).*lg@js-kit\\.com")
        ("jskit-uldev" "^\\(To\\|Cc\\).*uldev@js-kit\\.com")
        ("jskit-dev" "^\\(To\\|Cc\\).*dev@js-kit\\.com")
        ("jskit-codereview" "^\\(To\\|Cc\\).*code-review@js-kit\\.com")
        ("jskit-all" "^\\(To\\|Cc\\).*all@js-kit\\.com")

        ("NOTES" "^Subject: NOTE")
        ("private" "^To.*\\(zevlg@yandex\\.ru\\|lg@itgsoft\\.ru\\|lg@s?xemacs\\.org\\|lg@xwem.org\\)")
        ("freesol" "^To.*lg@freesol\\.ru")

        ("gmail" "^To.*lg\\.zevlg@gmail\\.com")

        ("unknown" "")))

(setq gnus-outgoing-message-group "nnml:outbox-archive")

(defun lg-gnus-xwem-signature-generator ()
  (let ((signs '("XWEM - The (S)XEmacs Window Manager."
                 "XWEM - Take the best and make it better."
                 "XWEM - The best just got better."
                 "XWEM - When power is under control."
                 "XWEM - Feel the freedom."
                 "XWEM - Beyond the (S)XEmacs."
                 "XWEM - Binding of the day - `H-h k H-o H-a x'."
                 "XWEM - Breaking the boundaries."
                 "XWEM - Removing the limitations."
                 "XWEM - The best environment for hacker."
                 "XWEM - Taking the geek where no geek has gone before!"
                 "XWEM - (S)XEmacs With Extra Muscle."
                 "XWEM - The intellect cannot embrace."
                 "XWEM - The vital essence."
                 "XWEM - Stays crunchy in milk."
                 "XWEM - Don't leave home without it."
                 "XWEM - Turning geeks everywhere into sex gods."
                 "XWEM - Specialising in miracles and impossibilites."
                 "XWEM - Makes RMS run (S)XEmacs in secret."
                 "XWEM - RMS runs Emacs from inside an XWEM frame."
                 "XWEM - Making vi users everywhere question their choice."
                 "XWEM - Making GNU/Emacs users everywhere question their choice."
                 "XWEM - The Window Manager of Gods."
                 "XWEM - Your kids will be proud."
                 "XWEM - Sizzles under SXEmacs!")))
    (nth (random (length signs)) signs)))

(setq gnus-posting-styles
      '((".*"
         (name "Zajcev Evgeny")
	 (address "lg.zevlg@gmail.com")
	 (signature-file "~/.signature")
         )

        ((header "to" "lg@s?xemacs\\.org")
         (address "lg@sxemacs.org"))

        ("SXEmacs-devel"
         (address "lg@sxemacs.org")
         (to "SXEmacs devel <sxemacs-devel@sxemacs.org>"))
         
        ("\\(xemacs-beta\\|SXEmacs-patches\\)"
         (address "lg@sxemacs.org"))

        ("xwem-devel"
         (organization "xwem.org")
         (address "lg@xwem.org")
         ("X-Home-Page" "http://www.xwem.org")
         (signature lg-gnus-xwem-signature-generator))

        ("jskit-.*"
         (organization "JS Kit")
         (address "lg@js-kit.com")
         ("X-Home-Page" "http://js-kit.com")
	 (signature-file "~/.signature-jskit"))

        ((header "to" "lg@js-kit\.com")
         (organization "JS Kit")
         (address "lg@js-kit.com")
         ("X-Home-Page" "http://js-kit.com")
	 (signature-file "~/.signature-jskit"))

        ((header "to" "zevlg@yandex\.ru")
         (address "zevlg@yandex.ru"))

         ((header "to" "lg@freesol\.ru")
          (organization "FreeSolutions")
          (name "Zajcev Evgeny")
          (address "lg@freesol.ru")
          ("X-Home-Page" "http://freesol.ru")
          (signature-file "~/.signature-freesol-lg"))

         ("freesol-lg"
          (organization "FreeSolutions")
          (name "Zajcev Evgeny")
          (address "lg@freesol.ru")
          ("X-Home-Page" "http://freesol.ru")
          (signature-file "~/.signature-freesol-lg"))

        ((header "to" "info@freesol\.ru")
         (organization "FreeSolutions")
         (name "Zajcev Evgeny")
         (address "info@freesol.ru")
         ("X-Home-Page" "http://freesol.ru")
         ("X-Face" "")
	 (signature-file "~/.signature-freesol-info"))

        ("pereslavl"
         (address "zevlg@yandex.ru"))
	))

(setq gnus-summary-line-format
      (concat
       "%*%5{%U%R%z%}" ;%uc"
       "%4{|%}"
       "%-4k"
       "%4{|%}"
       "%2{%-8&user-date;%}" ; Datum
       "%4{|%}"
       "%2{ %}%(%-16,16uB" ;From/To
       "%)%4{|%}"
       "%2{ %}%3{%B%}%1{%s%}\n"))


(defun gnus-user-format-function-parts (header)
  (let ((ctype (cdr (assq 'Content-Type
			  (mail-header-extra header)))))
    (if (or (not ctype)
	    (string-match "^text/plain" ctype)
	    (string-match "^multipart/\\(encrypted\\|signed\\)" ctype))
	" "
      "@")))

; gnus-face-1
(copy-face 'default 'mysubject)
(setq gnus-face-1 'mysubject)

;gnus-face-2
(copy-face 'default 'mytime)
(set-face-foreground 'mytime "indianred4")
(setq gnus-face-2 'mytime)

;gnus-face-3
;(copy-face 'ct-face1 'mythreads)
(copy-face 'default 'mythreads)
(set-face-foreground 'mythreads "indianred4")
(setq gnus-face-3 'mythreads)

;gnus-face-4
(copy-face 'default 'mygrey)
(set-face-foreground 'mygrey "grey")
(setq gnus-face-4 'mygrey)

;gnus-face-5
(copy-face 'default 'myblack)
(set-face-foreground 'myblack "grey60")
(setq gnus-face-5 'myblack)

;gnus-face-6
(copy-face 'default 'mybiggernumbers)
(set-face-foreground 'mybiggernumbers "indianred4")
(setq gnus-face-6 'mybiggernumbers)

(setq gnus-group-line-format
       "%6{%M%S%p    %}%(%2{%4y%}%4{|%}%s%-34,34G%3O%l %4{|%}%2{%4U.%}%4{|%}%2{%3T!%}%4{|%}%2{%3I?%}%4{|%}%2{%5t%} %)%4{| %}%1{%D%}\n")


(setq gnus-sum-thread-tree-root "+ ")
(setq gnus-sum-thread-tree-single-indent "* ")
(setq gnus-sum-thread-tree-vertical "| ")
(setq gnus-sum-thread-tree-indent "  ") ;; 3 Leerzeichen funktioniert
(setq gnus-sum-thread-tree-leaf-with-other "+-> ")
(setq gnus-sum-thread-tree-single-leaf "`-> ")


(setq gnus-sum-thread-tree-false-root "> ")
;(setq gnus-sum-thread-tree-root "")

; Darstellung von Followups
;(setq gnus-summary-same-subject ">")
(setq gnus-summary-same-subject "")

;(setq gnus-build-sparse-threads 'some)
;(setq gnus-fetch-old-headers 'some)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(add-hook 'gnus-group-prepare-hook 'highline-on)
(add-hook 'gnus-summary-prepare-hook 'highline-on)

(setq gnus-summary-default-high-score 500)
(setq gnus-summary-default-low-score -1)
(setq gnus-score-expiry-days 14
      gnus-save-score t
      bbdb/gnus-score-default 10
      gnus-score-thread-simplify t)

(add-hook 'gnus-started-hook 'bbdb-insinuate-gnus)

;; Encoding
(unless (fboundp 'coding-system-base)
  (defun coding-system-base (cs)
    8))

(push 'koi8-r mm-coding-system-list)
(push 'iso-8859-1 mm-coding-system-list)
(push 'cp1251 mm-coding-system-list)
(push 'utf8 mm-coding-system-list)

(setq gnus-article-decode-mime-words t)
(setq gnus-article-decode-charset 1)

(when (featurep 'mule)
  (define-coding-system-alias 'cp1251 'undecided)
;(define-coding-system-alias 'windows-1251 'cp1251)
;(define-coding-system-alias 'windows-1251 'undecided)
;(define-coding-system-alias 'Windows-1251 'cp1251)
  (define-coding-system-alias 'utf8 'undecided))

;; Hack to make `gnus-article-de-base64-unreadable' work properly
(defadvice base64-decode-region (around ignore-errors activate)
  (ignore-errors
    ad-do-it))

(defun lg-de-base64-unreadable (&rest args)
  (flet ((mm-decode-coding-region (&rest args) (apply #'ignore args)))
    (apply #'gnus-article-de-base64-unreadable args)))

(add-hook 'gnus-article-decode-hook 'article-decode-charset)
(add-hook 'gnus-article-decode-hook 'gnus-article-decode-encoded-words)
;(add-hook 'gnus-article-decode-hook 'lg-de-base64-unreadable)

;; Decode Subject words when replying/followuping
(add-hook 'message-header-setup-hook
          (lambda ()
            (mail-decode-encoded-word-region (point-min) (point-max))))

(add-hook 'gnus-message-setup-hook
          (lambda ()
            (mail-decode-encoded-word-region (point-min) (point-max))))

(require 'gnus-demon)
(gnus-demon-add-handler 'gnus-group-get-new-news 4 t)
(gnus-demon-init)

;;; New mail notifier
(defvar lg-gnus-beepable-groups "private\\|xwem"
  "A regexp that matches groups for which mail notification should take place.")

(defun lg-gnus-notify ()
  "Beep if we got mail in an interesting folder."
  (let ((case-fold-search t)
	(you-got-mail nil))
    (dolist (group nnmail-split-history)
      (message (format "%s" (caar group)))
      (when (string-match lg-gnus-beepable-groups (caar group))
	(setq you-got-mail t)))
    (when you-got-mail
      (beep)
      (sleep-for 0.2)
      (beep)
      (sleep-for 0.1)
      (beep))))

(add-hook 'gnus-after-getting-new-news-hook 'lg-gnus-notify)

;; Keywords header generation
;(autoload 'message-keyword-insert "messkeyw")
;(add-hook 'message-send-hook 'message-keyword-insert)

;; Make sure my followups are scored higher.
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)

(setq mm-discouraged-alternatives (list "text/html"))

;; quick nuke people :-)
;;
;; This function should be called from the summary buffer with point
;; on the article to nuke.
;; It needs an entry in all.SCORE of (files "<score-directory>/SPAMMERS").
(defun gnus-scum-expunge ()
  "Remove this spammer from existance as much as possible."
  (interactive)
  (let ((oldscfile gnus-current-score-file))
    (gnus-score-change-score-file "SPAMMERS")
    (gnus-summary-score-entry
     "Subject" (aref (gnus-summary-article-header) 1) 'S' -1 nil nil nil nil)
    (gnus-summary-score-entry
     "From" (aref (gnus-summary-article-header) 2) 'S' -9999 nil nil nil nil)
    (gnus-score-change-score-file oldscfile)
    (gnus-score-save)))

(define-key gnus-summary-mode-map '"\C-cx" 'gnus-scum-expunge)

(unless (member '("^.*/SPAMMERS$" . gnus-score-mode) auto-mode-alist)
  (add-to-list 'auto-mode-alist '("^.*/SPAMMERS$" . gnus-score-mode)))

;;; BBDB setup
(bbdb-initialize 'gnus 'message)
(setq bbdb-dwim-net-address-allow-redundancy t
      bbdb-always-add-addresses t
      bbdb/gnus-summary-prefer-real-names t
      bbdb/gnus-summary-prefer-bbdb-data t
      bbdb-completion-type 'primary-or-name
      bbdb-use-pop-up nil
      bbdb-send-mail-style 'gnus
      bbdb-file-coding-system 'koi8-r)

(setq bbdb-auto-notes-alist
      (quote (("Subject" (".*" last-subj 0 t))
	      ("Organisation" (".*" company 0 t))
	      ("Organization" (".*" company 0 t))
	      ("X-Newsreader" (".*" mailer 0 t))
	      ("X-Mailer" (".*" mailer 0 t))
	      ("User-Agent" (".*" mailer 0 t))
	      ("X-URL" (".*" url 0 t))
	      ("X-Face" (".+" face 0 t))
	      ("Face" (".+" cface 0 t))
	      )))

(autoload 'gnus-convert-face-to-png "gnus-fun")
(defun lg-bbdb-display-colour-face ()
  "Search for face properties and display the faces."
  (let ((inhibit-read-only t)		; edit the BBDB buffer
	(all-records bbdb-records)
	cface record start)
    (while all-records
      (setq record (caar all-records)
	    cface (bbdb-record-getprop record 'cface)
	    start (marker-position (nth 2 (car all-records))))
      (if cface
	  (progn
	    (set-extent-begin-glyph
	     (make-extent start start)
	     (make-glyph
	      (list (vector 'png ':data (gnus-convert-face-to-png cface)))))
	    (insert " ")))
      (setq all-records (cddr all-records)))))

(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
(add-hook 'bbdb-list-hook 'lg-bbdb-display-colour-face)

;; picons
(require 'gnus-art)
(add-to-list 'gnus-picon-databases (expand-file-name "~/.gnus/icons"))

;;; Treatment
;; Do not substitute Date: header if using lapsed date treatment
(setq gnus-article-date-lapsed-new-header t)

;; Wash out cp1251 encoding
(defvar lg-gnus-treat-wash-cp1251 nil)
(defalias 'lg-gnus-wash-cp1251 'mym-w3m-translate-chars)

(push '(lg-gnus-treat-wash-cp1251 lg-gnus-wash-cp1251)
      gnus-treatment-function-alist)

;; Do treatment in reverse order, so article gets buttonized even if
;; html washed
(setq gnus-treatment-function-alist
      (nreverse gnus-treatment-function-alist))

(define-key gnus-summary-mode-map (kbd "W W") 'lg-gnus-wash-cp1251)

;; For `W h' washing
(setq gnus-article-wash-function 'w3m)

(setq gnus-treat-mail-picon 'head
      gnus-treat-from-picon 'head
      gnus-treat-fill-long-lines '("F1News.ru")
      gnus-treat-date-lapsed 'head
      gnus-treat-display-smileys nil
      gnus-treat-strip-cr t
      gnus-treat-wash-html '("Lambda the Ultimate"
                             "lib.mexmat.ru"
                             "lib.mexmax.ru"
                             "CS updates on arXiv.org"
                             "bsdtalk"
                             "E-Buki"
                             "Natahaus")
      lg-gnus-treat-wash-cp1251 '("lib.mexmax.ru"
                                  "F1News.ru"
                                  "dpni.org"
                                  "Bash.Org.Ru"
                                  "Natahaus")
      gnus-treat-unsplit-urls nil
      )

(setq nnmail-expiry-target 'nnmail-fancy-expiry-target)

;; Adjust keys
(add-hook 'gnus-article-mode-hook
          (lambda ()
            (define-key gnus-article-mode-map (kbd "M-C-l") nil)))
(add-hook 'gnus-group-mode-hook
          (lambda ()
            (define-key gnus-group-mode-map (kbd "M-C-l") nil)))
(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (define-key gnus-summary-mode-map (kbd "M-C-l") nil)))

;;}}}
;;{{{   `-- FUME mode to display current function in modeline

(add-hook 'scheme-mode-hook 'turn-on-fume-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-fume-mode)
(add-hook 'lisp-mode-hook 'turn-on-fume-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-fume-mode)
(add-hook 'c-mode-hook 'turn-on-fume-mode)
(add-hook 'python-mode-hook 'turn-on-fume-mode)
(add-hook 'pyrex-mode-hook 'turn-on-fume-mode)

;;}}}
;;{{{   `-- SQL

(setq sql-product-alist
      '((sqlite
         :font-lock sql-mode-sqlite-font-lock-keywords
         :sqli-login (database)
         :sqli-connect sql-connect-sqlite
         :sqli-prompt-regexp "^sqlite> "
         :sqli-prompt-length 8)))

(setq sql-product 'sqlite
      sql-sqlite-program "sqlite3")

(defun lg-setup-sql-mode ()
  (define-key sql-mode-map (kbd "C-c e b") 'sql-send-buffer)
  (define-key sql-mode-map (kbd "C-c e r") 'sql-send-region)
  (define-key sql-mode-map (kbd "C-c e s") 'sql-send-paragraph)

  (defadvice sql-send-region (before run-sql-programm activate)
    "Start SQL product automatically when sending region."
    (save-window-excursion
      (unless (buffer-live-p sql-buffer)
        (sql-product-interactive)
        (sql-set-sqli-buffer-generally)))))

(add-hook 'sql-mode-hook 'lg-setup-sql-mode)

;;}}}

;;}}}

;;{{{ `-- Myemacs customization

;;; Parenface begin
;;; Taken from <X-URL:http://www.davep.org/emacs/parenface.el>
(defvar paren-face 'paren-face)

(defface paren-face
    '((((class color))
       (:foreground "DimGray")))
  "Face for displaying a paren."
  :group 'faces)

(defmacro paren-face-add-support (keywords)
  "Generate a lambda expression for use in a hook."
  `(lambda ()
    (let* ((regexp "(\\|)")
           (match (assoc regexp ,keywords)))
      (unless (eq (cdr match) paren-face)
        (setq ,keywords (append (list (cons regexp paren-face)) ,keywords))))))

(add-hook 'scheme-mode-hook (paren-face-add-support scheme-font-lock-keywords-2))
(add-hook 'lisp-mode-hook (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'emacs-lisp-mode-hook (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'lisp-interaction-mode-hook (paren-face-add-support lisp-font-lock-keywords-2))
;;; Parenface ends

;; Evaluate current-buffer
(defun lg-emacs-eval-buffer (buffer)
  "Evaluate BUFFER."
  (interactive (list (current-buffer)))
  (eval-buffer buffer)
  (message (format "Buffer(%s) evaluated." (buffer-name buffer))))

;; Evaluate active region
(defun lg-emacs-eval-region (start end &optional arg)
  "Evaluate region from START to END.
If prefix ARG is specified, then replace region with the evaluation result."
  (interactive "r\nP")
  (save-excursion
    (let ((res ""))
      (eval-region start end
                   (lambda (chr)
                     (setq res (concat res (char-to-string chr)))))
      ;; Fixate RES
      (when (> (length res) 1)
        (setq res (substring res 1 (1- (length res)))))
      ;; Display or insert RES
      (if (null arg)
          (message "Region(%d %d) => %s" start end res)
        (delete-region start end)
        (insert res)))))

;; NOTE: Installs C-ce Prefix for elisp operations (for lisp-mode only)
(define-key emacs-lisp-mode-map (kbd "C-c e b") 'lg-emacs-eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c e r") 'lg-emacs-eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c e f") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c e s") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c e e") 'macroexpand-sexp)

;; Editing functions
(define-key global-map (kbd "C-c e l") 'lg-fix-long-lines)
(define-key global-map (kbd "C-c e c") 'checkdoc)
(define-key global-map (kbd "C-c e i") 'lg-ispell-region-or-buffer)
(define-key global-map (kbd "C-c e w") 'whitespace-cleanup)
(define-key global-map (kbd "C-c e y") 'yofy)

;;; From myemacs, buffers quick switcher
(autoload 'buqis-gotonext "buqis" nil t)
(autoload 'buqis-gotoprev "buqis" nil t)
(autoload 'buqis-put-frame-ctx "buqis" nil t)
(autoload 'buqis-current-context "buqis")

(add-hook 'buqis-load-hook 'buqis-add-default-ctxs)

(define-key global-map (kbd "C-M-[") 'buqis-gotoprev)
(define-key global-map (kbd "C-M-]") 'buqis-gotonext)

;;; Display buqis context in title
(setq frame-title-format
      '((erc-modified-channels-alist erc-modified-channels-string)
        "Emacs: %b"
        (buqis-current-context " -")
        (buqis-current-context buqis-current-context)
        (buqis-current-context "-")
        " %*%& %f"))

;;; From myemacs, list w3m buffers
;; <X-URL:http://lgarc.narod.ru/xemacs/wl-list.el>
(autoload 'wl-show-list "wl-list" nil t)

;; Installs C-cw Prefix for W3M commands
(define-key global-map (kbd "C-c w l") 'wl-show-list)
(define-key global-map (kbd "C-c w s") 'w3m-select-buffer)
(define-key global-map (kbd "C-c w u") 'browse-url)
(define-key global-map (kbd "C-c w w") 'browse-url-at-point)
(define-key global-map (kbd "C-c w f") 'browse-url-firefox)
(define-key global-map (kbd "C-c w o") 'browse-url-opera)

;;; Hyper-apropos
(defun lg-hyper-apropos-highlight ()
  "Highlight search string."
  (lg-highlight-text hyper-apropos-last-regexp))

(add-hook 'hyper-apropos-mode-hook 'lg-hyper-apropos-highlight)

;;}}}
;;{{{ `-- Pair skeleton

;; Inserting pairs
(setq skeleton-end-hook nil)            ; do not insert newline after skeleton insertation

(defvar myskeleton-pairs
  '((?\" . (?\" ?\" ?\" _ ?\"))
    (?\( . (?\( ?\) ?\( _ ?\)))
    (?\[ . (?\[ ?\] ?\[ _ ?\]))
    (?\{ . (?\{ ?\} ?\{ _ ?\}))
    (?\' . (?\' ?\' ?\' _ ?\'))
    (?\` . (?\` ?\' ?\` _ ?\'))
    (?<  . (?< ?> ?< _ ?>)))
  "Table of skeletons pairs. Maybe local to buffer.")

(defvar myskeleton-pair-char-syntaxes
  '((?_ . ?w)
    (?- . ?w))
  "Character syntaxes to modify when inserting pairs.")

(defun myskeleton-pair-insert (arg)
  "Inserts pairs."
  (interactive "*P")

  (let* ((chr (event-key last-command-event))
         (pair (assoc chr myskeleton-pairs)))
    (if (null pair)
        (message "Character `%c' is not in `myskeleton-pairs'." chr)
      (cond ((and (listp arg) (not (null arg)))
             ;; Surraund current word with
             (let ((sse (mapcar (lambda (c)
                                  (cons c (char-syntax c)))
                                (mapcar 'car myskeleton-pair-char-syntaxes))))
               ;; Modify charecter syntax table entries
               (mapc (lambda (cse)
                       (modify-syntax-entry (car cse) (char-to-string (cdr cse))))
                     myskeleton-pair-char-syntaxes)

               (save-excursion
                 (when (not (looking-at "\\<"))
                   (backward-word 1))
                 (when (looking-at "\\sw")
                   (let ((pl 0)
                         (r (prefix-numeric-value arg)))
                     (while (> r 1)
                       (setq r (/ r 4))
                       (setq pl (1+ pl)))

                     (insert (make-string pl (nth 0 (cdr pair))))
                     (forward-word 1)
                     (insert (make-string pl (nth 1 (cdr pair)))))))

               ;; Restore character syntax table entries
               (mapc (lambda (cse)
                       (modify-syntax-entry (car cse) (char-to-string (cdr cse))))
                     sse)))

            (t (mapc (lambda (not-used)
                       (flet ((insert-before-markers (&rest args)
                                (apply #'insert args))) ; XXX hack to make M-( work in slime's REPL
                         (skeleton-insert
                          (cons nil (cdddr pair)))))
                     (make-list (prefix-numeric-value arg) 'not-used)))))))

(define-key global-map (kbd "C-M-{") 'backward-paragraph)
(define-key global-map (kbd "C-M-}") 'forward-paragraph)
(define-key global-map (kbd "C-M-'") 'abbrev-prefix-mark)

(define-key global-map (kbd "M-\"") 'myskeleton-pair-insert)
(define-key global-map (kbd "M-`") 'myskeleton-pair-insert)
(define-key global-map (kbd "M-'") 'myskeleton-pair-insert)
(define-key global-map (kbd "M-{") 'myskeleton-pair-insert)
(define-key global-map (kbd "M-(") 'myskeleton-pair-insert)
(define-key global-map (kbd "M-[") 'myskeleton-pair-insert)

;; pairing for C-mode
(defun lg-skelpair-cmode ()
  (make-local-variable 'myskeleton-pairs)
  (setq myskeleton-pairs (copy-alist myskeleton-pairs))
  (remassoc ?\{ myskeleton-pairs)
  (add-to-list 'myskeleton-pairs '(?{ . (?{ ?} ?{ '(progn (indent-according-to-mode) nil) \n _ \n ?} '(progn (indent-according-to-mode) nil)))))

(add-hook 'c-mode-hook 'lg-skelpair-cmode)
(add-hook 'objc-mode-hook 'lg-skelpair-cmode)

;;}}}
;;{{{ `-- Minor modes menu

;; Simple copy of `modeline-minor-mode-menu', however `event-buffer'
;; may be nil, so tpum's events emulation will work
(defun lg-minor-mode-menu (&optional event)
  "Popup minor modes menu."
  (interactive "e")
  (popup-menu
   (cons
    "Minor Mode Toggles"
    (sort
     (delq nil (mapcar
                #'(lambda (x)
                    (let* ((toggle-sym (car x))
                           (toggle-fun (or (get toggle-sym
                                                'modeline-toggle-function)
                                           (and (commandp toggle-sym)
                                                toggle-sym)))
                           (menu-tag (symbol-name (if (symbolp toggle-fun)
                                                      toggle-fun
                                                    toggle-sym))
                                     ;; Here a function should
                                     ;; maybe be invoked to
                                     ;; beautify the symbol's
                                     ;; menu appearance.
                                     ))
                      (and toggle-fun
                           (vector menu-tag
                                   toggle-fun
                                   ;; The following two are wrong
                                   ;; because of possible name
                                   ;; clashes.
                                        ;:active (get toggle-sym :active t)
                                        ;:included (get toggle-sym :included t)
                                   :style 'toggle
                                   :selected (and (boundp toggle-sym)
                                                  toggle-sym)))))
                minor-mode-alist))
     (lambda (e1 e2)
       (string< (aref e1 0) (aref e2 0)))))))

;;}}}
;;{{{ `-- Switching windows customization

;;; Enhance window switching a little
(defvar lg-other-window-exclude
  "\\(\\*[cChH][oe][ml].*\\|TAGS\\)"
  "Regexp for buffer name which window to skip.")

(defun lg-other-window (arg)
  "Switch to other window, but skip uninterested windows."
  (interactive "p")
  (let ((fwin (selected-window))        ;first window
        (swin nil)
        (nextwinf (if (> arg 0) 'other-window 'backward-other-window)))

    (setq arg (abs arg))
    (while (not swin)
      (funcall nextwinf 1)
      (if (eq (selected-window) fwin)
          (setq swin (selected-window))
        (let ((bname (buffer-name)))
          (unless (string-match lg-other-window-exclude bname)
            (if (> arg 1)
                (setq arg (1- arg))
              (setq swin (selected-window)))))))
    (if (eq swin fwin)
        (beep)
      (select-window swin))))

(defun lg-backward-other-window (arg)
  "Switch backward to other window, but skip uninterested windows."
  (interactive "p")
  (lg-other-window (- arg)))

(define-key global-map (kbd "C-tab") 'other-window)
(define-key global-map (kbd "C-iso-left-tab") 'backward-other-window)
(define-key global-map (kbd "C-x C-tab") 'lg-other-window)
(define-key global-map (kbd "C-x C-iso-left-tab") 'lg-backward-other-window)

;;}}}
;;{{{ `-- Window configurations customization

;;; Useful window configurations operations
(defun lg-pop-winconfig (arg)
  "Run `pop-window-configuration' ARG times."
  (interactive "p")
  (mapcar (lambda (not-used) (pop-window-configuration)) (make-list arg nil))
  (message "%d configs in stack." (undoable-stack-a-length (window-config-stack))))

(defun lg-unpop-winconfig (arg)
  "Run `unpop-window-configuration' ARG times."
  (interactive "p")
  (mapcar (lambda (not-used) (unpop-window-configuration)) (make-list arg nil))
  (message "%d configs in stack." (undoable-stack-a-length (window-config-stack))))

(define-key global-map (kbd "C-x C-,") 'lg-pop-winconfig)
(define-key global-map (kbd "<f11>") 'lg-pop-winconfig)
(define-key global-map (kbd "C-x C-.") 'lg-unpop-winconfig)

;;}}}
;;{{{ `-- Reading mode, make fonts large

(defface lg-reading-face nil nil)
(set-face-font 'lg-reading-face "-xos4-terminus-bold-r-*-*-32-*-*-*-*-*-koi8-r")

(defface lg-reading-emph-face nil nil)
(set-face-font 'lg-reading-emph-face "-xos4-terminus-bold-i-*-*-32-*-*-*-*-*-koi8-r")

(defface lg-reading-header-face nil nil)
(set-face-font 'lg-reading-header-face "-*-helvetica-bold-r-*-*-32-*-*-*-*-*-koi8-r")
(set-face-background 'lg-reading-header-face "gray70")

(defface lg-reading-subheader-face nil nil)
(set-face-font 'lg-reading-subheader-face "-*-helvetica-medium-r-*-*-32-*-*-*-*-*-koi8-r")
(set-face-background 'lg-reading-subheader-face "gray75")

(defvar lg-reading-mode nil "Non-nil mean `lg-reading-face' used for buffer.")
(make-variable-buffer-local 'lg-reading-mode)

(defun lg-reading-mode (arg)
  "Apply `lg-reading-face' to current buffer."
  (interactive "P")

  (setq lg-reading-mode (not lg-reading-mode))
  (if lg-reading-mode
      (progn
          (add-text-properties
           (point-min) (point-max) '(face lg-reading-face))

          (save-excursion
            ;; Search for headers
            (beginning-of-buffer)
            (while (re-search-forward "^[0-9]+\\.[^0-9].*" nil t)
              (add-text-properties
               (point-at-bol) (point-at-eol)
               '(face lg-reading-header-face))))
          (save-excursion
            (beginning-of-buffer)
            ;; Search for subheaders and subsubheaders
            (while (re-search-forward "^[0-9]+\\.[0-9]+.*" nil t)
              (add-text-properties
               (point-at-bol) (point-at-eol)
               '(face lg-reading-subheader-face))))

          (save-excursion
            (beginning-of-buffer)
            (while (re-search-forward "\".+\"" nil t)
              (add-text-properties
               (match-beginning 0) (match-end 0)
               '(face lg-reading-emph-face))))
          )

    (remove-text-properties (point-min) (point-max) '(face)))
  (message "My reading mode: %s" (if lg-reading-mode "ON" "OFF")))

(define-key global-map (kbd "C-c m R") 'lg-reading-mode)

;;}}}
;;{{{ `-- Hide carriage return (^M) mode

(require 'atomic-extents)

(defvar hide-cr-mode nil)
(make-variable-buffer-local 'hide-cr-mode)

(define-minor-mode hide-cr-mode
  "Hide carriage return minor mode."
  nil " HideCR" nil
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\x0d" nil t)
      (add-text-properties (match-beginning 0) (match-end 0)
                           `(invisible ,hide-cr-mode atomic t)))))

;;}}}

;;{{{ +-  Keyboard bindings
;;{{{   `-- Different bindings

(define-key global-map (kbd "<f3>") 'cvs-examine)
(define-key global-map (kbd "<f4>") 'svn-status)
(define-key global-map (kbd "<f5>") 'ibuffer)
(define-key global-map (kbd "<f6>") 'mode-compile)
(define-key global-map (kbd "<f7>") 'next-error)
(define-key global-map (kbd "<f9>") 'call-last-kbd-macro)

(global-set-key [iso-left-tab] [(shift tab)])
(global-set-key [(meta iso-left-tab)] 'ispell-complete-word)

;;; --- Override some Emacs bindings
(define-key global-map (kbd "C-j") 'lg-insert-nl-at-eol)
(define-key global-map (kbd "C-x C-c") 'lg-ask-exit-emacs)
(define-key global-map (kbd "C-x e") 'call-last-kbd-macro)
(define-key global-map (kbd "C-k") 'lg-kill-line)
(define-key global-map (kbd "C-w") 'lg-kill-region)
(define-key global-map (kbd "M-q") 'lg-fill-paragraph-or-region)

;; Mark operations
(define-key global-map (kbd "C-SPC") 'himarks-set-mark-command)
(define-key global-map (kbd "C-@") 'himarks-set-mark-command)
(define-key global-map (kbd "C-x SPC") 'himarks-set-mark-no-activate)
(define-key global-map (kbd "<f8>") 'lg-jump-to-next-mark)

;;; For quick region commenting
(define-key global-map (kbd "C-c C-c") 'comment-region)
(define-key global-map (kbd "C-c ;") 'comment-region)

;; C-c C-s - switch to supplying buffer (comint or any other)

;;; Remove annoying iconifying
(define-key global-map (kbd "C-z") 'nil)

;;; Use cool `ibuffer' instead of ugly `list-buffers'
(define-key global-map (kbd "C-x C-b") 'ibuffer)

;; Sh-Ins inserts x-selection
(define-key global-map (kbd "S-insert") 'lg-mouse-yank)

;;; Set column for C-n, C-p commands
(define-key global-map (kbd "C-x C-n") 'set-goal-column)

;; <tab> -> indent or complete
;(define-key global-map '(tab) 'indent-or-complete)

;; <Shift-Tab> -> old <tab>
(define-key global-map (kbd "S-tab") 'indent-for-tab-command)

;; keymap for X selections operations
;(define-key global-map (kbd "C-c s r") 'mym-region-to-cutbuff)

;; Killing bindings
(define-key global-map (kbd "M-<f4>") 'lg-kill-current-buffer)
(define-key global-map (kbd "C-x k") 'lg-kill-current-buffer)
(define-key global-map (kbd "s-k") 'lg-kill-current-buffer)

;; Hmm, long to type binding, but I used to it
(define-key global-map (kbd "C-x 4 C-M-l") 'lg-switch-to-other-other-window)
(define-key global-map (kbd "s-l") 'lg-switch-to-other-other-window)

;; Killing buffer in other window
(define-key global-map (kbd "C-x 4 k") 'lg-kill-buffer-in-other-window)

;; some useful bindings
(define-key global-map (kbd "C-x M-=") 'count-lines-buffer)
(define-key global-map (kbd "M-p") 'scroll-down-command)
(define-key global-map (kbd "M-n") 'scroll-up-command)
(define-key global-map (kbd "M-o") 'lg-do-cmd-under-save-excursion)
(define-key global-map (kbd "M-<SPC>") 'lg-fixup-whitespace)
(define-key global-map (kbd "M-\\") 'lg-delete-horizontal-space)

(define-key global-map (kbd "M-z") 'zap-to-char)
(define-key global-map (kbd "C-x M-z") 'zap-up-to-char)

;; To join two lines (aka vi's J)
(define-key global-map (kbd "C-^") (kbd "C-u M-^"))

;; Redoing
;; Unbound old 'undo, so new 'undo from redo package can be autoloaded
(fmakunbound 'undo)

;; In case some one want to use it
(defvar last-buffer-undo-list nil)
(make-variable-buffer-local 'last-buffer-undo-list)

(autoload 'undo "redo" "Undo from redo package." t)
(autoload 'redo "redo" "Redo from redo package." t)

(define-key global-map (kbd "C-/") 'undo)
(define-key global-map (kbd "C-x C-/") 'redo)

(define-key global-map (kbd "C-x r d") 'delete-rectangle)
(define-key global-map (kbd "C-x r t") 'replace-rectangle)
(define-key global-map (kbd "C-x r #") 'lg-calc-register)

;; Shut ut debugger
(add-to-list 'debug-ignored-errors "No further undo")

;; Marking
(defun lg-mark-beginning-of-line (arg)
  "Put mark at the beginning of line."
  (interactive "p")
  (mark-something 'lg-mark-beginning-of-line 'beginning-of-line arg))

(defun lg-mark-beginning-of-sentance (arg)
  "Put mark at the beginning of sentance."
  (interactive "p")
  (mark-something 'lg-mark-beginning-of-sentance 'backward-sentence arg))

(define-key global-map (kbd "C-x M-b") 'lg-mark-beginning-of-line)
(define-key global-map (kbd "C-x M-f") 'mark-end-of-line)
(define-key global-map (kbd "C-x M-l") 'mark-end-of-line)
(define-key global-map (kbd "C-x M-a") 'lg-mark-beginning-of-sentance)
(define-key global-map (kbd "C-x M-e") 'mark-end-of-sentence)
(define-key global-map (kbd "C-x M-h") 'mark-sexp)
(define-key global-map (kbd "C-x M-s") 'mark-sexp)

;;}}}
;;{{{   `-- C-o - Prefix for -other- commands

;; Make C-o to be keymap for othering
(define-key global-map (kbd "C-O") 'open-line)

(define-key global-map (kbd "C-o") nil)
(define-key global-map (kbd "C-o 0") 'lg-kill-buffer-and-window)
(define-key global-map (kbd "C-o C-f") 'find-file-other-window)
(define-key global-map (kbd "C-o v") 'find-variable-other-window)
(define-key global-map (kbd "C-o f") 'find-function-other-window)
(define-key global-map (kbd "C-o l") 'find-library-other-window)
(define-key global-map (kbd "C-o t") 'find-tag-other-window)
(define-key global-map (kbd "C-o b") 'iswitchb-buffer-other-window)
(define-key global-map (kbd "C-o a") 'add-change-log-entry-other-window)
(define-key global-map (kbd "C-o d") 'dired-other-window)
(define-key global-map (kbd "C-o C-o") 'iswitchb-display-buffer)
(define-key global-map (kbd "C-o M-C-l") 'lg-switch-to-other-other-window)
(define-key global-map (kbd "C-o k") 'lg-kill-buffer-in-other-window)

;;}}}
;;{{{   `-- C-ch - Prefix for Help commands

;; Install C-ch for help commands
(define-key global-map (kbd "C-c h s") 'info-lookup-symbol)
(define-key global-map (kbd "C-c h m") 'lg-manual-entry)
(define-key global-map (kbd "C-c h l") 'lispdoc)

;;}}}
;;{{{   `-- C-ck - Prefix for Keyboard commands

;;; Installs C-ck Prefix for Keyboard control commands
(defconst lg-keyboard-rate-default '(250 . 100)
  "Current keyboard rate.")
(put 'lg-keyboard-rate 'name 'default)
(put 'lg-keyboard-rate 'delay (car lg-keyboard-rate-default))
(put 'lg-keyboard-rate 'rate (cdr lg-keyboard-rate-default))

(defmacro define-lg-keyrate (name delay rate)
  "Define new keyboard rate setter command.
NAME - Rate name
DELAY - Delay before repeating
RATE - Repeat rate per second"
  (let ((fsym (intern (format "lg-keyrate-set-%s" name))))
    `(defun ,fsym ()
       ,(format "Set %s keyboard rate." name)
       (interactive)
       (put 'lg-keyboard-rate 'name ',name)
       (put 'lg-keyboard-rate 'delay ,delay)
       (put 'lg-keyboard-rate 'rate ,rate)
       (shell-command (format "xset r rate %d %d" ,delay ,rate))
       (message (format "%s keyrate intalled." (capitalize (symbol-name ',name)))))))

(define-lg-keyrate default (car lg-keyboard-rate-default) (cdr lg-keyboard-rate-default))
(define-lg-keyrate fast 300 150)
(define-lg-keyrate slow 250 25)

(defun lg-keyrate-current ()
  "Get current keyboard rate."
  (interactive)
  (message "Current keyboard rate is %s: %d delay, %d rate\n"
           (get 'lg-keyboard-rate 'name) (get 'lg-keyboard-rate 'delay)
           (get 'lg-keyboard-rate 'rate)))

(define-key global-map (kbd "C-c k d") 'lg-keyrate-set-default)
(define-key global-map (kbd "C-c k f") 'lg-keyrate-set-fast)
(define-key global-map (kbd "C-c k s") 'lg-keyrate-set-slow)
(define-key global-map (kbd "C-c k h") 'lg-keyrate-current)

;(add-hook 'after-init-hook 'lg-keyrate-set-fast)

;;}}}
;;{{{   `-- C-cr - Prefix for Regexp commands

;; C-cr - Prefix for REGULAR EXPRESSIONS
(define-key global-map (kbd "C-c r b") 're-builder)
(define-key global-map (kbd "C-c r s") 're-search-forward)
(define-key global-map (kbd "C-c r r") 're-search-backward)
(define-key global-map (kbd "C-c r e") 'query-replace-regexp)
(define-key global-map (kbd "C-c r g") 'igrep)
(define-key global-map (kbd "C-c r o") 'occur)

;;}}}
;;{{{   `-- C-cd - Prefix for Dictionaries commands

;; C-cd Prefix for DICT
(define-key global-map (kbd "C-c d d") 'dict)
(define-key global-map (kbd "C-c d r") 'rdict)
(define-key global-map (kbd "C-c d t") 'google-translate-region)

;;}}}
;;{{{   `-- C-cf - Prefix for Finding commands

;; C-cf Prefix for FIND functions
(define-key global-map (kbd "C-c f f") 'find-function)
(define-key global-map (kbd "C-c f v") 'find-variable)
(define-key global-map (kbd "C-c f l") 'find-library)
(define-key global-map (kbd "C-c f k") 'find-function-on-key) ;; equivalent to C-x K
(define-key global-map (kbd "C-c f t") 'find-tag)
(define-key global-map (kbd "C-c f T") 'lg-find-tag-regex)
(define-key global-map (kbd "C-c f h") 'hexl-find-file)
(define-key global-map (kbd "C-c f a") 'copy-from-above-command)

;;}}}
;;{{{   `-- C-cm - Prefix for Modes/Misc commands

;; Installs C-cm Prefix for MISC commands
(define-key global-map (kbd "C-c m c") 'lg-mini-calc)
(define-key global-map (kbd "C-c m f") 'folding-mode)
(define-key global-map (kbd "C-c m a") 'ascii-display)
(define-key global-map (kbd "C-c m g") 'imenu)
(define-key global-map (kbd "C-c m o") 'occur)
(define-key global-map (kbd "C-c m |") 'vertical-mode)
(define-key global-map (kbd "C-c m v") 'vvb-mode)
(define-key global-map (kbd "C-c m h") 'highline-local-mode)
(define-key global-map (kbd "C-c m w") 'old-whitespace-mode)
(define-key global-map (kbd "C-c m F") 'flyspell-mode)
(define-key global-map (kbd "C-c m i") 'flyspell-mode)
(define-key global-map (kbd "C-c m m") 'himarks-mode)
(define-key global-map (kbd "C-c m s") 'stripes-mode)
(define-key global-map (kbd "C-c m l") 'lg-column-ruler)
(define-key global-map (kbd "C-c m d") 'dot-mode)
(define-key global-map (kbd "C-c m r") 'rst-mode)
(define-key global-map (kbd "C-c m <RET>") 'hide-cr-mode)
(define-key global-map (kbd "C-c m p") 'pabbrev-mode)

;; Marking
(define-key global-map (kbd "C-c m t") 'id-select-thing)

;;}}}
;;{{{   `-- C-cl - Prefix for Listing commands

(define-key global-map (kbd "C-c l a") 'list-abbrevs)
(define-key global-map (kbd "C-c l b") 'list-bookmarks)
(define-key global-map (kbd "C-c l h") 'list-command-history)
(define-key global-map (kbd "C-c l c") 'list-colors-display)
(define-key global-map (kbd "C-c l f") 'list-faces-display)
(define-key global-map (kbd "C-c l i") 'list-itimers)
(define-key global-map (kbd "C-c l m") 'list-matching-lines)
(define-key global-map (kbd "C-c l t") 'list-text-properties-at)
(define-key global-map (kbd "C-c l p") 'list-processes)
(define-key global-map (kbd "C-c l s") 'list-strokes)
(define-key global-map (kbd "C-c l .") 'list-tags)
(define-key global-map (kbd "C-c l k") 'browse-kill-ring)
(define-key global-map (kbd "C-c l r") 'list-register)
(define-key global-map (kbd "C-c l o") 'ofupd-packages-list)

;;}}}
;;{{{   `-- C-cc - Prefix for Count commands

(define-key global-map (kbd "C-c c l") 'lg-count-lines)
(define-key global-map (kbd "C-c c w") 'lg-count-words)
(define-key global-map (kbd "C-c c m") 'lg-count-matches)

;;}}}
;;{{{   `-- Mouse buttons bindings

;; Mouse wheel support
(define-key global-map [button4] 'lg-scroll-down)
(define-key global-map [button5] 'lg-scroll-up)

(define-key global-map (kbd "S-<button1>") 'vvb-popup-menu)
(define-key global-map (kbd "S-<button3>") 'imenu)
(define-key global-map (kbd "C-<button1>") 'lg-minor-mode-menu)
(define-key global-map (kbd "C-<button3>") 'popup-buffer-menu)

;;}}}
;;}}}

;;{{{ `-- Candy view settings

;; Maybe start gnuserv
(setq gnuserv-temp-file-regexp "")
(unless (= (random 100) 13)
  (gnuserv-start))

;; Useful if using 14x8 terminus fonts
(defface lg-grido-face
  '((((class color) (background dark))
     (:foreground "gray10"))
    (((class color) (background light))
     (:foreground "gray77"))
    (t (:foreground "gray77")))
  "Face for grid.")

(push '("grido" (face-foreground 'lg-grido-face)) xpm-color-symbols)

(defconst lg-square-64x64-xpm
  (concat
   "/* XPM */\n"
   "static char *mini_square_xpm[] = {\n"
   "/* columns rows colors chars-per-pixel */\n"
   "\"64 64 2 1\",\n"
   "\"       c None s background\",\n"
   "\".      c gray77 s grido\",\n"
   "/* pixels */"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"................................................................\"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "};"))

(set-face-background-pixmap
 'default
 (make-image-specifier (vector 'xpm :data lg-square-64x64-xpm)))

;;}}}
;;{{{ `-- XWEM

(setq find-function-regexp
      (concat "^\\s-*(\\(def[^cgvW]\\w+\\*?"
              "\\|define-function"
              "\\|define-obsolete-function-alias"
              "\\|define-compatible-function-alias"
              "\\|define-derived-mode"
              "\\|define-xwem-command"
              "\\)\\s-+%s\\(\\s-\\|$\\)"))

; (setq initial-frame-plist '(top 10 left 10 width 98 height 41)
;       default-frame-plist '(top 10 left 10 width 98 height 41))

(setq sxemacs-under-xwem nil)
(when (string= (getenv "XWEM_RUNNING") "notyet")
  (setq sxemacs-under-xwem t)
;  (setq xwem-debug t)
;  (setq xwem-meta-modifier 'control)
  (add-menu-button '("File") ["KBD QUIT" xwem-kbd-quit t])
  (xwem-init))

;;}}}
;;{{{ `-- Recent files

(setq recent-files-save-file
      (expand-file-name ".recent-files.el" user-init-directory))
(recent-files-initialize)

(defun lg-select-recent-file (fname)
  "Switch to some recent file."
  (interactive
   (list (progn
           (require 'iswitchb)
           (flet ((iswitchb-make-buflist
                    (default)
                    (setq iswitchb-buflist
                          (mapcar #'file-name-nondirectory
                                  (mapcar #'car recent-files-list)))))
             (iswitchb-read-buffer "Recent file: ")))))
  (let ((file (loop for fn in (mapcar #'car recent-files-list)
                when (string= fname (file-name-nondirectory fn))
                return fn)))
    (find-file file)))

(define-key global-map (kbd "C-x B") 'lg-select-recent-file)

;;}}}
;;{{{ `-- Desktop

(require 'desktop)
(setq desktop-dirname user-init-directory)

(push 'dired-mode desktop-modes-not-to-save)
(push 'Wand-mode desktop-modes-not-to-save)
(push "worklog" desktop-clear-preserve-buffers)
(setq desktop-buffers-not-to-save
      "\\(^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\|^worklog\\)$")
;; Do not save any files
;(setq desktop-files-not-to-save ".*")

(desktop-load-default)
(setq desktop-globals-to-save
      '(desktop-missing-file-warning
        kill-ring-yank-pointer
        kill-ring

        tags-file-name
        tags-table-list
        search-ring
        regexp-search-ring
        register-alist
        file-name-history
        minibuffer-history
        read-expression-history
        read-command-history
        shell-command-history

        sudoku-custom-puzzles
        sudoku-solved-puzzles
        command-history))

;; Load the desktop
(defvar lg-desktop-last "default")

(defvar lg-desktops
  '(("default" . ".desktop.default")
    ("Elisp" . ".desktop.elisp")
    ("SXEmacs" . ".desktop.sxemacs")
    ("xwem" . ".desktop.xwem"))
  "Alist of desktop files.")

(defun lg-desktop-load (&optional name)
  (interactive (list (completing-read
                      "Desktop [default]: "
                      (mapcar #'(lambda (el) (list (car el))) lg-desktops))))
  (when (or (null name) (string= name ""))
    (setq name "default"))
  (let ((desktop-basefilename (cdr (assoc name lg-desktops)))
        (default-directory (expand-file-name user-init-directory)))

    (setq desktop-partially-loaded-p t)
    (load (expand-file-name desktop-basefilename user-init-directory)
          t t t)
    (run-hooks 'desktop-delay-hook)
    (setq desktop-delay-hook nil)
    (setq desktop-partially-loaded-p nil)
    (message (format "%s desktop loaded" name))))

(defun lg-desktop-save (&optional name)
  (interactive (list (completing-read
                      (format "Desktop [%s]: " lg-desktop-last)
                      (mapcar #'(lambda (el) (list (car el))) lg-desktops))))
  (when (or (null name) (string= name ""))
    (setq name lg-desktop-last))
  (let ((desktop-basefilename (cdr (assoc name lg-desktops))))
    (desktop-save (expand-file-name user-init-directory)))
  (setq lg-desktop-last name))

(defun lg-desktop-setup ()
  (mapc #'(lambda (v)
            (desktop-truncate v 50))
        (list search-ring regexp-search-ring minibuffer-history
              read-command-history shell-command-history command-history)))

(remove-hook 'kill-emacs-hook 'desktop-kill) ; remove default desktop saver
(add-hook 'kill-emacs-hook 'lg-desktop-save) ; save lg desktop on exit
(add-hook 'kill-emacs-hook 'lg-desktop-setup)

;;}}}
;;{{{ `-- Editing tools

;; Backing up in single directory
(require 'backup-dir)
(setq bkup-backup-directory-info
      '(("/home/lg/.*" "/~/.backups/" ok-create full-path prepend-name)))

;; mode-motion+
(autoload 'mode-motion+-hook-install "mode-motion+" nil t)
(global-set-key (kbd "C-c m M") 'mode-motion+-hook-install)

;; Powerup keyboard macroses
(require 'power-macros)
;; Use explicit M-x pm-define RET to save last macro
(setq power-macros-run-on-end-macro nil)
(setq power-macros-file
      (expand-file-name "custom-macros.el" user-init-directory))
(ignore-errors (load power-macros-file))

(defvar lg-minibuffer-color-saved
  (face-background-name 'default))
(defvar lg-minibuffer-color-when-defining-macro "deeppink"
  "Color to use for minibuffer when defining macro.
I hate this color, so i wont forget to finish macro wheen needed.")
(defun lg-colorize-minibuffer (color)
  (setq lg-minibuffer-color-saved
        (face-background-name 'default (minibuffer-window)))
  (set-face-background 'default color (minibuffer-window)))

(defadvice pm-start-kbd-macro (before lg-colorize-minibuffer activate)
  "Start colorizing minibuffer untill macro is defined."
  (lg-colorize-minibuffer lg-minibuffer-color-when-defining-macro))

(defadvice pm-end-kbd-macro (before lg-decolorize-minibuffer activate)
  "Macro defined, stop colorizing minibuffer."
  (lg-colorize-minibuffer lg-minibuffer-color-saved))

;; Save places of visited files
(require 'saveplace)
(setq save-place-file
      (expand-file-name ".emacs-places" user-init-directory))
(setq-default save-place t)

(defun emacs-places-save ()
  "Save places to file."
  (interactive)
  (save-place-kill-emacs-hook))

;; Keep point while scrolling
(require 'scroll-in-place)

;;}}}

;;{{{ `-- BBDB

(setq bbdb-file (expand-file-name ".bbdb" user-init-directory)
      bbdb-quiet-about-name-mismatches 0 ; do not ask
      bbdb-offer-save 'just-save-withot-offering)

(require 'bbdb-print)
(push 'face bbdb-print-omit-fields)
(push 'cface bbdb-print-omit-fields)

;;}}}
;;{{{ `-- MoiKrug BBDB

(autoload 'moikrug-bbdb-invite "moikrug" nil t)
(autoload 'moikrug-bbdb-merge-in "moikrug" nil t)

(setq moikrug-login "zevlg@yandex.ru")

(add-hook 'bbdb-mode-hook
          (lambda ()
            (define-key bbdb-mode-map [?u] 'moikrug-bbdb-merge-in)
            (define-key bbdb-mode-map [?i] 'moikrug-bbdb-invite)))

;;}}}
;;{{{ `-- Printing

(setq ps-italic-faces '(font-lock-comment-face))
(setq ps-bold-faces '(font-lock-keyword-face font-lock-function-name-face))

(setq ps-print-header t
      ps-print-footer nil)

(setq ps-print-color-p nil)             ; don't use colors for printing
(setq ps-paper-type 'a4 )               ; the type of paper

(setq ps-font-size 9)
(setq ps-spool-duplex nil)
(setq ps-printer-name nil)

;; page margins
(setq ps-left-margin (/ (* 72  1.5) 2.54)) ;   1.5 cm
(setq ps-right-margin (/ (* 72  0.7) 2.54)) ;   0.7 cm
(setq ps-inter-column (/ (* 72  1.0) 2.54)) ;   1 cm

;(setq ps-bottom-margin (/ (* 72  0.5) 2.54)) ; 0.5 cm
(setq ps-bottom-margin (/ (* 72  0.9) 2.54)) ; 0.9 cm
(setq ps-top-margin    (/ (* 72  0.8) 2.54)) ; 0.8 cm
(setq ps-header-offset (/ (* 72  0.3) 2.54)) ; 0.3 cm
(setq ps-header-line-pad 0.15)

(setq ps-n-up-margin (/ (* 72  0.5) 2.54)) ; 0.5 cm

;; C-cp is prefix for printing
(define-key global-map (kbd "C-c p b") 'ps-print-buffer-with-faces)
(define-key global-map (kbd "C-c p r") 'ps-print-region-with-faces)
(define-key global-map (kbd "C-c p s") 'ps-spool-buffer-with-faces)
(define-key global-map (kbd "C-c p d") 'ps-despool)

(define-key global-map [print] 'ps-print-buffer-with-faces)

;;}}}
;;{{{ `-- Yoficator

;; Workaround mac os
(when (eq system-type 'darwin)
  (ffi-load-library "/opt/local/lib/libsqlite3"))

;; I'm from Ulyanovsk(Karamzin's birthplace), so I like YO symbol and
;; try widely use it.
;; 
;; This is SXEmacs specific! uses ffi-sqlite to acces FTS3 virtual
;; tables.
;; 
;; Some code taken from http://python.anabar.ru/macros/emin/yo/yo.el
(defcustom yo-db-file (expand-file-name "yo.db" user-init-directory)
  "File with yo database.")

(defcustom yo-cutting-strings (list "\\-" "\"=" "\"~")
  "Words in the text may be splitting by some strings:
for example: hy\\-phe\\-na\\-ti\\-on in TeX")

(defconst yo-cutting-regexp
  (concat "\\(?:"
          (mapconcat 'regexp-quote
                     yo-cutting-strings "\\|")
          "\\)"))

;; Under Mule: 1493 - little e, 1461 - big E
(defconst yo-e-word-regexp
  (concat "\\(?:\\w\\(?:\\w\\|" yo-cutting-regexp "\\)*\\)?"
          "\\(?:" (char-to-string (int-to-char 1493)) "\\|"
          (char-to-string (int-to-char 1461)) "\\)"
          "\\(?:\\w\\(?:\\w\\|" yo-cutting-regexp "\\)*\\)?"))

(defvar yo-db nil)

(defun yo-word (word &optional table)
  "Return yo variant of the WORD.
Return value:
  nil     - no yo variant
  string  - unambiguous yo variant
  (maybe . string) - ambiguous yo variant"
  (unless yo-db
    (setq yo-db (sqlite-open yo-db-file)))
  (let* ((sql (format "select val from %S where key match ?"
                      (or table 'onlyyo) word))
         (eword (encode-coding-string word 'utf-8))
         (result (car (car (sqlite-rows yo-db sql (list eword))))))
    (if result
        (decode-coding-string result 'utf-8)
      (when (or (null table) (eq table 'onlyyo))
        (let ((mres (yo-word word 'maybeyo)))
          (when mres (cons 'maybe mres)))))))

(defun yofy-region (begin end)
  "Yofy the region."
  (interactive "r")
  (save-restriction
    (save-excursion
      (narrow-to-region begin end)
      (beginning-of-buffer)
      (let (current-e-word current-yo-word)
        (while (re-search-forward yo-e-word-regexp nil t)
          (save-match-data
            (setq current-e-word
                  (downcase
                   (replace-in-string
                    (match-string 0) yo-cutting-regexp "")))
            (setq current-yo-word (yo-word current-e-word)))

          (cond ((stringp current-yo-word)
                 (replace-match current-yo-word nil))
                ((and (consp current-yo-word)
                      (eq (car current-yo-word) 'maybe))
                 (setq current-yo-word (cdr current-yo-word))
                 (if search-highlight
                     (isearch-highlight (match-beginning 0) (match-end 0)))
                 (when (y-or-n-p (format "\"%s\" --> \"%s\"? "
                                         current-e-word current-yo-word))
                   (undo-boundary)
                   (replace-match current-yo-word nil))
                 (isearch-dehighlight))))
        ))))

(defun yofy-buffer ()
  "Yofy whole buffer."
  (interactive)
  (yofy-region (point-min) (point-max)))

(defun yofy ()
  "Yofy region or buffer."
  (interactive)
  (if (region-active-p)
      (yofy-region (region-beginning) (region-end))
    (yofy-buffer)))

;;}}}
;;{{{ `-- Maxima - Free algebra system

;; Image maxima
;; <X-URL:http://members3.jcom.home.ne.jp/imaxima/>

(push "/usr/local/share/maxima/5.17.0/emacs" load-path)
(setq maxima-save-input-history t
      maxima-input-history-length 200
      maxima-input-history-file
      (expand-file-name ".maxima_history" user-init-directory))

;; Setup imaxima
;(setq imaxima-use-maxima-mode-flag t)
(setq imaxima-equation-color "navy"
      imaxima-label-color "red3")
(setq imaxima-fnt-size "huge")

(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(setq imaxima-use-maxima-mode-flag t)

(autoload 'maxima "maxima" "Frontend for maxima" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input")

(defun lg-maxima-send-single-line ()
  "Send single line."
  (interactive)
  (end-of-line)
  (unless (= (char-before) ?\;)
    (insert ";"))
  (inferior-maxima-check-and-send-line))

(add-hook 'inferior-maxima-mode-hook
          (lambda ()
            (local-set-key (kbd "C-j") 'lg-maxima-send-single-line)
            (local-set-key (kbd "C-M-l") 'switch-to-other-buffer)
            (when (fboundp 'set-process-coding-system)
              (set-process-coding-system
               inferior-maxima-process 'binary))
            ))
;             ;; Initialize imaxima
;             (require 'imaxima)
;             (make-directory
;              (setq imaxima-tmp-subdir
;                    ;; For some reason TeX doesn't grok underscores in file names
;                    (imaxima-subst-char-in-string
;                     ?_ ?= (make-temp-name
;                            (expand-file-name
;                             "imaxima" imaxima-tmp-dir)))))
;             (imaxima-setup)
;             ;; Mule
;             (when (fboundp 'set-process-coding-system)
;               (set-process-coding-system
;                inferior-maxima-process 'binary))
;             ))

;;}}}
;;{{{ `-- Webjump

;; <X-URL:http://www.neilvandyke.org/webjump/webjump-plus.el>
(ofupd-register "webjump-plus" "~/.sxemacs/lisp/thirdpart/webjump-plus.el"
                "http://www.neilvandyke.org/webjump/webjump-plus.el"
                #'ofupd-ve-version-colon)

(setq webjump-sites
      `(
        ("Google" .
         [simple-query "www.google.com" "www.google.com/search?q=" ""])

        ("Yandex.RU" .
         [simple-query "www.ya.ru"
                       "http://www.yandex.ru/yandsearch?text="
                       "&rpt=rad"])
        ("Youtube.com" .
         [simple-query "www.youtube.com"
                       "http://www.youtube.com/results?search_query="
                       "&search_type=&aq=f"])

        ("MiniNova" .
         [simple-query "www.mininova.org"
                       "http://www.mininova.org/search/?search=" ""])

        ("CodeSearch" .
         [simple-query "www.google.com" "www.google.com/codesearch?q=" ""])

        ("Images" .
         [simple-query "images.google.com" "images.google.com/images?q=" ""])

        ("LispDoc" .
         [simple-query "lispdoc.com" "http://lispdoc.com/?q=" ""])

        ("Ru.Wikipedia.ORG" .
         [simple-query "ru.wikipedia.org"
                       "http://ru.wikipedia.org/wiki/Special:Search?search=" ""])

        ("En.Wikipedia.ORG" .
         [simple-query "en.wikipedia.org"
                       "http://en.wikipedia.org/wiki/Special:Search?search=" ""])

        ("Gramota.RU" .
         [simple-query
          "www.gramota.ru" "http://dic.gramota.ru/search.php?word="
          "&lop=x&gorb=x&efr=x&zar=x&ag=x&ab=x&lv=x&pe=x&az=x"])

        ("Price.RU" .
         [simple-query "www.price.ru"
                       "www.price.ru/bin/price/ctgrlist?pnam="
                       "&base=1&where=00"])

        ("Define" .
         [simple-query "www.google.com"
                       "www.google.com/search?q=define%3A" ""])

        ("Undev PM" . [simple-query "pm.undev.cc" "pm.undev.cc/issues/show/" ""])

        ("RFC Editor" .
         [simple-query "www.rfc-editor.org"
                       "www.rfc-editor.org/cgi-bin/rfcsearch.pl?searchwords="
                       ,(concat "&opt=All%20Fields"
                                "&filefmt=txt"
                                "&search_doc=search_all"
                                "&match_method=prefix"
                                "&sort_method=newer"
                                "&num=25"
                                "&format=ftp")])
        ))

(define-key global-map (kbd "C-c w j") 'webjump)

;;}}}

(when nil
;;{{{ `-- Faces Setup

(set-face-font 'default "-*-terminus-medium-r-*-*-32-*-*-*-*-*-*-r")

(set-face-font 'italic "-*-terminus-medium-i-*-*-32-*-*-*-*-*-*-r")
(set-face-font 'bold "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")
(set-face-font 'bold-italic "-*-terminus-bold-i-*-*-32-*-*-*-*-*-*-r")

(set-face-font 'dired-face-directory "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")

(set-face-font 'info-heading "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")
(set-face-font 'info-node "-*-terminus-bold-i-*-*-32-*-*-*-*-*-*-r")
(set-face-font 'info-xref "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")

(set-face-font 'cvs-header-face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")
(set-face-font 'cvs-marked-face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")
(set-face-font 'cvs-msg-face "-*-terminus-medium-i-*-*-32-*-*-*-*-*-*-r")

(make-face 'diary-face)
(set-face-background 'diary-face "red")
(set-face-font 'diary-face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")

(set-face-font 'tpum-title-face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")
(set-face-font 'tpum-toggled-face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")
(set-face-font 'tpum-pseudo-face1 "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")
(set-face-font 'tpum-plain-face1 "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")

(set-face-font 'font-lock-warning-face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")

(set-face-font 'slime-inspector-value-face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")
(set-face-font 'slime-reader-conditional-face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")
(set-face-font 'slime-repl-input-face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")
(set-face-font 'slime-repl-output-mouseover-face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")

(require 'cus-edit)
(set-face-font 'custom-button-face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")
(set-face-font 'custom-variable-button-face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")

(set-face-font 'widget-button-face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r")

;; ERC
(add-hook 'erc-mode-hook
          (lambda ()
            (mapc (lambda (face)
                    (set-face-font face "-*-terminus-bold-r-*-*-32-*-*-*-*-*-*-r"))
                  '(erc-prompt-face erc-timestamp-face erc-nick-default-face
                                    erc-nick-msg-face erc-notice-face))))

;;}}}
)

;;; Mac OS X specifics
(when (eq system-type 'darwin)
  ;; Make C-w, M-y interact with Mac OS X clipboard
  (defun lg-paste-from-osx ()
    (let ((pstr (shell-command-to-string "pbpaste")))
      (and (positivep (length pstr)) pstr)))
  (defun lg-paste-to-osx (text &optional push)
    (let* ((process-connection-type nil)
           (proc (start-process "pbcopy" nil "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc)))

  (setq interprogram-paste-function 'lg-paste-from-osx)
  (setq interprogram-cut-function 'lg-paste-to-osx))

;; GNU Emacs like stiff
(defun do-applescript (&rest script-lines)
  (let ((script (mapconcat 'identity script-lines "\n"))
        start cmd return)
    (while (string-match "\n" script)
      (setq script (replace-match "\r" t t script)))
    (while (string-match "\"" script start)
      (setq start (+ 2 (match-beginning 0))
            script (replace-match "\\\"" t t script)))
    (setq cmd (concat "osascript -e \"\"\"" script "\"\"\""))
    (setq return (shell-command-to-string cmd))
    (concat "\"" return "\"")))

;; SXEmacsen interface to growl
(defun growl (title message)
  (do-applescript
   "tell application \"GrowlHelperApp\""
   (format "notify with name \"SXEmacs Notification\" title %S description %S application name \"SXEmacs\" image from location \"file:///opt/local/share/sxemacs-22.1.10/etc/sxemacs-icon.png\"" title message)
   "end tell"))

;; C-c w w - to browse url at point with firefox
(defun lg-browse-in-firefox-with-as (url &optional new-window)
  (do-applescript
   "tell application \"Firefox\" to activate"
   "tell application \"System Events\""
   "keystroke \"t\" using {command down}"
   "keystroke \"l\" using {command down}"
   (format "keystroke %S" url)
   "keystroke return"
   "end tell"))

(if (eq system-type 'darwin)
    (setq browse-url-browser-function 'lg-browse-in-firefox-with-as)

;  (setq browse-url-browser-function 'browse-url-generic
;        browse-url-generic-program "xulrunner"
;        browse-url-generic-args '("/usr/local/share/conkeror/application.ini")))

  (setq browse-url-browser-function 'browse-url-firefox))

(defvar lg-say-voices
  '((agnes . "Agnes") (albert . "Albert") (alex . "Alex")
    (badnews . "BadNews") (bahh . "Bahh") (bells . "Bells")
    (boing . "Boing") (bruce . "Bruce") (bubbles . "Bubbles")
    (cellos . "Cellos") (deranged . "Deranged") (fred . "Fred")
    (goodnews . "GoodNews") (hysterical . "Hysterical") (junior . "Junior")
    (kathy . "Kathy") (organ . "Organ") (princess . "Princess")
    (ralph . "Ralph") (trinoids . "Trinoids") (vicki . "Vicki")
    (victoria . "Victoria") (whisper . "Whisper") (zarvox . "Zarvox")))

(defun lg-say-the-text (text &optional voice)
  (let* ((sv (cdr (assq voice lg-say-voices)))
         (svoice (if sv (format " using %S" sv) ""))
         ;; " -> ' in order to avoid osascript confusion
         (nt (replace-in-string text "\"" "'")))
    (do-applescript (format "say %S%s" nt svoice))
    (growl "Text via VoiceOver" "Done!")))

(defun lg-say-region-or-buffer (arg)
  "Say contents of the buffer or region.
If prefix ARG is specified then use custom voice."
  (interactive "P")
  (let ((voice (and current-prefix-arg
                    (intern
                     (completing-read "Voice: "
                      (mapcar #'(lambda (v)
                                  (list (symbol-name (car v))))
                              lg-say-voices)
                      nil t))))
        (text (if (region-active-p)
                  (buffer-substring (region-beginning) (region-end))
                (buffer-substring))))
    (lg-say-the-text text voice)))

(define-key global-map (kbd "C-c s") 'lg-say-region-or-buffer)

;;; JS-Kit
(define-derived-mode adl-mode text-mode "adl"
  (font-lock-mode 1)
  (push (cons (concat "^\\<\\("
                      "\\(section\\|subsection\\|group"
                      "\\|param\\|div\\|item\\|include\\)"
                      "\\)\\>")
              'font-lock-keyword-face)
        font-lock-keywords)
  (push (cons "^#.*$" 'font-lock-comment-face)
        font-lock-keywords)
  (push (cons "^[ ]*\\(:[^:]+:\\)" 'font-lock-type-face)
        font-lock-keywords)
  (push (cons "->" 'font-lock-reference-face)
        font-lock-keywords)
  )

(add-to-list 'auto-mode-alist '("\\.adl\\'" . adl-mode))

;;; Wand-display

;; Mac OS, workaround libmagic absence
;(condition-case nil
;    (require 'ffi-magic)
;  (t (defun magic:file-type (file)
;       (let ((sr (shell-command-to-string
;                  (format "file -b %s" (expand-file-name file)))))
;         (substring sr 0 (1- (length sr)))))))

(setq Wand-mode-query-for-overwrite nil)
(Wand-find-file-enable)                 ; `C-x C-f' to open images
(defun lg-Wand-mode-hook ()
  ;; Disable tpum menus for Wand-mode
  (make-variable-buffer-local 'tpum-global-mode)
  (setq tpum-global-mode nil))
(add-hook 'Wand-mode-hook 'lg-Wand-mode-hook)

;;; Make *scratch-file* and make it unkillable
(add-hook 'term-setup-hook (lambda () (kill-buffer "*scratch*")))
(setq lsf-buffer (find-file-noselect lg-scratch-file))

(defun lg-save-lsf-buffer ()
  "Save *scratch-file* buffer on exit."
  (when (buffer-live-p lsf-buffer)
    (with-current-buffer lsf-buffer
      (save-buffer))))

;; Finally load site local settings and some my passwords used by
;; Emacs
(load (expand-file-name "local.settings.el" user-init-directory))
(load (expand-file-name "local.passwords.el" user-init-directory))

;; Enable UTF-8 encoding
(setq buffer-file-coding-system-for-read 'utf-8)

;; Load lg's default desktop only for xwem sxemacs instance
(when sxemacs-under-xwem
  (lg-desktop-load)
)
(message (format "%s loaded" user-init-file))

;; Select *scratch-file* to be active buffer
(add-hook 'window-setup-hook 
          (lambda () (pop-to-buffer "*scratch-file*") (delete-other-windows)))

;; XXX remove this
(push "~/dev/sxempathy/" load-path)
(setq empathy-icons-path "/home/lg/dev/sxempathy/icons/")
(setq empathy-icons-path "/usr/local/share/empathy/icons/hicolor/16x16/status/")
(autoload 'empathy "empathy" nil t)
(autoload 'empathy-roster-display "empathy-roster" nil t)

;; Themes
; color-theme-clarity
; color-theme-pok-wob

;;; init.el ends here
