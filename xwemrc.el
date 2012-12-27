;;; .xwemrc.el --- Configuration file for XWEM.

;; Copyright (C) 2003-2010 by Zajcev Evgeny

;;; EXPERIMENTAL (not yet)
;(setq xwem-tray-use-groups t)

(setq xwem-frame-autoiconify-mode 'intersect)

;(setq xwem-frame-default-properties
;      (plist-put xwem-frame-default-properties 'title-layout 'none))

;;; Code:
(setq X-default-timeout 20)
;(setq xwem-custom-display ":10")

(setq xwem-special-default-strategy 'center)
(setq special-display-frame-plist
      (xwem-misc-merge-plists
       special-display-frame-plist
       '(height 25 width 100
         top 100 left 100 border-width 3 border-color "red4")))

(setq minibuffer-frame-plist
      (plist-put minibuffer-frame-plist 'internal-border-width 2))

(setq debug-on-quit nil)
(setq debug-on-error nil)
(setq comint-password-prompt-regexp
      "\\(\\(\\([Oo]ld \\|[Nn]ew \\|^\\)[Pp]assword\\|pass ?phrase\\)\\|\\([pP]assword\\)\\):\\s *\\'")

;; No evil locks please
(setq xwem-kbd-evillocks nil)

;; If you use tpum - do this
(setq xwem-popup-menu-function 'old-popup-menu)

;;; Customize XWEM variables
(custom-set-variables
 '(xwem-debug nil)
 '(xwem-minibuffer-width 130)
 '(xwem-minibuffer-height 2)
 '(xwem-minibuffer-active-border-color "cyan2")
;      (xwem-default-focus-mode follow-mouse)
 '(xwem-edmacro-can-edit-nonmacro t)
 '(xwem-keyboard-echo-keystrokes 0.1)   ; immediately show pressed keys
 '(xwem-frame-on-delim-resize-mode 'opaque)
; '(xwem-cl-switch-other-omit-mode-specific t)
 ;; Turbo mode
 '(xwem-misc-turbo-mode t)

 '(xwem-minibuffer-emacs-frames-has-minibuffer nil)
 '(xwem-minib-resize-mode t)
 
 '(xwem-launcher-use-nohup t))

;; Xterm (H-a x) configuration
(setq xwem-xterm-font3 "-*-terminus-medium-r-*-*-14-*-*-*-*-*-iso10646-*")

;; Make an abbrev table for xwem-launcher-query
(define-abbrev-table 'xwem-launcher-abbrev-table
  '(("gterm" "xterm -fg green -hc green4" nil 0) ; green
    ("gfterm" "xterm -fg green -hc green4 -fn fixed" nil 0) ; green fixed
    ("aqterm" "xterm -fg aquamarine -hc aquamarine4" nil 0) ; aqua
    ("aqfterm" "xterm -fg aquamarine -hc aquamarine4 -fn fixed" nil 0) ; aqua fixed
    ("gsterm" "xterm -fg darkseagreen1 -hc darkseagreen4" nil 0) ; gsea
    ("gsfterm" "xterm -fg darkseagreen1 -hc darkseagreen4 -fn fixed" nil 0) ; gsea fixed
    ("oterm" "xterm -fg OliveDrab1 -hc OliveDrab4" nil 0) ; olive
    ("ofterm" "xterm -fg OliveDrab1 -hc OliveDrab4 -fn fixed" nil 0) ; olive fixed
    ("orterm" "xterm -fg orange -hc orange4" nil 0) ; orange
    ("orfterm" "xterm -fg orange -hc orange4 -fn fixed" nil 0) ; orange fixed

    ("earth" "xearth -font \"-*-fixed-*-*-*-*-8-*-*-*-*-*-*-*\" -pos \"fixed 55 37\" -markerfile emacswiki.markerfile")
    ))

;; Add sounds, beeping type
(xwem-sound-load-default t)

;; Frames configuration
(setq xwem-frame-default-properties
      (xwem-misc-merge-plists
       xwem-frame-default-properties
       '(inner-border-width 4 inner-border-thickness 2)))

;;{{{ `-- Worklog

;; Working at home from 22:00 up to 04:00
(setq xwem-worklog-day-start 22
      xwem-worklog-day-ends 2)

(setq xwem-worklog-silent t)            ; shutup worklog

(setq xwem-worklog-pwin-height 900
      xwem-worklog-pwin-width 1100
      xwem-worklog-pwin-border-width 6)

;;}}}

;; Weather tracker
(require 'xwem-weather)
(setq xwem-weather-station-id "uuww")
(customize-set-variable 'xwem-weather-update-frequency 1800)
(set-face-font 'xwem-weather-osd-face "-*-fixed-bold-*-*-*-14-*-*-*-*-*-*-*")
(set-face-foreground 'xwem-weather-osd-face "blue4")

;; Display APM battery status
;(load-module "~/.xemacs/modules/battery.ell")
(setq xwem-batt-height 22)
(setq xwem-batt-width 11)

;;; Client switcher (xwem-clswi)
(xwem-global-set-key (xwem-kbd "H-.") 'xwem-clswi-next)
(xwem-global-set-key (xwem-kbd "H-,") 'xwem-clswi-prev)

;; Show client info only for fullscreen clients
(setq xwem-clswi-show-info 'xwem-cl-fullscreen-p)

;;; Rdict in special frame
(require 'rdict)
(defun lg-special-display-popup-frame (buffer &optional args)
  (frame-selected-window
   (xwem-special-popup-frame buffer t)))

(push "*RDict*" special-display-buffer-names)
(setq special-display-function 'lg-special-display-popup-frame)

(define-xwem-command lg-rdict ()
  "Command to run rdict from xwem."
  (xwem-interactive "_")
  (rdict "en"))
;  (rdict-word word)
;  (call-interactively 'rdict))

(define-key rdict-mode-map [?q] (lambda ()
                                  (interactive)
                                  (rdict-restore-windows)
                                  (kill-buffer (current-buffer))))
(xwem-global-set-key (xwem-kbd "H-c d") 'lg-rdict)

;;; Managing
(defun lg-xterm-default-directory ()
  "Return current directory."
  (if (string= (car (xwem-client-application)) "xterm")
      (let ((wmname (xwem-client-name)))
        (when (string-match "\\[\\([~/][^]]*\\)\\]" wmname)
          (concat (match-string 1 wmname) "/")))
    (default-directory)))

(setq xwem-default-directory-function 'lg-xterm-default-directory)

(setq xwem-open-file-commands-alist
      (put-alist "\\.pdf\\'" "xpdf -z 220"
                 xwem-open-file-commands-alist))

;; Cool image viewer - xzgv
(push '("xzgv" (and (class-name "^Xzgv$")
                    (class-inst "^xzgv$")))
      xwem-applications-alist)

(setq xwem-manage-list
      '(
        (generic (ignore-has-input-p t)
                 (and (class-inst "sun-awt-X11-XFramePeer")
                      (class-name "com-mucommander-Launcher")))
        (generic (ignore-has-input-p t) (application "gnuplot"))

        (dummy nil (name "Desktop"))
        ;; handle xfce desktop on root window as dummy client
        (rooter (dummy-client-p t)
                (and (class-inst "xfdesktop")
                     (class-name "Xfdesktop")))
        (rooter nil
                (application "xfce4-panel"))
        (rooter (noselect nil xwem-focus-model generic)
                (application "xfrun4"))
        (rooter nil
                (and (class-inst "orage")
                     (class-name "Orage")))
        (fullscreen nil (and (class-inst "thunar")
                             (class-name "Thunar")))
        (rooter nil (and (application "dia") (role "start_dialog")))
        ;; Mplayer intents mostly to run in fullscreen
        (fullscreen (ignore-has-input-p t x-border-width 0 fs-real-size t)
                    (application "mplayer"))
        ;; All remote desktops are also fullscreen
        (fullscreen (ignore-has-input-p t fs-real-size t x-border-width 0)
                    (application "rdesktop"))
        ;; for gpg-agent
        (fullscreen (fs-real-size t xwem-focus-model click-focus)
                    (application "pinentry"))
        ;; Note: this comes _before_ dedicated xzgv
        (rooter nil (or (name "^Lupe$")
                        (and (application "xzgv")
                             (name "^\\(Updating Thumbnails\\|Recursive Update\\|xzgv error\\)$"))))
        (dedicated (xwem-focus-model click-focus)
                   (or (application "display")
                       (application "gimp")
                       (application "xzgv")))
        ;; This shit goes directly to root window
        (rooter (dummy-client-p t)
                (or (application "xclock")
                    (application "cairo-clock")
                    (application "gkrellm")
                    (application "gdesklets")
                    (application "gdeskcal")
                    (and (class-inst "buiciClock")
                         (class-name "BuiciClock"))
                    ))
        (systray nil 
                 (or (and (class-inst "stickytime")
                          (class-name "STICKYTIME"))
                     (name "tpager")))
        ))

;; Set rank of xfce desktop client to be the lowest
(require 'xwem-rooter)
(push '((or (application "xclock")
            (application "cairo-clock")
            (and (class-inst "xfdesktop")
                 (class-name "Xfdesktop"))) . (-1 . -1))
      xwem-rooter-always-on-top-spec)

(define-key xwem-rooter-mode-map (xwem-kbd "ESC") 'xwem-client-kill)

(defun lg-select-xfrun4 (cl old new)
  "Select xfrun4 applcation changing state from Withdraw to Normal."
  (when (and (xwem-cl-match-p cl '(application "xfrun4"))
             (eq old 'withdrawn) (eq new 'active))
    (xwem-select-client cl)))

(add-hook 'xwem-cl-state-change-hook 'lg-select-xfrun4)

;;; Tabber config
(require 'xwem-register)
(setq xwem-registers-auto-registers
      '((?e (application "xemacs"))))

;; XXX
(defadvice xwem-register-client (around call-client-change activate)
  "Call client change hook."
  (let ((rval (xwem-register-get (event-key (ad-get-arg 0)))))
    ad-do-it

    (when (and (xwem-cl-p rval)
               (not (eq rval (xwem-cl-selected))))
      (run-hook-with-args 'xwem-cl-change-hook rval))

    (run-hook-with-args 'xwem-cl-change-hook (xwem-cl-selected))))

(xwem-set-face-font
 'xwem-tabber-face "-*-terminus-bold-r-*-*-14-*-*-*-*-*-koi8-r"
 '(frame-selected tab-selected))
(xwem-set-face-font
 'xwem-tabber-face "-*-fixed-medium-*-*-*-14-*-*-*-*-*-koi8-r"
 '(frame-selected tab-nonselected))
(xwem-set-face-font
 'xwem-tabber-face "-*-terminus-bold-r-*-*-14-*-*-*-*-*-koi8-r"
 '(frame-nonselected tab-selected))
(xwem-set-face-font
 'xwem-tabber-face "-*-fixed-medium-*-*-*-14-*-*-*-*-*-koi8-r"
 '(frame-nonselected tab-nonselected))

(xwem-set-face-font
 'xwem-tabber-face1 "-*-terminus-bold-r-*-*-14-*-*-*-*-*-koi8-r"
 '(frame-selected tab-selected))
(xwem-set-face-font
 'xwem-tabber-face1 "-*-fixed-medium-*-*-*-14-*-*-*-*-*-koi8-r"
 '(frame-selected tab-nonselected))
(xwem-set-face-font
 'xwem-tabber-face1 "-*-terminus-bold-r-*-*-14-*-*-*-*-*-koi8-r"
 '(frame-nonselected tab-selected))
(xwem-set-face-font
 'xwem-tabber-face1 "-*-fixed-medium-*-*-*-14-*-*-*-*-*-koi8-r"
 '(frame-nonselected tab-nonselected))
 
(setq xwem-tab-default-format " %i  %*%#%I%1%r%0  %n")

;; Restore desktop
(xwem-desktop-load-onetime)
(xwem-desktop-load)
(add-hook 'xwem-exit-hook 'xwem-desktop-save)
(add-hook 'xwem-exit-hook 'xwem-desktop-save-onetime)

;;; Enable keytt in clients that has support for
(xwem-keytt-global-mode)

;; Nifty root icon
(require 'xwem-rooticon)

;; Frame transparency
(require 'xwem-frametrans)

;; last.fm player
(push "/home/lg/.xwem/icons" xwem-icon-dirs)
(push '("lastfm" (and (class-inst "^lfmplayer$") (class-name "^Lfmplayer$")))
      xwem-applications-alist)
(push '("mini-lastfm.xpm" (application "lastfm")) xwem-icons-list)


(defun xwem-turn-on-debug ()
  (interactive)
  (setf (X-Dpy-log-buffer (xwem-dpy)) "*xwem-debug*")
  (X-Dpy-set-log-routines (xwem-dpy)
                          (nconc '(x-misc x-event x-tray x-error x-record)
                                 xwem-debug-routines)))

(defun xwem-turn-off-debug ()
  (interactive)
  (setf (X-Dpy-log-buffer (xwem-dpy)) nil))

(defun xwem-turn-on-record-debug ()
  (interactive)
  (require 'xwem-keymacro)
  (setf (X-Dpy-log-buffer xwem-keymacro-dpy) "*XREC.log*")
  (X-Dpy-set-log-routines xwem-keymacro-dpy '(x-record)))

(defun xwem-turn-off-record-debug ()
  (interactive)
  (require 'xwem-keymacro)
  (setf (X-Dpy-log-buffer xwem-keymacro-dpy) nil))


;; Eicq, enable eicq dock
(add-hook 'eicq-log-mode-hook 'eicq-xwem-init)

(add-hook 'xwem-after-init-hook
          (lambda ()
            (load-file (expand-file-name "~/.xwem/after-init-xwemrc.el"))))

;;; Google calc
(define-xwem-command xwem-google-calc (string arg)
  "Evaluate STRING using google calc."
  (xwem-interactive
   (list (or (xwem-selection 'local)
             (xwem-read-from-minibuffer "Google Calc: "))
         xwem-prefix-arg))
  (zmacs-deactivate-region)             ; XXX
  (let ((res (google-calc string t)))
    (if arg
        (xwem-kbd-add-pending-keys res)
      (xwem-message 'info "%s" res))))

(xwem-global-set-key (xwem-kbd "H-$") 'xwem-google-calc)

;; Additional applications
(define-xwem-command my-xwem-start-gnus ()
  "Start gnus in new frame."
  (xwem-interactive "_")
  (with-selected-frame (make-frame)
    (gnus)
    (set-buffer-dedicated-frame
     (get-buffer "*Group*") (selected-frame))))
(xwem-global-set-key (xwem-kbd "H-a g") 'my-xwem-start-gnus)

(define-xwem-command my-xwem-start-eicq ()
  "Start eicq in new frame."
  (xwem-interactive "_")
  (let ((eicq-frame (make-frame)))
    (eicq-login)
    (eicq-show-window)))
(xwem-global-set-key (xwem-kbd "H-a i") 'my-xwem-start-eicq)

(defun lg-select-on-run-application (app)
  (or (xwem-select-application app (xwem-clients-list))
      (xwem-launch app)))

(defmacro define-lg-start-command (command)
  (let ((sym (intern (format "lg-xwem-start-%S" command)))
        (doc (format "Launch %S." command)))
  `(define-xwem-command ,sym ()
     ,doc
     (xwem-interactive "_")
     (lg-select-on-run-application (symbol-name ',command)))))

;(xwem-global-set-key (xwem-kbd "H-d") (define-lg-start-command thunar))
(xwem-global-set-key (xwem-kbd "H-a f") (define-lg-start-command firefox))
;(xwem-global-set-key (xwem-kbd "H-a o") (define-lg-start-command opera))
;(xwem-global-set-key (xwem-kbd "H-a O") (define-lg-start-command openoffice.org-2.3.1))

(define-xwem-command lg-xwem-start-xterm-screen (&optional arg)
  "Start xterm with screen as command."
  (xwem-interactive "_p")
  (xwem-launch
   (concat (xwem-xterm-construct-cmd arg)
           " -lc -xrm 'XTerm*VT100.translations:     #override \\n"
           "<Btn4Down>: string(0x1b) string(\"[5;2~\") \\n"
           "<Btn5Down>: string(0x1b) string(\"[6;2~\") \\n"
           "Shift <Key>Prior: string(0x1b) string(\"[5;2~\") \\n"
           "Shift <Key>Next: string(0x1b) string(\"[6;2~\")'"
           " -e screen -RR")))

(xwem-global-set-key (xwem-kbd "H-a x") 'lg-xwem-start-xterm-screen)

(xwem-global-set-key
 (xwem-kbd "H-a X")
 (define-xwem-command lg-xwem-start-xterm (&optional arg)
   "Start xterm."
   (xwem-interactive "_p")
   (xwem-launch
    (concat (xwem-xterm-construct-cmd arg)
            " -lc -xrm 'XTerm*VT100.translations:     #override \\n"
            "Shift <Key>Prior: string(0x1b) string(\"[5;2~\") \\n"
            "Shift <Key>Next: string(0x1b) string(\"[6;2~\")'"))))

;; clients navigator
(xwem-global-set-key [XF86ApplicationRight] 'xwem-clswi-next)
(xwem-global-set-key [XF86ApplicationLeft] 'xwem-clswi-prev)

(xwem-global-set-key (xwem-kbd "H-C-l") 'xwem-cl-switch-to-other)
(xwem-global-set-key (xwem-kbd "H-C-x") 'xwem-execute-extended-command)

(define-xwem-command lg-maxima-pop-special ()
  "Pop *maxima* buffer in special frame."
  (xwem-interactive)
  (exit-minibuffer)
  (xwem-special-popup-frame "*maxima*" t))

(defvar lg-mmin-map
  (let ((map (make-sparse-keymap)))
    (define-key map (xwem-kbd "H-%") 'lg-maxima-pop-special)
    map))

(define-xwem-command lg-maxima-minibuffer (input)
  "Read from minibuffer and execute in maxima."
  (xwem-interactive (list (xwem-read-from-minibuffer
                           "XWEM Maxima: " nil nil nil
                           (progn
                             (require 'maxima)
                             maxima-minibuffer-history))))
  (maxima-start)
  (maxima-single-string-wait
   "block(emacsdisplay:display2d,display2d:false,linenum:linenum-1,%);")
  (maxima-single-string-wait
   (maxima-strip-string-add-semicolon input))
  (setq output (maxima-remove-whitespace-from-ends
                (maxima-last-output-noprompt)))
  (maxima-single-string-wait
   "block(display2d:emacsdisplay,linenum:linenum-1,%);")
  (setq output (maxima-replace-in-string "%" "%%" output))
  (xwem-message 'info "[%s] => %s" input output))
(xwem-global-set-key (xwem-kbd "H-%") 'lg-maxima-minibuffer)

(xwem-global-set-key (xwem-kbd "H-<f1>") 'xwem-frame-showroot)
(xwem-global-set-key (xwem-kbd "H-<f4>") 'xwem-client-kill)

;; Minibuffer visibility toggler
(define-xwem-command toggle-lg-xwem-minibuffer-iconic (&optional state)
  "Toggle checking for trailing space in buffer."
  (xwem-interactive)
  (when xwem-minibuffer
    (let ((mcl (xwem-minib-cl xwem-minibuffer)))
      (if (or (and state (eq state 'activate))
              (and (not state) (xwem-cl-iconified-p mcl)))
          (xwem-activate mcl)
        (xwem-iconify mcl)))))

(xwem-global-set-key (xwem-kbd "H-x \\") 'toggle-lg-xwem-minibuffer-iconic)

;;; Setup for dedicated frames and xzgv
(setq xwem-dedicated-frame-default-properties
      (plist-remprop xwem-dedicated-frame-default-properties
                     'manual-position))

(defun lg-xwem-uniconify-minibuffer (cl)
  (when (string= (car (xwem-client-application cl)) "xzgv")
    (toggle-lg-xwem-minibuffer-iconic 'activate)))

(defun lg-fit-dedic-frame (cl)
  (let ((frame (xwem-cl-frame cl)))
    (when (and (xwem-frame-p frame)
               (eq (xwem-frame-type frame) 'dedicated)
               (string= (car (xwem-client-application cl)) "xzgv"))
      (xwem-frame-fit-screen frame t)
      (toggle-lg-xwem-minibuffer-iconic 'iconify))))

(add-hook 'xwem-cl-manage-hook 'lg-fit-dedic-frame)
(add-hook 'xwem-cl-destroy-hook 'lg-xwem-uniconify-minibuffer)


;; commands for strokes
(define-xwem-command lg-xwem-lupe-in ()
  "Start lupe."
  (xwem-interactive)
  (xwem-launch
   "lupe -noshape -nohud -mag 3 -geometry 520x110+200+700")
  (xwem-global-set-key (xwem-kbd "H-+") 'lg-xwem-lupe-out))

(define-xwem-command lg-xwem-lupe-out ()
  "Kill lupe."
  (xwem-interactive)
  (xwem-execute-program "killall lupe")
  (xwem-global-set-key (xwem-kbd "H-+") 'lg-xwem-lupe-in))

;; jabber
(when nil
(require 'jabber-xwem)
(push '(jabber . "orangered") xwem-osd-notifier-colors)
(push (cons 'jabber (font-create-name (make-font :size 32)))
      xwem-osd-notifier-fonts)
(setq jabber-alert-message-hooks
      '(jabber-message-xwem-message jabber-message-scroll))
)

;; musicpd and internet radio
(defvar lg-radios
  '(("Psy" . "0") ("RECPEKT!" . "1") ("DC" . "2")
    ("Centr" . "3") ("Drum" . "4") ("NRJ" . "5"))
  "List of radios according to radios.m3u")

(define-xwem-command lg-select-radio (radio)
  "Select radio."
  (xwem-interactive
   (list (xwem-completing-read "Radio: " lg-radios)))
  (unless (featurep 'xwem-mpd)
    (xwem-mpd))
  (mpd-send "clear")
  (mpd-send "load radios")
  (mpd-send (concat "play " (cdr (assoc radio lg-radios)))))

(xwem-global-set-key (xwem-kbd "<f10>") 'lg-select-radio)
(xwem-global-set-key (xwem-kbd "H-x H-r") 'lg-select-radio)
(xwem-global-set-key (xwem-kbd "H-+") 'lg-xwem-lupe-in)

;;; Webjumping
(setq browse-url-browser-function 'browse-url-firefox)
;(setq browse-url-firefox-arguments '("-new-window"))
;; XXX hack to fool browse-url
(provide 'dos-w32)

(defvar lg-webjump-last-site nil)

(define-xwem-command lg-webjump (arg)
  "Webjump for xwem.
With prefix arg you can select site to jump to,
otherwise jump to last visited site."
  (xwem-interactive "P")
  (let* ((completion-ignore-case t)
         (wj-site (or (and (not arg) lg-webjump-last-site)
                      (xwem-completing-read "WJ Site: " webjump-sites nil t)))
	 (item (assoc-ignore-case wj-site webjump-sites))
	 (name (car item))
	 (expr (cdr item))
         (url   (with-xwem-read-from-minibuffer
                 (webjump-url-fix
                  (cond ((not expr) "")
                        ((stringp expr) expr)
                        ((vectorp expr) (webjump-builtin expr name))
                        ((listp expr) (eval expr))
                        ((symbolp expr)
                         (if (fboundp expr)
                             (funcall expr name)
                           (error "WebJump URL function \"%s\" undefined." expr)))
                        (t (error "WebJump URL expression for \"%s\" invalid."
                                  name)))))))

    ;; save last webjump site on C-u C-u
    (when (> (prefix-numeric-value arg) 4)
      (setq lg-webjump-last-site wj-site))

    ;; Resort webjump-sites to make ITEM be most recent
    (setq webjump-sites (sort webjump-sites (lambda (a b) (eq a item))))

    ;; Try to select already running firefox application
    (let ((fcls (xwem-cl-list-sort-by-recency
                 (xwem-clients-list (lambda (cl)
                                      (string= (car (xwem-client-application cl))
                                               "firefox"))))))
      (when fcls
        (xwem-select-client (car fcls))))

    (browse-url-firefox url)))

;; fix `webjump-read-string' to support queries in russian
(require 'webjump)
(defadvice webjump-read-string (after encode-string-utf8 activate)
  "Encode input string into UTF-8."
  (when (stringp ad-return-value)
    (setq ad-return-value
          (encode-coding-string ad-return-value 'utf-8))))

(define-xwem-command lg-browse-url (url)
  "Browse URL."
  (xwem-interactive "sURL: ")
  (browse-url url))

(xwem-global-set-key (xwem-kbd "H-c w") 'lg-webjump)
(xwem-global-set-key (xwem-kbd "H-c H-w") 'lg-browse-url)

;; Pointer warper when switching frames
(defun lg-warp-to-frame-center ()
  "Warp the pointer to the center of the frame."
  (let* ((cf (xwem-frame-selected))
         (fw (xwem-frame-width cf))
         (fh (xwem-frame-height cf)))
    (XWarpPointer
     (xwem-dpy) (make-X-Win :id X-None)
     (xwem-frame-xwin cf) 0 0 0 0 (/ fw 2) (/ fh 2))))

(add-hook 'xwem-frame-select-hook 'lg-warp-to-frame-center)


;; Powerfull `H-m' prefix for user defined macroses
(require 'xwem-edmacro)
(add-hook 'xwem-exit-hook 'xwem-keymacro-save-macros)
(add-hook 'xwem-after-init-hook 'xwem-keymacro-load-macros)

;;; .xwemrc.el ends here
