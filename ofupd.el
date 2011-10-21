;;; ofupd.el --- One file updater.

;; Copyright (C) 2006-2007 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Nov 21 17:18:14 MSK 2006
;; Keywords: packages
;; Version: 0.2
;; X-CVS: $Id: ofupd.el,v 1.4 2008/07/25 00:02:28 lg Exp $

;; This file is NOT part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; PKG is plist with known keywords:

;;   :name "PKG-NAME"                      string
;;   :filename "/path/to/package/pkg.el"   string
;;   :source "http://source.url/pkg.el"    string
;;   :version-extractor #'function         function of one argument (buffer)
;;   :nver-range '(111 . 222)              cons of numbers

;;; Code:

;;{{{ `-- Custom variables

(defgroup ofupd nil
  "Group to customize extput package."
  :version "0.1"
  :link '(url-source "http://emacswiki.org/cgi-bin/wiki/download/ofupd.el")
  :group 'package-tools)

(defcustom ofupd-load-path
  (list (expand-file-name "lisp" user-init-directory)
        (expand-file-name "lisp/thirdpart" user-init-directory))
  "List of path's for thirdpart packages."
  :type 'list
  :group 'ofupd)

(defcustom ofupd-default-version-extractor #'ofupd-ve-cvs-or-svn
  "Default version extractor for ofupd.
Used only if `version-extractor' is not set for package."
  :type 'function
  :group 'ofupd)

(defcustom ofupd-default-nver-range '(0 . 1024)
  "Default download range.
Used only if `range' is not set for package."
  :type '(cons (number :tag "Start") (number :tag "End"))
  :group 'ofupd)

(defvar ofupd-packages nil
  "List of registered packages.
Do not modify directly, use `ofupd-register' to add entries.")

;;}}}

(defstruct ofupd-pkg
  name                                  ; PKG name
  filename                              ; relative filename
  source                                ; source URL
  (version-extractor ofupd-default-version-extractor) ; version extractor
  (nver-range ofupd-default-nver-range) ; range
  )


;;{{{ `-- Version functions

(defun ofupd-compare-versions (ver1 ver2)
  "Compare two versions VER1 and VER2.
Return >0 if VER1 is greater then VER2.
Return  0 if they are equal.
Return <0 if VER1 is less then VER2."
  (if (fboundp 'product-version-compare)
      (funcall #'product-version-compare ver1 ver2)

    (while (and ver1 ver2 (= (car ver1) (car ver2)))
      (setq ver1 (cdr ver1)
            ver2 (cdr ver2)))
    (if ver1 (if ver2 (- (car ver1) (car ver2)) 1) (if ver2 -1 0))))

(defun ofupd-convert-version (version)
  "Convert VERSION string into list of numbers."
  (mapcar #'string-to-number (split-string-by-char version ?.)))

;;}}}
;;{{{ `-- Version Extractor functions

(defun ofupd-ve-cvs-or-svn (buf)
  "Version Extractor(ve) to extract version from $Id: ofupd.el,v 1.4 2008/07/25 00:02:28 lg Exp $ keyword."
  (with-current-buffer buf
    (goto-char (point-min))
    (when (re-search-forward "\$Id: [^ ]+ \\([0-9\.]+\\)" nil t)
      (match-string 1))))

(defun ofupd-ve-version-colon (buf)
  "Extract version from \"Version: XXX\" specification."
  (with-current-buffer buf
    (goto-char (point-min))
    (when (re-search-forward "\\(Version\\|Revision\\):[ \t]+\\([0-9\.]+\\)" nil t)
      (match-string 2))))

;;}}}
;;{{{ `-- PKG functions

(defun ofupd-register (name filename source &optional ve range)
  "Register new package PKG."
  (let ((pkg (make-ofupd-pkg
              :name name :filename filename :source source)))
    (when ve
      (setf (ofupd-pkg-version-extractor pkg) ve))
    (when range
      (setf (ofupd-pkg-nver-range pkg) range))

    (push pkg ofupd-packages)
    pkg))

(defun ofupd-pkg-absolute-filename (pkg)
  "Return absolute filename of PKG."
  (let ((fn (ofupd-pkg-filename pkg)))
    (loop for ptr in ofupd-load-path
      if (file-exists-p (expand-file-name fn ptr))
      return (expand-file-name fn ptr))))

(defun ofupd-pkg-local-version (pkg)
  "Return PKG's local version."
  (let ((buf (find-file-noselect
              (ofupd-pkg-absolute-filename pkg))))
    (unwind-protect
        (funcall (or (ofupd-pkg-version-extractor pkg)
                     ofupd-default-version-extractor)
                 buf)
      (kill-buffer buf))))

(defun ofupd-pkg-fetch-new (pkg &optional range)
  "Fetch PKG's package sources.
If optional RANGE is specified, then limit fetching range to that RANGE."
  ;; Fix RANGE
  (when range
    (setq range (or (ofupd-pkg-nver-range pkg) range)))

  (let* ((pkg-url (ofupd-pkg-source pkg))
         (url-request-extra-headers
          (and range
               `(("Range" . ,(format "bytes=%d-%d" (car range) (cdr range))))))
         (buf (cdr (url-retrieve pkg-url))))
    (get-buffer buf)))
  
(defun ofupd-pkg-remote-version (pkg)
  "Return PKG's remote version."
  (save-excursion
    (funcall (or (ofupd-pkg-version-extractor pkg)
                 ofupd-default-version-extractor)
             (ofupd-pkg-fetch-new pkg ofupd-default-nver-range))))
  
(defun ofupd-pkg-need-update-p (pkg)
  "Return non-nil if PKG need update."
  (positivep
   (ofupd-compare-versions
    (ofupd-convert-version
     (ofupd-pkg-remote-version pkg))
    (ofupd-convert-version
     (ofupd-pkg-local-version pkg)))))

(defun ofupd-pkg-update (pkg)
  "Update package PKG.
Overwrites old contents with newly fetched."
  (with-current-buffer (ofupd-pkg-fetch-new pkg)
    (copy-file (ofupd-pkg-absolute-filename pkg)
               (format "%s-old" (ofupd-pkg-absolute-filename pkg)))
    (write-file (ofupd-pkg-absolute-filename pkg) nil)
    (byte-compile-file (ofupd-pkg-absolute-filename pkg))
    ))

(defvar ofupd-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [?u] 'ofupd-package-update)
    map)
  "Keymap for ofupd.")

;;}}}
;;{{{ `-- Interactive commands

(defun ofupd-package-update ()
  "Update package at point."
  (interactive)
  (let ((pkg (get-text-property (point) 'ofupd-package)))
    (when (ofupd-pkg-p pkg)
      (ofupd-pkg-update pkg))))

(defun ofupd-packages-list (arg)
  "List all registered packages.
If prefix ARG is given, then check remote versions."
  (interactive "P")
  (with-current-buffer (get-buffer-create " *ofupd packages*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Package                 LV      RV      NU\n")
    (insert "-------                 --      --      --\n")
    (display-buffer (current-buffer))
    (save-excursion
      (mapc #'(lambda (p)
                (let* ((lv (ofupd-pkg-local-version p))
                       (rv (and arg (ofupd-pkg-remote-version p)))
                       (nu (if arg
                               (positivep
                                (ofupd-compare-versions
                                 (ofupd-convert-version rv)
                                 (ofupd-convert-version lv)))
                             ""))
                       bp ep)
                  (setq bp (point))
                  (insert-face
                   (format "%-24s%-8s%-8s%s\n" (ofupd-pkg-name p) lv rv nu)
                   (if (eq nu t) 'paren-mismatch 'default))
                  (setq ep (point))
                  (put-text-property bp ep 'ofupd-package p)
                  ))
            ofupd-packages))

    ;; Enable ofupd-mode
    (setq major-mode 'ofupd-list-mode)
    (setq buffer-read-only t)
    (use-local-map ofupd-keymap)
    ))

;;}}}

(provide 'ofupd)

;;; ofupd.el ends here
