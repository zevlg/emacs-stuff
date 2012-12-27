;;; cocol.el --- Count columns and lines minor mode.

;; Copyright (C) 2004 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sun Nov 14 02:34:07 MSD 2004
;; Keywords: utils
;; X-CVS: $Id: cocol.el,v 1.1 2004/11/30 10:45:26 lg Exp $

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

;; Show information about total number of lines and nuber of columns
;; for line at point in modeline.  To turn it on use:

;;    M-x cocol-turn-on-mode RET

;; cocol mode does not strike performance.  Simple profiling
;; `cocol-update-lines' and `cocol-update-columns' functions gives next
;; result:

;; On big files (lines: ~1500)

;;    Function Name       Call Count  Elapsed Time  Average Time
;;    ==================  ==========  ============  ============
;;    cocol-update-lines    1086        0.8463739999  0.0007793499
;;    cocol-update-columns  1086        0.0369940000  3.406...e-05

;; On typical files (lines: < 500)

;;    Function Name       Call Count  Elapsed Time  Average Time
;;    ==================  ==========  ============  ============
;;    cocol-update-lines    1273        0.0640020000  5.027...e-05
;;    cocol-update-columns  1273        0.0432100000  3.394...e-05

;;; Note:

;; cocol does not provide way to turn it off. joke. Use:

;;    M-x cocol-turn-off-mode RET

;; But I'm sure you will not ever use it.

;;; Code:


(defcustom cocol-minor-mode nil
  "*Non-nil mean cocol minor mode is enabled."
  :type 'boolean
  :set 'cocol-set-minor-mode
  :initialize 'custom-initialize-default)

(defvar cocol-total-lines "")
(make-variable-buffer-local 'cocol-total-lines)

(defvar cocol-total-columns "")
(make-variable-buffer-local 'cocol-total-columns)


(defun cocol-update-lines ()
  "Update `cocol-total-lines' value."
  (setq cocol-total-lines
        (concat "/"
                (int-to-string (1+ (if (> (point-max) (point-min))
                                       (count-lines (point-min) (point-max))
                                     0))))))

(defun cocol-update-columns ()
  "Update `cocol-total-columns' value."
  (setq cocol-total-columns
        (concat "/" (int-to-string (- (point-at-eol) (point-at-bol))))))

;;;###autoload
(defun cocol-turn-on-mode ()
  "Enable cocol minor mode for current buffer."
  (interactive)

  (add-hook 'post-command-hook 'cocol-update-lines)
  (add-hook 'post-command-hook 'cocol-update-columns)

  (setq cocol-minor-mode t)
  (cocol-update-modeline-format))

(defun cocol-turn-off-mode ()
  "Disable cocol minor mode for current buffer."
  (interactive)

  (setq cocol-minor-mode nil)

  (remove-hook 'post-command-hook 'cocol-update-lines)
  (remove-hook 'post-command-hook 'cocol-update-columns))

;;;###autoload
(defun cocol-set-minor-mode (svar sval)
  "Set function for `cocol-minor-mode'.
SVAR is not used.
If SVAL is non-nil then enable `cocol-minor-mode', otherwise disable it."
  (funcall (if sval 'cocol-turn-on-mode 'cocol-turn-off-mode)))

(defun cocol-update-modeline-format ()
  "Update `modeline-format' to show cocol info."
  (unless (get 'cocol 'updated-p)
    (let ((ml modeline-format))
      (while ml
        (when (listp (car ml))
          (cond ((eq (car (car ml)) 'line-number-mode)
                 (setcar ml '(line-number-mode "L%l"))
                 (setcdr ml (nconc '((cocol-minor-mode cocol-total-lines)
                                     (line-number-mode "--"))
                                   (cdr ml)))
                 (setq ml (cdr (cdr ml))))
                ((eq (car (car ml)) 'column-number-mode)
                 (setcar ml '(column-number-mode "C%c"))
                 (setcdr ml (nconc '((cocol-minor-mode cocol-total-columns)
                                     (column-number-mode "--"))
                                   (cdr ml)))
                 (setq ml (cdr (cdr ml))))))
        (setq ml (cdr ml))))
    (put 'cocol 'updated-p t)))


(provide 'cocol)

;;; cocol.el ends here
