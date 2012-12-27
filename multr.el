;;; multr.el --- Multiple regions handling.

;; Copyright (C) 2004 by Free Software Foundation, Inc.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Apr 14 19:09:29 MSD 2004
;; Keywords: regions, interactive
;; X-CVS: $Id$
;; X-NOTE: Firstly suggested by Mathias Dahl

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

;; 

;;; Code:

(defvar mr-regions-face (make-face 'mr-regions-face)
  "Face used to hilite multiple regions.")
(set-face-background mr-regions-face "lightblue")

(defvar mr-regions-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-default-binding map 'mr-region-docmd)
    (define-key map [(control ?h)] 'describe-prefix-bindings)
    (define-key map [?a] 'mr-region-add)
    (define-key map [?u] 'mr-region-unmark)
    map)
  "Keymap used for multiple regions operations.")

(defvar mr-regions nil
  "List of regions in buffer.
NOTE: buffer local variable.")
(make-variable-buffer-local 'mr-regions)

(defun mr-region-add (sr er)
  "Add region started at SR and ends at ER to `mr-regions' list."
  (interactive "r")

  (save-excursion
    (push (cons (progn (goto-char sr) (point-marker))
                (progn (goto-char er) (point-marker)))
          mr-regions))

  (let ((ex (extent-at sr nil 'mr-region)))
    (unless (and ex (eq ex (extent-at er nil 'mr-region)))
      (setq ex (make-extent sr er))
      (set-extent-property ex 'face mr-regions-face)
      (set-extent-property ex 'mr-region t))
    ))

(defun mr-region-unmark ()
  "Unmark all mr regions."
  (interactive)

  (setq mr-regions nil)
  (map-extents (lambda (ex notused)
                 (when (extent-property ex 'mr-region)
                   (delete-extent ex)))))

(defun mr-region-activate ()
  "Activate multiple regions."
  (interactive)
  (mapc (lambda (r)
          (let (ex)
            (unless (extent-at (car r) nil 'mr-region)
              (setq ex (make-extent (car r) (cdr r))))
            (set-extent-property ex 'face mr-regions-face)
            (set-extent-property ex 'mr-region t)))
        mr-regions))

(defun mr-region-docmd (&optional arg)
  "Execute command on all mr regions."
  (interactive "P")

  (setq unread-command-events (cons last-command-event unread-command-events))

  (let* ((cmd-keys (read-key-sequence (concat (key-description last-command-event) " ") t))
         (cmd (key-binding cmd-keys)))
    (if (not (functionp cmd))
      (error (format "%s not binded" (key-description cmd-keys)))

      ;; XXX hack over execute extented command
      (when (eq cmd 'execute-extended-command)
        (setq cmd (read-command "M-x ")))

      (mapc (lambda (r)
              (let ((start (min (car r) (cdr r)))
                    (end (max (car r) (cdr r)))
                    (zex zmacs-region-extent)
                    (zmacs-region-active-p t))
                (when (extent-at start nil 'mr-region)
                  (save-excursion
                    (push-mark start nil)
                    (goto-char end)
                    (setq zmacs-region-extent (make-extent start end))
                    (setq current-prefix-arg arg)
                    (call-interactively cmd)
                    (setq zmacs-region-extent zex))
                  )))
            (reverse mr-regions))

      (mr-region-unmark))
    ))


(defun mr-region-accept ()
  (interactive)
  (call-interactively #'mr-region-add))

(defun mr-region-bindings-install ()
  )

(defun mr-region-bindings-revert ()
  )
 
(provide 'multr)

;;; multr.el ends here
