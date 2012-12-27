;;; gc-info.el --- G. Chen's info util on hiliting for XEmacs

(require 'info)

(set-face-foreground 'info-xref "blue")
(set-face-foreground 'info-node "firebrick")

(defface info-heading '((t (:bold t)))
  "Face used for headings in info."
  :group 'info-faces)

(set-face-foreground 'info-heading "red")

(defadvice Info-fontify-node (after Info-fontify-node-extra activate)
  (save-excursion
    (let ((case-fold-search nil)
	  p)

      ;;; ** hide xref `dressing' **
      ;; -- there seems 4 types of xref info links --
      ;; 1. *note <node>::
      ;; 2. *note <label>: <node>[,.]
      ;; 3. *note (<infofile>)<node>::
      ;; 4. *note <label>: (<infofile>)<node>[,.]
      (goto-char (point-min))
      (while (re-search-forward "\\(*[Nn]ote[ \t\n]*\\)\\(([^()]*)\\)?\\([^:]+\\)\\(:[^:;.,]*[:;.,]\\)" nil t)
	(set-extent-property (make-extent (match-beginning 1) (match-end 1)) 'invisible t)
	(set-extent-property (make-extent (match-beginning 4) (match-end 4)) 'invisible t)
	(and (match-beginning 2)
	     (null (equal (match-string 3) "Top"))
	     (set-extent-property (make-extent (match-beginning 2) (match-end 2)) 'invisible t))
	)

      ;; hilite lines of CAP-letters
      (goto-char (point-min))
      (while (re-search-forward "\n\\([A-Z][A-Z0-9 _]+\\)\n" nil t)
	(set-extent-property (make-extent (match-beginning 1) (match-end 1))
			     'face 'info-heading))

      ;; hilite 'underlined' lines:
      (goto-char (point-min))
      (while (re-search-forward "\n\\(-+\\|=+\\|\\*+\\)\n" nil t)
	(setq p (point))
	(save-excursion
	  (next-line -2)
	  (beginning-of-line)
	  (set-extent-property (make-extent (point) p)
			       'face 'info-heading))
	)))
)


;;;eof