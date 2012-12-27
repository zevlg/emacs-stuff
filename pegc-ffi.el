;;; pegc-ffi.el --- FFI for pegc library.
        
;; Copyright (C) 2008-2010 by Zajcev Evgeny.
        
;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Dec 10 03:17:23 2008
;; Keywords: ffi
         
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

;; 

;;; Code:

;(ffi-load "/Users/00000000/down/pegc/src/libpegc")

;;; Types
;; Bool is char for pegc
(define-ffi-type pegc-bool byte)
(define-ffi-translator-to-foreign pegc-bool
  (if value 1 0))
(define-ffi-translator-from-foreign pegc-bool
  (not (zerop value)))

(define-ffi-type pegc-parser (pointer void))

(define-ffi-struct PegcRuleClient
  (flags unsigned-int)
  (data pointer))

(define-ffi-type PegcRule_mf pointer)

(declare-ffi-type PegcRule)
(define-ffi-struct PegcRule
  (rule PegcRule_mf)
  (data pointer)
  (proxy (pointer PegcRule))
  ;; XXX inline client data
;  (flags unsigned-int)
;  (data pointer)
  (client PegcRuleClient)
  (name (pointer char)))

(define-ffi-type pegc-iterator (pointer char))
(define-ffi-struct pegc-cursor
  (begin pegc-iterator)
  (pos pegc-iterator)
  (end pegc-iterator))

(define-ffi-struct pegc-stats
  ;; Number of GC entries in the context.
  (gc-count unsigned-int)

  ;; A rough *approximatation* of amount of memory allocated for
  ;; pegc-specific internal structures used by the context, not
  ;; including the size of the underlying GC hashtable(s).
  (alloced unsigned-int)

  ;; Reports the *approximate* storage allocated by the
  ;; GC hashtable(s). See alloced for caveats.
  (gc-internals-alloced unsigned-int))

(define-ffi-type pegc-action pointer)


;;; Functions
(cffi:defcfun ("pegc_create_parser" pegc-create-parser-1) pegc-parser
  (in pointer) (len long))

(defun pegc-prepare-input (input)
  "Return prepared INPUT.
Suitable for use in `pegc-create-parser-1' or `pegc-set-input-1'"
  (cond ((stringp input)
         (list (ffi-create-fo 'c-string input) (length input)))
        ((bufferp input)
         (let ((bs (buffer-substring
                    (point-min input) (point-max input) input)))
           (list bs (length bs))))
        (t
         (list (ffi-null-pointer) 0))))

(defun pegc-create-parser (input)
  "Create new parser object.
INPUT might be buffer, string or nil."
  (multiple-value-bind (inp inl)
      (pegc-prepare-input input)
    (let ((p (pegc-create-parser-1 inp inl)))
      ;; Hold reference on INP to prevent it from GCing
      (put p 'input inp)
      p)))

(cffi:defcfun ("pegc_set_input" pegc-set-input-1) pegc-bool
  "Set input for parser."
  (p pegc-parser) (begin pegc-iterator) (length long))

(defun pegc-set-input (p input)
  "For parser P set INPUT.
INPUT can be string, buffer or nil."
  (multiple-value-bind (inp inl)
      (pegc-prepare-input input)
    (prog1
        (pegc-set-input-1 p inp inl)
      (put p 'input inp))))

(cffi:defcfun pegc-destroy-parser pegc-bool
  "Clear the parser's internal state.
Free any resources created internally by the parsing process. It then
calls free() to deallocate the parser.

This routine returns non-nil only if st is 0."
  (p pegc-parser))

(cffi:defcfun ("pegc_parse" pegc-parse-1) pegc-bool
  (p pegc-parser) (prule (pointer PegcRule)))

(defun pegc-parse (p ruleset &optional start-rule)
  "Parse P's input using RULESET.
RULESET obtained from `pegc-intern-ruleset'.
START-RULE is starting rule used in parsing.  If ommited then first
rule in RULESET is used.
Return non-nil if P's input matches."
  (pegc-parse-1
   p (ffi-address-of
      (pegc-find-rule (or start-rule (caar ruleset)) ruleset))))

(cffi:defcfun pegc-get-stats pegc-stats
  "Return the current stats for the given context."
  (p pegc-parser))

(cffi:defcfun pegc-get-match-string pegc-iterator
  (p pegc-parser))

(cffi:defcfun pegc-get-match-cursor pegc-cursor
  "Returns a cursor pointing to the current match in st."
  (p pegc-parser))

(cffi:defcfun pegc-cursor-tostring (pointer char)
  "Returns a copy of the string delimited by curs, or 0 if there
is no match or there is a length-zero match. The caller is
responsible for deallocating the returned string using free()."
  (curs pegc-cursor))

(cffi:defcfun ("pegc_matches_string" pegc-matches-string-1) pegc-bool
  (p pegc-parser) (str c-string) (strlen long) (case-sensitive pegc-bool))

(defun pegc-matches-string (p str &optional case-sensitive)
  "Return non-nil if the next characters of P match STR.
Does not consume input."
  (pegc-matches-string-1 p str (length str) case-sensitive))

(cffi:defcfun pegc-clear-match void
  (p pegc-parser))

(cffi:defcfun pegc-set-client-data void
  "Associates client-side DATA with the given parser P."
  (p pegc-parser) (data pointer))

(cffi:defcfun pegc-get-client-data pointer
  (p pegc-parser))

(defalias 'pegc-client-data 'pegc-get-client-data)
(defsetf pegc-client-data (p) (data)
  `(pegc-set-client-data ,p ,data))

;; positions
(cffi:defcfun pegc-begin pegc-iterator
  "Return very beginning position."
  (p pegc-parser))

(cffi:defcfun pegc-pos pegc-iterator
  "Return current position."
  (p pegc-parser))

(cffi:defcfun pegc-set-pos pegc-bool
  "Set P position to POS."
  (p pegc-parser) (pos pegc-iterator))

(defsetf pegc-pos (p) (pos)
  `(pegc-set-pos ,p ,pos))

;; Rules
(cffi:defcfun ("pegc_is_rule_valid" pegc-rule-valid-p) pegc-bool
  (rp (pointer PegcRule)))

(defun pegc-rule (name mf-rule &optional data)
  (let ((r (make-ffi-object 'PegcRule))
        (rn (ffi-create-fo 'c-string name)))
    (setf (PegcRule->data r) (or data (ffi-null-pointer))
          (PegcRule->name r) rn
          (PegcRule->rule mf-rule) mf-rule)
    ;; Hold reference on name
    (put r 'name rn)
    r))

(cffi:defcfun pegc-r-char PegcRule
  (cin char) (case-sensitive pegc-bool))

(cffi:defcfun pegc-r-notchar PegcRule
  (cin char) (case-sensitive pegc-bool))

(cffi:defcfun pegc-r-string PegcRule
  (cin pegc-iterator) (case-sensitive pegc-bool))

(cffi:defcfun pegc-r-oneof PegcRule
  "Return a rule which matches if any character in the given string.
Matches the next input char."
  (cin (pointer char)) (case-sensitive pegc-bool))

(cffi:defcfun pegc-r-star-p PegcRule
  "Create a 'star' rule for the given proxy rule.
This rule acts like a the regular expression \(Rule\)*. Always
matches but may or may not consume input. It is \"greedy\",
matching as long as it can UNLESS the proxy rule does not
consume input, in which case this routine stops at the first
match to avoid an endless loop."
  (proxy (pointer PegcRule)))

(cffi:defcfun pegc-r-plus-p PegcRule
  "Create a 'plus' rule for the given proxy rule.
Works like `pegc-r-star-p', but matches 1 or more times.  This
routine is \"greedy\", matching as long as it can UNLESS the
proxy rule does not consume input, in which case this routine
stops at the first match to avoid an endless loop."
  (proxy (pointer PegcRule)))

(cffi:defcfun pegc-r-opt-p PegcRule
  "Always returns true but only consumes if proxy does.
Equivalent expression: \(RULE\)?"
  (proxy (pointer PegcRule)))

(cffi:defcfun pegc-r-char-range PegcRule
  "Return a rule which matches any character in the range [start,end]."
  (start char) (end char))

(cffi:defcfun pegc-r-char-spec PegcRule
  "Matche a single char in a set defined by the spec parameter.
spec must be in the format \"[a-z]\", where \"abc\" is a range
specified such as \"a-z\", \"A-Z\", or \"a-zA-Z\". It uses sscanf()
to do the parsing, so it supports any definition supported by
the '%[' specifier.
       
If st or spec are null, or the first character of spec
is not a '[' then an invalid rule is returned."
  (p pegc-parser) (spec c-string))

(cffi:defcfun pegc-r-at-p PegcRule
  "Create a rule which matches if proxy matches, but does not consume.
PROXY must not be null-pointer and must outlive the returned object."
  (proxy (pointer PegcRule)))

(cffi:defcfun pegc-r-notat-p PegcRule
  (proxy (pointer PegcRule)))

(cffi:defcfun pegc-r-until-p PegcRule
  "Create a rule which consumes input until the proxy rule matches.
If the proxy never matches then false is returned and input is not
consumed.
The match string will range from the pre-rule position to the end if
the proxy parse. If you only want to parse up TO the proxy without
consuming it, wrap the proxy in an \"at\" rule using pegc_r_at_v() or
pegc_r_at_p()."
  (proxy (pointer PegcRule)))

(cffi:defcfun pegc-r-list-a PegcRule
  "Create a rule which performs either an OR or AND operation.
if orOp is non-nil do OR otherwise do AND operation on the given
list of rules. The list MUST be terminated with either NULL, or
an entry where entry->rule is 0 \(i.e. an invalid rule\), or
results are undefined \(almost certainly an overflow\).

All rules in li must outlive the returned object.
\(BUG: all rules in li are currently copied \(shallowly\) instead
of pointed to.\)

This routine allocates resources for the returned rule which
belong to this API and are freed when st is destroyed.

If st or li are null then an invalid rule is returned.

The null-termination approach was chosen over the client
explicitly providing the length of the list because when
editing rule lists \(which happens a lot during development\) it
is more problematic to verify and change that number than it is
to add a trailing 0 to the list \(which only has to be done
once\). Alternately, you can use an invalid rule to mark the
end of the list."
  (orOp pegc-bool) (li (pointer PegcRule)))

(cffi:defcfun pegc-r-error PegcRule
  "Create a rule which always returns false.
Sets the parser error message to msg. The msg string is not copied
until the rule is triggered, so it must outlive the returned rule."
  (msg c-string))


(cffi:defcfun pegc-r-action-i-p PegcRule
  "Create rule which, when it matches, triggers an action immediately."
  (p pegc-parser) (rule (pointer PegcRule))
  (on-match pegc-action) (client-data pointer))

(cffi:defcfun pegc-trigger-actions void
  "Causes queued actions to be activated, in the order they were queued."
  (p pegc-parser))

(cffi:defcfun pegc-clear-actions void
  "Deallocates all queued actions."
  (p pegc-parser))

;; bind predefined rules
(mapc #'(lambda (rn)
          (set (intern (concat "pegc-rule-" rn))
               (ffi-bind 'PegcRule (concat "PegcRule_" rn))))
      '("invalid" "noteof" "alnum" "alpha" "ascii"
        "latin1" "blank" "blanks" "cntrl" "digit" "graph"
        "lower" "print" "punct" "space" "upper" "xdigit"
        "digits" "int_dec_strict" "int_dec" "double"
        "eof" "success" "failure" "eol" "bol" "has_error"))

;;; High level interface
(defvar pegc-builtin-rules
  `((invalid       . ,pegc-rule-invalid)
    ((success t)   . ,pegc-rule-success)
    ((failure nil) . ,pegc-rule-failure)
    ((any -)       . ,pegc-rule-noteof)
    ((bol ^)       . ,pegc-rule-bol)
    ((eol $)       . ,pegc-rule-eol)

    (ascii         . ,pegc-rule-ascii)
    (spaces        . ,pegc-rule-blanks)
    ((space ws)    . ,pegc-rule-space)
    (digits        . ,pegc-rule-digits)
    (double        . ,pegc-rule-double)))

(defun pegc-find-rule (name &optional ruleset)
  "Find rule by NAME in RULESET.
If RULESET is ommited then `pegc-builtin-rules' is used."
  (cdr (find name (or ruleset pegc-builtin-rules)
             :test #'(lambda (nm rnm)
                       (cond ((listp rnm) (memq nm rnm))
                             (t (eq nm rnm))))
             :key #'car)))

(defun pegc-proxy-op-p (rspec)
  (memq (car rspec) '(opt one-or-more zero-or-more at notat
                          repeat pad until)))

(defun pegc-proxy-op-spec (rspec)
  "Return non-nil if RSPEC specifies proxy operation."
  (let ((proxy-op-table
         '((opt . pegc-r-opt-p)
           (one-or-more . pegc-r-plus-p)
           (zero-or-more . pegc-r-star-p)
           (at . pegc-r-at-p)
           (notat . pegc-r-notat-p)
           (until . pegc-r-until-p))))
    (assq (car rspec) proxy-op-table)))

(defun pegc-set-rule-val (rname rval)
  "Set rule RNAME to have RVAL value in RULESET."
  (declare (special ruleset))
  (let ((ov (cdr (assq rname ruleset))))
    (if (not ov)
        ;; Put anonymous rule in the list
        (setf ruleset (put-alist rname rval ruleset))

      (c:memcpy (ffi-address-of ov) (ffi-address-of rval)
                (ffi-size-of-type 'PegcRule))
      ;; Copy properties as well
      (when (get rval 'data)
        (put ov 'data (get rval 'data)))))

  ;; Set property that rule is interned
  (let ((rv (cdr (assq rname ruleset))))
    (put rv 'interned t)))

(defun pegc-rspec-fix-anon (rspec)
  (if (not (symbolp rspec))
      (list (gensym "pegcr-anon") rspec)
    rspec))

(defun pegc-intern-complex-rule (rname rsp)
  "Intern complex rule with name RNAME and spec RSP."
  (cond ((pegc-proxy-op-p rsp)
         (let ((pop (pegc-proxy-op-spec rsp))
               (prul (pegc-intern-rule
                      (pegc-rspec-fix-anon (cadr rsp)))))
           (funcall (cdr pop) (ffi-address-of prul))))

        ((eq (car rsp) 'str)
         (let* ((fstr (ffi-create-fo 'c-string (cadr rsp)))
                (srul (pegc-r-string fstr (caddr rsp))))
           (put srul 'data fstr)
           srul))

        ((eq (car rsp) 'error)
         (let* ((estr (ffi-create-fo 'c-string (cadr rsp)))
                (srul (pegc-r-error estr)))
           (put erul 'data estr)
           erul))

        ((eq (car rsp) 'char)
         (pegc-r-char (cadr rsp) (caddr rsp)))

        ((eq (car rsp) 'notchar)
         (pegc-r-notchar (cadr rsp) (caddr rsp)))

        ((memq (car rsp) '(chars one-of))
         (let* ((clst (ffi-create-fo 'c-string (cadr rsp)))
                (crul (pegc-r-oneof clst (caddr rsp))))
           (put crul 'data clst)
           crul))

        ((eq (car rsp) 'chars-range)
         (pegc-r-char-range (cadr rsp) (caddr rsp)))

        ((memq (car rsp) '(or and))
         (let* ((pr-size (ffi-size-of-type 'PegcRule))
                (rsits (length (cdr rsp)))
                (ffrus (make-ffi-object
                        (list 'array 'PegcRule (1+ rsits)))))
           ;; Initialize it, so all rules are valid
;            (c:memset ffrus 0 (* (1+ rsits) pr-size))
;            (loop for off from 0 below (* (1+ rsits) pr-size)
;              by pr-size
;              do (setf (PegcRule->rule (cffi:inc-pointer ffrus off))
;                       (cffi:make-pointer 1.0)))
           (let ((rlst (pegc-r-list-a (eq (car rsp) 'or) ffrus)))
             ;; Hold reference to ffrus to prevent it from GCing
             (put rlst 'data ffrus)

             ;; put the value explicitely to ruleset in case
             ;; one of siblings backrefers to this rule
             (pegc-set-rule-val rname rlst)

             ;; Now setup all the sibling rules
             (loop for idx from 0 below rsits
               do (ffi-aset ffrus idx
                            (pegc-intern-rule
                             (pegc-rspec-fix-anon (nth idx (cdr rsp))) t)))
             ;; Set final rule
             (ffi-aset ffrus rsits pegc-rule-invalid)
             rlst)))

        ((eq (car rsp) 'action)
         ;; TODO: handle actions stuff
         (let ((aru (pegc-intern-rule
                     (pegc-rspec-fix-anon (caddr rsp)) t)))
           (pegc-r-action-i-p p (ffi-address-of aru) XXX FFF)
           ))

        (t (error "Unknown pegc rule type" (car rsp)))))

(defun pegc-intern-rule (rspec &optional force)
  "Intern rule specified by RSPEC."
  (declare (special ruleset))
  (if (symbolp rspec)
      (or (pegc-find-rule rspec)
          (let ((pfr (pegc-find-rule rspec ruleset)))
            (if (or (not force) (get pfr 'interned))
                pfr
              (declare (special rules-spec))
              (pegc-intern-rule (assq rspec rules-spec)))))

    (unless (consp rspec)
      (error "RSPEC must be list" rspec))

    (multiple-value-bind (rname rsp) rspec
      (let ((pfr (pegc-find-rule rspec ruleset)))
        (if (get pfr 'interned)
            ;; Already interned rule
            pfr

          (let ((rval (pegc-intern-complex-rule rname rsp)))
            (pegc-set-rule-val rname rval)
            rval))))))

(defun pegc-alloc-ruleset (rules-spec)
  "Allocate ruleset for RULES-SPEC"
   (mapcar #'(lambda (rule)
               (let ((r (make-ffi-object 'PegcRule)))
                 (c:memset (ffi-address-of r) 0 (ffi-size-of-type 'PegcRule))
                 ;; Set rule to some dump pointer, so rule will be valid
                 (setf (PegcRule->rule r) (cffi:make-pointer 1.0))
                 (cons (car rule) r)))
           rules-spec))

(defun pegc-intern-ruleset (rules-spec &optional p)
  "Intern ruleset represented by RULES-SPEC."
  (let ((ruleset (pegc-alloc-ruleset rules-spec)))
    (mapc #'(lambda (rspec)
              (let ((rule (pegc-intern-rule rspec)))
                (put rule 'rule-spec rspec)))
          rules-spec)
    ruleset))

(provide 'pegc-ffi)

;;; pegc-ffi.el ends here
