;;; wand-diagram.el --- Diagrams drawer using ffi-wand.
        
;; Copyright (C) 2009 by Zajcev Evegny.
        
;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Dec 10 17:05:50 2009
;; Keywords: wand
         
;; This file is NOT part of SXEmacs.

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

;; DOT:
;;   Constructor: (cons x y)
;;   Selectors: (car D), (cdr D)
(defun wandiag-distance (d1 d2)
  "Return distance betwean dots D1 and D2."
  (sqrt (apply #'+ (mapcar #'(lambda (x) (* x x))
                           (list (- (car d1) (car d2))
                                 (- (cdr d1) (cdr d2)))))))

(defun wandiag-compare-dots (d1 d2 selector op)
  "Compare dots D1 and D2 using SELECTOR and OP."
  (apply op (mapcar selector (list d1 d2))))

(defmacro define-wandiag-dot-compare (op)
  (let ((sym (make-symbol (concat "wandiag-dot-" (symbol-name op)))))
    `(defun ,sym (d1 d2 selector)
       (wandiag-compare-dots d1 d2 selector ,op))))

(define-wandiag-dot-compare <=)
(define-wandiag-dot-compare >=)

(defun wandiag-dot-between (d d1 d2 selector)
  "Return non-nil if dot D is between dots D1 and D2.
SELECTOR is used to get absciss or ordinate."
  (or (and (wandiag-dot->= d d1 selector)
           (wandiag-dot-<= d d2 selector))
      (and (wandiag-dot->= d d2 selector)
           (wandiag-dot-<= d d1 selector))))

(defun wandiag-set-colors (dw col fill-col)
  (Wand-with-pixel-wand pw
    (if col
        (setf (Wand:pixel-color pw) col
              (Wand:draw-stroke-color dw) pw)
      (setf (Wand:draw-stroke-opacity dw) 0.0))
    (if fill-col
        (setf (Wand:pixel-color pw) fill-col
              (Wand:draw-fill-color dw) pw)
      (setf (Wand:draw-fill-opacity dw) 0.0))))

(defun wandiag-draw-rect (dw ocol d1 d2 d3 d4 &optional fill-col)
  "On drawing wand DW draw polygon.
Use outline color OCOL.
Vertexes are at D1, D2 D3 and D4.
Optional FILL-COL used to fill the polygon."
  (wandiag-set-colors dw ocol fill-col)
  (Wand:draw-lines dw (list d1 d2 d3 d4 d1)))

(defun wandiag-draw-bar (dw ocol x y w h &optional fill-col)
  "Draw plain bar."
  (wandiag-draw-rect
   dw ocol (cons x y) (cons (+ x w) y)
   (cons (+ x w) (+ y h)) (cons x (+ y h))
   fill-col))

(defun wandiag-draw-3d-bar (dw ocol x y w h bar-width &optional fill-col)
  "Draw 3d bar."
  (let* ((d1 (cons x y))
         (d2 (cons (+ x w) y))
         (d3 (cons (+ x w) (+ y h)))
         (d4 (cons x (+ y h)))
         (x-off (/ bar-width 2))
         (y-off (/ bar-width 2))
         (dd1 (cons (+ x-off (car d1)) (- (cdr d1) y-off)))
         (dd2 (cons (+ x-off (car d2)) (- (cdr d2) y-off)))
         (dd3 (cons (+ x-off (car d3)) (- (cdr d3) y-off))))

    (wandiag-set-colors dw ocol fill-col)
    (Wand:draw-lines dw (list d1 d2 d3 d4 d1))
    (Wand:draw-lines dw (list d1 dd1 dd2 d2 d1))
    (Wand:draw-lines dw (list d2 dd2 dd3 d3 d2))))

(defun wandiag-calc-arc-dot-at (cnt-dot w h a)
  "Calculte dot position."
  (let* ((ra (/ (* a pi) 180))
         (cra (cos ra))
         (sra (sin ra))
         (rx (* w cra))
         (ry (* h sra)))
    (cons (round (+ (car cnt-dot) rx))
          (round (+ (cdr cnt-dot) (- ry))))))

(defun wandiag-calc-sector-dots (x y w h a1 a2)
  (let* ((mcnt (cons (+ x (/ w 2)) (+ y (/ h 2))))
         (d1 (wandiag-calc-arc-dot-at mcnt (/ w 2) (/ h 2) a1))
         (d2 (wandiag-calc-arc-dot-at mcnt (/ w 2) (/ h 2) a2)))
    (list d1 mcnt d2)))

(defun wandiag-draw-sector (dw col x y w h a1 a2 &optional fill-col)
  "Draw sector, return new dots."
  (let ((dots (wandiag-calc-sector-dots x y w h a1 a2))
        (as (- a2))
        (es (- a1)))
    (wandiag-set-colors dw col fill-col)
    (Wand:draw-arc dw x y (+ x w) (+ y h) as es)
    (Wand:draw-lines dw dots)
    dots))

(defstruct wandiag-percents
  percents
  color
  (label t)
  (offset 0))
  
(defun* wandiag-draw-percentage-plain
  (spec dw edge-col x y width height &key (label-factor 0.6)
        (label-font [:family ("Verdana") :size "14px"]))
  "Draw plain percentage diagram."
  (let ((start-angle 10.0)
        angle-begin curang)
    (flet ((draw-sector (sel angbeg angle)
             (let ((off (wandiag-percents-offset sel))
                   (lbl (wandiag-percents-label sel))
                   (xint-off 0) (yint-off 0))

               (when (not (zerop off))
                 (let ((ra (/ (* pi (+ angbeg (/ angle 2))) 180)))
                   (setq xint-off (* off (cos ra))
                         yint-off (- (* off (sin ra))))))

               (wandiag-draw-sector
                dw edge-col (+ x xint-off)
                (+ y yint-off) width height
                angbeg (+ angbeg angle) (wandiag-percents-color sel))

               (message "label: %S" lbl)
               (when lbl
                 (message "here")
                 (let* ((k label-factor)
                        (nw (* width k))
                        (nh (* height k))
                        (nx (+ x xint-off (/ (- width nw) 2)))
                        (ny (+ y yint-off (/ (- height nh) 2)))
                        (cd (wandiag-calc-sector-dots
                             nx ny nw nh angbeg
                             (+ angbeg (/ angle 2))))
                        (col edge-col)
                        (text (if (stringp lbl)
                                  lbl
                                (format "%d%%"
                                        (wandiag-percents-percents sel))))
                        )
                   (setf (Wand:draw-font-family dw) "Verdana"
                         (Wand:draw-font-size dw) 9)
                   (wandiag-set-colors dw nil "white")
                   (Wand:draw-annotation dw (float (car (nth 2 cd)))
                                         (float (cdr (nth 2 cd))) text)))
               )))

      (setq angle-begin start-angle)
      (mapc #'(lambda (sss)
                (setq curang
                      (* 360.0 (/ (wandiag-percents-percents sss) 100.0)))
                (draw-sector sss angle-begin curang)
                (incf angle-begin curang))
            spec))))

(defun* wandiag-draw-coordinates
  (dw col x y w h x-step y-step &key (notch-len 4) (with-grid nil)
      (grid-dash-even 1) (grid-dash-odd 3) (with-labels nil)
      (labels-offset 4) (labels-col col) (center-x 0) (center-y 0)
      (scale-x x-step) (scale-y y-step))
  (let (x-notches y-notches noff sls)
    (setq noff (% center-x x-step))
    (while (< noff w)
      (setq x-notches (cons (list (cons (+ x noff) (- y center-y))
                                  (cons (+ x noff) (- y center-y notch-len)))
                            x-notches))
      (setq noff (+ noff x-step)))

    (setq noff (% center-y y-step))
    (while (< noff h)
      (setq y-notches (cons (list (cons (+ x center-x) (- y noff))
                                  (cons (+ x center-x notch-len) (- y noff)))
                            y-notches))
      (setq noff (+ noff y-step)))

    ;; TODO: with-grid
    ;; TODO: with-labels

    (mapc #'(lambda (s)
              (Wand:draw-lines dw s))
          (nconc (list (list (cons x (- y center-y))
                             (cons (+ x w) (- y center-y)))
                       (list (cons (+ x center-x) y)
                             (cons (+ x center-x) (- y h))))
                 x-notches y-notches))))

(defun* wandiag-draw-point (dw col dot &key (point-type 'circle)
                               (point-size 2))
  "Draw a point of TYPE."

  (wandiag-set-colors dw col nil)
  (let ((dx (float (car dot)))
        (dy (float (cdr dot))))
    (ecase point-type
      (circle
       (Wand:draw-arc
        dw (- dx point-size) (- dy point-size)
        (+ dx point-size) (+ dy point-size) 0.0 360.0)
       (Wand-with-pixel-wand pw
         (setf (Wand:pixel-color pw) col)
         (setf (Wand:draw-fill-color dw) pw))
       (Wand:draw-point dw dx dy))

      (plus
       (Wand:draw-line dw (- dx point-size) dy
                       (+ dx point-size) dy)
       (Wand:draw-line dw dx (- dy point-size)
                       dx (+ dy point-size)))

      (cross
       (Wand:draw-line dw (- dx point-size) (- dy point-size)
                       (+ dx point-size) (+ dy point-size))
       (Wand:draw-line dw (- dx point-size) (+ dy point-size)
                       (+ dx point-size) (- dy point-size)))
      )))

(defun* wandiag-plot-dots (type dw col x y dots &key
                                (point-type 'circle) (point-size 2))
  "Draw dots in cartesian coordinate system which has 0 at X Y."
  ;; Adjust dots, according to X Y
  (let ((ndots (mapcar #'(lambda (dot)
                           (cons (+ x (car dot)) (- y (cdr dot))))
                       dots)))

    (ecase type
      (points
       (mapc #'(lambda (dot)
                 (wandiag-draw-point dw col dot :point-type point-type
                                     :point-size point-size))
             ndots))
      (lines
       (wandiag-set-colors dw col nil)
       (Wand:draw-lines dw ndots))

      (linespoints
       (let ((rargs (list dw col x y dots :point-type point-type
                          :point-size point-size)))
         (apply #'wandiag-plot-dots 'points rargs)
         (apply #'wandiag-plot-dots 'lines rargs)))

      (impulses
       (wandiag-set-colors dw col nil)
       (Wand:draw-segments
        dw (mapcar #'(lambda (d) (cons (cons (car d) y) d)) ndots)))

      (steps
       (wandiag-set-colors dw col nil)
       (Wand:draw-lines
        dw (mapcan #'(lambda (d dn)
                       (if dn
                           (list d (cons (car dn) (cdr d)))
                         (list d)))
                   ndots (nconc (cdr ndots) (list nil)))))

      (fsteps
       (wandiag-set-colors dw col nil)
       (Wand:draw-lines
        dw (mapcan #'(lambda (d dn)
                       (if dn
                           (list d (cons (car d) (cdr dn)))
                         (list d)))
                   ndots (nconc (cdr ndots) (list nil)))))
      )))

(defun wandiag-read-data-file (file &optional using x-scale y-scale)
  "Read data FILE and return list of dots lists.
USING is cons cell that specifies which columns to use.
X-SCALE is x coordinates scalling.
Y-SCALE is y coordinates scalling."
  (unless using (setq using (cons 1 2)))

  (with-temp-buffer
    (let (cdots dlist)
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((looking-at "^#"))

              ((looking-at "^[ \t]*$")
               ;; dots set delimiter
               (setq dlist (cons cdots dlist))
               (setq cdots nil))

              (t
               (let ((sc (split-string
                          (buffer-substring (point-at-bol) (point-at-eol))
                          "[ \t]+")))
                 (setq sc (delete "" sc))
                 (setq cdots (cons (cons (* (or x-scale 1)
                                            (string-to-int
                                             (nth (1- (car using)) sc)))
                                         (* (or y-scale 1)
                                            (string-to-int
                                             (nth (1- (cdr using)) sc))))
                                   cdots)))))
        (forward-line 1))

      ;; Add last cdots, if any
      (when cdots
        (setq dlist (cons cdots dlist)))
      dlist)))

;; Testing:
; (set-extent-end-glyph
;  (make-extent (point) (point))
;  (wandiag-glyph 400 200 "white"
;    (mapc (lambda (dots)
;            (wandiag-plot-dots 'lines dw "darkblue" 200 100 dots))
;          (wandiag-read-data-file "~/devel/emacs/world.dat"))
;    (wandiag-plot-dots 'points dw "red"
;                       200 100
;                       (car (wandiag-read-data-file
;                             "~/devel/emacs/world.cor"))
;                       :point-type 'cross)

(defmacro wandiag-glyph (dw width height bg-col &rest code)
  `(Wand-with-wand wandiag-wand-gen11
     (setf (Wand:image-size wandiag-wand-gen11) (cons ,width ,height))
     (Wand:MagickReadImage wandiag-wand-gen11 (concat "xc:" ,bg-col))
     (Wand-with-drawing-wand ,dw
       ,@code
       (Wand:MagickDrawImage wandiag-wand-gen11 ,dw))
    (Wand:glyph wandiag-wand-gen11)))
(put 'wandiag-glyph 'lisp-indent-function 'defun)


(provide 'wand-diagram)

;;; wand-diagram.el ends here
