;;; wand-button.el --- Generate buttons using ffi-wand.
        
;; Copyright (C) 2009 by Zajcev Evegny.
        
;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Dec  3 18:59:06 2009
;; Keywords: wand, ImageMagick
         
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

(require 'ffi-wand)

(defun* wand-button-gen-colors (col &key (amo 0.1))
  "Generate lighter and darker version of color COL.
AMO specifies light step in HSL values."
  (let* ((ohsl (Wand-with-pixel-wand pw
                 (setf (Wand:pixel-color pw) col)
                 (Wand:pixel-hsl pw)))
         (ohs (butlast ohsl))
         (ol (nth 2 ohsl)))
    (flet ((cdec (p) (max 0.0 (- p amo)))
           (cinc (p) (min 1.0 (+ p amo))))
      (list (append ohs (list (cdec ol)))
            (append ohs (list (cinc ol)))))))

(defun* wand-button (width height text &key (type 'released)
                           (bg "white") (fg "black")
                           (wand-font nil) (depth 2) (round 0.0)
                           (contrast 0.1) (xoff 0) (yoff 0))
  "Create button of WIDTHxHEIGHT size with TEXT in the center.
TYPE is one of `pressed' or `released', default is `released'.
BG specifies button's background.
FG specifies button's foreground.
FONT-OBJ is the specification for the font to use.
DEPTH specifies 3D look depth.
ROUND is the float specifies radius for round corners.
Return image specification."
  (Wand-with-wand wand
    (setf (Wand:image-size wand) (cons width height))
    (Wand:MagickReadImage
     wand (concat "xc:" (face-background-name 'default)))
    (Wand-with-drawing-wand dw
      (Wand:draw-push-clip-path dw "clip_1")
      (let ((h2 (/ height 2)))
        (Wand:draw-polygon dw (list (cons 0 height)
                                    (cons h2 h2)
                                    (cons (- width h2) h2)
                                    (cons width 0)
                                    (cons width height))))
      (Wand:draw-pop-clip-path dw)

      (flet ((drr ()
               (loop for i from 0 below depth do
                 (Wand:draw-round-rectangle
                  dw (+ 0.0 i) (+ 0.0 i) (+ 0.0 (- width 1) (- i))
                  (+ 0.0 (- height 1) (- i))
                  (max (- round i) 0.0) (max (- round i) 0.0)))))

        ;; Set button's background
        (Wand-with-pixel-wand pw
          (setf (Wand:pixel-color pw) bg
            (Wand:draw-fill-color dw) pw))

        ;; Draw outlines of the button
        (let ((bcols (wand-button-gen-colors bg :amo contrast)))
          (when (eq type 'released)
            (setq bcols (nreverse bcols)))

          (Wand:push-drawing-wand dw)
          (Wand-with-pixel-wand pw
            (setf (Wand:pixel-hsl pw) (first bcols)
                  (Wand:draw-stroke-color dw) pw))
          (drr)

          (Wand-with-pixel-wand pw
            (setf (Wand:pixel-hsl pw) (second bcols)
                  (Wand:draw-stroke-color dw) pw))
          (setf (Wand:draw-clip-path dw) "clip_1")
          (drr)
          (Wand:pop-drawing-wand dw)

          ;; Draw button's label
          (Wand-with-pixel-wand pw
            (setf (Wand:pixel-color pw) fg
                  (Wand:draw-fill-color dw) pw))
          (when wand-font
            (setf (Wand:draw-font dw) wand-font))
          (setf (Wand:text-gravity dw) :CenterGravity)
          (Wand:draw-annotation dw (float xoff) (float yoff) text)
          ;; 
          (Wand:MagickDrawImage wand dw)

          (vector 'rawrgb
            :data (Wand:get-image-pixels-internal wand 0 0 width height)
            :pixel-width width
            :pixel-height height))))))

(provide 'wand-button)

;;; wand-button.el ends here
