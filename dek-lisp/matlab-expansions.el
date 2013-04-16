;;; matlab-expansions.el --- matlab-mode expansions for expand-region

;; Copyright (C) 2012 Mark Hepburn

;; Author: Mark Hepburn
;; Keywords: marking region

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Feel free to contribute any other expansions for Matlab at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)

;;; Matlab-mod received a major rewrite between versions 23 and 24 of
;;; Emacs, for example using the new smie package instead of
;;; hand-coding a lot of motion commands.  Unfortunately for our
;;; purposes here, in the process the behaviour of `matlab-mark-block'
;;; changed slightly.  So, in order to behave identically across both
;;; versions we need to check which is which in a few places and
;;; adjust accordingly:

(defun er/matlab-mark-up-block ()
  "Mark the containing block, assuming the current block has
already been marked."
  (interactive)
  (when (use-region-p)
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (matlab-forward-sexp)
    (push-mark (point) nil t)
    (matlab-backward-sexp)
    ))

(defun er/matlab-mark-function ()
  "Mark the function"
  (interactive)
    (let ((curpoint (point)))
      (matlab-end-of-defun)
      (push-mark (point) nil t)
      (goto-char curpoint))
    (matlab-beginning-of-defun)
    )

(defun er/matlab-mark-command ()
  "Mark the command"
  (interactive)
  (when (use-region-p)
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (matlab-end-of-command)
    (push-mark (point) nil t)
    (matlab-beginning-of-command)
    ))

(defun er/matlab-mark-block ()
  "Not for general use; this is a work-around for the different
behaviour of `matlab-mark-block' between emacs versions 23 and
24."
  (interactive)
  (forward-sexp)
  (push-mark (point) nil t)
  (backward-sexp)
  )

(defun er/matlab-expansions ()
  "Adds matlab/matlab-specific expansions for buffers in matlab-mode"
  (let ((try-expand-list-additions '(
				     er/matlab-mark-command
				     er/matlab-mark-block
				     er/matlab-mark-function
				     )))
    (make-local-variable 'er/try-expand-list)
    (setq er/try-expand-list try-expand-list-additions)))

(er/enable-mode-expansions 'matlab-mode 'er/matlab-expansions)

(provide 'matlab-expansions)
