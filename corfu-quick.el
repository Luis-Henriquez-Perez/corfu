;;; corfu-quick.el --- Completion Overlay Region FUnction -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.17
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/corfu

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package is a corfu extension, which provides quick keys.

;;; Code:

;; Taken directly from `corfu'.

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defcustom corfu-quick1 "asdfgh"
  "Single level quick keys."
  :type 'string
  :group 'corfu)

(defcustom corfu-quick2 "jkl"
  "Two level quick keys."
  :type 'string
  :group 'corfu)

(defface corfu-quick1
  '((((class color) (min-colors 88) (background dark))
     :background "#7042a2" :weight bold :foreground "white")
    (((class color) (min-colors 88) (background light))
     :weight bold :background "#d5baff" :foreground "black")
    (t :background "magenta" :foreground "white"))
  "Face used for the first quick key."
  :group 'corfu-faces)

(defface corfu-quick2
  '((((class color) (min-colors 88) (background dark))
     :background "#004065" :weight bold :foreground "white")
    (((class color) (min-colors 88) (background light))
     :weight bold :background "#8ae4f2" :foreground "black")
    (t :background "blue" :foreground "white"))
  "Face used for the second quick key."
  :group 'corfu-faces)

(defvar-local corfu-quick--list nil
  "An alist whose elements are (quick-key . index).")
(defvar-local corfu-quick--first nil)

(defun corfu-quick--letters ()
  "Return string of propertized quick keys."
  (propertize (char-to-string (seq-first corfu-quick1)))
  ;; (let* ((fst (length corfu-quick1))
  ;;        (snd (length corfu-quick2))
  ;;        (len (+ fst snd))
  ;;        (idx (- index start)))
  ;;   (if (>= idx fst)
  ;;       (let ((first (elt corfu-quick2 (mod (/ (- idx fst) len) snd)))
  ;;             (second (elt (concat corfu-quick1 corfu-quick2) (mod (- idx fst) len))))
  ;;         (cond
  ;;          ((eq first corfu-quick--first)
  ;;           (push (cons second index) corfu-quick--list)
  ;;           (concat " " (propertize (char-to-string second) 'face 'corfu-quick1)))
  ;;          (corfu-quick--first "  ")
  ;;          (t
  ;;           (push (cons first (list first)) corfu-quick--list)
  ;;           (concat (propertize (char-to-string first) 'face 'corfu-quick1)
  ;;                   (propertize (char-to-string second) 'face 'corfu-quick2)))))
  ;;     (let ((first (elt corfu-quick1 (mod idx fst))))
  ;;       (if corfu-quick--first
  ;;           "  "
  ;;         (push (cons first index) corfu-quick--list)
  ;;         (concat (propertize (char-to-string first) 'face
  ;; 			      'corfu-quick1) " ")))))
  )

;; Is it possible to use `avy' for this?
;; In theory I can just prepend the letters to the candidates and then
;; `corfu--format-candidates' will take care of the rest.
;; One difference from this and its counterpart in `vertico' is that vertico
;; only formats one candidate at a time.
(defun corfu-quick--format-candidates (orig candidates)
  "Format candidate, see `corfu--format-candidate' for arguments."
  ;; Candidates are of the form ((candidate1 prefix1 suffix1)...).
  (let ((updated-candidates nil))
    (pcase-dolist (`(,candidate ,prefix ,suffix) candidates)
      ;; Update the prefix.
      (setq quick-letters (corfu-quick--letters))
      (setq prefix (concat quick-letters " " prefix))
      (push (list candidate prefix suffix) updated-candidates))
    (cl-callf reverse updated-candidates)
    (apply orig updated-candidates)))

(defun corfu-quick--read ()
  "Read quick key given FIRST pressed key."
  (cl-letf* ((old-fn (symbol-function #'corfu--format-candidate))
	     (new-fn (apply-partially #'corfu-quick--format-candidate old-fn))
	     ((symbol-function #'corfu--format-candidate) new-fn))
    (alist-get (read-key) corfu-quick--list)))

;;;###autoload
(defun corfu-quick-jump ()
  "Jump to candidate using quick-keys."
  (interactive)
  (if (zerop corfu--total)
      (and (message "No match") nil)
    (let ((idx (corfu-quick--read)))
      (when (consp idx) (setq idx (corfu-quick--read (car idx))))
      (when idx (setq corfu--index idx)))))

