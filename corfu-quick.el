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

(defcustom corfu-quick1 "asdfgh"
  "Single level quick keys."
  :type 'string
  :group 'corfu)

(defcustom corfu-quick2 "jkl"
  "Two level quick keys."
  :type 'string
  :group 'corfu)

(defface vertico-quick1
  '((((class color) (min-colors 88) (background dark))
     :background "#7042a2" :weight bold :foreground "white")
    (((class color) (min-colors 88) (background light))
     :weight bold :background "#d5baff" :foreground "black")
    (t :background "magenta" :foreground "white"))
  "Face used for the first quick key."
  :group 'vertico-faces)

(defface vertico-quick2
  '((((class color) (min-colors 88) (background dark))
     :background "#004065" :weight bold :foreground "white")
    (((class color) (min-colors 88) (background light))
     :weight bold :background "#8ae4f2" :foreground "black")
    (t :background "blue" :foreground "white"))
  "Face used for the second quick key."
  :group 'vertico-faces)

(defvar-local corfu-quick--list nil)
(defvar-local corfu-quick--first nil)

(defun corfu-quick--format-candidate (orig)
  "Format candidate, see `corfu--format-candidate' for arguments."
  (let* ((fst (length corfu-quick1))
         (snd (length corfu-quick2))
         (len (+ fst snd))
         (idx (- index start))
         (keys (if (>= idx fst)
                   (let ((first (elt corfu-quick2 (mod (/ (- idx fst) len) snd)))
                         (second (elt (concat corfu-quick1 corfu-quick2) (mod (- idx fst) len))))
                     (cond
                      ((eq first corfu-quick--first)
                       (push (cons second index) corfu-quick--list)
                       (concat " " (propertize (char-to-string second) 'face 'corfu-quick1)))
                      (corfu-quick--first "  ")
                      (t
                       (push (cons first (list first)) corfu-quick--list)
                       (concat (propertize (char-to-string first) 'face 'corfu-quick1)
                               (propertize (char-to-string second) 'face 'corfu-quick2)))))
                 (let ((first (elt corfu-quick1 (mod idx fst))))
                   (if corfu-quick--first
                       "  "
                     (push (cons first index) corfu-quick--list)
                     (concat (propertize (char-to-string first) 'face 'corfu-quick1) " "))))))))
