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

(defvar-local corfu-quick--list nil)
(defvar-local corfu-quick--first nil)

(defun corfu-quick--letters (start keys1 keys2)
  "Return string of propertized quick keys."
  (list (propertize (char-to-string key1))
	(propertize (char-to-string key2))))

;; Is it possible to use `avy' for this?
;; In theory I can just prepend the letters to the candidates and then
;; `corfu--format-candidates' will take care of the rest.
(defun corfu-quick--format-candidate (orig candidates)
  "Format candidate, see `corfu--format-candidate' for arguments."
  ;; Candidates are of the form ((candidate1 prefix1 suffix1)...).
  (let ((updated-candidates nil)
	(quick-letter-index 0)
	(quick-letter nil))
    (dolist (candidate candidates)
      ;; Update the prefix.
      (setf (nth 1 candidate) (concat quick-letter " " (nth 1 candidate)))
      (push candidate updated-candidates)
      (cl-incf quick-letter-index))
    (cl-callf reverse updated-candidates)
    (apply orig updated-candidates)))

;;;###autoload
(defun corfu-quick-jump ()
  "Jump to candidate using quick-keys."
  (cl-letf (((symbol-function #'corfu--format-candidate)
             (apply-partially #'corfu-quick--format-candidate
                              (symbol-function #'corfu--format-candidate)))
            (corfu-quick--first first)
            (corfu-quick--list))
    (alist-get (read-key) corfu-quick--list)))

;; (defun corfu-quick--read ()
;;   "Read quick key given FIRST pressed key."
;;   (cl-letf (((symbol-function #'corfu--format-candidate)
;;              (apply-partially #'corfu-quick--format-candidate
;;                               (symbol-function #'corfu--format-candidate)))
;;             (corfu-quick--first first)
;;             (corfu-quick--list))
;;     (corfu--exhibit)
;;     (alist-get (read-key) corfu-quick--list)))
