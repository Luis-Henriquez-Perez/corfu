;;; corfu.el --- Completion Overlay Region FUnction -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Luis Henriquez-Perez <luis@luishp.xyz>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.19
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

;; This package is a Corfu extension, which prefixes candidates with indices and
;; allows you to select with prefix arguments. This is designed to be a faster
;; alternative to selecting a candidate with `corfu-next' and `corfu-previous'.

;;; Code:

(require 'corfu)

(defface corfu-indexed
  '((t :height 0.75 :inherit font-lock-comment-face))
  "Face used for the candidate index prefix."
  :group 'corfu-faces)

(defvar corfu-indexed--commands
  '(corfu-insert corfu-complete)
  "Commands that should be indexed.")

(defvar corfu-indexed--max 0)

(defun corfu-indexed--format-candidate (orig cands)
  "Advice for `corfu--format-candidates' that add an index to candidates.
See `corfu--format-candidates'."
  (let ((updated-cands nil)
	(index 0)
	(index-string nil))
    (pcase-dolist (`(,prefix ,suffix ,cand) cands)
      (cl-incf index)
      (setq index-string (format (format "%%%ds " (if (> corfu-count 10) 2 1)) index))
      (setq prefix (concat (propertize index-string 'face 'corfu-indexed) prefix))
      (push (list prefix suffix cand) updated-cands))
    (setq corfu-indexed--max index)
    (funcall orig (reverse updated-cands))))

(defun corfu-indexed--handle-prefix (orig &rest args)
  "Handle prefix argument before calling ORIG function with ARGS."
  (if (and current-prefix-arg (called-interactively-p t))
      (let ((corfu--index (+ 0 (prefix-numeric-value current-prefix-arg))))
        (if (or (< corfu--index 0)
                (> corfu--index corfu-indexed--max)
                (= corfu--total 0))
            (message "Out of range")
          (funcall orig)))
    (apply orig args)))

;;;###autoload
(define-minor-mode corfu-indexed-mode
  "Prefix candidates with indices."
  :global t :group 'corfu
  (cond
   (corfu-indexed-mode
    (advice-add #'corfu--format-candidates :around #'corfu-indexed--format-candidate)
    (dolist (cmd corfu-indexed--commands)
      (advice-add cmd :around #'corfu-indexed--handle-prefix)))
   (t
    (advice-remove #'corfu--format-candidates #'corfu-indexed--format-candidate)
    (dolist (cmd corfu-indexed--commands)
      (advice-remove cmd #'corfu-indexed--handle-prefix)))))

(provide 'corfu-indexed)
;;; corfu-indexed.el ends here
