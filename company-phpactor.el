;;; company-phpactor.el --- company-mode backend for Phpactor -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: Martin Tang <martin.tang365@gmail.com>
;;         Mikael Kermorgant <mikael@kgtech.fi>
;; Created: 18 Apr 2018
;; Version: 0.1.0
;; Keywords: tools, php
;; Package-Requires: ((emacs "24.3") (company "0.9.6") (phpactor "0.1.0"))
;; URL: https://github.com/emacs-php/phpactor.el
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Company integration for Phpactor.

;;; Code:
(require 'company)
(require 'phpactor)

(defgroup company-phpactor nil
  "Company backend for Phpactor."
  :prefix "company-phpactor-"
  :group 'company
  :group 'phpactor)

(defcustom company-phpactor-request-async t
  "When non-NIL, asynchronous recuest to Phpactor."
  :type 'boolean
  :group 'company-phpactor)

(defun company-phpactor--grab-symbol ()
  "If point is at the end of a symbol, return it.
Otherwise, if point is not inside a symbol, return an empty string.
Here we create a temporary syntax table in order to add $ to symbols."
  (let (($temp-syn-table (make-syntax-table php-mode-syntax-table)))
    (modify-syntax-entry ?\$ "_" $temp-syn-table)

    (with-syntax-table $temp-syn-table
      (if (looking-at "\\_>")
          (buffer-substring (point) (save-excursion (skip-syntax-backward "w_")
                                                    (point)))
        (unless (and (char-after) (memq (char-syntax (char-after)) '(?w ?_)))
          "")))))

(defun company-phpactor--get-suggestions ()
  "Get completions for current cursor."
  (let ((response (phpactor--rpc "complete" (phpactor--command-argments :source :offset))))
    (plist-get (plist-get (plist-get response  :parameters) :value) :suggestions)))

(defun company-phpactor--get-candidates (suggestions)
  "Build a list of candidates with text-properties extracted from phpactor's output `SUGGESTIONS'."
  (let (candidate)
    (mapcar
     (lambda (suggestion)
       (setq candidate (plist-get suggestion :name))
       (put-text-property 0 1 'annotation (plist-get suggestion :short_description) candidate)
       (put-text-property 0 1 'type (plist-get suggestion :type) candidate)
       (if (string= (plist-get suggestion :type) "class")
           (put-text-property 0 1 'class_import (plist-get suggestion :class_import) candidate))
       candidate)
     suggestions)))

(defun company-phpactor--post-completion (arg)
  "Trigger auto-import of completed item ARG when relevant."
  (if (get-text-property 0 'class_import arg)
      (phpactor-import-class (get-text-property 0 'class_import arg)))
  (if (member (get-text-property 0 'type arg) '(list "method" "function"))
      (let ((parens-require-spaces nil)) (insert-parentheses))))

(defun company-phpactor--annotation (arg)
  "Show additional info (ARG) from phpactor as lateral annotation."
  (message (concat " " (get-text-property 0 'annotation arg))))

(defun company-phpactor--get-candidates-async (callback)
  "Get completion candidates asynchronously calling `CALLBACK' by Phpactor."
  (if (not company-phpactor-request-async)
      (funcall callback (company-phpactor--get-candidates (company-phpactor--get-suggestions)))
    (phpactor--rpc-async "complete" (phpactor--command-argments :source :offset)
      (lambda (proc)
        (let* ((response (phpactor--parse-json (process-buffer proc)))
               (suggestions
                (plist-get (plist-get (plist-get response  :parameters) :value) :suggestions)))
          (funcall callback (company-phpactor--get-candidates suggestions)))))))

(defvar-local company-phpactor--completion-cache nil
  "Cached completion. It's an alist of (prefix . completion).
PREFIX is the prefix string.
COMPLETION is a cache-item created by `company-phpactor--cache-item-new'.")

(defun company-phpactor--cache-put (prefix candidates)
  "Set cache for PREFIX to be CANDIDATES.
CANDIDATES is a cache item created by `company-phpactor--cache-item-new'."
  (setq company-phpactor--completion-cache
        (cons (cons prefix candidates)
              company-phpactor--completion-cache)))

(defun company-phpactor--cache-get (prefix)
  "Get the cached completion for PREFIX.
Return a cache item if cache for PREFIX exists. Otherwise return nil."
  (-when-let* ((cached (company-phpactor--cache-find-closest prefix))
               (subprefix (car cached))
               (cache-item (cdr cached)))
    (cond
     ((string= prefix subprefix)
      ;; Found exact match.
      cache-item)
     ((company-phpactor--cache-item-incomplete-p cache-item)
      ;; Closest subprefix has incomplete result. Return nil to ask for narrowed
      ;; down results.
      nil)
     (t
      ;; Narrow down complete results for subprefix.
      (let* ((candidates (company-phpactor--cache-item-candidates cache-item))
             (new-candidates (company-phpactor--filter-candidates candidates prefix))
             (new-cache (company-phpactor--cache-item-new new-candidates nil)))
        (company-phpactor--cache-put prefix new-cache)
        new-cache)))))

(defun company-phpactor--cache-find-closest (prefix)
  "Find cached completion with the longest sub-prefix of PREFIX.
Return a cons cell of (subprefix . cache-item) or nil."
  (let ((len (length prefix)))
    (cl-dotimes (i (1+ len))
      (when-let (item (assoc (substring prefix 0 (- len i))
                             company-phpactor--completion-cache))
        (cl-return item)))))

(defun company-phpactor--cache-item-new (candidates incomplete)
  "Create a new cache item.
CANDIDATES: A list of strings. The completion candidates.
INCOMPLETE: t or nil. Whether the candidates are incomplete or not."
  (list :incomplete incomplete :candidates candidates))

(defun company-phpactor--cache-item-incomplete-p (cache-item)
  "Determine whether a CACHE-ITEM is incomplete."
  (plist-get cache-item :incomplete))

(defun company-phpactor--cache-item-candidates (cache-item)
  "Get candidates from a CACHE-ITEM."
  (message "cache hit: %s" (plist-get cache-item :candidates))
  (plist-get cache-item :candidates))

;;;###autoload
(defun company-phpactor (command &optional arg &rest ignored)
  "`company-mode' completion backend for Phpactor."
  (interactive (list 'interactive))
  (when phpactor-executable
    (save-restriction
      (widen)
      (pcase command
        (`post-completion (company-phpactor--post-completion arg))
        (`annotation (company-phpactor--annotation arg))
        (`interactive (company-begin-backend 'company-phpactor))
        (`prefix (company-phpactor--grab-symbol))
	(`doc-buffer (let ((doc-buffer (company-doc-buffer
					(company-phpactor--annotation arg))))
		       (with-current-buffer doc-buffer
			 (visual-line-mode))
		       doc-buffer))
        (`candidates (or (company-phpactor--cache-item-candidates (company-phpactor--cache-get arg))
			 (cons :async #'company-phpactor--get-candidates-async)))))))

(provide 'company-phpactor)
;;; company-phpactor.el ends here
