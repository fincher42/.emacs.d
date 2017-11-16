;;; gedcom.el --- Aid in editing lifelines GEDCOM records

;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.
;;               1994 Stephen A. Wood

;; Author: Stephen A. Wood (saw@cebaf.gov)
;; Version 0.1

;; This file is not part of GNU Emacs, but works with GNU Emacs.  The
;; licensing terms are the same as those that cover GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; For using emacs (in server mode) as the editor for lifelines.
;;
;; When you are done editing a gedcom record, type C-c C-c.  This will cause
;; the change date to be added to the record.  If you don't want the change
;; date added, exit with C-x # instead.
;;
;; Code to edit the notes that get put below CHAN tags was taken
;; from the GNU Emacs file vc.el.

;;; Code:

(provide 'gedcom)

(require 'ring)

(if (not (assoc 'gedcom-parent-buffer minor-mode-alist))
    (setq minor-mode-alist
          (cons '(gedcom-parent-buffer gedcom-parent-buffer-name)
                minor-mode-alist)))

;;; So that buffer gets deleted when server returns file
(setq server-temp-file-regexp "\\.[0-9]*ltmp$")

;; General customization

(defvar gedcom-edit-date-with-time t
  "*If non-nil, include the time with the CHAN tag.")

(defvar gedcom-edit-date-with-note t
  "*If non-nil, prompt for a note to put with the CHAN tag.")


(defconst gedcom-maximum-note-ring-size 32
  "Maximum number of saved notes in the note ring.")


;; Variables the user doesn't need to know about

(defvar gedcom-parent-buffer nil)
(defvar gedcom-parent-buffer-name nil)
(defvar gedcom-note-ring nil)
(defvar gedcom-note-ring-index nil)
(defvar gedcom-last-note-match nil)

;;;(add-hook 'server-visit-hook
;;;       (function (lambda ()
;;;                   (local-set-key "\C-c\C-c" 'gedcom-edit-date))))

(defvar gedcom-mode-map ()
  "Keymap used in gedcom mode.")

(if gedcom-mode-map
    ()
  (setq gedcom-mode-map (make-sparse-keymap))
  (define-key gedcom-mode-map "\C-c\C-c" 'gedcom-edit-date))

;;;###autoload
(defun gedcom-mode ()
  "Major mode for editing GEDCOM records.

Key definitions:
\\{gedcom-mode-map}
"
  (interactive)
  (use-local-map gedcom-mode-map)
  (setq mode-name "Gedcom")
  (setq major-mode 'gedcom-mode)
  (run-hooks 'gedcom-mode-hook))

(defun gedcom-edit-date ()
  "Add a CHAN tag to indicate when the last change was made."
  (interactive)
  (if (buffer-modified-p)
      (let* ((now (current-time-string))
             (today (concat (substring now 4 11) (substring now -4)))
             (time (substring now 11 16)))
        (beginning-of-buffer)
        (if     (search-forward "1 CHAN" nil 'move)
            (beginning-of-line))
        (insert "1 CHAN\n  2 DATE ")
        (insert today)
        (insert "\n")
        (if gedcom-edit-date-with-time
            (progn
              (insert "    3 TIME ")
              (insert time)
              (insert "\n")))
        (if gedcom-edit-date-with-note
            (gedcom-start-noteentry)
          (server-edit)))
    (server-edit)))

;;; Keymap for
(defvar gedcom-note-entry-mode ()
  "Keymap used in gedcom note entry mode.")
(if gedcom-note-entry-mode
    ()
  (setq gedcom-note-entry-mode (make-sparse-keymap))
  (define-key gedcom-note-entry-mode "\M-n" 'gedcom-next-note)
  (define-key gedcom-note-entry-mode "\M-p" 'gedcom-previous-note)
  (define-key gedcom-note-entry-mode "\M-r" 'gedcom-note-search-reverse)
  (define-key gedcom-note-entry-mode "\M-s" 'gedcom-note-search-forward)
  (define-key gedcom-note-entry-mode "\C-c\C-c" 'gedcom-finish-noteentry))

(defun gedcom-edit-note-mode ()
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map gedcom-note-entry-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'gedcom-edit-note-mode)
  (setq mode-name "NOTE-Log")
  ;;  (make-local-variable 'gedcom-log-file)
  ;;  (make-local-variable 'gedcom-log-version)
  (make-local-variable 'gedcom-note-ring-index)
  (set-buffer-modified-p nil)
  (setq buffer-file-name nil)
  (run-hooks 'text-mode-hook 'gedcom-edit-note-mode-hook)
  )

(defun gedcom-start-noteentry ()
  (let ((parent (current-buffer)))
    (pop-to-buffer (get-buffer-create "*Modification note*"))
    (set (make-local-variable 'gedcom-parent-buffer) parent)
    (set (make-local-variable 'gedcom-parent-buffer-name)
         (concat " from " (buffer-name gedcom-parent-buffer)))
    (gedcom-edit-note-mode)
    (message "Type C-c C-c when done.")))

(defun gedcom-finish-noteentry ()
  (interactive)
  (goto-char (point-max))
  (if (not (bolp))
      (newline))
  (if (null gedcom-note-ring)
      (setq gedcom-note-ring (make-ring gedcom-maximum-note-ring-size)))
  (ring-insert gedcom-note-ring (buffer-string))
  (gedcom-dress-buffer 2 "NOTE")
  (pop-to-buffer gedcom-parent-buffer)
  (insert-buffer "*Modification note*")
  (let ((logbuf (get-buffer "*Modification note*")))
    (delete-windows-on logbuf)
    (kill-buffer logbuf))
  (server-edit))

(defun gedcom-dress-buffer (level tag)
  (goto-char 0)
  (if (not (eobp))
      (progn
        (insert-char ?  level)
        (insert (int-to-string level))
        (insert " ")
        (insert tag)
        (insert " ")
        (forward-line)
        (while (not (eobp))
          (insert-char ?  (+ level 2))
          (insert (int-to-string (1+ level)))
          (insert " CONT ")
          (forward-line)))))

;; Code for access to the comment ring

(defun gedcom-previous-note (arg)
  "Cycle backwards through note history."
  (interactive "*p")
  (let ((len (ring-length gedcom-note-ring)))
    (cond ((<= len 0)
           (message "Empty note ring")
           (ding))
          (t
           (erase-buffer)
           ;; Initialize the index on the first use of this command
           ;; so that the first M-p gets index 0, and the first M-n gets
           ;; index -1.
           (if (null gedcom-note-ring-index)
               (setq gedcom-note-ring-index
                     (if (> arg 0) -1
                         (if (< arg 0) 1 0))))
           (setq gedcom-note-ring-index
                 (mod (+ gedcom-note-ring-index arg) len))
           (message "%d" (1+ gedcom-note-ring-index))
           (insert (ring-ref gedcom-note-ring gedcom-note-ring-index))))))

(defun gedcom-next-note (arg)
  "Cycle forwards through note history."
  (interactive "*p")
  (gedcom-previous-note (- arg)))

(defun gedcom-note-search-reverse (str)
  "Searches backwards through note history for substring match."
  (interactive "sNote substring: ")
  (if (string= str "")
      (setq str gedcom-last-note-match)
    (setq gedcom-last-note-match str))
  (if (null gedcom-note-ring-index)
      (setq gedcom-note-ring-index -1))
  (let ((str (regexp-quote str))
        (len (ring-length gedcom-note-ring))
        (n (1+ gedcom-note-ring-index)))
    (while (and (< n len) (not (string-match str (ring-ref gedcom-note-ring n))))
      (setq n (+ n 1)))
    (cond ((< n len)
           (gedcom-previous-note (- n gedcom-note-ring-index)))
          (t (error "Not found")))))

(defun gedcom-note-search-forward (str)
  "Searches forwards through note history for substring match."
  (interactive "sNote substring: ")
  (if (string= str "")
      (setq str gedcom-last-note-match)
    (setq gedcom-last-note-match str))
  (if (null gedcom-note-ring-index)
      (setq gedcom-note-ring-index 0))
  (let ((str (regexp-quote str))
        (len (ring-length gedcom-note-ring))
        (n gedcom-note-ring-index))
    (while (and (>= n 0) (not (string-match str (ring-ref gedcom-note-ring n))))
      (setq n (- n 1)))
    (cond ((>= n 0)
           (gedcom-next-note (- n gedcom-note-ring-index)))
          (t (error "Not found")))))



  (setq gedcom-font-lock-keywords (purecopy
   (list
    ;; fontify #'s as comments.
    '("^#.*" . font-lock-comment-face)
    ;; fontify function names.
    '("\\(NAME\\)" . font-lock-function-name-face)
    ;; fontify strings - font-lock-string-face
    '("\\/[A-Za-z0-9._]*" . font-lock-string-face)
    ;;
    ;; fontify keywords - font-lock-keyword-face
    '("\\(FAMS\\)" . font-lock-keyword-face)
;
;'font-lock-doc-string-face
    '("asdfasdf" . font-lock-doc-string-face)
    )))

;;; End of lifelines definitions

