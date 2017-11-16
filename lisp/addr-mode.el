;;; addr-mode.el --- Addr code editing commands for GNU Emacs

(defconst addr-font-lock-keywords-1
  '(
    ;; Phone numbers
    ("\\([0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]\\)" 1 font-lock-function-name-face)
)
  "Subdued level highlighting for Addr mode.")

(defconst addr-font-lock-keywords-2
  (append addr-font-lock-keywords-1
   (list
    ;;
    (concat "\\<\\("
	    "Fincher\\|Kane\\|Sievers\\|Kawai\\|"
	    "\\)\\>")
    ;;
    ;; Last Name, FirstName&others
    '("\\(^[A-Z][A-Za-z]+\,\ [A-Z][A-Za-z&]+\\)" . font-lock-type-face)
    ;;
    ;; Area Codes for phone numbers
    '("\\([0-9][0-9][0-9]-\\)" 1 font-lock-function-name-face)
    ;; 
    '("[$*]{?\\(\\sw+\\)" 1 font-lock-variable-name-face)
    ;; email addresses
    '("\\([a-zA-Z\.]*@[A-Za-z\.]*\\)" 1 font-lock-variable-name-face)
    ;; Fontify keywords with/and labels as we do in `c++-font-lock-keywords'.
    '("\\<\\(continue\\|goto\\|last\\|next\\|redo\\)\\>[ \t]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-reference-face nil t))
    '("^[ \t]*\\(\\sw+\\)[ \t]*:[^:]" 1 font-lock-reference-face)))
  "Gaudy level highlighting for Addr mode.")

;;font-lock-type-face green
;;font-lock-reference-face teal
;;font-lock-variable-name-face gold
;;font-lock-function-name-face blue

(defvar addr-font-lock-keywords addr-font-lock-keywords-1
  "Default expressions to highlight in Addr mode.")


(defun addr-mode ()
  "Major mode for editing Addr ."
  (interactive)
  (setq major-mode 'addr-mode)
  (setq mode-name "Addr")
  ;; Tell font-lock.el how to handle Addr.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((addr-font-lock-keywords
			      addr-font-lock-keywords-1
			      addr-font-lock-keywords-2)
			     nil nil ((?\_ . "w"))))
  (run-hooks 'addr-mode-hook))
;;;;;;;; That's all, folks! ;;;;;;;;;
