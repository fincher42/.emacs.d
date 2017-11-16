;;example file of emacs syntax highlighting for log files - mitch fincher 2017
(setq zoo-log-highlights
      '(
	("INFO\\|DEBUG\\|WARN\\|ERROR" . font-lock-function-name-face)
	;; 2017-03-14 20:36:34,406
	;; \\{4\\} is regex for repeat previous item 4 times
        ("[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9][0-9]:[0-9][0-9]:[0-9][0-9],[0-9]\\{3\\}" . font-lock-constant-face)
	("Entering\\|Exiting" . font-lock-keyword-face)
	;;hightlight line that starts with "====" followed by anything
	("^====.*" . font-lock-comment-face)
	("Gorillas\\|Monkeys\\|Lions" . font-lock-doc-face)
	)
)

(define-derived-mode zoo-log-mode fundamental-mode "zoo-log"
  "major mode for editing zoo-log log files."
  (setq font-lock-defaults '(zoo-log-highlights)))

;;override default colors for some
(set-face-foreground 'font-lock-doc-face        "Purple")
(set-face-foreground 'font-lock-comment-face        "LightGreen")

;; available faces
;;font-lock-builtin-face
;;font-lock-comment-face
;;font-lock-comment-delimiter-face
;;font-lock-constant-face
;;font-lock-doc-face
;;font-lock-doc-string-face
;;font-lock-function-name-face
;;font-lock-keyword-face
;;font-lock-negation-char-face
;;font-lock-preprocessor-face
;;font-lock-string-face
;;font-lock-type-face
;;font-lock-variable-name-face
;;font-lock-warning-face
