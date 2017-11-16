(setq marketplace-log-highlights
      '(
	("INFO\\|DEBUG\\|WARN\\|ERROR" . font-lock-function-name-face)
	;; 2017-03-14 20:36:34,406
        ("[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9][0-9]:[0-9][0-9]:[0-9][0-9],[0-9]\\{3\\}" . font-lock-constant-face)
	("Enter Request\\|Exit Request" . font-lock-keyword-face)
	("^====.*" . font-lock-string-face)
	("^time elapsed.*" . font-lock-doc-face)
	(".*action method:.*" . font-lock-comment-face)
	("^URL.*" . font-lock-preprocessor-face)
	;;b6bd14b7-1eb4-44aa-93e9-1b684bdcd37b - 9 -
	("[a-z0-9]\\{8\\}-[a-z0-9]\\{4\\}-[a-z0-9]\\{4\\}-[a-z0-9]\\{4\\}-[a-z0-9]\\{12\\}" . font-lock-warning-face)
	)
)

(define-derived-mode marketplace-log-mode fundamental-mode "marketplace-log"
  "major mode for editing marketplace-log language code."
  (setq font-lock-defaults '(marketplace-log-highlights)))

(set-face-foreground 'font-lock-doc-face        "Purple")
(set-face-foreground 'font-lock-comment-face        "LightGreen")
(set-face-foreground 'font-lock-preprocessor-face        "Coral")
(set-face-foreground 'font-lock-warning-face        "#eee")

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
