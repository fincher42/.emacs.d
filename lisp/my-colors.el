;;color::
(defun set-colors-light()
  (interactive)
(setq colors-dark nil)
(set-face-background 'default "white")
(set-face-foreground 'default "black")
)
(defun set-colors-dark()
  (interactive)
(setq colors-dark 't)
(set-face-background 'default "black")
(set-face-foreground 'default "white") 
)
(defun toggle-colors()
  (interactive)
  (if colors-dark
      (set-colors-light)
      (set-colors-dark)
  )
)
(set-colors-light)
;(set-colors-dark)


(blink-cursor-mode 0) ;;turn off blinking
(set-face-attribute 'default nil :height 150)
(defun my-set-colors ()
  (interactive)
  "sets colors and fonts for font-locking"
;  (let
     ;; ((Default-Font (face-font (get-face 'default))))
    (setq-default font-lock-auto-fontify t)
    (setq-default font-lock-use-fonts t)
    (setq-default font-lock-use-colors t)
    (setq-default font-lock-use-maximal-decoration t)
    (setq-default font-lock-mode-enable-list t)
    (setq-default font-lock-mode-disable-list nil)

    (require 'font-lock)
    (set-face-foreground 'font-lock-builtin-face        "Red")
    (set-face-foreground 'font-lock-comment-face        "#934")
    (set-face-foreground 'font-lock-constant-face       "Red")
    (set-face-foreground 'font-lock-function-name-face  "Blue")
    (set-face-foreground 'font-lock-keyword-face	"#3f3")
    (set-face-foreground 'font-lock-string-face         "#88f")
    (set-face-foreground 'font-lock-type-face           "#373")
    (set-face-foreground 'font-lock-warning-face        "Red")
    (set-face-foreground font-lock-variable-name-face   "#0b7")
    (set-face-underline-p 'font-lock-string-face nil)
    (set-cursor-color "red")
  )
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))
