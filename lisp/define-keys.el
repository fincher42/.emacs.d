;; ===================== The Mouse Key Family =====================
(setq mouse-drag-copy-region 't)
(setq mouse-scroll-delay .25)
;(global-set-key [S-down-mouse-3]  '(message "down mouse3") )
(global-set-key [S-down-mouse-1]  'kill-a-line )
(global-set-key [C-down-1]  'killaword)
(global-set-key [mouse-2]  'delete-char)
(global-set-key [mouse-3]  'yank)
(global-set-key [C-down-mouse-1]  'kill-word )
(define-key global-map [C-S-down-mouse-3]  'line-to-top)
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil) ;; or 1 to make it accelerate
;; 
(global-set-key [S-mouse-3]  '(lambda ()(interactive) (transpose-lines 1)(previous-line 2)(beginning-of-line)))
;;(global-set-key [S-mouse-3]  '(lambda ()(interactive)(beginning-of-line) (set-mark (point) )(transpose-lines 1)(goto-char (mark)) ))

;; ===================== The Right Keypad Family =====================
(global-set-key [kp-0] 'switch-to-other-buffer)
(global-set-key [insert] 'switch-to-other-buffer)
(global-set-key [C-kp-insert] 'switch-to-other-buffer)
(global-set-key [home] 'beginning-of-line)
(global-set-key [C-kp-home] 'beginning-of-buffer)
(global-set-key [M-kp-home] 'find-today-in-log-toappend)
(global-set-key [M-kp-7] 'find-today-in-log)
(global-set-key [C-M-kp-home] '(lambda () (interactive) (find-today-in-log)(forward-line -4)(insert-date-stamp)))
(global-set-key [C-M-kp-7] '(lambda () (interactive) (find-today-in-log)(forward-line -4)(insert-date-stamp)))

(global-set-key [C-kp-next] 'end-of-buffer) ;; page down
(global-set-key [C-kp-prior] 'beginning-of-buffer) ;; page up
(global-set-key [C-home] 'beginning-of-buffer)
(global-set-key [s-home] 'find-today-in-log-toappend)
(global-set-key [end] 'end-of-line)
(global-set-key [C-end] 'end-of-buffer)
(global-set-key [C-kp-end] 'end-of-buffer)
(global-set-key [M-kp-end] 'end-of-buffer)
(global-set-key [C-M-kp-7] 'find-today-in-log)

;; ===================== The F Key Family =====================
(global-set-key [f1]  'goto-line)
(global-set-key [(shift f1)]  'bookmark-jump)
;; DONT RESET F2 ON NT
(global-set-key [f3]  'query-replace)
(global-set-key [(shift f3)]  'query-replace-regexp)
(global-set-key [f4]  '(lambda () (interactive) (end-of-line)(eval-last-sexp )))
(global-set-key [(shift f4)]  'downcase-word)
(global-set-key [f5]  'start-kbd-macro)
(global-set-key [f6]  'end-kbd-macro)
(global-set-key [f7]  'call-last-kbd-macro)
(global-set-key [(shift f7)]  'bookmark-jump)
(global-set-key [f8]  'auto-fill-mode)
(global-set-key [f9]  'fill-paragraph)
(global-set-key [(shift f9)]  'upcase-region)
(global-set-key [f10]  'tagify-word)
(global-set-key [(shift f5)]  'bookmark-set)
(global-set-key [f11]  'insert-buffer-name)
(global-set-key [(shift control f9)]  'format-branch-name)
(global-set-key [(shift control f1)]  'set-mark-command)
(global-set-key [(shift control f2)]  'pre)
(global-set-key [(shift control f11)]  'insert-buffer-name-and-lineno)
(global-set-key [f12]  '(lambda () (interactive) (font-lock-fontify-buffer)(message "font locking")))
;(global-set-key [(shift f12)]  'my-c++-indent-defun)

;; ===================== The Meta (Esc) Key Family =====================
(define-key global-map "\M-p" '(lambda () (interactive)(beginning-of-line)(insert "<p>")(end-of-line)(insert "</p>")(forward-line 1)(beginning-of-line)     ))
(define-key global-map "\C-p" '(lambda () (interactive)(beginning-of-line)(insert "<p>")(end-of-line)(insert "</p>")(forward-line 1)(beginning-of-line)     ))
(define-key esc-map "$" 'ispell-word)
;(define-key global-map "\M-\C-m" 'vm)
(define-key global-map "\M-1" 'delete-other-windows)
(define-key global-map "\M-2" 'split-window-vertically)
(define-key global-map "\M-3" 'switch-to-third-buffer)
(define-key global-map "\M-4" 'ispell-buffer)
(define-key global-map "\M-5" 'split-window-horizontally)
(define-key global-map "\M-b" 'bury-buffer)
(define-key global-map "\M-d" 'insert-date-stamp)
(define-key global-map "\M-D" 'insert-time-stamp)
(define-key global-map "\M-e" 'editemacs)
(define-key global-map "\M-f" 'find-file)
(define-key global-map "\M-k" 'kill-buffer-now)
(define-key global-map "\M-g" 'goto-line)
(define-key global-map "\M-i" 'insert-file)
(define-key global-map "\M-j" 'jump-back)
(define-key global-map "\M-l" 'editlog)
(define-key global-map "\M-L" '(lambda () (interactive) (editlog) (goto-char (point-min))))
(define-key global-map "\C-L" '(lambda () (interactive) (beginning-of-line)(insert "<li>")(end-of-line)(insert "</li>")(forward-char -5) ))
(define-key global-map "\M-\C-p" '(lambda () (interactive) (insert "<pre></pre>")(forward-char -6)))
(define-key global-map "\M-r" '(lambda () (interactive) (revert-buffer t t)))
(define-key global-map "\M-s" 'shell)
(define-key global-map "\M-T" 'insert-time-stamp)
(define-key global-map "\M-\C-d" 'insert-date-stamp-news)
(define-key global-map "\M-u" 'undo)
;; ===================== The Ctl Key Family =====================
(define-key global-map (kbd "C-+")  '(lambda () (interactive) (beginning-of-line)(search-forward "-")(forward-char -1)(delete-char 1)(insert "+")(forward-char -1)(forward-line 1)))

(define-key global-map (kbd "C-~")  '(lambda () (interactive) (beginning-of-line)(delete-char 1)(insert "~")(forward-char -1)(forward-line 1)))
(global-set-key "\C-x\C-c" nil) ;; comment out the easy exit
(define-key global-map "\C-o" 'find-file)
(define-key global-map "\C-\M-Q"  'reload-emacs-file)
(define-key global-map "\C-w" 'kill-buffer-now)
(define-key global-map "\C-R"  'replace-string)
(define-key global-map "\C-n"  'next-line)
(define-key global-map "\C-_"  'search-forward)		; really the ^/
(define-key global-map "\C-f"  'isearch-forward)
(define-key global-map "\C-v"  'clipboard-yank)
(define-key global-map "\C-\M-S"  'search-for-word)
(define-key global-map "\C-^"  'enlarge-window2)
(define-key global-map "\C-xi" 'insert-buffer)
(define-key global-map "\C-x\C-x" 'delete-region)
(define-key global-map "\C-x\C-w" 'eval-last-sexp)
(define-key global-map "\C-k"  'kill-a-line)
(global-set-key(kbd "S-C-k") 'kill-rectangle)
(define-key global-map "\C-z"  'undo)
(define-key global-map "\C-s"  'save-buffer)
(define-key global-map "\C-\M-A" '(lambda () (interactive) (copy-region-as-kill (point-min)(point-max)) (message "file copied to paste-buffer")))
(define-key global-map "\C-a" '(lambda () (interactive) (beginning-of-line)))
(define-key global-map "\C-D" 'dl)
;(define-key global-map "\C-{" 'brace4it)
(define-key global-map "\C-\M-t"  'insert-time-stamp)
(define-key global-map "\C-c\C-c"  '(lambda () (interactive)(copy-region-as-kill (point-min)(point-max))))
(setq fill-prefix "   ")
(define-key global-map "\C-c>"  '(lambda () (interactive)(indent-region)))
