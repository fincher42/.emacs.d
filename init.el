;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MITCH's FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; keyboard template for keybindings
; set-mark-command ; copy-region-as-kill ; clipboard-yank
; copy-rectangle-to-register yank-register
; ^C-p to print a file
;(setq debug-on-error t)
;(setq-default buffer-file-coding-system 'undecided-unix)
; C-x C-k n //names last keyboard macro only for session
;C-x C-k b //bind to key sequence
;M-x insert-kbd-macro <RET> macroname <RET> //inserts into current file, e.g., .emacs
; http://ergoemacs.org/emacs/keyboard_shortcuts_examples.html
; https://github.com/fincher42/Emacs.git
								;    Last Updated:<time datetime='2017-11-16' pubdate> November 16, 2017</time>.
; TODO:
;; replace (concat ) with find-file for ispell-dict
;; separate into smaller files
;; move to .emacs-d
;; move to init.el
;; learn to autoload log.txt with html and flyspell modes
;; ===================== Critical Startup Tasks =====================

(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows")
    (setq emacs-dir "~/.emacs.d")
    (defun set-frame-windows() (interactive)
       (set-frame-position (selected-frame) 965 0)
       (set-frame-size (selected-frame) 60 32)
    )
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
    ))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (message "Mac OS X")
   (setq emacs-dir "~/Dropbox/Emacs")
   (add-to-list 'exec-path "/usr/local/bin")

  (defun set-frame-windows() (interactive)
    (set-frame-position (selected-frame) 10 0)
    (set-frame-size (selected-frame) 155 38)
  )
   (global-set-key [s-up]  'beginning-of-buffer )
   (global-set-key [s-down]  'end-of-buffer )
   (global-set-key [s-l]  'editlog)
   (global-set-key [C-M-up] 'beginning-of-buffer)
   (global-set-key [C-M-down] 'end-of-buffer)
   (global-set-key [C-M-o] 'switch-to-other-buffer)
    ))
 )
(add-to-list 'load-path (concat emacs-dir "/lisp"))
(setq  home-dir-fincher "~")
(setq bookmark-default-file (concat emacs-dir "/.emacs.bmk"))

;; ===================== ispell =====================

(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary (concat emacs-dir "/.aspell.en.pws"))
(require 'ispell)
(autoload 'ispell "ispell" "Run ispell over buffer" t)
(autoload 'ispell-region "ispell" "Run ispell over region" t)
(autoload 'ispell-word "ispell" "Check word under cursor" t)
(setq-default ispell-program-name "aspell")

(dolist (hook '(text-mode-hook html-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1)))); add to text-mode
(dolist (hook '(change-log-mode-hook log-edit-mode-hook)); exclude these modes
  (add-hook hook (lambda () (flyspell-mode -1))))

;; ===================== Misc =====================
(set-frame-windows)
(setq debug-wait 0)
(setq visible-bell t)
(tool-bar-mode 0)
(setq bell-volume 0);; turn off that annoying bell

(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
(put 'eval-expression 'disabled nil)


;; ===================== Load Extras =====================
(load "tabbar-master/tabbar.el")
(load "remotes.el")
(load "my-menus")
(load "flyspell-1.7q")
;(load "marketplace-log-mode")
;(require 'marketplace-log-mode)
;(load "zoo-log-mode")
(require 'sgml-mode)
(require 'json-snatcher) ;https://github.com/Sterlingg/json-snatcher
(require 'json-reformat) ;https://github.com/gongo/json-reformat
(require 'json-mode) ;https://github.com/joshwnj/json-mode;
 ;to use:  select all (c-x h) m-x  json-reformat-region

;; ===================== The Mouse Key Family =====================
(setq mouse-drag-copy-region 't)
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

;////////////////////////// Key Board Macros ///////////////////////
;; converts "term" //definition to <dt>term</dt><dd>def</dd>
(fset 'dl
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 60 100 116 62 6 32 47 47 left right backspace backspace backspace 60 47 100 116 62 60 100 100 62 5 60 47 100 100 62 1 134217848 102 111 114 119 97 114 100 45 108 105 110 101 return] 0 "%d")) arg)))

(fset 'pre
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([60 112 114 101 62 return 60 47 112 114 101 62 return up 1 return up] 0 "%d")) arg)))

(fset 'brace4it
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([123 6 32 left 125] 0 "%d")) arg)))

;//////////////////////////Quickies//////////////////////////////////
(defun editemacs () (interactive) (find-file (concat emacs-dir "/init.el") ))
(defun reload-emacs-file () (interactive) (save-buffer)(load-file (concat emacs-dir "/init.el") ))
(defun find-today-in-log ()(interactive)(editlog)(beginning-of-buffer)(search-forward "<h3>")(forward-line 3))
(defun find-today-in-log-toappend ()(interactive)(editlog)(beginning-of-buffer)(search-forward "<h3>")(search-forward "<h3>")(forward-line -2))
(defun editlog () (interactive) (find-file (concat home-dir-fincher "/log.txt")))
(defun mark-and-copy-whole-buffer ()(interactive)(copy-region-as-kill (point-min)(point-max)))
(defun  switch-to-other-buffer()"switch to the second buffer" (interactive)(switch-to-buffer nil) )
(defun  switch-to-third-buffer()"switch to the second buffer" (interactive)(switch-to-buffer (car (list-buffers))) )

(defun insert-other-buffer-name ()(interactive) (insert (buffer-name(other-buffer))))
(defun insert-buffer-name ()(interactive) (insert (buffer-name)))
(defun insert-buffer-name-and-lineno ()(interactive) (insert (buffer-name))(insert ":")(insert (what-line)))
(defun kill-buffer-now () (interactive) (kill-buffer nil) )
(defun editaddr () (interactive) (find-file "~/.pers/addr") )
(defun kill-a-line () (interactive) "2 kills-lines" (beginning-of-line)(kill-line 1))

 (defun sort-words (reverse beg end)
      "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.
  
    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.
  
    See `sort-regexp-fields'."
      (interactive "*P\nr")
      (sort-regexp-fields reverse "\\w+" "\\&" beg end))


;////////////////////////////////////////////////////////////
;; Add the Last Updated: timestamp.
;    Last Updated:<time datetime='2017-11-16' pubdate> November 16, 2017</time>.
(defvar writestamp-date-format " %B %e, %Y" "*Format for displaying time")
(add-hook 'write-file-hooks 'update-writestamps)
(defun update-writestamps ()
"Finds the string \"Last Updated: (date).\" and replace them with the current time. The string must be the first nonwhitespace on the line and the period must be the last character on the line.  Written by Bob Glickstein."
(interactive "*")
(save-excursion  ;;will  restore original cursor location
  (save-restriction  ;; will restore (narrow) boundaries
    (save-match-data   ;; will restore item in search string
      (widen)(goto-char (point-min))
      (while (re-search-forward "^[ ;\t\"#]*Last Updated:\\(.*\\)\\."  nil t)
        (replace-match (format-time-string writestamp-date-format (current-time)) t t nil 1 )
        (insert "</time>")
        (beginning-of-line)
        (search-forward ":")
        (insert "<time datetime=\'")
        (insert (format-time-string "%Y-%m-%d" (current-time)))
        (insert "' pubdate>")
    ))))
nil)

;(add-hook 'simple-html-mode-hook 'auto-fill-mode 0)
;(add-hook 'simple-html-mode-hook 'syntax-table-stuff)
;(add-hook 'simple-html-mode-hook 'font-lock-fontify-buffer)
;(add-hook 'c++-mode-hook '(setq wrap-long-lines nil))
(add-hook 'marketplace-lod-mode-hook 'font-lock-fontify-buffer)

(defun downcase-tag ()(interactive)
"downcases HTML tags to make them more like xhtml wants. (<HTML> and </HTML> are downcased.  Bad Side Effects to be corrected:  Permanently changes case-fold-search and starts by downcasing the word at the point."
(setq case-fold-search nil)
(downcase-word 1)
(search-forward-regexp "<[/]*[A-Z]")
(forward-char -1)
(setq case-fold-search t)
)

(defun syntax-table-stuff ()
(interactive)
(modify-syntax-entry 31 "w")
(modify-syntax-entry 45 "w")
)

(defun taber () (interactive)
  (search-forward-regexp "	[0-2]")
  (backward-char 1)
   (message (format " current column is %d " (current-column) ))
   (if (< (current-column) 18)
       (insert "	"))
)

(defun display-file-info()
"trival function to show find-file-hooks functionality"
(message (concat "the filename is " buffer-file-name " and it is "
(if buffer-read-only "read only." "writable")))
)
(add-hook 'find-file-hooks 'display-file-info)

;; =================== functions =============================
(defun line-to-top()
"Puts the current line at the top of the window"
(interactive)
(recenter 0))


(defun kill-to-end ()
  "Kills text from point to end of buffer."
  (interactive)
  (kill-region (point) (point-max)))

(defun kill-to-beginning ()
  "Kills text from point to beginning of buffer."
  (interactive)
  (kill-region (point) (point-min)))


(defun delete-leading-whitespace ()
  (interactive)
  (narrow-to-region  (point) (mark))
    (goto-char (point-min))
    (replace-regexp "^[\t ]*" "")
 (widen)
)

(defun unify-region (top bottom &optional macro)
"removes all carriage returns in region"
  (interactive "r")
  (save-excursion
    (let ((end-marker (progn
			(goto-char bottom)
			(beginning-of-line)
			(point-marker)))
	  next-line-marker)
      (goto-char top)
      (if (not (bolp))
	  (forward-line 1))
      (setq next-line-marker (point-marker))
      (while (< next-line-marker end-marker)
	(goto-char next-line-marker)
	(save-excursion
	  (forward-line 1)
	  (set-marker next-line-marker (point)))
	(save-excursion
        ;; command goes here
	  (end-of-line)(delete-char 1)
         ))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil))
  )
)
(defun delete-blank-lines-in-region (top bottom &optional macro)
"removes all carriage returns in region"
  (interactive "r")
  (save-excursion
    (let ((end-marker (progn
			(goto-char bottom)
			(beginning-of-line)
			(point-marker)))
	  next-line-marker)
      (goto-char top)
      (if (not (bolp))
	  (forward-line 1))
      (setq next-line-marker (point-marker))
      (while (< next-line-marker end-marker)
	(goto-char next-line-marker)
	(save-excursion
	  (forward-line 1)
	  (set-marker next-line-marker (point)))
	(save-excursion
        ;; command goes here
	  (delete-blank-lines)
         ))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil))
  )
)


(defun comment-region (top bottom &optional macro)
"comments the hightlighted region by prefacing lines with correct characters"
  (interactive "r")
  (save-excursion
    (let ((end-marker (progn
			(goto-char bottom)
			(beginning-of-line)
			(point-marker)))
	  next-line-marker)
      (goto-char top)
      (if (not (bolp))
	  (forward-line 1))
      (setq next-line-marker (point-marker))
      (while (< next-line-marker end-marker)
	(goto-char next-line-marker)
	(save-excursion
	  (forward-line 1)
	  (set-marker next-line-marker (point)))
	(save-excursion
        ;; command goes here
        (beginning-of-line)(insert comment-start)
         ))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil))
  )
)

(defun indent-according-to-mode-region (top bottom &optional macro)
"name says it all"
  (interactive "r")
(message "indenting marked region...")
  (save-excursion
    (let ((end-marker (progn
			(goto-char bottom)
			(beginning-of-line)
			(point-marker)))
	  next-line-marker)
      (goto-char top)
      (if (not (bolp))
	  (forward-line 1))
      (setq next-line-marker (point-marker))
      (while (< next-line-marker end-marker)
	(goto-char next-line-marker)
	(save-excursion
	  (forward-line 1)
	  (set-marker next-line-marker (point)))
	(save-excursion
        ;; command goes here
        (indent-according-to-mode)
         ))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil))
  )
(message "indenting marked region...done")
)

(defun switch-to-third-buffer ()
(interactive)
;; needs to be fixed
(switch-to-other-buffer)
  )
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

(progn (message (concat "at .45"))(sit-for debug-wait))

(defun trim ()
  "Delete trailing whitespace everywhere after point"
;Tim Peters   Kendall Square Research Corp
;tim@ksr.com,         ksr!tim@uunet.uu.net
  (interactive)
  (replace-regexp "[ \t]+$" "")
)


(defun insert-numbers (startnumber endnumber)
"insert numbers into a file  "
(interactive "NStarting Number: \nNEnding Number: ")
(setq i startnumber)
(while (<= i endnumber)
  (insert (int-to-string i))
  (backward-char (length (int-to-string i)))
  (next-line 1)
  (setq i (1+ i))
  )
)

(defun my-c++-indent-defun ()
"puts message in message line when we start and finish"
(interactive)
(message "starting to reformat function...")
(c++-indent-defun)
(message "finished.")
)

(defun my-call-process (program infile &rest program-args)
  "See `call-process' documentation for meaning of `infile' argument."
  (let* ((b (generate-new-buffer " my-call-process"))
         (exit-code (apply 'call-process program infile b nil program-args))
         (output-string (save-excursion
                          (set-buffer b)
                          (buffer-string))))
    (kill-buffer b)
    (or (equal exit-code 0)
        (error "Running %S on %S lost: %S" program program-args exit-code))
   (setq oldfilename output-string)  ;; strip out ending character
   (setq filenamelength (length oldfilename))
   (setq newfilename (substring oldfilename 0 (- filenamelength 1)))
    newfilename)
)

(defun tagify-word()
 (interactive )
(forward-word -1)
(insert "<span class=tag>&lt;")
(forward-word 1)
(insert "&gt;</span>")
)


(defun search-for-word()
;; by mitch fincher
"searches for next occurance of the word at the point"
 (interactive )
(setq p0 (point))
(forward-word 1)
(setq p1 (point))
(setq looking-for (buffer-substring p0 p1))
(setq status (search-forward looking-for nil t))
(backward-char (length looking-for))
(if status
   (progn
   (message (format " searching for \"%s\" " looking-for))
   )
   (progn
   (message " search failed for \"%s\" , wrapping to beginning." looking-for)
   (beep)
   (goto-char 0)
   (search-forward looking-for)
   (backward-char (length looking-for))
   ))
)

(defun jump-back ()
;; by mitch fincher."
"Returns to cursors previous position"
  (interactive "*" )
(pop-mark)
(goto-char (mark t))
)


(defun insert-heading-info()
"inserts the file name and date at the top of a file."
(interactive)
(insert "                                           " )
(insert-date-stamp)
(insert "                                           " buffer-file-name)
(insert "\n\n")
)


(defun insert-file-name()
"inserts the file name and date at the top of a file."
(interactive)
(insert buffer-file-name)
)


(defun backup-current-file ()
  "backs up a file with date embedded. e.g., 'init.el' is copied to 'init.el-2010-07-16'"
  (interactive)
  (let ((backupfilename (concat  (buffer-file-name) "-" (format-time-string "%C%y-%m-%d-%H-%M" (current-time)))))
  (message (concat "copying file " (buffer-file-name) " to " backupfilename) )
  (copy-file (buffer-file-name) backupfilename)
  ))

(message (concat "at .5"))(sit-for debug-wait)

(defun insert-date-stamp ()
  "Insert current date at current position."
  (interactive "*")
  (message "starting to date stamp the line...")
    (beginning-of-line)
    (insert "<!------------------------------------------------->\n<h3>")
    (insert (format-time-string "%A %B %d, %C%y" (current-time)))
    (insert "</h3>\n<!------------------------------------------------->")
    (insert "\ntimetool:\n\n")
;    (if (equal "Monday" (format-time-string "%A" (current-time))) (insert "- timesheet\n"))
 ;   (insert "\n")
    (forward-char -1)
    (message "starting to date stamp the line - finished.")
)

(defun getQuarter ()
(interactive "*")
(setq year  (format-time-string "%C%y" (current-time)))
(setq month  (string-to-number (format-time-string "%m" (current-time))))
(setq quarter (cond 
              ((< month 4) "a")
              ((< month 7) "b")
              ((< month 10) "c")
              ((< month 13) "d")
              ))
(concat year quarter )
)

(defun insert-date-stamp-news ()
  "Insert current date at current position."
  (interactive "*")
  (message "starting to date stamp the line...")
    (beginning-of-line)
    (insert "<h3>")
    (insert (format-time-string "%A %B %d, %C%y" (current-time)))
    (insert "</h3>\n<p></p>\n")
    (forward-char -5)
    (message "starting to date stamp the line - finished.")
    )

(defun insert-date-stamp-minimal ()
  "Insert current date at current position."
  (interactive)
    (insert (format-time-string "//start end mdf %D" (current-time)))
  )

(defun insert-time-stamp ()
  "Insert current time at current position."
  (interactive)
  (message "starting to time stamp the line...")
    (insert (format-time-string "%D %I%p:%M" (current-time)))
    (insert " ")
  (message "starting to time stamp the line - finished.")
)

(defun my-indent-defun ()
"calls system indent-rigidly with correct args"
(interactive "*")
(message "starting to indent region..." )
(sit-for 0)
(indent-rigidly (mark) (point) 3)
(message "starting to indent region...done")
)


(defun save-buffer-and-collect()
"the name says it all"
 (interactive)
(save-buffer)
;(garbage-collect)
;;(store-savbufs)
)

(defun align-column (nth)
  "deletes all blanks infront of cursor and then moves down 1 line. -mdf"
(interactive "p")
  (while (looking-at " ")
     (delete-char 1))
  (fkey-next-line 1)
)
(defun scroll-to-end (nth)
  "Like end-of-buffer, but makes sure that the last non-blank line is
displayed on the last line of the window.  Leaves point at the bottom of
the window.  Sets mark to point's previous location \(just like end-of-buffer
does).  With a numeric arg scroll to the NTH from last line.  \(Negative
args do the right thing)"
  (interactive "p")
  ;(set-mark (point))
  (goto-char (1- (point-max)))
  ;; Walk backwards until we find non-whitespace
  (while (looking-at "\n\\|\\s ")
    (forward-char -1))
  ;; I know it's ugly, but it works.
  (if (> nth 1)
      (progn
	(vertical-motion (- 2 nth))
	(backward-char 2)
	(setq nth 1)
	))
  ;;  Now starting from point, count up the right number of screen lines.
  (let ((last-pos (1+ (point))))
    ;; Step up the appropriate number of lines (taking into account
    ;; continuation lines).
    (vertical-motion (- 3 (window-height) nth))
    ;; Wherever we landed should be the first char displayed in the window.
    (set-window-start (selected-window) (point))
    (goto-char last-pos)
    )
;(message "going to point max")
(sit-for 0)
    (goto-char (1- (point-max)))
    )

(defun killaword(click)
  (interactive "@e")
(message "kill-word")
(sit-for 1)
(kill-word 1)
)


(defun scroll-updown (event)
  "This allows the user to scroll up and down with the mouse in
test area.  This should be bound to a mouse click event type.
by Mitch Fincher, Dec 93"
  (interactive "e")
  (let (
	 ;(goto-char (posn-point (event-start event)))
	(p1 (posn-point (event-start event) ))
	(p2 (posn-point (event-end event) ))
       )
       (if (> p1 p2)
	(scroll-up (count-lines p1 p2 ))
	(scroll-down (count-lines p1 p2 ))
       )
  )
)

(defun scroll-updown-old (click)
  "This allows the user to scroll up and down with the mouse in
test area.  This should be bound to a mouse click event type.
by Mitch Fincher, Dec 93"
  (interactive "e")
  (let (
	 ;(goto-char (posn-point (event-start event)))
	(p1 (posn-point (event-start click) ))
	(p2 (posn-point (event-end click) ))
       )
       (if (> p1 p2)
	(scroll-up (count-lines p1 p2 ))
	(scroll-down (count-lines p1 p2 ))
       )
  )
)




(defun reload ()
  (interactive)
  (load-file (buffer-file-name)))

(setq
quick-redisplay 1
auto-save-default nil
wrap-long-lines t
backup-before-writing 1
backup-by-copying-when-linked 1
completion-auto-help t
inhibit-startup-message t
require-final-newline nil
)

;; save backups to c:\windows\temp\fincherm
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))

(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))




;;;  In Fundamental mode, auto fill as well.
;
(setq fundamental-mode-hook
      '(lambda (&rest args)
         (auto-fill-mode 0)
         (setq fill-column 72)))

(setq text-mode-hook
      '(lambda (&rest args)
         (auto-fill-mode 0)
         (setq fill-column 70)))

(setq default-major-mode 'text-mode)
(put 'set-fill-column 'disabled nil)

(defun mark-long-comment ()
  (interactive)
  (let ((at (point)))
    (beginning-of-line)
    (while(and (not (eobp))
	       (or  (looking-at comment-start)
		    ;(looking-at "[ 	]*\n")
		    ))
      (forward-line 1))
    (set-mark (point))
    (goto-char at)
    (while(and (not (bobp))
	       (or  (looking-at comment-start)
		    ;(looking-at "[ 	]*\n")
		    ))
      (forward-line -1))
    (or (bobp )(forward-line 1))))

(message "at .75")(sit-for debug-wait)

(defun fill-long-comment ()
  (interactive)
  (mark-long-comment)
  (let ((beg (min (dot) (mark)))
	(end (max (dot) (mark))) (n 0)m)
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (looking-at ";")
      (forward-char 1))
    (setq n (- (point) beg))
    (goto-char (point-min))
    (while (not (eobp))
      (setq m n)
      (while (> m  0)
	(cond ((looking-at ";")
	       (delete-char 1)
	       (cond ((looking-at " ")(delete-char 1)(setq m 0)))
	       (setq m (- m 1)))
	      (t (setq m 0))))
      (forward-line 1))
    (fill-region (dot-min) (dot-max))
    (goto-char (point-min))
    (while (not (eobp))
      (cond ((looking-at "\n")
	     nil)
	    (t(insert ";;; ")))
      (forward-line 1))
   (goto-char (point-min))
   (set-mark (point-max))
   (widen)))

(put 'eval-expression 'disabled nil)

(put 'backward-page 'disabled t)
(put 'forward-page 'disabled t)
(put 'mark-page 'disabled t)
;(put 'minibuffer-completion-help 'disabled t)
(message (concat "at .75")(sit-for debug-wait))

(setq auto-mode-alist
      '(("\\.text$" . indented-text-mode)
	("\\.zlog$" . zoo-log-mode)
	("\\.c$" . c-mode)
	("\\.h$" . c++-mode)
	("\\.y$" . c-mode)
        ("\\.cpp$" . c++-mode) ("\\.C$" . c++-mode)
	("\\.hxx$" . c++-mode) ("\\.cc$" . c++-mode)
	("\\.l$" . c-mode)
        ("\\.el$" . emacs-lisp-mode)
	("\\.lisp$" . emacs-lisp-mode)
	("\\.log$" . marketplace-log-mode)
	("\\.java$" . java-mode)
	("\\.cs$" . java-mode)
	("\\.mocha$" . java-mode)
	("\\.js$" . javascript-mode)
        ("\\.qqc$" . qqc-mode)
        ("\\.build$" . html-mode)
        ("\\.include$" . html-mode)
        ("\\.xsl$" . html-mode)
        ("\\.xslt$" . html-mode)
        ("\\.shtml$" . html-mode)
        ("log\\.txt$" . html-mode)
        ("\\.php$" . html-mode)
        ("\\.html$" . html-mode)
        ("\\.stm$" . html-mode)
        ("\\.asp$" . html-mode)
        ("\\.htm$" . html-mode)
        ("\\.dtd$" . html-mode)
        ("\\.config$" . html-mode)
        ("\\.xml$" . html-mode)
        ("\\.sql$" . my-sql-mode)
	("\\.tk$" .  tcl-mode)
	("addr$" .  addr-mode)
	("\\.rb$" .  ruby-mode)
	("css$" .  css-mode)
	("[Mm]akefile$" .  makefile-mode)
	("\\.mak$" .  makefile-mode)
	("\\.mk$" .  makefile-mode)
	("\\.pl$" .  perl-mode)
	("\\.json$" .  json-mode)
	("\\.pm$" .  perl-mode)
	("\\.cgi$" .  perl-mode)
	("\\.tcl$" .  tcl-mode)
	("\\.ged$" .  gedcom-mode)
))

(setq truncate-partial-width-windows nil)
(setq truncate-lines nil)

(defun switch-to-existing-buffer (bufname)
  (interactive "BSwitch to buffer: ")
  (let ((b (get-buffer bufname)))
    (if b
	(switch-to-buffer bufname)
      (error "Buffer %s does not exist." bufname))))


(defun me-look-at (s)
  (interactive "sRegexp:")
  (print (buffer-substring (point) (point-max)))
  (if (looking-at s)
      (progn (print (concat s " Matched."))
	     t)
    (progn (print (concat s " Not Matched."))
	   nil
	   )))

;;;;;;;; Printing ;;;;;;;;;;;;
;(setq enscript-switches (list (getenv "ENSCRIPT")))
(setq lpr-switches (list (format "-P%s" (getenv "PRINTER"))))
;(setq lpr-switches (list "-P'iR-ADV C5051-B1'"))
;(setq enscript-switches (list "-2Gr"))
(setq lpr-command "print")
(autoload 'shell "shell" "" t nil)
;; This should allow me to print the buffer, but it is not working.
;; check out
;; http://www.cs.washington.edu/homes/voelker/ntemacs/contrib/printing2.txt
;; for the source
(require 'ps-print)
;(message "debugger message 1" ) (sit-for 1)
(setq ps-paper-type 'letter)
(setq ps-lpr-command "print")
;(setq printer-name "//koausd00049/MyPrinter")
;(setq ps-lpr-switches '("/d:\\\\\\\\DS008\\\\MBIQHP5Sin5")) ; the printer name
;(setq ps-lpr-switches '("/d:\\\\\\\\Mbausprn001\\\\KTAUSPC5051")) ; the printer name
;(setq ps-lpr-switches '("/d:'\\\\\\\\Mbausprn001\\\\iR-ADV C5051-B1'")) ; the printer name
(setq ps-lpr-buffer "c:\\\\temp\\\\psspool.ps")       ; a tmp spool file
(setq ps-line-number t)
(setq ps-landscape-mode t)
(setq ps-font-size  '10)

(defun nt-ps-print-buffer-with-faces ()
  (interactive)
  (ps-print-buffer-with-faces ps-lpr-buffer)
  (shell-command
   (apply 'concat (append (list ps-lpr-command " ")
			  ps-lpr-switches
			  (list " " ps-lpr-buffer))))
)

(define-key global-map "\C-cp" 'nt-ps-print-buffer-with-faces)


(fset 'revert-buffer-now
   "Å¯revert-bufferyes")
(fset 'kill-buffer-please
   "Å¯kill-buffer")
(fset 'insert-tab-please
   " quoted-insert	")

(setq buffers-menu-max-size 35)  ; maximum buffers listed on menu
(setq buffers-menu-max-width 30)
(setq line-number-mode t)

(transient-mark-mode 1)
(setq Manual-query-multiple-pages t)
(setq blink-matching-paren t)
(add-hook 'nntp-server-opened-hook 'nntp-send-authinfo)

(put 'erase-buffer 'disabled nil)
(setq delete-selection-mode t)
(delete-selection-mode t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq completion-ignored-extensions '(".o" ".elc" "~" ".h" ".bin" ".bak" ".obj" ".map" ".a" ".ln" ".class" ".exe"))

;; modeline stuff
(setq
column-number-mode t
line-number-mode t
buffers-menu-sort-function nil
 frame-title-format "TheOneTrueEditor: \"%f\""
)
(set-face-background 'mode-line "plum")
(set-face-foreground 'mode-line "black")

(modify-syntax-entry 31 "w") ; specifies "_" char 31 to be a word
(modify-syntax-entry 45 "w") ; specifies "-" char 45 to be a word
(display-time)

;(add-hook 'diary-hook 'appt-make-list)
(message "at .85")(sit-for debug-wait)

(defun copy-filename-to-kill-buffer ()
(interactive)
(kill-new (buffer-file-name))
)

(defun toggle-slashes (start end)
"toggles the slashes between unix and dos in the region.
It changes all / to \ and all \ to / -mdf"
(interactive "*r")
(save-excursion
(goto-char (mark))
(while ( < (point) end)
  (if (or (looking-at "/") (looking-at "\\\\"))
      (progn (if (looking-at "/")(insert "\\"))
	     (if (looking-at "\\\\")(insert "/"))
    (delete-char 1))
    )
  (forward-char 1)
  ) ;; while
;(set-mark )
)
)
;;  aa\bb\cc\dd

(setq mouse-scroll-delay .25)

(setq
search-highlight t
query-replace-highlight t
track-eol t
suggest-key-bindings nil
)
(setq tab-width 5)
(setq yank-menu-length 80)
(show-paren-mode 1)  ; highlight matching parentheses
(which-func-mode 1)  ; turn on the which function mode

(setq bookmark-save-flag 1)
(put 'narrow-to-region 'disabled nil)

(defun my-replace-string (old new)
    (goto-char (point-min))
    (replace-string old new)
    (goto-char (point-min))
)
;; Use BASH shell instead of DOS shell
(setq binary-process-input t)
(setq w32-quote-process-args ?\")
(setq shell-file-name "bash.exe")
;; or sh if you rename your bash executable to sh.
;C:\Program Files\Git\bin\bash.exe
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)
(setq explicit-sh-args '("-login" "-i"))
;(setq w32-quote-process-args ?\"   ;; use Cygnus quoting rules.
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
 (defun format-branch-name ()
" replaces spaces and : with _ to make branch name"
  (interactive "*")
(setq badchars '(" " ":" "*" "," "."  "/" "__" "_-_" "\"" "\'"))
  (narrow-to-region  (point) (mark))
  (mapcar (lambda (keyword) 
    (goto-char (point-min))
    (replace-string keyword "_" )
) badchars)
(widen)
;(set-mark-command (beginning-of-line) (end-of-line))
(message "format-branch-name done.")
)

(defun upcase-sql ()
" upcases sql keywords in selected region"
  (interactive "*")
(setq sqlkeywords '("add " "all " "alter " "and " "any " "as " "asc " "authorization " "backup " "begin " "between " "break " "browse " "bulk " "by " "cascade " "case " "check " "checkpoint " "close " "clustered " "coalesce " "collate " "column " "commit " "compute " "constraint " "contains " "containstable " "continue " "convert " "create " "cross " "current " "current_date " "current_time " "current_timestamp " "current_user " "cursor " "database " "dbcc " "deallocate " "declare " "default " "delete " "deny " "desc " "disk " "distinct " "distributed " "double " "drop " "dummy " "dump " "else " "end " "errlvl " "escape " "except " "exec " "execute " "exists " "exit " "fetch " "file " "fillfactor " "for " "foreign " "freetext " "freetexttable " "from " "full " "function " "goto " "grant " "group " "having " "holdlock " "identity " "identitycol " "identity_insert " "if " "in " "index " "inner " "insert " "intersect " "into " "is " "join " "key " "kill " "left " "like " "lineno " "load " "national " "nocheck " "nonclustered " "not " "null " "nullif " "of " "off " "offsets " "on " "open " "opendatasource " "openquery " "openrowset " "openxml " "option " "or " "order " "outer " "over " "percent " "plan " "precision " "primary " "print " "proc " "procedure " "public " "raiserror " "read " "readtext " "reconfigure " "references " "replication " "restore " "restrict " "return " "revoke " "right " "rollback " "rowcount " "rowguidcol " "rule " "save " "schema " "select " "session_user " "set " "setuser " "shutdown " "some " "statistics " "system_user " "table " "textsize " "then " "to " "top " "tran " "transaction " "trigger " "truncate " "tsequal " "union " "unique " "update " "updatetext " "use " "user " "values " "varying " "view " "waitfor " "when " "where " "while " "with " "writetext " "min(" "max(" "real"))
  (narrow-to-region  (point) (mark))
(mapcar (lambda (keyword) 
    (goto-char (point-min))
    (replace-string keyword (upcase keyword) )
    (message keyword)(sit-for 0 2)
) sqlkeywords)
 (widen)
(message "sqlupcase complete.")
)
(message "at .95")(sit-for debug-wait)

 (defun insert-list ()
" inserts html list into selected region"
  (interactive "*")
  (narrow-to-region  (point) (mark))
    (goto-char (point-min))
    (insert "<ol>\n     <li>")

     (while (<  (+ 2 (point)) (point-max))
         (forward-line)(insert "</li><li>")
     )
    (goto-char (point-max))
  (insert "</li></ol>")
 (widen)
(message "list complete.")
)

(defun simple-convert-html-angles ()
" replaces all & < and > to &amp;, &lt; and &;gt; in the region"
  (interactive "*")
  (narrow-to-region  (point) (mark))
    (goto-char (point-min))    (replace-string "&" "&amp;")
    (goto-char (point-min))    (replace-string "<" "&lt;")
    (goto-char (point-min))    (replace-string ">" "&gt;")
 (widen)
)
(defun simple-unconvert-html-angles ()
" replaces all &amp;, &lt; and &gt; to & < and > in the region"
  (interactive "*")
  (narrow-to-region  (point) (mark))
    (goto-char (point-min))    (replace-string "&amp;" "&" )
    (goto-char (point-min))    (replace-string "&lt;" "<" )
    (goto-char (point-min))    (replace-string "&gt;" ">" )
 (widen)
)

(my-set-colors)
(editlog)
(recentf-mode 1)
(setq recentf-max-saved-items 50)
(setq delete-by-moving-to-trash t)
(message "Let's rock! (Emacs version is %s)" emacs-version )



