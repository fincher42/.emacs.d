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
								;    Last Updated:<time datetime='2017-11-17' pubdate> November 17, 2017</time>.
;; TODO:
;; separate into smaller files
;; move to .emacs-d
;; move to init.el
;; learn to autoload log.txt with html and flyspell modes
;; remove unneeded functions
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
(setq emacs-lisp-dir (concat emacs-dir "/lisp"))
(add-to-list 'load-path emacs-lisp-dir)
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

(recentf-mode 1)
(setq
delete-by-moving-to-trash t
recentf-max-saved-items 50
quick-redisplay 1
auto-save-default nil
wrap-long-lines t
backup-before-writing 1
backup-by-copying-when-linked 1
completion-auto-help t
inhibit-startup-message t
require-final-newline nil
truncate-partial-width-windows nil
truncate-lines nil
)

;; ===================== Load Extras =====================
(load "tabbar-master/tabbar.el")
(load "remotes.el")
(load "my-menus")
(load "flyspell-1.7q")
(load "define-keys.el")
(load "my-colors.el")
(load "html-utils.el")
(load "time-utils.el")
;(load "marketplace-log-mode")
;(require 'marketplace-log-mode)
;(load "zoo-log-mode")
(require 'sgml-mode)
(require 'json-snatcher) ;https://github.com/Sterlingg/json-snatcher
(require 'json-reformat) ;https://github.com/gongo/json-reformat
(require 'json-mode) ;https://github.com/joshwnj/json-mode;
 ;to use:  select all (c-x h) m-x  json-reformat-region


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

;(add-hook 'simple-html-mode-hook 'auto-fill-mode 0)
;(add-hook 'simple-html-mode-hook 'syntax-table-stuff)
;(add-hook 'simple-html-mode-hook 'font-lock-fontify-buffer)
;(add-hook 'c++-mode-hook '(setq wrap-long-lines nil))
(add-hook 'marketplace-mode-hook 'font-lock-fontify-buffer)

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

 
(my-set-colors)
(editlog)
(message "Let's rock! (Emacs version is %s)" emacs-version )



