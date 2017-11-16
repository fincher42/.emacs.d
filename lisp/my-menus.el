(defun add-findfile-menu-item (mymenu-bar mytempdir mytempfile myi)
(interactive)
(define-key mymenu-bar (vector myi) `(,mytempfile .  (lambda () (interactive) (find-file ,(concat mytempdir mytempfile)))))
)
(defun add-files-menu-item (mymenu-bar mytempdir mytemplist)
  (interactive)
  (setq i 0)
  (while mytemplist
    (setq mytempfile (car mytemplist))
    (add-findfile-menu-item mymenu-bar mytempdir mytempfile i)
    (setq mytemplist (cdr mytemplist))
    (setq i (1+ i))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;; Edit Files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar menu-bar-myfiles-menu (make-sparse-keymap "MyFiles"))
(define-key global-map [menu-bar myfiles] (cons "Myfiles" menu-bar-myfiles-menu))
(define-key menu-bar-myfiles-menu [mymenus] '("mymenus.el" .  (lambda () (interactive) (find-file "~/emacs/mymenus.el"))))
(define-key menu-bar-myfiles-menu [MarketPlaceLog] '("MarketPlaceLog" .  (lambda () (interactive) (find-file "c:/Git/gm.marketing.marketplace/MarketPlace.Web/Gm.Dm.Marketplace.WebUI/logs/"))))
(define-key menu-bar-myfiles-menu [MarketPlaceLogActual] '("MarketPlaceLog Today" .
    (lambda () (interactive)
      (setq marketplace-log-name (format-time-string "c:/Git/gm.marketing.marketplace/MarketPlace.Web/Gm.Dm.Marketplace.WebUI/logs/MarketPlaceLog%Y-%m-%d.log" (current-time)))
      (find-file marketplace-log-name)
      )))

(define-key menu-bar-myfiles-menu [simple-html-mode] '("simple-html-mode.el" .  (lambda () (interactive) (find-file "~/emacs/simple-html-mode.el"))))

(define-key menu-bar-myfiles-menu [separator-mitch2]  '("--"))
(define-key menu-bar-myfiles-menu [tree] '("inetpub/Index.html" .  (lambda () (interactive) (find-file "c:/inetpub/wwwroot/Index.html"))))
(define-key menu-bar-myfiles-menu [tree2] '("AutoHotkey" .  (lambda () (interactive) (find-file "c:/Users/jzf39y/bin/AutoHotkey.ahk"))))

(define-key menu-bar-myfiles-menu [separator-SiteDocumentation]  '("--"))
(define-key menu-bar-myfiles-menu [gitconfig] '(".gitconfig" .  (lambda () (interactive) (find-file "C:/Users/jzf39y/.gitconfig"))))

(define-key menu-bar-myfiles-menu [user-aliases.cmd] '("user-aliases.cmd" .  (lambda () (interactive) (find-file "C:/Users/jzf39y/Documents/cmder/config/user-aliases.cmd"))))

(define-key menu-bar-myfiles-menu [zipcodes] '("zip codes" .  (lambda () (interactive) (find-file "c:/Users/jzf39y/Documents/Visual Studio 2017/Projects/ZipRepository/ZipRepository/ValidUsZipCodes.txt"))))

;;;;;;;;;;;;;;;;;;;;;;;;; Mitch Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar menu-bar-mitchs-menu (make-sparse-keymap "Mitchs"))


(define-key menu-bar-mitchs-menu [separator-mitch9]  '("--"))





(define-key menu-bar-mitchs-menu [set-frame-windows] '("set-frame-windows" .  (lambda () (interactive) (set-frame-windows))))

(define-key menu-bar-mitchs-menu [simple-html-mode] '("simple-html-mode" .  (lambda () (interactive) (simple-html-mode))))

(define-key menu-bar-mitchs-menu [reload-emacs] '("reload-emacs" .  (lambda () (interactive) (reload-emacs-file))))

(define-key menu-bar-mitchs-menu [add-inverse-global] '("add-inverse-global" . (lambda () (interactive) (inverse-add-global-abbrev))))

(define-key menu-bar-mitchs-menu [show-colors] '("list-colors-display" .  (lambda () (interactive) (list-colors-display))))

(define-key global-map [menu-bar mitchs] (cons "Mitchs" menu-bar-mitchs-menu))

(define-key menu-bar-mitchs-menu [update-writestamps] '("update-writestamps" .  update-writestamps))

(define-key menu-bar-mitchs-menu [convert-html] '("simple-convert-html-angles" . simple-convert-html-angles))
(define-key menu-bar-mitchs-menu [insert-anchor] '("Insert Anchor Href" . (lambda () (interactive)(insert "<a href=\"")(clipboard-yank)(insert "\">")(clipboard-yank)(insert "</a>"))))

  (define-key menu-bar-mitchs-menu [insert-codeblock] '("Insert Codeblock" . (lambda () (interactive)(insert "</li><li><p></p>\n<div class=\"codeblock\"><pre class='sh_javascript'>\n\n</pre></div>") (forward-char -13)(set-mark (point)) (clipboard-yank) (simple-convert-html-angles))))
(define-key menu-bar-mitchs-menu [separator-mitch58]  '("--"))
(define-key menu-bar-mitchs-menu [read-savbufs] '("restore list of files" . read-savbufs))
(define-key menu-bar-mitchs-menu [write-savbufs] '("save list of files" . write-savbufs))
(define-key menu-bar-mitchs-menu [savbufs] '("edit .savbufs" .  (lambda () (interactive) (find-file "~/.savbufs"))))
(define-key menu-bar-mitchs-menu [separator-mitch5]  '("--"))
(define-key menu-bar-mitchs-menu [fontify] '("fontify" . font-lock-fontify-buffer))
(define-key menu-bar-mitchs-menu [whitespace] '("delete-leading-whitespace" . delete-leading-whitespace))


(define-key menu-bar-mitchs-menu [indent] '("indent-according-to-mode" . indent-according-to-mode-region))
(define-key menu-bar-mitchs-menu [sort] '("sort-lines" . sort-lines))
(define-key menu-bar-mitchs-menu [sort--]  '("--"))
(define-key menu-bar-mitchs-menu [copy-whole-buffer] '("copy-whole-buffer" .  (lambda () (interactive) (copy-region-as-kill (point-min)(point-max)))))
(define-key menu-bar-mitchs-menu [copy-filename-to-kill-buffer] '("copy-filename-to-kill-buffer" . copy-filename-to-kill-buffer))
(define-key menu-bar-mitchs-menu [downcase-region] '("downcase-region" . downcase-region))
(define-key menu-bar-mitchs-menu [unify-region] '("unify-region" . unify-region))



(define-key menu-bar-mitchs-menu [separator-mitch8]  '("--"))
(define-key menu-bar-mitchs-menu [toggle-slashes] '("toggle-slashes" .  (lambda () (interactive) (toggle-slashes (mark) (point) ))))
(define-key menu-bar-mitchs-menu [format-branch-name] '("format-branch-name" .  (lambda () (interactive) (format-branch-name))))

(define-key menu-bar-mitchs-menu [separator-mitch7]  '("--"))
(define-key menu-bar-mitchs-menu [set-colors-dark] '("set-colors-dark" .  (lambda () (interactive) (set-colors-dark))))
(define-key menu-bar-mitchs-menu [set-colors-light] '("set-colors-light" .  (lambda () (interactive) (set-colors-light))))
(define-key menu-bar-mitchs-menu [toggle-colors] '("toggle-colors" .  (lambda () (interactive) (toggle-colors))))
(define-key menu-bar-mitchs-menu [separator-mitch6]  '("--"))
