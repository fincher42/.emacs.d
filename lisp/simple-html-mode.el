;; simple-html-mode.el --- simple-html code editing commands for GNU Emacs
;; written by Mitch Fincher, 1998
;;
;; font-lock commands adapted from: Ulrik Dickow <dickow@nbi.dk> 
;; (http://www.nbi.dk/~dickow) implementation of font-html.el

(defvar simple-html-font-lock-keywords
  (let ((tword "\\(h1\\|title\\)")          ; Titles, like function defs
	(bword "\\(b\\|h[2-4]\\|strong\\)") ; Names of tags to boldify
	(iword "\\(address\\|cite\\|em\\|i\\|var\\)") ; ... to italify
	;; Regexp to match shortest sequence that surely isn't a bold end.
	;; We simplify a bit by extending "</strong>" to "</str.*".
	;; Do similarly for non-italic and non-title ends.
	(not-bend (concat "\\([^<]\\|<\\([^/]\\|/\\([^bhs]\\|"
			  "b[^>]\\|"
			  "h\\([^2-4]\\|[2-4][^>]\\)\\|"
			  "s\\([^t]\\|t[^r]\\)\\)\\)\\)"))
	(not-iend (concat "\\([^<]\\|<\\([^/]\\|/\\([^aceiv]\\|"
			  "a\\([^d]\\|d[^d]\\)\\|"
			  "c\\([^i]\\|i[^t]\\)\\|"
			  "e\\([^m]\\|m[^>]\\)\\|"
			  "i[^>]\\|"
			  "v\\([^a]\\|a[^r]\\)\\)\\)\\)"))
	(not-tend (concat "\\([^<]\\|<\\([^/]\\|/\\([^ht]\\|"
			  "h[^1]\\|t\\([^i]\\|i[^t]\\)\\)\\)\\)")))
    (list ; Avoid use of `keep', since XEmacs will treat it the same as `t'.
     ;; First fontify the text of a HREF anchor.  It may be overridden later.
     ;; Anchors in headings will be made bold, for instance.
     '("<a\\s-+href[^>]*>\\([^>]+\\)</a>"
       1 font-lock-reference-face t)
     ;; Tag pairs like <b>...</b> etc.
     ;; Cunning repeated fontification to handle common cases of overlap.
     ;; Bold complex --- possibly with arbitrary other non-bold stuff inside.
     (list (concat "<" bword ">\\(" not-bend "*\\)</\\1>")
	   2 'font-lock-type-face t)
     ;; Italic complex --- possibly with arbitrary non-italic kept inside.
     (list (concat "<" iword ">\\(" not-iend "*\\)</\\1>")
	   2 'font-lock-type-face t)
     ;; Bold simple --- first fontify bold regions with no tags inside.
     (list (concat "<" bword ">\\("  "[^<]"  "*\\)</\\1>")
	   2 'font-lock-type-face t)
     ;; Any tag, general rule, just after bold/italic stuff.
     '("\\(<[^>]*>\\)" 1 font-lock-type-face t)
     ;; Titles and level 1 headings (anchors do sometimes appear in h1's)
     (list (concat "<" tword ">\\(" not-tend "*\\)</\\1>")
	   2 'font-lock-function-name-face t)
     ;; Underline is rarely used. Only handle it when no tags inside.
     ;;'("<u>\\([^<]*\\)</u>" 1 simple-html-underline-face t)
     ;; Forms, anchors & images (also fontify strings inside)
     '("\\(<\\(form\\|i\\(mg\\|nput\\)\\)\\>[^>]*>\\)"
       1 font-lock-variable-name-face t)
     '("</a>" 0 font-lock-keyword-face t)
     '("\\(<a\\b[^>]*>\\)" 1 font-lock-keyword-face t)
     '("=[ \t\n]*\\(\"[^\"]+\"\\)" 1 font-lock-string-face t)
     ;; Large-scale structure keywords (like "program" in Fortran).
     ;;   "<html>" "</html>" "<body>" "</body>" "<head>" "</head>" "</form>"
     '("</?\\(body\\|form\\|h\\(ead\\|tml\\)\\)>"
       0 font-lock-variable-name-face t)
     ;; HTML special characters
     '("&[^;\n]*;" 0 font-lock-string-face t)
     ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside.
     '("\\(<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>\\)"
       1 font-lock-comment-face t)
     ;; Comments: <!-- ... -->. They traditionally override anything else.
     ;; It's complicated 'cause we won't allow "-->" inside a comment, and
     ;; font-lock colours the *longest* possible match of the regexp.
     '("\\(<!--\\([^-]\\|-[^-]\\|--[^>]\\)*-->\\)"
       1 font-lock-comment-face t)))
    "Additional expressions to highlight in HTML helper mode.")

     (set-face-foreground 'font-lock-comment-face        "Red")
;;     (set-face-foreground 'font-lock-function-name-face  "Blue")
;;     (set-face-foreground 'font-lock-keyword-face	"Gold")
;;     (set-face-foreground 'font-lock-reference-face      "Sienna")
;;     (set-face-foreground 'font-lock-string-face         "SeaGreen")
;;     (set-face-foreground 'font-lock-type-face           "Coral")
;;     (set-face-foreground font-lock-variable-name-face   "DarkGoldenrod") 

(defun simple-html-insert-list-withpara (numitems)
  "inserts a template for lists in HTML"
  (interactive "*NHow many list items:")
  (beginning-of-line)
  (insert "<ol>\n")
  (setq i 0)
  (insert "        <li>\n")
  (while (< i (- numitems 1))
    (insert "   </li><li>\n     <p></p>\n")
    (setq i (1+ i))
    )
  (insert "   </li>\n</ol>\n")
)

(defun simple-html-insert-list (numitems)
  "inserts a template for lists in HTML"
  (interactive "*NHow many list items:")
  (beginning-of-line)
  (insert "<ol>\n")
  (setq i 0)
  (insert "        <li>\n")
  (while (< i (- numitems 1))
    (insert "   </li><li>\n")
    (setq i (1+ i))
    )
  (insert "   </li>\n</ol>\n")
)

(defun simple-html-insert-list-item ()
  "inserts a single list item in HTML"
  (interactive)
  (insert "</li><li>\n")
  (forward-char -1)
)

(defun simple-html-insert-para-tags ()
  "inserts a single list item in HTML"
  (interactive)
  (insert "<p></p>\n")
  (forward-char -5)
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



;;;;;;;;;;;;;;;;;;;;;;;;;; C#  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun simple-convert-code-wrapper ()
"highlights syntax in HTML using CSS span tags"
  (interactive "*")
  (setq mytext (buffer-substring (point) (mark)))
  (setq mytext (simple-convert-code mytext))
  (kill-region (point) (mark))
  (insert mytext)
)
(defun replace-words (mywords myclass text) 
"surrounds keywords with span tags for css"
(dolist(word mywords text) 
    (setq text (replace-regexp-in-string (concat "\\(" word "\\)\\([\( \);\[]\\)") (concat "<span class='" myclass "'>\\1</span>\\2") text)))
)
(defun simple-convert-code (mytext)
" replaces all & < and > to &amp;, &lt; and &gt; in the text and classifies code"
  (interactive "*")
    (setq mytext (replace-regexp-in-string "&" "&amp;" mytext))
    (setq mytext (replace-regexp-in-string "<" "&lt;" mytext))
    (setq mytext (replace-regexp-in-string ">" "&gt;" mytext))
    (setq mytext (replace-regexp-in-string "\\(\".*\"\\)" "<span class='codetext'>\\1</span>" mytext)) ;;strings
    
    (setq typewords (list "bool" "byte" "char" "decimal" "double" "enum" "fixed" "float" "int" "long" "object" "sbyte" "short" "string" "uint" "ulong" "ushort" "void"))
    (setq mytext (replace-words typewords "codetype" mytext))

    (setq keywords '("abstract" "as " "base" "break" "case" "catch" "checked" "class" "const" "continue" "default" "delegate" "do" "else" "event" "explicit" "extern" "finally" "for" "foreach" "goto" "if" "implicit" "in " "interface" "internal" "is" "lock" "namespace" "new" "operator" "out" "override" "params" "private" "protected" "public" "readonly" "ref" "return" "sealed" "sizeof" "stackalloc" "static" "struct" "switch" "this" "throw" "try" "typeof" "unchecked" "unsafe" "using" "var" "virtual" "volatile" "while"))
    (setq mytext (replace-words keywords "codekeyword" mytext))
    (setq mytext (replace-words '("true" "false" "null") "codekeyword" mytext))

(setq mytext (replace-regexp-in-string "\\(//.*\\)" "<span class='codecomment'>\\1</span>" mytext)) ;;comments
(setq mytext (replace-regexp-in-string "\\([0-9][0-9\.]+\\)" "<span class='codetext'>\\1</span>" mytext)) ;;numbers
(setq mytext (replace-regexp-in-string "\\([0-9]+[\,]+\\)" "<span class='codetext'>\\1</span>" mytext)) ;;numbers
)
;(simple-convert-code "public true x=1; int i=0; Main(")

(defun test-simple-convert-code-wrapper ()
(setq tmp (simple-convert-code "if(true) b = false;"))
(setq tmp (simple-convert-code " catch(e) if(true) b = false;"))
(message tmp)
)
;;;;;;;;;;;;;;;;;;;;;;;;;; end of C#  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;; Sql   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun simple-convert-Sql-code-wrapper ()
"highlights syntax in HTML using CSS span tags"
  (interactive "*")
  (setq mytext (buffer-substring (point) (mark)))
  (setq mytext (simple-sql-convert-code mytext))
  (kill-region (point) (mark))
  (insert mytext)
)
(defun simple-sql-convert-code (mytext)
" replaces all & < and > to &amp;, &lt; and &gt; in the text and classifies code"
  (interactive "*")
    (setq mytext (replace-regexp-in-string "&" "&amp;" mytext))
    (setq mytext (replace-regexp-in-string "<" "&lt;" mytext))
    (setq mytext (replace-regexp-in-string ">" "&gt;" mytext))
    (setq mytext (replace-regexp-in-string "\\(\".*\"\\)" "<span class='codetext'>\\1</span>" mytext)) ;;strings
    
    (setq typewords (list "bool" "byte" "char" "decimal" "double" "enum" "fixed" "float" "int" "long" "object" "sbyte" "short" "string" "uint" "ulong" "ushort" "void"))
    (setq mytext (replace-words typewords "codetype" mytext))

    (setq keywords '("add " "all " "alter " "and " "any " "as " "asc " "authorization " "backup " "begin " "between " "break " "browse " "bulk " "by " "cascade " "case " "check " "checkpoint " "close " "clustered " "coalesce " "collate " "column " "commit " "compute " "constraint " "contains " "containstable " "continue " "convert " "create " "cross " "current " "current_date " "current_time " "current_timestamp " "current_user " "cursor " "database " "dbcc " "deallocate " "declare " "default " "delete " "deny " "desc " "disk " "distinct " "distributed " "double " "drop " "dummy " "dump " "else " "end " "errlvl " "escape " "except " "exec " "execute " "exists " "exit " "fetch " "file " "fillfactor " "for " "foreign " "freetext " "freetexttable " "from " "full " "function " "goto " "grant " "group " "having " "holdlock " "identity " "identitycol " "identity_insert " "if " "in " "index " "inner " "insert " "intersect " "into " "is " "join " "key " "kill " "left " "like " "lineno " "load " "national " "nocheck " "nonclustered " "not " "null " "nullif " "of " "off " "offsets " "on " "open " "opendatasource " "openquery " "openrowset " "openxml " "option " "or " "order " "outer " "over " "percent " "plan " "precision " "primary " "print " "proc " "procedure " "public " "raiserror " "read " "readtext " "reconfigure " "references " "replication " "restore " "restrict " "return " "revoke " "right " "rollback " "rowcount " "rowguidcol " "rule " "save " "schema " "select " "session_user " "set " "setuser " "shutdown " "some " "statistics " "system_user " "table " "textsize " "then " "to " "top " "tran " "transaction " "trigger " "truncate " "tsequal " "union " "unique " "update " "updatetext " "use " "user " "values " "varying " "view " "waitfor " "when " "where " "while " "with " "writetext " "min(" "max(" "real"))
    (setq mytext (replace-words keywords "codekeyword" mytext))
    (setq mytext (replace-words '("true" "false" "null") "codekeyword" mytext))

(setq mytext (replace-regexp-in-string "\\(//.*\\)" "<span class='codecomment'>\\1</span>" mytext)) ;;comments
(setq mytext (replace-regexp-in-string "\\([0-9][0-9\.]+\\)" "<span class='codetext'>\\1</span>" mytext)) ;;numbers
(setq mytext (replace-regexp-in-string "\\([0-9]+[\,]+\\)" "<span class='codetext'>\\1</span>" mytext)) ;;numbers
)
;(simple-convert-code "public true x=1; int i=0; Main(")

(defun test-simple-sql-convert-code-wrapper ()
(setq tmp (simple-sql-convert-code "select * from traffic_data"))
(message tmp)
(setq tmp (simple-convert-code " catch(e) if(true) b = false;"))
(message tmp)
)
;;;;;;;;;;;;;;;;;;;;;;;;;; end of Sql  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun convert-html-angles (mytext)
" replaces all & < and > to &amp;, &lt; and &gt; in the text"
  (interactive "*")
    (setq mytext (replace-regexp-in-string "&" "&amp;" mytext))
    (setq mytext (replace-regexp-in-string "<" "&lt;" mytext))
    (setq mytext (replace-regexp-in-string ">" "&gt;" mytext))
)
;(convert-html-angles "<h1>A & M </h1>")
;(reduce '+ '(1 2 3 4 5))
;(+ 1 (+ 2 (+ 3 (+ 4 (+ 5)))))
;(reduce 'expt '(2 3 4 5))

  
(defun simple-html-insert-html-template ()
"inserts a basic html template for an HTML document"
(interactive "*")
(beginning-of-line)
(insert "<html>\n<head>\n<title></title>\n</head>\n<body>\n\n\n</body>\n</html>\n")
(message "inserting a html-template")
)

(defun simple-html-insert-br ()
"inserts a br"
(interactive "*")
(insert "<br />")
(message "inserting a <br />")
)

(defun simple-html-insert-table (nrows ncols)
"inserts a table template -mdf"
(interactive "nNumber of Rows: \nnNumber of Columns: ")
(beginning-of-line)
(insert "<table>\n")
(setq i -1);; -1 instead of 0, since the header row takes up one loop
(while (< i nrows) ;; loop over the rows
  (insert "<tr>")

  (setq j 0) 
  (while (< j ncols) ;; loop over the cols
    (if (eq i -1)
	(insert "<th> </th>")
      (insert "<td> </td>")
      )
    (setq j (1+ j))
    )
  (insert "</tr>\n")
  (setq i (1+ i))
  )
(insert "</table>\n")
(forward-line (- (+ nrows 3)))
(message "%d rows, %d number of columns" nrows ncols)
)


(defun simple-html-wrap-codeblock-tags ()
"inserts a codeblock tag -mdf"
(interactive)
;;(simple-convert-html-angles)
(insert (concat "</pre></div>"))
(goto-char (mark))
(insert (concat "<div class='codeblock'><pre class='sh_csharp'>\n"))
)


(defun simple-html-wrap-span-class (tagstring)
"wraps span -mdf"
(insert (concat "</span>"))
(goto-char (mark))
(insert (concat "<span class='" tagstring "'>"))
)

(defun simple-html-wrap-codecomment-span-class ()
"wraps span -mdf"
(interactive)
(simple-html-wrap-span-class "codecomment")
)
(defun simple-html-wrap-codekeyword-span-class ()
(interactive)(simple-html-wrap-span-class "codekeyword"))
(defun simple-html-wrap-codekeyword-span-class ()
(interactive)(simple-html-wrap-span-class "codekeyword"))




(defun simple-html-wrap-tag (tagstring)
"inserts a tag -mdf"
(interactive "*sTag to insert(e.g., h1,p): ")
(insert (concat "</" tagstring ">"))
(goto-char (mark))
(insert (concat "<" tagstring ">"))
)

(defun simple-html-wrap-p-tag ()
"inserts a tag -mdf"
(interactive)
(insert (concat "</p>"))
(goto-char (mark))
(insert (concat "<p>"))
;(if (= (point) (mark)) (message "are equal"))
)
(defun simple-html-wrap-p-line-tag ()
"wraps a line with a paragraph tag"
(interactive)
(beginning-of-line)(insert "<p>")(end-of-line)(insert "</p>")
)
(defun simple-html-wrap-comment-tag ()
"inserts a tag -mdf"
(interactive)
(insert (concat " -->\n"))
(goto-char (mark))
(insert (concat "<!-- \n"))
)

(defun simple-html-wrap-li-tag ()
"inserts a tag -mdf"
(interactive)
(insert (concat "</li>"))
(goto-char (mark))
(insert (concat "<li>"))
)


(defun simple-html-create-href ()
  "given a bare url, this creates the href for it.  If www.fincher.org is highlighted, then this will produce <a href=\"www.fincher.org\">www.fincher.org</a>"
(interactive "*")
(setq simple-html-start-marker (make-marker))
(setq simple-html-end-marker (make-marker))
(set-marker simple-html-start-marker (mark))
(set-marker simple-html-end-marker (point))
(copy-region-as-kill (mark) (point))
(goto-char simple-html-start-marker)
(insert "<a href=\"")
(goto-char simple-html-end-marker)
(insert "\">")
;(clipboard-yank)
(insert "</a>")
)

(defun simple-html-create-href-clipboard-li ()
  "given a bare url in the clipboard, this creates the href for it.  If www.fincher.org is in the paste buffer, then this will produce <li><a href=\"www.fincher.org\"></a></li>"
(interactive "*")
(insert "<li>")
(insert "<a href=\"")
(clipboard-yank)
(insert "\">")
(insert "</a>")
(insert "</li>\n")
(forward-char 1)
)

(defun simple-html-create-amazon-link ()
  "given an ISBN, creates a book link"
(interactive "*")
(insert "<a href='http://www.amazon.com/exec/obidos/ASIN/")
(clipboard-yank)
(insert "/phonelistscom/'>  <img src='http://images.amazon.com/images/P/")
(clipboard-yank)
(insert ".01.TZZZZZZZ.jpg'  alt='graphics' /></a>")
(forward-char -6)
)

(defun simple-html-create-href-clipboard ()
  "given a bare url in the clipboard, this creates the href for it.  If www.fincher.org is in the paste buffer, then this will produce <a href=\"www.fincher.org\"></a>"
(interactive "*")
(insert "<a href=\"")
(clipboard-yank)
(insert "\">")
;(clipboard-yank)
(insert "</a>")
(forward-char -4)
)
(defun simple-html-create-href-clipboard2 ()
  "given a bare url in the clipboard, this creates the href for it.  If www.fincher.org is in the paste buffer, then this will produce <a href=\"www.fincher.org\">www.fincher.org</a>"
(interactive "*")
(insert "<a href=\"")
(clipboard-yank)
(insert "\">")
(clipboard-yank)
(insert "</a>")
(forward-char -4)
)
(defun simple-html-create-img-clipboard ()
  "given a bare url in the clipboard, this creates the img tag for it."
(interactive "*")
(insert "<img src=\"")
(clipboard-yank)
(insert "\" ")
;(clipboard-yank)
(insert " alt=\"graphics\" />.")
(forward-char -6)
)

(defun simple-html-create-fincher-img-clipboard ()
  "given a bare url in the clipboard, this creates the img tag for it."
(interactive "*")
(insert "<img src=\"http://www.fincher.org/images/")
(clipboard-yank)
(insert "\" ")
;(clipboard-yank)
(insert " alt=\"graphics\" align=\"right\" width=\"200\" />.")
(forward-char -6)
)

(defun simple-html-create-li-href ()
  "given a bare url, this creates the href for it.  If www.fincher.org is highlighted, then this will produce <a href=\"www.fincher.org\">www.fincher.org</a>"
(interactive "*")
(setq simple-html-start-marker (make-marker))
(setq simple-html-end-marker (make-marker))
(set-marker simple-html-start-marker (mark))
(set-marker simple-html-end-marker (point))
(copy-region-as-kill (mark) (point))
(goto-char simple-html-start-marker)
(insert "<li><a href=\"")
(goto-char simple-html-end-marker)
(insert "\">")
(clipboard-yank)
(insert "</a></li>\n")
)
(defun simple-html-create-p-href ()
  "given a bare url, this creates the href for it.  If www.fincher.org is highlighted, then this will produce <p><a href=\"www.fincher.org\"></a></p>"
(interactive "*")
(setq simple-html-start-marker (make-marker))
(setq simple-html-end-marker (make-marker))
(set-marker simple-html-start-marker (mark))
(set-marker simple-html-end-marker (point))
(copy-region-as-kill (mark) (point))
(goto-char simple-html-start-marker)
(insert "<p><a href=\"")
(goto-char simple-html-end-marker)
(insert "\"></a>.</p>\n")
)

(defun simple-html-create-img ()
  "given a bare url, this creates the img for it."
(interactive "*")
(setq simple-html-start-marker (make-marker))
(setq simple-html-end-marker (make-marker))
(set-marker simple-html-start-marker (mark))
(set-marker simple-html-end-marker (point))
(copy-region-as-kill (mark) (point))
(goto-char simple-html-start-marker)
(insert "<img src=\"")
(goto-char simple-html-end-marker)
(insert "\" width= height= alt=\"")
(clipboard-yank)
(insert "\">")
)

(defvar simple-html-mode-map nil
  "Keymap used in simple-html mode.")

(defun simple-html-mode ()
  "Major mode for editing html files.
Special Commands:
\\{simple-html-mode-map}"
  (interactive)
  (auto-fill-mode 0)
  (kill-all-local-variables)
  (setq major-mode 'simple-html-mode)
  (setq mode-name "simple-html")
  (use-local-map simple-html-mode-map)
  (run-hooks 'simple-html-mode-hook)
  ;(simple-html-mode-variables)
  ;; Tell font-lock.el how to handle simple-html.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((simple-html-font-lock-keywords)
			     nil nil ((?\_ . "w"))))

(if simple-html-mode-map
    nil
(setq simple-html-mode-map (make-sparse-keymap))
(define-key simple-html-mode-map "\C-ct" 'simple-html-insert-table)
(define-key simple-html-mode-map "\C-cl" 'simple-html-insert-list)
(define-key simple-html-mode-map "\C-c\d" 'simple-html-insert-list-withpara)
(define-key simple-html-mode-map "\C-cL" 'simple-html-insert-list-item)
;(define-key simple-html-mode-map "\C-cp" 'simple-html-insert-para-tags)
(define-key simple-html-mode-map "\C-cf" 'simple-html-insert-html-template)
(define-key simple-html-mode-map "\C-cb" 'simple-html-insert-br)
(define-key simple-html-mode-map "\C-cg" 'simple-html-wrap-tag)
(define-key simple-html-mode-map "\C-c\<" 'simple-html-wrap-p-tag)
(define-key simple-html-mode-map "\M-p" 'simple-html-wrap-p-line-tag)
(define-key simple-html-mode-map "\C-c\x" 'simple-html-wrap-comment-tag)
(define-key simple-html-mode-map "\C-c\c" 'simple-html-wrap-codeblock-tags)
(define-key simple-html-mode-map "\C-ci" 'simple-html-wrap-li-tag)
(define-key simple-html-mode-map "\C-ca" 'simple-html-create-href)
(define-key simple-html-mode-map "\C-cv" 'simple-html-create-href-clipboard)
(define-key simple-html-mode-map "\C-cw" 'simple-html-create-img-clipboard)
(define-key simple-html-mode-map "\C-cv" 'simple-html-create-href-clipboard-li)
(define-key simple-html-mode-map "\C-cz" 'simple-html-create-amazon-link)

(define-key simple-html-mode-map "\C-c\&" 'simple-convert-html-angles)
;;'(lambda () "insert <br>" (interactive)(insert "<br>")))
)

(defvar simple-html-menu-map nil "Menu for Simple HTML mode.")
;(if simple-html-menu-map nil
  (setq simple-html-menu-map (make-sparse-keymap "simple-html"))

  (define-key simple-html-menu-map [insert-codeblock] '("Insert Codeblock" . (lambda () (interactive)(insert "</li><li><p></p>\n<div class=\"codeblock\"><pre class='sh_csharp'>\n\n</pre></div>") (forward-char -13)(set-mark (point)) (clipboard-yank) (simple-convert-html-angles))))

;(define-key simple-html-menu-map [insert-codeblock] '("Insert Codeblock" . (lambda () (interactive)(insert "</li><li><p></p>\n<div class=\"codeblock\"><pre>\n")  (insert (clipboard-yank))(insert (clipboard-yank))(insert "\n</pre></div>") (insert "\n</pre></div>") )))

  (define-key simple-html-menu-map [insert-outputblock] '("Insert Output" . (lambda () (interactive)(insert "<p>Produces:</p>\n<div class=\"output\"><pre>\n")(clipboard-yank)(insert "\n</pre></div>")(forward-line -1))))

  (define-key simple-html-menu-map [insert-table] '("Insert Table" . simple-html-insert-table))
  (define-key simple-html-menu-map [insert-list]  '("Insert List" . simple-html-insert-list))
  (define-key simple-html-menu-map [insert-list-withpara]  '("Insert List with Para" . simple-html-insert-list-withpara))
  (define-key simple-html-menu-map [insert-list-item-br]  '("Insert <br />" . simple-html-insert-br))
  (define-key simple-html-menu-map [insert-list-item] '("Insert </li><li>" . (lambda () (interactive)(insert "</li><li>"))))
  (define-key simple-html-menu-map [insert-html-template]  '("Insert html template" . simple-html-insert-html-template))

  (define-key simple-html-menu-map [wrap-codeblock]  '("Wrap codeblock tags" . simple-html-wrap-codeblock-tags))
  (define-key simple-html-menu-map [wrap-commenttag]  '("Wrap <!-- --> Tags" . simple-html-wrap-comment-tag))
  (define-key simple-html-menu-map [wrap-litag]  '("Wrap <li> Tag" . simple-html-wrap-li-tag))
  (define-key simple-html-menu-map [wrap-tag]  '("Wrap a Tag" . simple-html-wrap-tag))
(define-key simple-html-menu-map  [separator1]  '("--"))
  (define-key simple-html-menu-map [create-href-clipboard2]  '("Create <a href=(clipboard)>(clipboard)</a>..." . simple-html-create-href-clipboard2))
  (define-key simple-html-menu-map [create-href-clipboard]  '("Create <a href=(clipboard)..." . simple-html-create-href-clipboard))
  (define-key simple-html-menu-map [create-img-clipboard]  '("Create <img src=(clipboard)..." . simple-html-create-img-clipboard))
  (define-key simple-html-menu-map [create-fincher-img-clipboard]  '("Create <img src=fincher(clipboard)..." . simple-html-create-fincher-img-clipboard))
  (define-key simple-html-menu-map [create-href-clipboard-li]  '("Create <li><a href=(clipboard)..." . simple-html-create-href-clipboard-li))
  (define-key simple-html-menu-map [create-href-amazon]  '("Create amazon link with (clipboard)..." . simple-html-create-amazon-link))
  (define-key simple-html-menu-map [create-href]  '("Create <a href=..." . simple-html-create-href))
  (define-key simple-html-menu-map [create-li-href]  '("Create <li><a href=..." . simple-html-create-li-href))
  (define-key simple-html-menu-map [create-p-href]  '("Create <p><a href=..." . simple-html-create-p-href))
  (define-key simple-html-menu-map [create-img]  '("Create <img=..." . simple-html-create-img))
(define-key simple-html-menu-map  [separator2]  '("--"))
  (define-key simple-html-menu-map [simple-convert-code-wrapper]  '("simple-convert-code-wrapper" . simple-convert-code-wrapper))

  (define-key simple-html-menu-map [convert-angles]  '("convert-html-angles" . simple-convert-html-angles))
  (define-key simple-html-menu-map [unconvert-angles]  '("unconvert-html-angles" . simple-unconvert-html-angles))
  (define-key simple-html-menu-map [simple-html-convert-to-xhtml]  '("convert-to-xhtml" . simple-html-convert-to-xhtml))
  (define-key simple-html-menu-map [insert-para-tags]  '("Insert <p>.</p>" . simple-html-insert-para-tags))
  (define-key simple-html-menu-map [wrap-ptag]  '("Wrap <p> Tag" . simple-html-wrap-p-tag))
  (define-key simple-html-menu-map [wrap-line-ptag]  '("Wrap <p> Tag on line" . simple-html-wrap-p-line-tag))


(define-key simple-html-mode-map [menu-bar simple-html] 
  (cons "HTML" simple-html-menu-map))

(provide 'simple-html)

)
;;; simple-html-mode.el ends here
