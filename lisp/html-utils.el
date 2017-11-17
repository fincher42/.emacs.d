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
