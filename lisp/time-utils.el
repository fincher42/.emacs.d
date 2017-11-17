;////////////////////////////////////////////////////////////
;; Add the Last Updated: timestamp.
;    Last Updated:<time datetime='2017-11-17' pubdate> November 17, 2017</time>.
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
