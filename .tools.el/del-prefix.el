(defun del-prefix (beg end)
  (interactive "r")
  (message "1: %S, 2: %S, pt: %S" beg end (point))
  (while (and
          (< (point) (point-max))
          (< (point) end))
    (message "1: %S, 2: %S, pt: %S" beg end (point))
    (beginning-of-line)
    (delete-char 4)
    (end-of-line)
    (forward-char 1)))

(global-set-key (kbd "C-c d") 'del-prefix)
