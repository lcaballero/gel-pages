(defun sed-on-string (s &rest args)
  (while args
    (let ((re (first args))
          (sub (second args))
          (rest (nthcdr 2 args)))
      (setq args rest)
      (setq s (s-replace-regexp re sub s))))
  s)

;; (sed-on-string "       \"title\": mime.HTML,"
;;                "," ")."
;;                ":" ","
;;                "^ *" ""
;;                "^" "Add(")
