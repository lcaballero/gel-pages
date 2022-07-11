(defhydra bookmarks-run-tools (:cool pink :hint nil :exit t)
  "
^Build Tools^
----------------------------------------------------------------------------------------------------------------
_b_: ./run.sh build

"
  ("b" (lambda () (interactive) (tools-run-command "bookmarks.sh" "build")) nil)
  ("q" nil "quit" :exit t))

(global-set-key (kbd "C-c .") 'bookmarks-run-tools/body)
