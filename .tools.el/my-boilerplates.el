(defhydra my/boilerplates (:color pink :hint nil :exit t)
    "
^Bash^                ^Go^
------------------------------------------
_b_: bash file        _h_: go http handler
_d_: bash $DIR        _t_: go test file

"
    ;; Boilerplates
    ("b" tmpl-new-bash nil)
    ("d" tmpl-script-dir nil)
    ("h" tmpl-go-http-handler-func nil)
    ("t" tmpl-go-new-test-file nil)
    ;; exit
    ("q" nil "quit" :exit t))
