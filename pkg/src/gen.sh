#!/bin/bash

code-writer sub-commands --data ./do-flags.data.yaml > ./gen_sub_commands.gen.go
code-writer context-opts --data ./do-flags.data.yaml > ./gen_context_opts.gen.go
code-writer cli-handler --data ./do-flags.data.yaml > ./gen_cli_handler.gen.go

gofmt -w .
