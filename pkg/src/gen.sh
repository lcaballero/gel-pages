#!/bin/bash

code-writer sub-commands --data ./do-flags.data.yaml > ./sub_commands.gen.go
code-writer context-opts --data ./do-flags.data.yaml > ./context_ops.gen.go
code-writer cli-handler --data ./do-flags.data.yaml > ./cli_handler.gen.go

gofmt -w .
