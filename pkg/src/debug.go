package main

import (
	"encoding/json"

	. "github.com/lcaballero/gel"
)

type Debug struct {
	Out interface{}
}

func (d Debug) ToNode() *Node {
	if d.Out == nil {
		return None().ToNode()
	}
	bin, err := json.MarshalIndent(d.Out, "", "  ")
	if err != nil {
		return None().ToNode()
	}
	return Div.Class("container debug code").Add(
		Code(
			Pre.Text(
				string(bin),
			),
		),
	).ToNode()
}
