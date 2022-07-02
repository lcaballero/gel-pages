package main

import (
	"encoding/json"

	. "github.com/lcaballero/gel"
)

type Debug struct {
	Out interface{}
}

func (d Debug) ToView() View {
	if d.Out == nil {
		return None()
	}
	bin, err := json.MarshalIndent(d.Out, "", "  ")
	if err != nil {
		return None()
	}
	return Div.Class("container debug code").Add(
		Code(
			Pre.Text(
				string(bin),
			),
		),
	)
}
