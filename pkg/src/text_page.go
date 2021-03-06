package main

import (
	"bytes"

	. "github.com/lcaballero/gel"
)

type TextPage struct {
	Labels
	Content View
}

func (p *TextPage) SetDebug(v View) {}

func (p *TextPage) ToNode() *Node {
	return Fragment{p.Content}.ToNode()
}

func (p *TextPage) String() string {
	return string(p.Bytes())
}

func (p *TextPage) Bytes() []byte {
	buf := bytes.NewBufferString("")
	p.ToNode().WriteTo(buf)
	return buf.Bytes()
}
