package main

import (
	"bytes"

	. "github.com/lcaballero/gel"
)

type TextPage struct {
	Meta    PageMeta
	Content View
}

func (p *TextPage) ToNode() *Node {
	return Frag(p.Content).ToNode()
}

func (p *TextPage) String() string {
	return string(p.Bytes())
}

func (p *TextPage) Bytes() []byte {
	buf := bytes.NewBufferString("")
	p.ToNode().WriteTo(buf)
	return buf.Bytes()
}
