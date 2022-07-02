package main

import (
	"bytes"

	. "github.com/lcaballero/gel"
)

type PlainPage struct {
	Meta    PageMeta
	Content View
}

func (p *PlainPage) ToNode() *Node {
	return Frag(p.Content).ToNode()
}

func (p *PlainPage) String() string {
	return string(p.Bytes())
}

func (p *PlainPage) Bytes() []byte {
	buf := bytes.NewBufferString("")
	p.ToNode().WriteTo(buf)
	return buf.Bytes()
}
