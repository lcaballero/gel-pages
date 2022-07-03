package main

import (
	"bytes"

	. "github.com/lcaballero/gel"
)

type MimeTypes struct {
	Html string
	Text string
	Xml  string
}

var Mime = MimeTypes{
	Html: "text/html",
	Text: "text/plain",
	Xml:  "application/xml",
}

type WebFile interface {
	View
	Meta() PageMeta
	SetDebug(v Viewable)
	Bytes() []byte
}

type Labels map[string]string

func (labels Labels) Location() string {
	return labels["location"]
}

func (labels Labels) ID() string {
	return labels["id"]
}

func (labels Labels) IsPost() bool {
	val, ok := labels["stage"]
	return ok && val == "post"
}

func (labels Labels) IsRooted() bool {
	switch labels.Location() {
	case "/index.html", "/sitemap.txt", "/sitemap.xml", "/robots.txt", "/google5d223b9b91f70029.html":
		return true
	default:
		return false
	}
}

func (labels Labels) IsHome() bool {
	switch labels.Location() {
	case "/index.html":
		return true
	default:
		return false
	}
}

type PageMeta struct {
	Labels
	Version int
	Title   string
}

type HtmlPage struct {
	PageMeta     PageMeta
	Scripts      Viewable
	Styles       Viewable
	Description  View
	Sidebar      Viewable
	AuxFooter    Viewable
	Content      View
	Author       string
	UseIndention bool
	UseAnalytics bool
	Debug        Viewable
}

func (p *HtmlPage) SetDebug(v Viewable) {
	p.Debug = v
}

func (p *HtmlPage) Meta() PageMeta {
	return p.PageMeta
}

func (p *HtmlPage) ToNode() *Node {
	return Frag(
		HTML5(),
		Html(
			Head(
				DefaultMeta(),
				Link.Atts("rel", "apple-touch-icon", "sizes", "180x180", "href", "/img/apple-touch-icon.png"),
				Link.Atts("rel", "icon", "type", "image/png", "sizes", "32x32", "href", "/img/favicon-32x32.png"),
				Link.Atts("rel", "icon", "type", "image/png", "sizes", "16x16", "href", "/img/favicon-16x16.png"),
				Link.Atts("rel", "manifest", "href", "/img/site.webmanifest"),
				Def(p.Styles, BaseCSS),
				Def(p.Scripts, BaseJS),
				Meta.Atts("name", "author", "content", DefaultAuthor(p.Author)),
				Meta.Atts("name", "title", "content", p.Meta().Title),
				Title(Def(p.Meta().Title, p.Meta().Title)),
			),
			Body(
				Div.Class("main").Add(
					Def(p.Sidebar, Sidebar{}),
					Div.Class("container shadow bg-1").Add(
						Div.Class("prose").Add(
							p.Content,
						),
					),
					Def(p.AuxFooter, AuxFooter{}),
					Def(p.Debug, Debug{}),
				),
			),
		),
	).ToNode()
}

func (p HtmlPage) String() string {
	if p.UseIndention {
		indent := NewIndent()
		buf := bytes.NewBufferString("")
		p.ToNode().WriteToIndented(indent, buf)
		return buf.String()
	}
	return p.ToNode().String()
}

func (p HtmlPage) Bytes() []byte {
	if p.UseIndention {
		indent := NewIndent()
		buf := bytes.NewBufferString("")
		p.ToNode().WriteToIndented(indent, buf)
		return buf.Bytes()
	}
	buf := bytes.NewBufferString("")
	p.ToNode().WriteTo(buf)
	return buf.Bytes()
}
