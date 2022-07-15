package main

import (
	"bytes"

	. "github.com/lcaballero/gel"
)

type IWeb interface {
	ID() string
	Location() string
	Title() string
	IsRooted() bool
	IsHome() bool
	IsPost() bool
	GetLabels() Labels
}

// WebFile represents the HTML, Text, or XML that can be written to
// disk and is capable of rendering additional debug information as
// well
type WebFile interface {
	View
	IWeb
	SetDebug(v View)
	Bytes() []byte
}

type HtmlPage struct {
	Labels
	Scripts      View
	Styles       View
	Description  View
	Sidebar      View
	AuxFooter    View
	Content      View
	Author       string
	UseIndention bool
	UseAnalytics bool
	Debug        View
	Env          Environment
}

func (p *HtmlPage) SetDebug(v View) {
	p.Debug = v
}

func (p *HtmlPage) ToNode() *Node {
	BaseCSS.ToNode().Println()
	return Fragment{
		HTML5(),
		Html(
			Head(
				DefaultMeta(),
				Link.Atts("rel", "apple-touch-icon", "sizes", "180x180", "href", "/img/apple-touch-icon.png"),
				Link.Atts("rel", "icon", "type", "image/png", "sizes", "32x32", "href", "/img/favicon-32x32.png"),
				Link.Atts("rel", "icon", "type", "image/png", "sizes", "16x16", "href", "/img/favicon-16x16.png"),
				Link.Atts("rel", "manifest", "href", "/img/site.webmanifest"),
				//Default(p.Styles, BaseCSS),
				BaseCSS,
				//Default(p.Scripts, BaseJS),
				BaseJS,
				Meta.Atts("name", "author", "content", DefaultAuthor(p.Author)),
				Meta.Atts("name", "title", "content", p.Title()),
				Title(Default(p.Title(), p.Title())),
				NewGoogleAnalyticsScriptTags(p.Env),
			),
			Body(
				Div.Class("main").Add(
					Default(p.Sidebar, Sidebar{}),
					Div.Class("container shadow bg-1").Add(
						Div.Class("prose").Add(
							p.Content,
						),
					),
					Default(p.AuxFooter, AuxFooter{}),
					Default(p.Debug, Debug{}),
				),
			),
		),
	}.ToNode()
}

// String turns the HtmlPage into a single string of HTML
func (p HtmlPage) String() string {
	if p.UseIndention {
		indent := NewIndent()
		buf := bytes.NewBufferString("")
		p.ToNode().WriteWithIndention(indent, buf)
		return buf.String()
	}
	return p.ToNode().String()
}

// Bytes is similar to String which renders the HTML without indention
func (p HtmlPage) Bytes() []byte {
	if p.UseIndention {
		indent := NewIndent()
		buf := bytes.NewBufferString("")
		p.ToNode().WriteWithIndention(indent, buf)
		return buf.Bytes()
	}
	buf := bytes.NewBufferString("")
	p.ToNode().WriteTo(buf)
	return buf.Bytes()
}
