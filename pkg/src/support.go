package main

import (
	"strings"

	. "github.com/lcaballero/gel"
)

var BaseCSS = Styles{
	"/css/reset.css",
	"/css/fonts.css",
	"/css/bullet-text.css",
	"/css/code.css",
	"/css/debug.css",
	"/css/colors.css",
	"/css/footer.css",
	"/css/main.css",
}

var BaseJS = Scripts{
	"/js/site.js",
	"/js/clipboard.min.js",
}

type Scripts []string

func (s Scripts) ToView() View {
	return JS(s...)
}

type Styles []string

func (s Styles) ToView() View {
	return CSS(s...)
}

func HTML5() View {
	return Text("<!doctype html>")
}

func DefaultMeta() View {
	return Frag(
		Meta.Atts("charset", "utf-8"),
		Meta.Atts("name", "viewport", "content", "width=device-width, initial-scale=1"),
	)
}

func DefaultAuthor(author string) string {
	if strings.TrimSpace(author) == "" {
		return "Lucas Caballero"
	}
	return author
}

func JS(href ...string) View {
	f := NewFragment()
	for _, h := range href {
		js := Script.Atts("src", h)
		f.Add(js)
	}
	return f
}

func CSS(href ...string) View {
	f := NewFragment()
	for _, h := range href {
		css := Link.Atts("href", h, "rel", "stylesheet")
		f.Add(css)
	}
	return f
}

func XLink(href, text string) View {
	return A.Atts("href", href).Text(text)
}
