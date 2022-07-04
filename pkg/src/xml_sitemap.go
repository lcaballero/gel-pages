package main

import (
	"fmt"
	"path"
	"sort"
	"strings"

	. "github.com/lcaballero/gel"
)

var (
	Urlset = El("urlset", false)
	Url    = El("url", false)
	Loc    = El("loc", false)
)

func NewXmlSitemap(loc Locator, pages WebFileLookup) *TextPage {
	urls := []string{}
	for _, page := range pages {
		if page.IsRooted() && !page.IsHome() {
			continue
		}
		filepath := path.Join(loc.Posts(), page.ID())
		file := fmt.Sprintf("%s/%s/", loc.Base(), filepath)
		urls = append(urls, file)
	}
	sort.Strings(urls)
	f := NewFragment()
	for _, file := range urls {
		f.Add(Url.Add(Loc.Add(Text(file))))
	}
	u := Urlset.
		Atts("xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9").
		Add(f)
	xml := u.ToNode().String()
	xml = strings.ReplaceAll(xml, "><url", ">\n<url")
	xml = strings.ReplaceAll(xml, "url></urlset", "url>\n</urlset")
	return &TextPage{
		Labels: NewLabels().
			Add("area", "sitemap").
			Add("mime", Mime.Xml).
			Add("id", "sitemap.xml").
			Add("location", "/sitemap.xml"),
		Content: Frag(
			Text(`<?xml version="1.0" encoding="UTF-8"?>`),
			Text("\n"),
			Text(xml),
		),
	}
}
