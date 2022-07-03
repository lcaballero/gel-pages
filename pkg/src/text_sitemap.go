package main

import (
	"bytes"
	"fmt"
	"path"
	"sort"

	. "github.com/lcaballero/gel"
)

func NewTextSitemap(loc Locator, pages WebFileLookup) *TextPage {
	urls := []string{}
	for _, page := range pages {
		if page.Meta().IsRooted() && !page.Meta().IsHome() {
			continue
		}
		filepath := path.Join(loc.Posts(), page.Meta().ID())
		file := fmt.Sprintf("%s/%s/", loc.Base(), filepath)
		urls = append(urls, file)
	}
	sort.Strings(urls)
	buf := bytes.NewBufferString("")
	for _, file := range urls {
		buf.WriteString(file)
		buf.WriteString("\n")
	}
	return &TextPage{
		PageMeta: PageMeta{
			Labels: NewLabels().
				Add("area", "sitemap").
				Add("mime", Mime.Text).
				Add("id", "sitemap.txt").
				Add("location", "/sitemap.txt"),
		},
		Content: Text(buf.String()),
	}
}
