package main

import (
	. "github.com/lcaballero/gel"
)

func NewSitemapText() PlainPage {
	return PlainPage{
		Meta: PageMeta{
			ID: "sitemap.txt",
			Labels: Labels{
				"area": "sitemap",
				"mime": "text/plain",
			},
		},
		Content: Frag(Text("Sitemap: https://www.read-later.net/sitemap.txt")),
	}
}
