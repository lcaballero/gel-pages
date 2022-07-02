package main

import (
	. "github.com/lcaballero/gel"
)

func NewSitemapText() TextPage {
	return TextPage{
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
