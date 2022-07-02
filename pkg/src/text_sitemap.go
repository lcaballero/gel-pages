package main

import (
	. "github.com/lcaballero/gel"
)

func NewSitemapText() TextPage {
	return TextPage{
		Meta: PageMeta{
			Labels: Labels{
				"area": "sitemap",
				"mime": "text/plain",
				"id":   "sitemap.txt",
			},
		},
		Content: Frag(Text("Sitemap: https://www.read-later.net/sitemap.txt")),
	}
}
