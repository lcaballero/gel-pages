package main

import (
	. "github.com/lcaballero/gel"
)

func NewTextRobots() *TextPage {
	return &TextPage{
		PageMeta: PageMeta{
			Labels: NewLabels().
				Add("area", "robots.txt").
				Add("mime", Mime.Text).
				Add("id", "robots.txt").
				Add("location", "/robots.txt"),
		},
		Content: Text("Sitemap: https://www.read-later.net/sitemap.txt"),
	}
}
