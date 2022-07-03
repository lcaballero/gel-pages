package main

import (
	. "github.com/lcaballero/gel"
)

func NewTextRobots() *TextPage {
	return &TextPage{
		PageMeta: PageMeta{
			Labels: Labels{
				"area":     "robots.txt",
				"mime":     Mime.Text,
				"id":       "robots.txt",
				"location": "/robots.txt",
			},
		},
		Content: Text("Sitemap: https://www.read-later.net/sitemap.txt"),
	}
}
