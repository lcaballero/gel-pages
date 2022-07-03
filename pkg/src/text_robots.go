package main

import (
	. "github.com/lcaballero/gel"
)

func NewTextRobots() *TextPage {
	return &TextPage{
		PageMeta: PageMeta{
			Labels: Labels{
				"area":     "google-site-verification",
				"mime":     Mime.Text,
				"id":       "google5d223b9b91f70029.html",
				"location": "/google5d223b9b91f70029.html",
			},
		},
		Content: Frag(Text("google-site-verification: google5d223b9b91f70029.html")),
	}
}
