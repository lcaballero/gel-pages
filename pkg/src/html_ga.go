package main

import (
	. "github.com/lcaballero/gel"
)

func NewPostGoogleAnalytics() *TextPage {
	return &TextPage{
		PageMeta: PageMeta{
			Title: "google-analytics",
			Labels: Labels{
				"area":     "analytics",
				"mime":     Mime.Html,
				"id":       "google5d223b9b91f70029.html",
				"location": "/google5d223b9b91f70029.html",
			},
		},
		Content: Text("google-site-verification: google5d223b9b91f70029.html"),
	}
}
