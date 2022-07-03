package main

import (
	. "github.com/lcaballero/gel"
)

func NewPostGoogleAnalytics() *TextPage {
	return &TextPage{
		PageMeta: PageMeta{
			Title: "google-analytics",
			Labels: NewLabels().
				Add("area", "analytics").
				Add("mime", Mime.Html).
				Add("id", "google5d223b9b91f70029.html").
				Add("location", "/google5d223b9b91f70029.html"),
		},
		Content: Text("google-site-verification: google5d223b9b91f70029.html"),
	}
}