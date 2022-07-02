package main

import (
	. "github.com/lcaballero/gel"
)

func NewPostSiteDesignTools() *HtmlPage {
	return &HtmlPage{
		PageMeta: PageMeta{
			Title: "Site Design Tools",
			Labels: Labels{
				"area": "aux",
				"mime": "text/html",
				"id":   "site-design-tools",
			},
		},
		Content: Div(
			H2.Text(`Site Design Tools`),
			P.Text(`I wanted to name this some thing like an awesome-list, but the category is too broad. So, consider this just a list of some tools I use to maintain this site.`),
			H3.Text(`Colors`),
			Ul(
				Li(
					XLink("https://galactic.ink/sphere/#", "UICube"),
				),
			),
		),
	}
}
