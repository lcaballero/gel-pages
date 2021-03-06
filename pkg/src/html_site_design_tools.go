package main

import (
	. "github.com/lcaballero/gel"
)

func NewPostSiteDesignTools(env Environment) *HtmlPage {
	return &HtmlPage{
		Labels: NewLabels().
			Add("title", "Site Design Tools").
			Add("area", "aux").
			Add("mime", Mime.Html).
			Add("id", "site-design-tools").
			Add("stage", "post"),
		Env: env,
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
