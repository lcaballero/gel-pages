package main

import (
	. "github.com/lcaballero/gel"
)

func NewPostOrganizingPins() *HtmlPage {
	return &HtmlPage{
		PageMeta: PageMeta{
			Title: "Organizing Pins",
			Labels: Labels{
				"area": "aux",
				"mime": Mime.Html,
				"id":   "organizing-pins",
			},
		},
		Content: Div.Class("with-bullets").Add(
			H2.Text(`Organizing Pins`),
			P.Text(`Attempting to keep some references to tools, tutorials, blogs, etc that can be used as references.`),
			H3(Code.Text("tmux")),
			Ul.Class("org-ul").Add(
				Li(
					A.Atts("href", "http://ryan.himmelwright.net/post/scripting-tmux-workspaces/").Text("scripting-tmux-workspaces"),
				),
				Li(
					A.Atts("href", "https://github.com/tmux/tmux").Text("github: tmux"),
				),
				Li(
					A.Atts("href", "https://github.com/rothgar/awesome-tmux").Text("awesome-tmux"),
				),
				Li(
					Text("The Tao of Tmux"),
					Ul.Class("org-ul").Add(
						Li.Add(
							Text("Original overview of "),
							A.Atts("href", "https://tmuxp.git-pull.com/about_tmux.html").Text("The Tao of tmux"),
						),
						Li.Add(
							Text("Online html pages: "),
							A.Atts("href", "https://leanpub.com/the-tao-of-tmux/read").Text("The Tao of tmux"),
						),
					),
				),
			),
			P(
				Code.Text("tmux list-clients"),
			),
		),
	}
}
