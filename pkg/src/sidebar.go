package main

import (
	. "github.com/lcaballero/gel"
)

type Sidebar struct {
}

func (s Sidebar) ToView() View {
	return Frag(
		Div.Class("side-bar container").Add(
			Header(
				H1(
					A.Atts("href", "/").Add(Text("read-later.net")),
				),
				Ul(
					Li(
						A.Class("c10").Atts("href", "/posts/side-projects/").Text("Side Projects"),
					),
					Li(
						A.Class("c10").Atts("href", "/posts/possible-site-org/").Text("Possible Site Org"),
					),
					Li(
						A.Class("c10").Atts("href", "/posts/cli-snippets/").Text("CLI Snippets"),
					),
				),
			),
		),
	)
}
