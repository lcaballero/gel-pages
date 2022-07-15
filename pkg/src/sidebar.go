package main

import (
	. "github.com/lcaballero/gel"
)

type Sidebar struct{}

func (s Sidebar) ToNode() *Node {
	return Fragment{
		Div.Class("banner container").Add(
			Div.Class("inner").Add(
				Div.Class("win").Add(
					Div.Class("img"),
					Div.Class("side-bar").Add(
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
				),
			),
		),
	}.ToNode()
}
