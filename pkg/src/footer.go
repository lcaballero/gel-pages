package main

import (
	. "github.com/lcaballero/gel"
)

type AuxFooter struct{}

func (a AuxFooter) ToNode() *Node {
	return Fragment{
		Footer(
			Div.Class("container shadow bg-1").Add(
				Div.Class("aux").Add(
					H3.Text("Auxiliary"),
					Ul(
						Li(
							A.Class("c10").Atts("href", "/posts/site-design-tools/").Text("Site Design Tools"),
						),
						Li(
							A.Class("c10").Atts("href", "/posts/organizing-pins/").Text("Organizing Pins"),
						),
					),
					H3.Text("Drafts"),
					Ul(
						Li(
							A.Class("c10").Atts("href", "/posts/first-steps-of-building-this-site/").Text("[DRAFT] First Steps of Building this Site"),
						),
					),
				),
			),
			Div.Class("container").Add(
				Small.Text(
					"Copyright © 2021 read-later.net.  All rights reserved.",
				),
			),
		),
	}.ToNode()
}
