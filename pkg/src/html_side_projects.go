package main

import (
	. "github.com/lcaballero/gel"
)

func NewPostSideProjects() *HtmlPage {
	return &HtmlPage{
		PageMeta: PageMeta{
			Labels: NewLabels().
				Add("title", "Side Projects").
				Add("area", "header").
				Add("mime", Mime.Html).
				Add("id", "side-projects").
				Add("stage", "post"),
		},
		Content: Div(
			H3.Text("Side Projects"),
			P.Text(`I plan to write about my side-projects here. Calling these ideas 'side-projects', might be too generous. Most of these projects won't be very ambitious. They probably won't involve the latest and greatest framework, but then again they might, because I'm overly curious about things that sound interesting.`),
			P.Add(
				Text(`The general idea is to find some useful way to use a technology. For example, this site uses `),
				XLink("https://www.nginx.com/", "NGINX"),
				Text(` as a reverse proxy in production, providing TLS by way of `),
				XLink("https://letsencrypt.org/", "Let's Encrypt"),
				Text(` and `),
				XLink("https://certbot.eff.org/", "certbot"),
				Text(`. But in development I use `),
				XLink("https://caddyserver.com/", "Caddy"),
				Text(` to preview the generated site. And recently I came across a way to use `),
				Code.Text("socat"),
				Text(` (check out this article on `),
				XLink("https://www.redhat.com/sysadmin/getting-started-socat", "getting started with socat"),
				Text(`).`),
			),
			P(
				Text(`At one point I mucked around trying to use an NGINX `),
				XLink("https://www.docker.com/", "Docker"),
				Text(` container to serve up the site locally. I eventually abandoned that approach due to the way user and file permissions work between the host and container.`),
			),
			P.Text(`That's an example where I abondoned an approach I started as a 'side-project'. However, in the end I did learn something about Docker permissions and the host file system.`),
		),
	}
}
