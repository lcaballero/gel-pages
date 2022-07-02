package main

import (
	. "github.com/lcaballero/gel"
)

func NewPostBeforeIGetStarted() *HtmlPage {
	return &HtmlPage{
		PageMeta: PageMeta{
			Title: "Before I Get Started",
			Labels: Labels{
				"area": "header",
				"mime": Mime.Html,
				"id":   "before-i-get-started",
			},
		},
		Content: Div(
			H2.Text("Before I Get Started"),
			H3(
				Text("Why did I sign up for "),
				XLink("https://bloggingfordevs.com/", "Blogging for Devs"),
				Text("?"),
			),
			P.Text(`I signed up because I wanted to learn about blogging from a dev and
from someone who knows what it's like to write and post things. I
think those two perspectives combined would make the experience better
since I share at least the dev perspective.`),
			P(
				Text(`I'm writing this having signed up in July of 2020. It is now March of
2021. That is serious procrastination. I don't know what's changed
since then really. I'm still a dev, I'm still social distancing, I'm
still interested in `),
				XLink("https://www.indiehackers.com/", "IndieHacker"),
				Text(`-ish things. But I've finally decided
to write something – well "something" might be a stretch.`),
			),
			P.Text(`It's a stretch for a few reasons.`),
			P.Text(`Do I think I have much to say that people would be interested in?
Probably not. I could write some tech-y things but there are so many
blogs out there, and I know first hand that I don't tend to read many
of them. But, the ones that I appreciate are those posts that have
gone the extra mile and given something I really value. And they have
generally been a level deeper than the "beginner" variety.`),
			P.Text(`If I had to make up my mind about what I'd write about it would be
some form of intermediate to advanced information. I think there are a
lot of beginner branded posts out there. I think they are good and
they work. I think I could even write some of them myself and they
might be useful. I'm simply torn, because I would like to help people
get some insights into a whole variety of tech, but I'd also like to
learn along the way.`),
			P.Text(`That's exactly the type of thing I have in mind. I'd like to write
about things as I learn them myself. I know I have a dev background so
I might not struggle on the beginner level stuff, but that doesn't
mean I'm not a beginner in some regards.`),
			H3.Text(`What's my motivation for blogging?`),
			P(
				Text(`I see myself as a novice when it comes to a wide range of things
tech. I know almost nothing about `),
				XLink("https://istio.io/", "Istio"),
				Text(`, or `),
				XLink("https://cassandra.apache.org/", "Cassandra"),
				Text(`. Low level
networking seems like magic, but I'm getting better at it since I
admin `),
				XLink("https://kubernetes.io/", "Kubernetes"),
				Text(` clusters. I still have so much to learn though.`),
			),
			P.Text(`I've worked with concurrency, programming languages, JavaScript, Go,
Bash, etc. So a discussion on these topics wouldn't sound like a
foreign language to me.`),
			P.Text(`That hints at my motivations actually. I think writing is a great way
to learn things. It helps me solidify my thinking once I have to put
those ideas, or practices, or insights, or learnings into writing.`),
			P.Text(`I'm not sure that is interesting enough as a blog, but it should be
interesting trying to accomplish that: learning and writing about what
I learn at the same time. I'm hoping that I can maintain a tone of
wonder, and a bit of excitement in trying and learning new
things. While at the same time, I get to learn a few things on topics
I've been eager to understand.`),
			H3.Text(`What do I want to learn?`),
			P.Text(`Beyond just learning some new tech, I want to learn to write better. I
want to learn how to get my posts noticed. Specifically in this longer
format. I'd like to accumulate some contacts with people who share the
same interests or at least can provide some interesting insights into
something I'm diving into.`),
			P.Text(`I feel like a lot of blog posts are the so-called number
posts. N-things you should know, or learn, or do. I don't really want
to post those kinds of things. So I want to learn how to be successful
without that kind of content.`),
			P.Text(`Learning some google analytics is also on the list. I'm not sure how
deep that rabbit hole goes, but I'm guessing there is quite a bit to
learn about GA.`),
			P.Text(`That's about it I guess.`),
			P.Text(`L-`),
		),
	}
}
