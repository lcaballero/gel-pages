package main

import (
	. "github.com/lcaballero/gel"
)

func NewPostPossibleSiteOrg() *HtmlPage {
	return &HtmlPage{
		PageMeta: PageMeta{
			Title: "Possible Site Org",
			Labels: Labels{
				"area":  "header",
				"mime":  Mime.Html,
				"id":    "possible-site-org",
				"stage": "post",
			},
		},
		Content: Div(
			H2.Text("Possible Site Organization [DRAFT]"),
			P(
				Text(`I've been thinking about how to layout the site. I see a number of blog sites that have the normal `),
				Code.Text("/Home"),
				Text(`, `),
				Code.Text("/About"),
				Text(`, `),
				Code.Text("/Projects"),
				Text(`, etc pages. And although I think that's fairly useful, I don't think it has to be that way. And I'm not convinced it works all that great.`)),
			H3.Text("Folder-like View"),
			P.Text(`The first way to organize the site might be something like the directory structure shown below. It's the typical heirarchy: files in folders, or pages inside sections for a website. Some sections with sub-sections with sub-section, etc.`),
			Text(
				`
<div class="code"><pre class="tree"><span style="color: rgb(64,64,64);">───────┬──────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">│ </span><span style="font-weight:bold;">STDIN</span>
<span style="color: rgb(64,64,64);">───────┼──────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">home</span>
<span style="color: rgb(64,64,64);">   2</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">├── list</span>
<span style="color: rgb(64,64,64);">   3</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">├── projects</span>
<span style="color: rgb(64,64,64);">   4</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">│&nbsp;&nbsp; ├── description</span>
<span style="color: rgb(64,64,64);">   5</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">│&nbsp;&nbsp; └── list</span>
<span style="color: rgb(64,64,64);">   6</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">├── snippets</span>
<span style="color: rgb(64,64,64);">   7</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">│&nbsp;&nbsp; └── why</span>
<span style="color: rgb(64,64,64);">   8</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">└── study</span>
<span style="color: rgb(64,64,64);">   9</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">    ├── backlog</span>
<span style="color: rgb(64,64,64);">  10</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">    ├── sources</span>
<span style="color: rgb(64,64,64);">  11</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">    └── topics</span>
<span style="color: rgb(64,64,64);">  12</span>   <span style="color: rgb(64,64,64);">│</span>
<span style="color: rgb(64,64,64);">  13</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">10 directories, 0 files</span>
<span style="color: rgb(64,64,64);">───────┴──────────────────────────────────────────────────────────────────────────</span>
</pre></div>
`,
			),
			H3.Text("Search Prefered Over The Folder View"),
			P.Text(`How do you help people find posts on a site? The simple answer is: Google. Meaning, do people really use the directory structure? Probably not.`),
			P.Text(`Why? Because everything is likely buried or strangely organized. In a big site this could easily be the case.`),
			P.Text(`I know I don't use the search bar provided by some sites.`),
			P(
				Text(`For example, I don't use the search feature on `),
				XLink("https://news.ycombinator.com/", "HackerNews"),
				Text(`. It's at the bottom of the page too, discouraging it's use, as if it's something of an after thought.`),
			),
			A.Atts("href", "https://news.ycombinator.com/").Add(
				Img.Atts(
					"src", "/img/hacker-news-search-input.png",
					"alt", "The search bar from HackerNews",
					"title", "",
					"style", "width:620px;")),
			P.Text(`Searching was never meant to be the way to find interesting things on HackerNews, anyhow. That site relies on the community to upvote articles, pushing interesting things to the top of the page.`),
			P.Text(`And right now this site isn't a social-organzied, so I have to decide on, and then implement the organziation manually.`),
			H3.Text(`Where Search Seems Very Useful`),
			P(
				Text(`I do like the search provided by `),
				XLink("https://www.algolia.com/", "algolia"),
				Text(` on the `),
				XLink("https://vuejs.org/", "VueJS"),
				Text(` site. I've known about `),
				XLink("https://www.algolia.com/", "algolia"),
				Text(` for a while. I've never used it on anything I've built, but might be a good search experience.`),
			),
			A.Atts("href", "https://vuejs.org/").Add(
				Img.Atts(
					"src", "/img/vuejs-search.png",
					"alt", "The search UI from VueJS site",
					"title", "VueJS Search Bar",
					"style", "width:620px;")),
			P(
				Text(`However, `),
				XLink("https://www.algolia.com/", "algolia"),
				Text(` may only work for a site with a strong focus like documentation. Then again `),
				XLink("https://www.algolia.com/", "algolia"),
				Text(` does have a few big names so my estimation might be too narrow.`),
			),
			P.Text(`Still is this the user experience I want?`),
			P(
				Text(`Of course, there's nothing stopping me from using a directory structure and `),
				XLink("https://www.algolia.com/", "algolia"),
				Text(`.`),
			),
			H3.Text(`Other Organizations to Consider`),
			P.Text(`Most blogs have a simple structure.`),
			P(
				Text(`Like `),
				XLink("https://jvns.ca/", "Julie Evans"),
				Text(` site. I love this site. It's simple and has a ton of useful information. I'm inspired by this site.`),
			),
			A.Atts("href", "https://jvns.ca/").Add(
				Img.Atts(
					"src", "/img/julia-evans-heading.png",
					"alt", "The banner from Julia Evans blog/site",
					"title", "Julia Evan's blog banner",
					"style", "width:620px;")),
			P.Text(`What alternatives are there? One that comes to mind is a kind of tagged version of pages.`),
			P.Text(`But, at this point in the post...I'm fairly sure I'm over thinking things.`),
		),
	}
}
