package main

import (
	. "github.com/lcaballero/gel"
)

func NewGeneratingHtmlWithGo(env Environment) *HtmlPage {
	s1 := Fragment{
		H2.Text(`Generating HTML with Go (not using templates)`),
		P(
			Text(`I wrote a library not too long ago that I called `),
			Code.Text("gel"),
			Text(` that generates HTML from within Go code.  The library doesn't use templates.  I wrote it years ago and kind of forgot about it.`),
		),
		P(
			Text(`But recently, I've started getting a bit annoyed with `),
			A.Atts("href", "https://pkg.go.dev/html/template@go1.18.3").Text("Go Templates"),
			Text(`.  So, I looked to using `),
			Code.Text("gel"),
			Text(` on this latest rendition of the site.  I'll call it the July 2022 rendition, just to mark the date.`),
		),
		P(
			Text(`This is the 3rd or 4th time I've written this site.  And for the record there was a `),
			A.Atts("href", "https://pugjs.org/api/getting-started.html").Text("Pug"),
			Text(` version, an `),
			A.Atts("href", "https://orgmode.org/").Text("org-mode"),
			Text(` using `),
			A.Atts("href", "https://www.emacswiki.org/emacs/BatchMode").Text("emacs batch"),
			Text(` conversion version, and, I think, a `),
			A.Atts("href", "https://github.com/vannizhang/react-redux-boilerplate").Text("react, redux, webpack"),
			Text(` version at some point as well.  This is the Gel version.`),
		),
	}
	s2 := Fragment{
		H2(
			Text(`What I like About this `),
			Code.Text("gel"),
			Text(` Version`),
		),
		P(
			Text(`Having rewriten the site now, I have a new appreciation for `),
			Code.Text("gel"),
			Text(`.`),
		),
		P(
			Text(`For clarity, this is what using `),
			Code.Text("gel"),
			Text(` tends to look like:`),
		),
		Div.Class("code").Add(
			Pre.Text(`
<span style="color: rgb(64,64,64);">───────┼────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">title</span><span style="color: #f8f8f2;"> </span><span style="color: #f92672;">:</span><span style="color: #f92672;">=</span><span style="color: #f8f8f2;">  Div</span><span style="color: #f8f8f2;">(</span>
<span style="color: rgb(64,64,64);">   2</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">  </span><span style="color: #be84ff;">H2</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Text</span><span style="color: #f8f8f2;">(</span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">Before I Get Started</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">   3</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">  </span><span style="color: #be84ff;">H3</span><span style="color: #f8f8f2;">(</span>
<span style="color: rgb(64,64,64);">   4</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">    Text</span><span style="color: #f8f8f2;">(</span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">Why did I sign up for </span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">   5</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">    XLink</span><span style="color: #f8f8f2;">(</span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">https://bloggingfordevs.com/</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">,</span><span style="color: #f8f8f2;"> </span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">Blogging for Devs</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">   6</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">    Text</span><span style="color: #f8f8f2;">(</span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">?</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">   7</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">  </span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">   8</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">  </span><span style="color: #be84ff;">P</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Text</span><span style="color: #f8f8f2;">(</span><span style="color: #ffffff;">"</span><span style="color: #e6db74;">I signed up because I wanted to learn about blogging...</span><span style="color: #ffffff;">"</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">   9</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">)</span>
<span style="color: rgb(64,64,64);">───────┴────────────────────────────────────────────────────────────────────────</span>
`),
		),
		P(
			Text(`And to output that `),
			Code.Text(`div`),
			Text(` as bytes:`),
		),
		Div.Class("code").Add(
			Pre.Text(`
<span style="color: rgb(64,64,64);">───────┼────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">indent</span><span style="color: #f8f8f2;"> </span><span style="color: #f92672;">:</span><span style="color: #f92672;">=</span><span style="color: #f8f8f2;"> NewIndent</span><span style="color: #f8f8f2;">(</span><span style="color: #f8f8f2;">)</span>
<span style="color: rgb(64,64,64);">   2</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">buf</span><span style="color: #f8f8f2;"> </span><span style="color: #f92672;">:</span><span style="color: #f92672;">=</span><span style="color: #f8f8f2;"> </span><span style="color: #f8f8f2;">bytes</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">NewBufferString</span><span style="color: #f8f8f2;">(</span><span style="color: #ffffff;">&quot;</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">)</span>
<span style="color: rgb(64,64,64);">   3</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">p</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">ToNode</span><span style="color: #f8f8f2;">(</span><span style="color: #f8f8f2;">)</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">WriteToIndented</span><span style="color: #f8f8f2;">(</span><span style="color: #f8f8f2;">indent</span><span style="color: #f8f8f2;">,</span><span style="color: #f8f8f2;"> </span><span style="color: #f8f8f2;">buf</span><span style="color: #f8f8f2;">)</span>
<span style="color: rgb(64,64,64);">   4</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">fmt</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Println</span><span style="color: #f8f8f2;">(</span><span style="color: #f8f8f2;">buf</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">String</span><span style="color: #f8f8f2;">(</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">)</span>
<span style="color: rgb(64,64,64);">───────┴────────────────────────────────────────────────────────────────────────</span>
`),
		),
		P(
			Text(`What I like about `),
			Code.Text(`gel`),
			Text(` is that it's pure `),
			Code.Text(`go`),
			Text(`.  I write `),
			Code.Text(`go`),
			Text(` directly from `),
			Code.Text(`go`),
			Text(` functions and build components using custom structs that implement interfaces.  If I need to loop, I loop over things in `),
			Code.Text(`go`),
			Text(`.  If I need to do some string manipulation I can do it using the `),
			Code.Text(`go`),
			Text(` SDK.`),
		),
		P.Text(`A component/view might look something like this:`),
		Div.Class("code").Add(
			Pre.Text(`
<span style="color: rgb(64,64,64);">───────┼───────────────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">func</span><span style="color: #f8f8f2;"> </span><span style="color: #f8f8f2;">(</span><span style="color: #f8f8f2;">p</span><span style="color: #f8f8f2;"> </span><span style="color: #f92672;">*</span><span style="color: #f8f8f2;">HtmlPage</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;"> ToNode</span><span style="color: #f8f8f2;">(</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;"> </span><span style="color: #f92672;">*</span><span style="color: #f8f8f2;">Node </span><span style="color: #f8f8f2;">{</span>
<span style="color: rgb(64,64,64);">   2</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">  </span><span style="color: #f92672;">return</span><span style="color: #f8f8f2;"> Fragment</span><span style="color: #f8f8f2;">{</span>
<span style="color: rgb(64,64,64);">   3</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">    </span><span style="color: #be84ff;">HTML5</span><span style="color: #f8f8f2;">(</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">   4</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">    Html</span><span style="color: #f8f8f2;">(</span>
<span style="color: rgb(64,64,64);">   5</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">      Head</span><span style="color: #f8f8f2;">(</span>
<span style="color: rgb(64,64,64);">   6</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">        Link</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Atts</span><span style="color: #f8f8f2;">(</span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">rel</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">,</span><span style="color: #f8f8f2;"> </span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">icon</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">,</span><span style="color: #f8f8f2;"> </span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">type</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">,</span><span style="color: #f8f8f2;"> </span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">image/png</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">,</span><span style="color: #f8f8f2;"> </span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">sizes</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">,</span><span style="color: #f8f8f2;"> </span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">16x16</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">,</span><span style="color: #f8f8f2;"> </span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">href</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">,</span><span style="color: #f8f8f2;"> </span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">/</span>
<span style="color: rgb(64,64,64);">    </span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">img/favicon-16x16.png</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">   7</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">        Def</span><span style="color: #f8f8f2;">(</span><span style="color: #f8f8f2;">p</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Styles</span><span style="color: #f8f8f2;">,</span><span style="color: #f8f8f2;"> BaseCSS</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">   8</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">        Def</span><span style="color: #f8f8f2;">(</span><span style="color: #f8f8f2;">p</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Scripts</span><span style="color: #f8f8f2;">,</span><span style="color: #f8f8f2;"> BaseJS</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">   9</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">        Title</span><span style="color: #f8f8f2;">(</span><span style="color: #f8f8f2;">p</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Title</span><span style="color: #f8f8f2;">(</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">  10</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">      </span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">  11</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">      Body</span><span style="color: #f8f8f2;">(</span>
<span style="color: rgb(64,64,64);">  12</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">        Div</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Class</span><span style="color: #f8f8f2;">(</span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">main</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">)</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Add</span><span style="color: #f8f8f2;">(</span>
<span style="color: rgb(64,64,64);">  13</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">          Sidebar</span><span style="color: #f8f8f2;">{</span><span style="color: #f8f8f2;">}</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">  14</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">          Div</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Class</span><span style="color: #f8f8f2;">(</span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">container shadow bg-1</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">)</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Add</span><span style="color: #f8f8f2;">(</span>
<span style="color: rgb(64,64,64);">  15</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">            Div</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Class</span><span style="color: #f8f8f2;">(</span><span style="color: #ffffff;">&quot;</span><span style="color: #e6db74;">prose</span><span style="color: #ffffff;">&quot;</span><span style="color: #f8f8f2;">)</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Add</span><span style="color: #f8f8f2;">(</span>
<span style="color: rgb(64,64,64);">  16</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">              </span><span style="color: #f8f8f2;">p</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">Content</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">  17</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">            </span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">  18</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">          </span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">  19</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">          AuxFooter</span><span style="color: #f8f8f2;">{</span><span style="color: #f8f8f2;">}</span>
<span style="color: rgb(64,64,64);">  20</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">          Debug</span><span style="color: #f8f8f2;">{</span><span style="color: #f8f8f2;">}</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">  21</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">        </span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">  22</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">      </span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">  23</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">    </span><span style="color: #f8f8f2;">)</span><span style="color: #f8f8f2;">,</span>
<span style="color: rgb(64,64,64);">  24</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">  </span><span style="color: #f8f8f2;">}</span><span style="color: #f92672;">.</span><span style="color: #f8f8f2;">ToNode</span><span style="color: #f8f8f2;">(</span><span style="color: #f8f8f2;">)</span>
<span style="color: rgb(64,64,64);">  25</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">}</span>
<span style="color: rgb(64,64,64);">───────┴───────────────────────────────────────────────────────────────────────────────────</span>
`),
		),
		P(
			Text(`This function is an example of the interface used to turn a component into HTML.  The structs used in the snippet: `),
			Code.Text(`Sidebar`),
			Text(`, `),
			Code.Text(`AuxFooter`),
			Text(`, `),
			Code.Text(`Debug`),
			Text(` implement a similar interface, and because of that `),
			Code.Text(`gel`),
			Text(` accepts them as `),
			Code.Text(`Views`),
			Text(` to `),
			Code.Text(`Add(...)`),
			Text(` functions.`),
		),
		P(
			Text(`Which means pages are `),
			Code.Text(`gel`),
			Text(` Views that can be rendered as HTML.  And since `),
			Code.Text(`gel`),
			Text(` is simply a `),
			Code.Text(`go`),
			Text(` library, I have the full power of `),
			Code.Text(`go`),
			Text(` to output those Views to disc in code.`),
		),
		P(
			Text(`Of course, `),
			Code.Text(`gel`),
			Text(` could be used in a web-server as well and the output could be written to an HTTP Response just like templates.  The downside there, is that your rendering HTML on the server instead of client-side.`),
		),
		P(
			Text(`So, to generate the static site I compile and execute the resulting binary.  I provide some flag parameters to tell it where to write files, the domain for sitemap creation, and specify a directory where to write published posts, etc.  Pretty simple, really.`),
		),
	}

	s3 := Fragment{
		H3.Text(`A Quick Tangent to Talk about Labels`),
		P(
			Text(`Internally, I've decorated pages with labels.  In the future I could query for a set of pages using some kind of `),
			I.Text(`label selector`),
			Text(` and render specifically that set of pages.`),
		),
		P(
			Text(`I find labels better than the `),
			Code.Text(`tag`),
			Text(` concept.  The difference being that labels are a `),
			I.Text(`list`),
			Text(` of key/value pairs and tags are a list of values (text slugs).  With labels I still have tagging, but with better intent and extensibility.`),
		),
		P(
			Text(`The label idea I borrowed from `),
			A.Atts("href", "https://kubernetes.io/docs/concepts/overview/working-with-objects/labels/").Text("Kubernetes Labels and Selectors"),
			Text(`.  My version though has multiple values for a single "key", and because of that I can use "tag" as a key, with multiple values, and so with labels recreate "tagging".`),
		),
		P.Text(`I can also use labels to differentiate "posts" vs "drafts". I can also categorize pages with labels to make views/components that lists that set of pages easily enough.`),
		P(
			Text(`For what it's worth, I've created labels using the `),
			Code.Text(`go`),
			Text(` query-parameter type called `),
			A.Atts("href", "https://pkg.go.dev/net/url#Values").Text("Values"),
			Text(`, and wrapped it within a struct so I can chain calls to `),
			A.Atts("href", "https://pkg.go.dev/net/url#Values.Add").Text("Add"),
			Text(`.`),
		),
	}
	s4 := Fragment{
		H2(
			Text(`Some Improvements to `),
			Code.Text("gel"),
		),
		P(
			Text(`I've found a few things to change in a v2 of the library.  Which is to say that the abstractions may not be intuitive enough.  Mainly the sequence of `),
			Code.Text(`.ToNode().WriteTo(..)`),
			Text(`.`),
		),
	}
	page := Fragment{s1, s2, s3, s4}
	return &HtmlPage{
		Labels: NewLabels().
			Add("title", "Generating HTML with Go").
			Add("area", "aux").
			Add("mime", Mime.Html).
			Add("id", "generating-html-with-go").
			Add("stage", "post"),
		Env:     env,
		Content: page,
	}
}
