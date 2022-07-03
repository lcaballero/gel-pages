package main

import (
	. "github.com/lcaballero/gel"
)

func NewPostFirstStepsOfBuildingThisSite() *HtmlPage {
	return &HtmlPage{
		PageMeta: PageMeta{
			Title: "First Steps I Did When Building This Site",
			Labels: Labels{
				"area":  "draft",
				"mime":  Mime.Html,
				"id":    "first-steps-of-building-this-site",
				"stage": "post",
			},
		},
		Content: Div(
			H2.Text(`The Steps I Did When Building This Site`),
			P.Text(`This is a short series where I write up how to setup a basic static site. Not quite sure how many parts will go into this series. This article is the first, and it is very basic.`),
			P.Text(`In this article I start with a script to generate a directory where the site can be served locally in development.`),
			P(
				Text(`Future articles will include getting a `),
				XLink("https://www.digitalocean.com/", "DigitalOcean"),
				Text(` droplet setup. Configure `),
				XLink("https://www.nginx.com/", "NGINX"),
				Text(` and using `),
				XLink("https://letsencrypt.org/", "Let's Encrypt"),
				Text(` to add TLS.`),
			),
			P.Text(`Along the way I'll show some of the ways I secure the server and make it easier for solo development with a few helper scripts.`),
			H3.Text(`Some background`),
			P.Text(`When I started this site I wanted to do everything minimal and uncommon. I also wanted to avoid frameworks becasue I didn't want them to hide how things actually run.`),
			P.Text(`That's not to say these initial steps aren't useful. They are still useful because they record how I might go about setting up a second site, or even show someone how to do it themselves.`),
			P.Text(`And it's more difficult to do something from scratch. So, this write-up isn't something to reach for when you don't have time or are uninterested in some of the low level details.`),
			P.Text(`You could always pay for WordPress or SquareSpace, and build pages there. New CMS services come out all the time and will get you further in a shorter amount of time.`),
			H3.Text(`Step 0 (starting with a shell script)`),
			P.Text(`I like using v0 (or v0.0.1) to number the early releases. It make me laugh and reminds me of Kung Fu Panda: "There is now a level zero."`),
			P(
				Text(`Here's what I mean: I start with this shell script I make executable named `),
				Code.Text("run.sh"),
				Text(`, which does almost nothing.`),
			),
			Text(`
<div class="code">
<pre class="tree"><span style="color: rgb(64,64,64);">───────┼───────────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #7c7865;">#</span><span style="color: #7c7865;">!/bin/bash</span>
<span style="color: rgb(64,64,64);">   2</span>   <span style="color: rgb(64,64,64);">│</span>
<span style="color: rgb(64,64,64);">   3</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #a6e22e;">build</span><span style="color: #ffffff;">(</span><span style="color: #ffffff;">)</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">{</span>
<span style="color: rgb(64,64,64);">   4</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">    </span><span style="color: #66d9ef;">echo</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">"</span><span style="color: #e6db74;">Hello, World!</span><span style="color: #ffffff;">"</span>
<span style="color: rgb(64,64,64);">   5</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">}</span>
<span style="color: rgb(64,64,64);">   6</span>   <span style="color: rgb(64,64,64);">│</span>
<span style="color: rgb(64,64,64);">   7</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">"</span><span style="color: #ffffff;">$</span><span style="color: #ffffff;">@</span><span style="color: #ffffff;">"</span>
<span style="color: rgb(64,64,64);">───────┴───────────────────────────────────────────────────────────────────────────────</span>
</pre></div>
`),
			P.Text(`It prints 'Hello, World!' to the terminal when ran from the command. Here is how you can run it at the command line:`),
			Text(`<div class="code">
<pre class="tree"><span style="color: rgb(64,64,64);">───────┼───────────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">$</span><span style="color: #ffffff;"> ./run.sh build</span>
<span style="color: rgb(64,64,64);">───────┴───────────────────────────────────────────────────────────────────────────────</span>
</pre></div>`),
			P(
				Text(`You might write a bash script for each task. One for converting index.org to index.html, another for generating a sitemap, and one to copy static assets to the deployment `),
				Code.Text(".dist/"),
				Text(` directory.`),
			),
			P(
				Code.Text("run.sh"),
				Text(` gives me an easy way to consolidate what would be multiple scripts into a single file. I can just add more functions and call them like `),
				Code.Text("build"),
				Text(` as shown above or use many of them from some other function in the script, and call that new function like `),
				Code.Text("build"),
				Text(`.`),
			),
			P(
				Text(`Sharing code between shell script tends to be a bit troublesome. That's just one of things about `),
				Code.Text("bash"),
				Text(` tend to like this approach so long as it doesn't get too long.`),
			),
			P.Text(`(Of course, maybe that's a great reason why I should have used something else instead of Bash. Ugh, oh-well, I'll change that later).`),
			P.Text(`The next step is to generate some html using this shiny new script and serve up some fresh static content.`),
			H3.Text(`Step 1 (minimal starter index.html page)`),
			P.Text(`I need a web server. After all, the end goal is too run the serve a static site. But before that I need something to serve up.`),
			P(
				Text(`I've updated `),
				Code.Text("build"),
				Text(` to output `),
				Code.Text("index.html"),
				Text(` to `),
				Code.Text(".dist/"),
				Text(`. I will overhaul this later, but for right now this will produce a page I can refactor and improved.`),
			),
			Text(`<div class="code">
<pre class="tree"><span style="color: rgb(64,64,64);">───────┼───────────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #7c7865;">#</span><span style="color: #7c7865;">!/bin/bash</span>
<span style="color: rgb(64,64,64);">   2</span>   <span style="color: rgb(64,64,64);">│</span>
<span style="color: rgb(64,64,64);">   3</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #a6e22e;">build</span><span style="color: #ffffff;">(</span><span style="color: #ffffff;">)</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">{</span>
<span style="color: rgb(64,64,64);">   4</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">    </span><span style="color: #ffffff;">mkdir</span><span style="color: #fd971f;"> -</span><span style="color: #fd971f;">p</span><span style="color: #ffffff;"> .dist</span>
<span style="color: rgb(64,64,64);">   5</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">    </span><span style="color: #ffffff;">cat</span><span style="color: #ffffff;"> </span><span style="color: #f92672;">&lt;&lt;</span><span style="color: #f92672;">EOF</span><span style="color: #ffffff;"> </span><span style="color: #f92672;">&gt;</span><span style="color: #ffffff;"> .dist/index.html</span>
<span style="color: rgb(64,64,64);">   6</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">&lt;!doctype html&gt;</span>
<span style="color: rgb(64,64,64);">   7</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">&lt;html lang="en"&gt;</span>
<span style="color: rgb(64,64,64);">   8</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">  &lt;head&gt;</span>
<span style="color: rgb(64,64,64);">   9</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">    &lt;title&gt;Home&lt;/title&gt;</span>
<span style="color: rgb(64,64,64);">  10</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">  &lt;/head&gt;</span>
<span style="color: rgb(64,64,64);">  11</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">  &lt;body&gt;</span>
<span style="color: rgb(64,64,64);">  12</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">    &lt;h1&gt;Home Page&lt;/h1&gt;</span>
<span style="color: rgb(64,64,64);">  13</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">  &lt;/body&gt;</span>
<span style="color: rgb(64,64,64);">  14</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">&lt;/html&gt;</span>
<span style="color: rgb(64,64,64);">  15</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f92672;">EOF</span>
<span style="color: rgb(64,64,64);">  16</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">}</span>
<span style="color: rgb(64,64,64);">  17</span>   <span style="color: rgb(64,64,64);">│</span>
<span style="color: rgb(64,64,64);">  18</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">"</span><span style="color: #ffffff;">$</span><span style="color: #ffffff;">@</span><span style="color: #ffffff;">"</span>
<span style="color: rgb(64,64,64);">───────┴───────────────────────────────────────────────────────────────────────────────</span>
</pre></div>`),
			H3.Text(`Step 3 (serving index.html)`),
			P(
				Text(`For local dev I've decided to use `),
				XLink("https://caddyserver.com/", "Caddy"),
				Text(`, to serve the content out of the `),
				Code.Text(".dist/"),
				Text(` folder.`),
			),
			P(
				Text(`I follow up that with the `),
				Code.Text("open"),
				Text(` command (MacOS) to open a browser.`),
			),
			P.Text(`These are the commands I use to stand up the dev server.`),
			Text(`<div class="code">
<pre class="tree"><span style="color: rgb(64,64,64);">───────┼───────────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">$</span><span style="color: #ffffff;"> ulimit</span><span style="color: #fd971f;"> -</span><span style="color: #fd971f;">n</span><span style="color: #ffffff;"> 8192</span><span style="color: #ffffff;"> </span><span style="color: #f92672;">&amp;&amp;</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">caddy</span><span style="color: #fd971f;"> -</span><span style="color: #fd971f;">root</span><span style="color: #ffffff;"> .dist</span><span style="color: #fd971f;"> -</span><span style="color: #fd971f;">port</span><span style="color: #ffffff;"> 9090 </span><span style="color: #be84ff;">2</span><span style="color: #f92672;">&gt;&amp;</span><span style="color: #be84ff;">1</span><span style="color: #ffffff;"> </span><span style="color: #f92672;">&gt;</span><span style="color: #ffffff;"> /dev/null</span><span style="color: #ffffff;"> </span><span style="color: #f92672;">&amp;</span>
<span style="color: rgb(64,64,64);">   2</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">$</span><span style="color: #ffffff;"> ./run.sh build</span><span style="color: #ffffff;"> </span><span style="color: #f92672;">&amp;&amp;</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">open</span><span style="color: #ffffff;"> http://localhost:9090</span>
<span style="color: rgb(64,64,64);">───────┴───────────────────────────────────────────────────────────────────────────────</span>
</pre></div>`),
			P.Text(`When I come back to update how the site is generated I just run the last command in the terminal: And bam! Google Chrome opens to the home page (since that's my default browser).`),
			H3.Text(`Step 3 (clean up and gitignore)`),
			P(
				Text(`I like to add a `),
				Code.Text("clean"),
				Text(` function to remove any generated files. Also, I keep my source in git even if I'm not going to push it to GitHub right away.`),
			),
			P(
				Text(`To do that I add a `),
				Code.Text("clean"),
				Text(` function, and a `),
				Code.Text(".gitignore"),
				Text(` file. When I add more utility scripts, I plan to use `),
				XLink("https://direnv.net/", "direnv"),
				Text(` and a `),
				Code.Text(".envrc"),
				Text(` file to keep things tidy.`),
			),
			P(
				Text(`The site consists of one directory and a single html file. So, removing the `),
				Code.Text(".dist/"),
				Text(` directory is the only thing that `),
				Code.Text("clean"),
				Text(` needs to do.`),
			),
			P(
				Text(`Also, might as well make the `),
				Code.Text("build"),
				Text(` first run `),
				Code.Text("clean"),
				Text(` to generate everything from scratch. Below is the full `),
				Code.Text("run.sh"),
				Text(` script with `),
				Code.Text("clean"),
				Text(` and with `),
				Code.Text("build"),
				Text(` updated.`),
			),
			P(
				Text(`Also `),
				Code.Text("run.sh"),
				Text(` has a few dev helpers 1) for starting Caddy. And 2) a call to `),
				Code.Text("open"),
				Text(` a browser to the local web server.`),
			),
			Text(`<div class="code">
<pre class="tree"><span style="color: rgb(64,64,64);">───────┼───────────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #7c7865;">#</span><span style="color: #7c7865;">!/bin/bash</span>
<span style="color: rgb(64,64,64);">   2</span>   <span style="color: rgb(64,64,64);">│</span>
<span style="color: rgb(64,64,64);">   3</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #7c7865;">#</span><span style="color: #7c7865;"> build provisions a directory to hold static site artifacts</span>
<span style="color: rgb(64,64,64);">   4</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #a6e22e;">build</span><span style="color: #ffffff;">(</span><span style="color: #ffffff;">)</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">{</span>
<span style="color: rgb(64,64,64);">   5</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">    </span><span style="color: #ffffff;">mkdir</span><span style="color: #fd971f;"> -</span><span style="color: #fd971f;">p</span><span style="color: #ffffff;"> .dist</span>
<span style="color: rgb(64,64,64);">   6</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">    </span><span style="color: #ffffff;">cat</span><span style="color: #ffffff;"> </span><span style="color: #f92672;">&lt;&lt;</span><span style="color: #f92672;">EOF</span><span style="color: #ffffff;"> </span><span style="color: #f92672;">&gt;</span><span style="color: #ffffff;"> .dist/index.html</span>
<span style="color: rgb(64,64,64);">   7</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">&lt;!doctype html&gt;</span>
<span style="color: rgb(64,64,64);">   8</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">&lt;html lang="en"&gt;</span>
<span style="color: rgb(64,64,64);">   9</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">  &lt;head&gt;</span>
<span style="color: rgb(64,64,64);">  10</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">    &lt;title&gt;Home&lt;/title&gt;</span>
<span style="color: rgb(64,64,64);">  11</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">  &lt;/head&gt;</span>
<span style="color: rgb(64,64,64);">  12</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">  &lt;body&gt;</span>
<span style="color: rgb(64,64,64);">  13</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">    &lt;h1&gt;Home Page&lt;/h1&gt;</span>
<span style="color: rgb(64,64,64);">  14</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">  &lt;/body&gt;</span>
<span style="color: rgb(64,64,64);">  15</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #e6db74;">&lt;/html&gt;</span>
<span style="color: rgb(64,64,64);">  16</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f92672;">EOF</span>
<span style="color: rgb(64,64,64);">  17</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">}</span>
<span style="color: rgb(64,64,64);">  18</span>   <span style="color: rgb(64,64,64);">│</span>
<span style="color: rgb(64,64,64);">  19</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #7c7865;">#</span><span style="color: #7c7865;"> serve starts caddy in the background to serve static content from</span>
<span style="color: rgb(64,64,64);">  20</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #7c7865;">#</span><span style="color: #7c7865;"> .dist/</span>
<span style="color: rgb(64,64,64);">  21</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #a6e22e;">serve</span><span style="color: #ffffff;">(</span><span style="color: #ffffff;">)</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">{</span>
<span style="color: rgb(64,64,64);">  22</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">    </span><span style="color: #66d9ef;">ulimit</span><span style="color: #ffffff;"> </span><span style="color: #fd971f;">-</span><span style="color: #fd971f;">n</span><span style="color: #ffffff;"> 8192</span><span style="color: #ffffff;"> </span><span style="color: #f92672;">&amp;&amp;</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">\</span>
<span style="color: rgb(64,64,64);">  23</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">        </span><span style="color: #ffffff;">caddy</span><span style="color: #fd971f;"> -</span><span style="color: #fd971f;">root</span><span style="color: #ffffff;"> .dist</span><span style="color: #fd971f;"> -</span><span style="color: #fd971f;">port</span><span style="color: #ffffff;"> 9090 </span><span style="color: #be84ff;">2</span><span style="color: #f92672;">&gt;&amp;</span><span style="color: #be84ff;">1</span><span style="color: #ffffff;"> </span><span style="color: #f92672;">&gt;</span><span style="color: #ffffff;"> /dev/null</span><span style="color: #ffffff;"> </span><span style="color: #f92672;">&amp;</span>
<span style="color: rgb(64,64,64);">  24</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">}</span>
<span style="color: rgb(64,64,64);">  25</span>   <span style="color: rgb(64,64,64);">│</span>
<span style="color: rgb(64,64,64);">  26</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #7c7865;">#</span><span style="color: #7c7865;"> browse opens the default browser on mac to the local dev server</span>
<span style="color: rgb(64,64,64);">  27</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #a6e22e;">browse</span><span style="color: #ffffff;">(</span><span style="color: #ffffff;">)</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">{</span>
<span style="color: rgb(64,64,64);">  28</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">    </span><span style="color: #ffffff;">open</span><span style="color: #ffffff;"> http://localhost:9090/</span>
<span style="color: rgb(64,64,64);">  29</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">}</span>
<span style="color: rgb(64,64,64);">  30</span>   <span style="color: rgb(64,64,64);">│</span>
<span style="color: rgb(64,64,64);">  31</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #7c7865;">#</span><span style="color: #7c7865;"> clean removes the location where generated files a assembled</span>
<span style="color: rgb(64,64,64);">  32</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #a6e22e;">clean</span><span style="color: #ffffff;">(</span><span style="color: #ffffff;">)</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">{</span>
<span style="color: rgb(64,64,64);">  33</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">    </span><span style="color: #ffffff;">rm</span><span style="color: #fd971f;"> -</span><span style="color: #fd971f;">rf</span><span style="color: #ffffff;"> .dist</span>
<span style="color: rgb(64,64,64);">  34</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">}</span>
<span style="color: rgb(64,64,64);">  35</span>   <span style="color: rgb(64,64,64);">│</span>
<span style="color: rgb(64,64,64);">  36</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">"</span><span style="color: #ffffff;">$</span><span style="color: #ffffff;">@</span><span style="color: #ffffff;">"</span>
<span style="color: rgb(64,64,64);">───────┴───────────────────────────────────────────────────────────────────────────────</span>
</pre></div>`),
			P(
				Text(`Since `),
				Code.Text(".dist/"),
				Text(` is an artifact of the build process it's a good idea to add it to a `),
				Code.Text(".gitignore"),
				Text(` too. Something like this works fine:`),
			),
			Text(`<div class="code">
<pre class="tree"><span style="color: rgb(64,64,64);">───────┼───────────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">.DS_Store</span>
<span style="color: rgb(64,64,64);">   2</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">.dist</span>
<span style="color: rgb(64,64,64);">───────┴───────────────────────────────────────────────────────────────────────────────</span>
</pre></div>`),
			P(
				Text(`Later on, I imagine the `),
				Code.Text(".gitignore"),
				Text(` file will grow to ignore more things.`),
			),
			P.Text(`And now this is the complete project directory:`),
			Text(`<div class="code"><pre class="tree"><span style="color: rgb(64,64,64);">───────┼───────────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">proj</span>
<span style="color: rgb(64,64,64);">   2</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">├── .dist</span>
<span style="color: rgb(64,64,64);">   3</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">│&nbsp;&nbsp; └── index.html</span>
<span style="color: rgb(64,64,64);">   4</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">├── .gitignore</span>
<span style="color: rgb(64,64,64);">   5</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">└── run.sh</span>
<span style="color: rgb(64,64,64);">   6</span>   <span style="color: rgb(64,64,64);">│</span>
<span style="color: rgb(64,64,64);">   7</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #f8f8f2;">1 directory, 3 files</span>
<span style="color: rgb(64,64,64);">───────┴───────────────────────────────────────────────────────────────────────────────</span>
</pre></div>`),
		),
	}
}
