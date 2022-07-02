package main

import (
	. "github.com/lcaballero/gel"
)

func NewPostCLISnippets() *HtmlPage {
	return &HtmlPage{
		PageMeta: PageMeta{
			ID:    "cli-snippets",
			Title: "CLI Snippet",
			Labels: Labels{
				"area": "header",
				"mime": "text/html",
			},
		},
		Content: Div(
			H2.Text("Snippets"),
			Text(`
<div class="code">
<textarea class="snippet" id="snippet-1">kubectl get pods -A --no-headers -o yaml | grep -v Running
</textarea>
<button class="btn" type="button" data-clipboard-action="copy" data-clipboard-target="#snippet-1"><img class="clippy" width="13" src="/img/clippy.svg" alt="Copy to Clipboard"></button>
<pre class="tree"><span style="color: rgb(64,64,64);">───────┬────────────────────────────────────────────────────────────────────────</span>
       <span style="color: rgb(64,64,64);">│ </span><span style="font-weight:bold;">STDIN</span>
<span style="color: rgb(64,64,64);">───────┼────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #a6e22e;">snippet.1</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">(</span><span style="color: #ffffff;">)</span><span style="color: #ffffff;"> </span>
<span style="color: rgb(64,64,64);">   2</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">{</span><span style="color: #ffffff;"> </span>
<span style="color: rgb(64,64,64);">   3</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">    </span><span style="color: #ffffff;">kubectl</span><span style="color: #ffffff;"> get pods</span><span style="color: #fd971f;"> -</span><span style="color: #fd971f;">A</span><span style="color: #fd971f;"> --</span><span style="color: #fd971f;">no-headers</span><span style="color: #fd971f;"> -</span><span style="color: #fd971f;">o</span><span style="color: #ffffff;"> yaml</span><span style="color: #ffffff;"> </span><span style="color: #f92672;">|</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">grep</span><span style="color: #fd971f;"> -</span><span style="color: #fd971f;">v</span><span style="color: #ffffff;"> Running</span>
<span style="color: rgb(64,64,64);">   4</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">}</span>
<span style="color: rgb(64,64,64);">───────┴────────────────────────────────────────────────────────────────────────</span>
</pre></div>
`),
			Text(`
<div class="code">
<textarea class="snippet" id="snippet-2">echo -e ${PATH//:/\\n}
</textarea>
<button class="btn" type="button" data-clipboard-action="copy" data-clipboard-target="#snippet-2"><img class="clippy" width="13" src="/img/clippy.svg" alt="Copy to Clipboard"></button>
<pre class="tree"><span style="color: rgb(64,64,64);">───────┬────────────────────────────────────────────────────────────────────────</span>
       <span style="color: rgb(64,64,64);">│ </span><span style="font-weight:bold;">STDIN</span>
<span style="color: rgb(64,64,64);">───────┼────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #a6e22e;">snippet.2</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">(</span><span style="color: #ffffff;">)</span><span style="color: #ffffff;"> </span>
<span style="color: rgb(64,64,64);">   2</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">{</span><span style="color: #ffffff;"> </span>
<span style="color: rgb(64,64,64);">   3</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">    </span><span style="color: #66d9ef;">echo</span><span style="color: #ffffff;"> </span><span style="color: #fd971f;">-</span><span style="color: #fd971f;">e</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">$</span><span style="color: #ffffff;">{</span><span style="color: #ffffff;">PATH</span><span style="color: #f92672;">/</span><span style="color: #fd971f;">/</span><span style="color: #ffffff;">:</span><span style="color: #f92672;">/</span><span style="color: #be84ff;">\\</span><span style="color: #ffffff;">n</span><span style="color: #ffffff;">}</span>
<span style="color: rgb(64,64,64);">   4</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">}</span>
<span style="color: rgb(64,64,64);">───────┴────────────────────────────────────────────────────────────────────────</span>
</pre></div>
`),
			Text(`
<div class="code">
<textarea class="snippet" id="snippet-3">kubectl drain --ignore-daemonsets --delete-local-data $nodename
</textarea>
<button class="btn" type="button" data-clipboard-action="copy" data-clipboard-target="#snippet-3"><img class="clippy" width="13" src="/img/clippy.svg" alt="Copy to Clipboard"></button>
<pre class="tree"><span style="color: rgb(64,64,64);">───────┬────────────────────────────────────────────────────────────────────────</span>
       <span style="color: rgb(64,64,64);">│ </span><span style="font-weight:bold;">STDIN</span>
<span style="color: rgb(64,64,64);">───────┼────────────────────────────────────────────────────────────────────────</span>
<span style="color: rgb(64,64,64);">   1</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #a6e22e;">snippet.3</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">(</span><span style="color: #ffffff;">)</span><span style="color: #ffffff;"> </span>
<span style="color: rgb(64,64,64);">   2</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">{</span><span style="color: #ffffff;"> </span>
<span style="color: rgb(64,64,64);">   3</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">    </span><span style="color: #ffffff;">kubectl</span><span style="color: #ffffff;"> drain</span><span style="color: #fd971f;"> --</span><span style="color: #fd971f;">ignore-daemonsets</span><span style="color: #fd971f;"> --</span><span style="color: #fd971f;">delete-local-data</span><span style="color: #ffffff;"> </span><span style="color: #ffffff;">$</span><span style="color: #ffffff;">nodename</span>
<span style="color: rgb(64,64,64);">   4</span>   <span style="color: rgb(64,64,64);">│</span> <span style="color: #ffffff;">}</span>
<span style="color: rgb(64,64,64);">───────┴────────────────────────────────────────────────────────────────────────</span>
</pre></div>
`),
		),
	}
}
