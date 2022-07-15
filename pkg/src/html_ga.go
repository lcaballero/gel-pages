package main

import (
	. "github.com/lcaballero/gel"
)

func NewGoogleAnalyticsScriptTags(env Environment) View {
	if env.IsDev() {
		return None()
	}
	return Text(`
   <!-- Global site tag (gtag.js) - Google Analytics -->
   <script async src="https://www.googletagmanager.com/gtag/js?id=G-YFZDVRTS83"></script>
   <script>
       window.dataLayer = window.dataLayer || [];
       function gtag(){dataLayer.push(arguments);}
       gtag('js', new Date());

       gtag('config', 'G-YFZDVRTS83');
   </script>
   <script>console.log("ga added");</script>
`)

}

func NewPostGoogleAnalytics() *TextPage {
	return &TextPage{
		Labels: NewLabels().
			Add("title", "google-analytics").
			Add("area", "analytics").
			Add("mime", Mime.Html).
			Add("id", "google5d223b9b91f70029.html").
			Add("location", "/google5d223b9b91f70029.html"),
		Content: Text("google-site-verification: google5d223b9b91f70029.html"),
	}
}
