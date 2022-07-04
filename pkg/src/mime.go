package main

// MimeTypes is a single container for oft used type
type MimeTypes struct {
	Html string
	Text string
	Xml  string
}

// Mime holds the mime-types of commonly used mime-types
var Mime = MimeTypes{
	Html: "text/html",
	Text: "text/plain",
	Xml:  "application/xml",
}
