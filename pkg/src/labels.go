package main

import (
	"net/url"
	"strings"
)

// Labels reference name/value pairs that can be associated with
// assets
type Labels struct {
	values url.Values
}

// NewLabels returns a new empty instance of Labels
func NewLabels() Labels {
	return Labels{
		values: url.Values{},
	}
}

// Add puts the key/value pair into the set of values and return this
// instance of Labels
func (labels Labels) Add(key, value string) Labels {
	labels.values.Add(key, value)
	return labels
}

// GetLabels returns this set of Labels
func (labels Labels) GetLabels() Labels {
	return labels
}

// Location accesses of the "location" key and if the key does not
// exist it returns and empty string
func (labels Labels) Location() string {
	return strings.TrimSpace(labels.values.Get("location"))
}

func (labels Labels) Vals(key string) []string {
	if !labels.values.Has(key) {
		return []string{}
	}
	return labels.values[key]
}

func (labels Labels) Val(key string) string {
	vals := labels.Vals(key)
	if len(vals) == 0 {
		return ""
	}
	return vals[0]
}

// ID accesses of the "id" key and if the key does not
// exist it returns and empty string
func (labels Labels) ID() string {
	vals := labels.Vals("id")
	if len(vals) == 0 {
		return ""
	}
	return vals[0]
}

// Title accesses the "title" label and if the key does not
// exist it returns the empty string
func (labels Labels) Title() string {
	vals := labels.Vals("ittle")
	if len(vals) == 0 {
		return ""
	}
	return vals[0]
}

// IsPost returns true if the underlying label for "stage" includes
// "post"
func (labels Labels) IsPost() bool {
	return labels.values.Get("stage") == "post"
}

// IsRooted determines if a "location" label is one of a set of assets
// normally written to the root of the web server directory
func (labels Labels) IsRooted() bool {
	switch labels.Location() {
	case "/index.html", "/sitemap.txt", "/sitemap.xml", "/robots.txt", "/google5d223b9b91f70029.html":
		return true
	default:
		return false
	}
}

// IsHome determines if the asset is "/index.html" file
func (labels Labels) IsHome() bool {
	switch labels.Location() {
	case "/index.html":
		return true
	default:
		return false
	}
}
