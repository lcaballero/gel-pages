package main

import "strings"

// Labels reference name/value pairs that can be associated with
// assets
type Labels struct {
	pairs map[string][]string
}

// NewLabels returns a new empty instance of Labels
func NewLabels() Labels {
	return Labels{
		pairs: map[string][]string{},
	}
}

// Location accesses of the "location" key and if the key does not
// exist it returns and empty string
func (labels Labels) Location() string {
	v, ok := labels.pairs["location"]
	if !ok || len(v) == 0 {
		return ""
	}
	return strings.TrimSpace(v[0])
}

// Add puts the key value pair in the labels set
func (labels Labels) Add(key, val string) Labels {
	vals, ok := labels.pairs[key]
	if !ok {
		vals = []string{}
	}
	vals = append(vals, val)
	labels.pairs[key] = vals
	return labels
}

func (labels Labels) Vals(key string) []string {
	vals, ok := labels.pairs[key]
	if !ok {
		return []string{}
	}
	return vals
}

func (labels Labels) Val(key string) string {
	vals := labels.Vals(key)
	if len(vals) == 0 {
		return ""
	}
	return vals[0]
}

func (labels Labels) HasVal(key, val string) bool {
	vals := labels.Vals(key)
	for _, v := range vals {
		if v == val {
			return true
		}
	}
	return false
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
	return labels.HasVal("stage", "post")
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
