package main

// Labels reference name/value pairs that can be associated with
// assets
type Labels map[string]string

// Location accesses of the "location" key and if the key does not
// exist it returns and empty string
func (labels Labels) Location() string {
	return labels["location"]
}

// ID accesses of the "id" key and if the key does not
// exist it returns and empty string
func (labels Labels) ID() string {
	return labels["id"]
}

// IsPost returns true if the underlying label for "stage" includes
// "post"
func (labels Labels) IsPost() bool {
	val, ok := labels["stage"]
	return ok && val == "post"
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
