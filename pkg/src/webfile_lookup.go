package main

import (
	"log"

	"github.com/lcaballero/gel"
)

func PostNotImplementedYet(page string) gel.View {
	log.Fatalf("page not implemented yet: '%s'", page)
	return nil
}

// Locator provides the directory information used to write WebFiles
// to disk
type Locator interface {
	// Root is the output directory where ALL WebFiles will be written
	// to either this directory or some sub-directory
	Root() string

	// Posts is a sub directory where published posts will be written
	Posts() string

	// Base is the protocol and hostname of the site used to create
	// sitemap references
	Base() string
}

// WebFileLookup maps the IDs of the WebFile to the WebFile itself
type WebFileLookup map[string]WebFile

// NewWebFileLookup creates a lookup using the Locator to make
// sitemap(s)
func NewWebFileLookup(loc Locator) WebFileLookup {
	pages := WebFileLookup{}
	pages.Add(NewPostBeforeIGetStarted())
	pages.Add(NewPostSideProjects())
	pages.Add(NewPostPossibleSiteOrg())
	pages.Add(NewPostCLISnippets())
	pages.Add(NewPostSiteDesignTools())
	pages.Add(NewPostOrganizingPins())
	pages.Add(NewPostFirstStepsOfBuildingThisSite())

	// These require page names and path information
	site0 := NewTextSitemap(loc, pages)
	site1 := NewXmlSitemap(loc, pages)
	robot := NewTextRobots()
	ga := NewPostGoogleAnalytics()

	pages.Add(site0)
	pages.Add(site1)
	pages.Add(robot)
	pages.Add(ga)
	return pages
}

// Add includes the WebFile using the WebFile ID as the lookup key
func (p WebFileLookup) Add(page WebFile) {
	p[page.Meta().ID()] = page
}

func (p WebFileLookup) ToDebug() Debug {
	m := map[string]interface{}{}
	pages := []map[string]interface{}{}
	for id, page := range p {
		pages = append(
			pages,
			map[string]interface{}{
				"id":     id,
				"title":  page.Meta().Title,
				"labels": page.Meta().Labels,
			})
	}
	m["pages"] = pages
	return Debug{Out: m}
}
