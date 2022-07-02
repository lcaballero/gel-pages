package main

import (
	"log"

	"github.com/lcaballero/gel"
)

func PostNotImplementedYet(page string) gel.View {
	log.Fatalf("page not implemented yet: '%s'", page)
	return nil
}

type Pages map[string]Page

func NewPages() Pages {
	pages := Pages{}
	pages.Add(NewPostBeforeIGetStarted)
	pages.Add(NewPostSideProjects)
	pages.Add(NewPostPossibleSiteOrg)
	pages.Add(NewPostCLISnippets)
	pages.Add(NewPostSiteDesignTools)
	pages.Add(NewPostOrganizingPins)
	pages.Add(NewPostFirstStepsOfBuildingThisSite)
	return pages
}

func (p Pages) Add(fn func() Page) {
	page := fn()
	p[page.Meta().ID] = page
}

func (p Pages) ToDebug() Debug {
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
