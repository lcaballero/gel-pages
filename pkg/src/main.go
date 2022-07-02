package main // import "github.com/lcaballero/gel-pages"

import (
	"fmt"
	"log"
	"os"

	"github.com/lcaballero/gel"
)

func PostNotImplementedYet(page string) gel.View {
	log.Fatalf("page not implemented yet: '%s'", page)
	return nil
}

type Pages map[string]Page

func (p Pages) Add(fn func() Page) {
	page := fn()
	p[page.Meta.ID] = page
}

func (p Pages) ToDebug() Debug {
	m := map[string]interface{}{}
	pages := []map[string]interface{}{}
	for id, page := range p {
		pages = append(
			pages,
			map[string]interface{}{
				"id":     id,
				"title":  page.Meta.Title,
				"labels": page.Meta.Labels,
			})
	}
	m["pages"] = pages
	return Debug{
		Out: m,
	}
}

func main() {
	target := os.Args[1]
	pages := Pages{}
	pages.Add(NewPostBeforeIGetStarted)
	pages.Add(NewPostSideProjects)
	pages.Add(NewPostPossibleSiteOrg)
	pages.Add(NewPostCLISnippets)
	pages.Add(NewPostSiteDesignTools)
	pages.Add(NewPostOrganizingPins)
	pages.Add(NewPostFirstStepsOfBuildingThisSite)
	if target == "list" {
		for id, page := range pages {
			fmt.Printf("id: '%s', title: '%s'\n", id, page.Meta.Title)
		}
		return
	}
	p, ok := pages[target]
	if !ok {
		log.Fatalf("target not found: '%s'", target)
	}
	p.Debug = pages.ToDebug()
	fmt.Println(p.ToNode().String())
}
