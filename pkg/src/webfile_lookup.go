package main

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

type Environment interface {
	IsProd() bool
	IsDev() bool
}

type DeploymentEnv string

func (e DeploymentEnv) IsProd() bool {
	return string(e) == "prod"
}

func (e DeploymentEnv) IsDev() bool {
	return !e.IsProd()
}

// NewWebFileLookup creates a lookup using the Locator to make
// sitemap(s)
func NewWebFileLookup(loc Locator, env Environment) WebFileLookup {
	pages := WebFileLookup{}
	pages.Add(NewPostBeforeIGetStarted(env))
	pages.Add(NewPostSideProjects(env))
	pages.Add(NewPostPossibleSiteOrg(env))
	pages.Add(NewPostCLISnippets(env))
	pages.Add(NewPostSiteDesignTools(env))
	pages.Add(NewPostOrganizingPins(env))
	pages.Add(NewPostFirstStepsOfBuildingThisSite(env))
	pages.Add(NewGeneratingHtmlWithGo(env))

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
	p[page.ID()] = page
}

func (p WebFileLookup) ToDebug() Debug {
	m := map[string]interface{}{}
	pages := []map[string]interface{}{}
	for _, page := range p {
		pages = append(
			pages,
			map[string]interface{}{
				"id":     page.ID(),
				"title":  page.Title(),
				"labels": page.GetLabels(),
			})
	}
	m["pages"] = pages
	return Debug{Out: m}
}
