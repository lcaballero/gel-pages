package main // import "github.com/lcaballero/gel-pages"

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path"

	cli "github.com/urfave/cli/v2"
)

//go:generate bash ./gen.sh

func HandleList(ctx *cli.Context) error {
	opts := DefaultListOpts(ctx)
	env := DeploymentEnv(opts.Env())
	pages := NewWebFileLookup(opts, env)
	vals := []map[string]interface{}{}
	for _, page := range pages {
		file := path.Join(
			opts.Root(), opts.Posts(), page.ID(),
			"index.html")
		if page.IsRooted() {
			file = path.Join(opts.Root(), page.Location())
		}
		vals = append(vals,
			map[string]interface{}{
				"id":     page.ID(),
				"title":  page.Title(),
				"path":   file,
				"home":   page.IsHome(),
				"rooted": page.IsRooted(),
			})
	}
	bin, err := json.MarshalIndent(vals, "", "  ")
	if err != nil {
		return err
	}
	fmt.Println(string(bin))
	return nil
}

func HandleGen(ctx *cli.Context) error {
	opts := DefaultGenOpts(ctx)
	env := DeploymentEnv(opts.Env())
	pages := NewWebFileLookup(opts, env)
	p, ok := pages[opts.Id()]
	if !ok {
		log.Fatalf("not found id:'%s'", opts.Id())
	}
	if ctx.Bool("debug") {
		p.SetDebug(pages.ToDebug())
	}
	fmt.Println(p.ToNode().String())
	return nil
}

func HandleWrite(ctx *cli.Context) error {
	opts := DefaultWriteOpts(ctx)
	env := DeploymentEnv(opts.Env())
	log.Println(opts.ToJSON(), opts.Env(), env.IsProd(), env.IsDev())
	pages := NewWebFileLookup(opts, env)
	if !opts.HasRoot() {
		return fmt.Errorf("output directory flag: '--root' is required")
	}
	for _, page := range pages {
		dir := path.Join(opts.Root(), opts.Posts(), page.ID())
		if page.IsPost() {
			err := os.MkdirAll(dir, 0755)
			if err != nil {
				return err
			}
			postIndex := path.Join(dir, "index.html")
			err = ioutil.WriteFile(postIndex, []byte(page.Bytes()), 0644)
			if err != nil {
				return err
			}
		}
		if page.IsRooted() {
			index := path.Join(opts.Root(), page.Location())
			err := ioutil.WriteFile(index, []byte(page.Bytes()), 0644)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func main() {
	//Bookmarks()
	GeneratePages()
}

func GeneratePages() {
	procs := Procs{
		DoList:  HandleList,
		DoGen:   HandleGen,
		DoWrite: HandleWrite,
	}
	err := NewCLI(procs).Run(os.Args)
	if err != nil {
		panic(err)
	}
}
