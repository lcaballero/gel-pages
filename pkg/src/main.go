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
	pages := NewWebFileLookup(opts)
	vals := []map[string]interface{}{}
	for _, page := range pages {
		file := path.Join(
			opts.Root(), opts.Posts(), page.Meta().ID(),
			"index.html")
		if page.Meta().IsRooted() {
			file = path.Join(opts.Root(), page.Meta().Location())
		}
		vals = append(vals,
			map[string]interface{}{
				"id":     page.Meta().ID(),
				"title":  page.Meta().Title,
				"path":   file,
				"home":   page.Meta().IsHome(),
				"rooted": page.Meta().IsRooted(),
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
	pages := NewWebFileLookup(opts)
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
	pages := NewWebFileLookup(opts)
	if !opts.HasRoot() {
		return fmt.Errorf("output directory flag: '--root' is required")
	}
	for _, page := range pages {
		dir := path.Join(opts.Root(), opts.Posts(), page.Meta().ID())
		if page.Meta().IsPost() {
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
		if page.Meta().IsRooted() {
			index := path.Join(opts.Root(), page.Meta().Location())
			err := ioutil.WriteFile(index, []byte(page.Bytes()), 0644)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func main() {
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
