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
	pages := NewPages()
	opts := DefaultListOpts(ctx)
	vals := []map[string]interface{}{}
	for _, page := range pages {
		vals = append(vals,
			map[string]interface{}{
				"id":    page.Meta().ID,
				"title": page.Meta().Title,
				"path":  path.Join(opts.Root(), page.Meta().ID(), "index.html"),
				"home":  opts.Home() == page.Meta().ID(),
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
	pages := NewPages()
	opts := DefaultGenOpts(ctx)
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
	pages := NewPages()
	if !opts.HasRoot() {
		return fmt.Errorf("output directory flag: '--root' is required")
	}
	hasWritenHomePage := false
	for _, page := range pages {
		dir := path.Join(opts.Root(), opts.Posts(), page.Meta().ID())
		rootIndex := path.Join(opts.Root(), "index.html")
		postIndex := path.Join(opts.Root(), opts.Posts(), page.Meta().ID(), "index.html")
		err := os.MkdirAll(dir, 0755)
		if err != nil {
			return err
		}
		bin := page.Bytes()
		err = ioutil.WriteFile(postIndex, []byte(bin), 0644)
		if err != nil {
			return err
		}
		if hasWritenHomePage {
			continue
		}
		if opts.HasHome() && page.Meta().ID() != opts.Home() {
			continue
		}
		err = ioutil.WriteFile(rootIndex, []byte(bin), 0644)
		if err != nil {
			return err
		}
		hasWritenHomePage = true
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
