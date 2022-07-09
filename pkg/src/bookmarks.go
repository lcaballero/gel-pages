package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
)

type ChromeBM struct {
	Checksum string `json:"checksum"`
	Roots    Roots  `json:"roots"`
}

type Roots struct {
	BookmarkBar BookmarkBar `json:"bookmark_bar"`
}

type BookmarkBar struct {
	Children Children `json:"children"`
}

type MetaInfo struct {
	LastVisitedDesktop uint64
}

type Children []Kid

type Kid struct {
	DateAdded    string   `json:"date_added"`
	DateModified string   `json:"date_modified"`
	Guid         string   `json:"guid"`
	ID           string   `json:"id"`
	MetaInfo     MetaInfo `json:"meta_info"`
	Name         string   `json:"name"`
	Type         string   `json:"type"`
	URL          string   `json:"url"`
	Children     Children `json:"children"`
}

func (w Children) Walk(up, down func(), fn func(kid Kid, err error) error) {
	for _, k := range w {
		err := fn(k, nil)
		if err != nil {
			panic(err)
		}
		if len(k.Children) > 0 {
			up()
			k.Children.Walk(up, down, fn)
			down()
		}
	}
}

func Bookmarks() {
	bin, err := ioutil.ReadFile("bookmarks.json")
	if err != nil {
		panic(err)
	}

	bm := &ChromeBM{}
	err = json.Unmarshal(bin, bm)
	if err != nil {
		panic(err)
	}
	level := 0
	tab := "    "
	indent := func(n int) string {
		buf := bytes.NewBufferString("")
		for i := 0; i < n; i++ {
			buf.WriteString(tab)
		}
		return buf.String()
	}
	up := func() {
		level++
	}
	down := func() {
		level--
	}
	fn := func(k Kid, err error) error {
		//fmt.Printf("level: %d\n", level)
		switch k.Type {
		case "folder":
			fmt.Printf(
				"%s/%s (%d)\n",
				indent(level), k.Name, len(k.Children))
		case "url":
			name := k.Name
			if len(name) == 0 {
				name = k.URL
			}
			fmt.Printf(
				"%s- %s\n",
				indent(level), name)
		}
		return nil
	}

	bm.Roots.BookmarkBar.Children.Walk(up, down, fn)
}
