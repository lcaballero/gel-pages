package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func Test_Labels_IsRooted(t *testing.T) {
	cases := []struct {
		msg      string
		labels   *Labels
		expected bool
		isHome   bool
	}{
		{
			msg:    "empty not one of the rooted locations",
			labels: NewLabels(),
		},
		{
			msg:      "home page is rooted file",
			labels:   NewLabels().Add("location", "/index.html"),
			expected: true,
			isHome:   true,
		},
		{
			msg:      "robots is rooted file",
			labels:   NewLabels().Add("location", "/robots.txt"),
			expected: true,
		},
		{
			msg:    "not in the set of rooted files",
			labels: NewLabels().Add("location", "/some-home.html"),
		},
	}
	for _, c := range cases {
		t.Run(c.msg, func(t *testing.T) {
			t.Logf("labels: %+v, location: %s\n",
				c.labels,
				c.labels.Location())
			assert.Equal(t, c.labels.IsRooted(), c.expected)
			assert.Equal(t, c.labels.IsHome(), c.isHome)
		})
	}
}

func Test_Labels_IsPost(t *testing.T) {
	cases := []struct {
		msg      string
		labels   *Labels
		expected bool
	}{
		{
			msg:      "no stage, not a post",
			expected: false,
			labels:   NewLabels(),
		},
		{
			msg:      "stage not 'post', not a post",
			labels:   NewLabels().Add("stage", "draft"),
			expected: false,
		},
		{
			msg:      "stage is post, is a post",
			labels:   NewLabels().Add("stage", "post"),
			expected: true,
		},
	}
	for _, c := range cases {
		t.Run(c.msg, func(t *testing.T) {
			assert.Equal(t, c.labels.IsPost(), c.expected)
		})
	}
}

func Test_Labels_Location(t *testing.T) {
	cases := []struct {
		msg      string
		labels   *Labels
		expected string
	}{
		{
			msg:      "Empty labels, no location",
			expected: "",
			labels:   NewLabels(),
		},
		{
			msg:      "location only whitespace",
			labels:   NewLabels().Add("location", "   \n"),
			expected: "",
		},
		{
			msg:      "location has a typical value",
			labels:   NewLabels().Add("location", "/index.html"),
			expected: "/index.html",
		},
	}
	for _, c := range cases {
		t.Run(c.msg, func(t *testing.T) {
			assert.Equal(t, c.labels.Location(), c.expected)
		})
	}
}
