package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func Test_Labels_IsRooted(t *testing.T) {
	cases := []struct {
		msg      string
		labels   Labels
		expected bool
		isHome   bool
	}{
		{
			msg:      "empty not one of the rooted locations",
			expected: false,
		},
		{
			msg:      "home page is rooted file",
			labels:   Labels{"location": "/index.html"},
			expected: true,
			isHome:   true,
		},
		{
			msg:      "robots is rooted file",
			labels:   Labels{"location": "/robots.txt"},
			expected: true,
		},
		{
			msg:      "not in the set of rooted files",
			labels:   Labels{"location": "/some-home.html"},
			expected: false,
		},
	}
	for _, c := range cases {
		t.Run(c.msg, func(t *testing.T) {
			assert.Equal(t, c.labels.IsRooted(), c.expected)
			assert.Equal(t, c.labels.IsHome(), c.isHome)
		})
	}
}

func Test_Labels_IsPost(t *testing.T) {
	cases := []struct {
		msg      string
		labels   Labels
		expected bool
	}{
		{
			msg:      "no stage, not a post",
			expected: false,
		},
		{
			msg: "stage not 'post', not a post",
			labels: Labels{
				"stage": "draft",
			},
			expected: false,
		},
		{
			msg: "stage is post, is a post",
			labels: Labels{
				"stage": "post",
			},
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
		labels   Labels
		expected string
	}{
		{
			msg:      "Empty labels, no location",
			expected: "",
		},
		{
			msg: "location only whitespace",
			labels: Labels{
				"location": "   \n",
			},
			expected: "",
		},
		{
			msg: "location has a typical value",
			labels: Labels{
				"location": "/index.html",
			},
			expected: "/index.html",
		},
	}
	for _, c := range cases {
		t.Run(c.msg, func(t *testing.T) {
			assert.Equal(t, c.labels.Location(), c.expected)
		})
	}
}