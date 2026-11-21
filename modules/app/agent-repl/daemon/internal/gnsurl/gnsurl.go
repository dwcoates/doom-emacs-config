// Package gnsurl recognizes the urls of GNS, the knowledge graph whose
// Knowledge UI addresses each key by a dotted name. It is the ONE place the
// daemon decides whether a url is GNS's, and which key it names, so the
// topbar's service icon and the links store agree on it.
package gnsurl

import (
	"net/url"
	"strings"
)

// IsHost reports whether HOST (a url's hostname, in any case) is a GNS
// host: one whose name carries "gns", as every GNS deployment's does
// (gns.int.sgns.chess-platform.com).
func IsHost(host string) bool {
	return strings.Contains(strings.ToLower(host), "gns")
}

// pageRoutes are the Knowledge UI routes that address one key: its page
// (`/key/<key>`), its prefix browse (`/keys/<key>`) and an app stored at the
// key (`/app/<key>`). Every other route (`/agents`, `/pipes`, `/search`,
// `/api`) names no key in the dotted hierarchy.
var pageRoutes = map[string]bool{"key": true, "keys": true, "app": true}

// Page is the GNS key a url addresses, on its host.
type Page struct {
	// Host is the url's hostname, lower-cased.
	Host string
	// Key is the dotted GNS key, verbatim: every segment non-empty.
	Key string
}

// ParsePage answers the GNS page RAW addresses: an http or https url on a GNS
// host whose path is one of pageRoutes followed by one dotted key, its query
// and fragment ignored. False for any other url.
func ParsePage(raw string) (Page, bool) {
	u, err := url.Parse(raw)
	if err != nil || (u.Scheme != "https" && u.Scheme != "http") || !IsHost(u.Hostname()) {
		return Page{}, false
	}
	route, key, found := strings.Cut(strings.TrimPrefix(u.Path, "/"), "/")
	if !found || !pageRoutes[route] || key == "" || strings.Contains(key, "/") {
		return Page{}, false
	}
	for _, segment := range strings.Split(key, ".") {
		if segment == "" {
			return Page{}, false
		}
	}
	return Page{Host: strings.ToLower(u.Hostname()), Key: key}, true
}

// Contains reports whether SUB is a subpage of P: a page on the same host
// whose key extends P's by one or more dotted segments. A page never contains
// itself, and the route either is addressed by plays no part.
func (p Page) Contains(sub Page) bool {
	return p.Host == sub.Host && strings.HasPrefix(sub.Key, p.Key+".")
}
