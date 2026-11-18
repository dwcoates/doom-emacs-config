// Package gnsurl recognizes the urls of GNS, the knowledge graph whose
// Knowledge UI addresses each key by a dotted name. It is the ONE place the
// daemon decides whether a url is GNS's, so the topbar's service icon and
// every later reading of a GNS url agree on it.
package gnsurl

import "strings"

// IsHost reports whether HOST (a url's hostname, in any case) is a GNS
// host: one whose name carries "gns", as every GNS deployment's does
// (gns.int.sgns.chess-platform.com).
func IsHost(host string) bool {
	return strings.Contains(strings.ToLower(host), "gns")
}
