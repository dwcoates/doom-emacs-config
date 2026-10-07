// Package recorded replays a committed recording (a corpus fixture or a
// captured transcript) as if this machine had recorded it.
//
// A recording names no one: the capture tooling replaced the home directory of
// whoever recorded it with HomeToken, and that home's vendor project slug with
// HomeToken's slug (see scripts/capture/anonymize.mjs in the shim). A recorded
// path is only absolute again once the token is expanded, and the sidecar's
// discovery refuses a transcript whose cwd is not absolute, so a reader that
// replays a recording through discovery expands it first. A reader that only
// converts recorded records may read them as committed.
package recorded

import (
	"strings"

	sharedlogging "agentrepl/logging"
)

// HomeToken is what a home directory became in a recording.
const HomeToken = "${HOME}"

// Expand answers text with HomeToken replaced by home, and HomeToken's vendor
// project slug replaced by home's.
func Expand(text, home string) string {
	text = strings.ReplaceAll(text, HomeToken, home)
	return strings.ReplaceAll(text, sharedlogging.VendorProjectSlug(HomeToken), sharedlogging.VendorProjectSlug(home))
}
