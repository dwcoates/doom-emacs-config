package logging

import "regexp"

// nonAlphanumeric spells the vendor's project-dir naming rule (see
// VendorProjectSlug).
var nonAlphanumeric = regexp.MustCompile(`[^A-Za-z0-9]`)

// VendorProjectSlug returns the vendor CLI's `projects/<name>` encoding of an
// absolute cwd: the directory, inside one config root, that the vendor files
// that cwd's transcripts under. It lives beside WorkspaceID because it is the
// same kind of thing, a path-derived correlation key every agent-repl runtime
// must spell identically: the daemon locates a transcript with it, and the
// sidecar attributes one with it.
//
// THE RULE, verified against the live install's ~/.claude/projects layout
// (2026-08-29): every byte of the absolute path that is not [A-Za-z0-9]
// becomes "-". That is broader than "slashes become dashes" and the breadth
// matters — `/Users/dodgecoates/.config/doom` files under
// `-Users-dodgecoates--config-doom` (the dot becomes a dash too, giving the
// doubled dash), and `/private/var/folders/_m/…` files under
// `-private-var-folders--m-…` (the underscore likewise). Case is preserved,
// and an existing dash is left alone, which is why a uuid inside the path
// survives verbatim.
//
// THE MAPPING IS LOSSY AND NOT INVERTIBLE: two directories can share one
// slug. Encode a known cwd and compare; never decode a slug into a path.
func VendorProjectSlug(cwd string) string {
	return nonAlphanumeric.ReplaceAllString(cwd, "-")
}
