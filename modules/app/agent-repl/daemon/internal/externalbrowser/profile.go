package externalbrowser

import (
	"encoding/json"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// DefaultLocalStatePath is Chrome's own `Local State` file under the standard
// macOS user-data dir. It is the authority on which on-disk profile directory
// belongs to which signed-in account, so the account→profile routing reads it
// rather than hardcoding a mapping that drifts the moment the operator adds or
// reorders a profile.
//
// An empty answer (no discoverable home) is not a failure here: the opener
// fails a link whose account cannot be routed, so a host where the path cannot
// even be formed fails exactly as a host whose file is missing does.
func DefaultLocalStatePath() string {
	home, err := os.UserHomeDir()
	if err != nil || home == "" {
		return ""
	}
	return filepath.Join(home, "Library", "Application Support", "Google", "Chrome", "Local State")
}

// localState is the narrow slice of Chrome's `Local State` document the routing
// needs. Chrome's file is a large, evolving vendor document; naming only
// info_cache keeps this immune to every field it adds elsewhere.
type localState struct {
	Profile struct {
		InfoCache map[string]struct {
			UserName string `json:"user_name"`
			GaiaName string `json:"gaia_name"`
			Email    string `json:"email"`
		} `json:"info_cache"`
	} `json:"profile"`
}

// ProfileForEmail resolves the Chrome on-disk profile directory (e.g.
// "Profile 1", "Default") whose account matches email, reading Chrome's own
// info_cache from a Local State document. The match is case-insensitive and
// compares email against each profile's user_name and email fields — the two
// that hold an address; gaia_name is a display name and is not matched on.
//
// A blank email, an unparseable document, or no matching profile all answer
// ("", false): the caller decides what a miss means (the opener fails the
// link and says so). Profile keys are matched in sorted order so a
// document with two profiles on the same account resolves deterministically.
func ProfileForEmail(data []byte, email string) (string, bool) {
	want := strings.TrimSpace(strings.ToLower(email))
	if want == "" {
		return "", false
	}
	var ls localState
	if err := json.Unmarshal(data, &ls); err != nil {
		return "", false
	}
	dirs := make([]string, 0, len(ls.Profile.InfoCache))
	for dir := range ls.Profile.InfoCache {
		dirs = append(dirs, dir)
	}
	sort.Strings(dirs)
	for _, dir := range dirs {
		info := ls.Profile.InfoCache[dir]
		if strings.ToLower(strings.TrimSpace(info.UserName)) == want ||
			strings.ToLower(strings.TrimSpace(info.Email)) == want {
			return dir, true
		}
	}
	return "", false
}
