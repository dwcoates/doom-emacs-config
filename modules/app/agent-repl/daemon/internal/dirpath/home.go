package dirpath

import (
	"fmt"
	"path/filepath"
	"strings"
)

// Absolute answers a path a PRODUCER wrote, with a leading `~` expanded to
// home: `~` alone is home, and `~/rest` is rest beneath it. Every other path is
// taken as written, and a path that is still not absolute afterwards is
// REFUSED rather than resolved against the daemon's working directory.
//
// The working directory is no answer for a daemon: it is wherever launchd
// started it, so `filepath.Abs("~/.config/doom")` named
// `/Users/me/~/.config/doom`, a directory nobody asked for, and the create the
// skill dispatched with that `git_root` was refused as an unknown repository
// with nothing on screen (2026-09-28). The skill's contract says a leading `~`
// is expanded downstream; this is where.
//
// `~user` names ANOTHER user's home, which this daemon has no business
// resolving, so it is refused too. home must itself be absolute whenever the
// path uses it.
func Absolute(path, home string) (string, error) {
	if path == "" {
		return "", fmt.Errorf("dirpath: an empty path names no directory")
	}
	expanded := path
	if rest, ok := strings.CutPrefix(path, "~"); ok {
		if rest != "" && !strings.HasPrefix(rest, "/") {
			return "", fmt.Errorf("dirpath: %q names another user's home directory, which is not expanded", path)
		}
		if !filepath.IsAbs(home) {
			return "", fmt.Errorf("dirpath: %q needs the home directory, and %q is not an absolute one", path, home)
		}
		expanded = filepath.Join(home, rest)
	}
	if !filepath.IsAbs(expanded) {
		return "", fmt.Errorf("dirpath: %q is not an absolute path, and a relative one would be resolved against wherever the daemon happens to run", path)
	}
	return filepath.Clean(expanded), nil
}
