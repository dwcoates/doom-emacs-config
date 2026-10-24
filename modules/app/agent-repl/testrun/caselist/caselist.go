// Package caselist reads the case lists `testrun redundancy` takes: a
// comma-separated flag and a -delete file. It is public because the daemon's
// command line (agent-repld test redundancy) reads the same lists into its
// request, and the two must read them alike.
package caselist

import (
	"fmt"
	"os"
	"strings"
)

// SplitList splits a comma-separated flag, dropping empty entries.
func SplitList(s string) []string {
	var out []string
	for _, p := range strings.Split(s, ",") {
		if p = strings.TrimSpace(p); p != "" {
			out = append(out, p)
		}
	}
	return out
}

// ReadDeleteSet reads a -delete file: one case per line; blank lines and
// lines starting with # are skipped.
func ReadDeleteSet(path string) ([]string, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read -delete %s: %w", path, err)
	}
	var out []string
	for _, l := range strings.Split(string(b), "\n") {
		if l = strings.TrimSpace(l); l != "" && !strings.HasPrefix(l, "#") {
			out = append(out, l)
		}
	}
	if len(out) == 0 {
		return nil, fmt.Errorf("-delete %s names no case", path)
	}
	return out, nil
}
