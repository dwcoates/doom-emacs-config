package dirpath

import (
	"path/filepath"
	"strings"
)

// Within reports whether PATH is DIR or lies beneath it by whole components,
// so a sibling whose name merely starts with DIR's is not within it. Both are
// compared as given: a caller that needs them cleaned, canonical or
// normalized does that first. EVERY "is this path inside that directory"
// question in the daemon asks it.
func Within(path, dir string) bool {
	return path == dir || strings.HasPrefix(path, dir+string(filepath.Separator))
}
