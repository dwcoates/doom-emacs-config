package kit

import (
	"path/filepath"
	"regexp"
)

// ResolvedPath answers path with every symlink resolved. A path that does not
// exist is a LOUD error rather than the path itself: the only caller resolves
// roots the daemon has already created, so a missing one is a defect, not a
// case to paper over.
func ResolvedPath(path string) (string, error) {
	real, err := filepath.EvalSymlinks(path)
	if err != nil {
		return "", err
	}
	return real, nil
}

// ScenarioOverride is a scenario form redefining a function: a `cl-letf' of
// its symbol-function, or an `advice-add' on it.
var ScenarioOverride = regexp.MustCompile(`(?:symbol-function|advice-add) #?'([a-zA-Z0-9-]+)`)
