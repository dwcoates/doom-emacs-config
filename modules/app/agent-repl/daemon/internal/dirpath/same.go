package dirpath

import "path/filepath"

// Same reports whether two directory spellings name one directory: each is
// cleaned and its symlinks resolved, and a spelling whose symlinks cannot be
// resolved (it does not exist) is compared cleaned. EVERY "is this the
// repository's main worktree" question asks it, so a deploy's target and a
// create's landing target are judged one way.
func Same(a, b string) bool {
	return resolved(a) == resolved(b)
}

// resolved is DIR cleaned with its symlinks resolved, or cleaned alone when
// they cannot be.
func resolved(dir string) string {
	clean := filepath.Clean(dir)
	if real, err := filepath.EvalSymlinks(clean); err == nil {
		return real
	}
	return clean
}
