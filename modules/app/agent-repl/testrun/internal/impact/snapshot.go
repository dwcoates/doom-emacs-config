package impact

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"sort"
	"strings"
)

// Snapshot is the content hash of every file one run's records depend on:
// its paths (repository-relative, sorted) and their blob hashes, in step. A
// record names its dependencies as a bitmap over the snapshot's paths, so a
// run's hundreds of records share one list of paths and hashes.
type Snapshot struct {
	Files  []string `json:"files"`
	Hashes []string `json:"hashes"`
}

// NewSnapshot is the snapshot of hashes, a map of path to blob hash.
func NewSnapshot(hashes map[string]string) Snapshot {
	files := make([]string, 0, len(hashes))
	for f := range hashes {
		files = append(files, f)
	}
	sort.Strings(files)
	s := Snapshot{Files: files, Hashes: make([]string, len(files))}
	for i, f := range files {
		s.Hashes[i] = hashes[f]
	}
	return s
}

// ID names the snapshot by its content, so two runs over the same files
// share one stored snapshot.
func (s Snapshot) ID() string {
	h := sha256.New()
	for i, f := range s.Files {
		fmt.Fprintf(h, "%s\x1f%s\n", f, s.Hashes[i])
	}
	return hex.EncodeToString(h.Sum(nil))
}

// Index answers the position of path in the snapshot.
func (s Snapshot) Index(path string) (int, bool) {
	i := sort.SearchStrings(s.Files, path)
	return i, i < len(s.Files) && s.Files[i] == path
}

// Validate refuses a snapshot whose paths and hashes are out of step or whose
// paths are unsorted or repeated: it is a stored file, read back whole.
func (s Snapshot) Validate() error {
	if len(s.Files) != len(s.Hashes) {
		return fmt.Errorf("impact: a snapshot names %d files and %d hashes", len(s.Files), len(s.Hashes))
	}
	for i := 1; i < len(s.Files); i++ {
		if s.Files[i-1] >= s.Files[i] {
			return fmt.Errorf("impact: a snapshot's files are not sorted and distinct at %q, %q", s.Files[i-1], s.Files[i])
		}
	}
	return nil
}

// Bitmap is a set of snapshot positions, one bit each.
type Bitmap []byte

// NewBitmap is the empty bitmap over n positions.
func NewBitmap(n int) Bitmap { return make(Bitmap, (n+7)/8) }

// Set adds position i.
func (b Bitmap) Set(i int) { b[i/8] |= 1 << (i % 8) }

// Has reports whether position i is in the bitmap.
func (b Bitmap) Has(i int) bool { return i/8 < len(b) && b[i/8]&(1<<(i%8)) != 0 }

// Count is how many positions the bitmap holds.
func (b Bitmap) Count() int {
	n := 0
	for _, x := range b {
		for ; x != 0; x &= x - 1 {
			n++
		}
	}
	return n
}

// Positions lists the bitmap's positions below limit, ascending.
func (b Bitmap) Positions(limit int) []int {
	var out []int
	for i := 0; i < limit; i++ {
		if b.Has(i) {
			out = append(out, i)
		}
	}
	return out
}

// FactsDigest is a stable one-line rendering of facts (non-file
// dependencies: the toolchain, the build tags), sorted by name.
func FactsDigest(facts map[string]string) string {
	names := make([]string, 0, len(facts))
	for n := range facts {
		names = append(names, n)
	}
	sort.Strings(names)
	var b strings.Builder
	for _, n := range names {
		fmt.Fprintf(&b, "%s=%s\x1f", n, facts[n])
	}
	return b.String()
}
