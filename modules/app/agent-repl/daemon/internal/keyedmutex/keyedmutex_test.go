package keyedmutex

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

func TestFor(t *testing.T) {
	cases := []struct {
		name  string
		first string
		then  string
		same  bool
	}{
		{name: "the same key answers the same mutex", first: "a", then: "a", same: true},
		{name: "another key answers another mutex", first: "a", then: "b", same: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			var set Set[string]

			// Act.
			first, then := set.For(tc.first), set.For(tc.then)

			// Assert.
			if first == nil || then == nil {
				t.Fatalf("answered %p and %p, want a mutex for each key", first, then)
			}
			if (first == then) != tc.same {
				t.Fatalf("mutexes for %q and %q are the same: %v, want %v", tc.first, tc.then, first == then, tc.same)
			}
		})
	}
}

func TestLockHoldsTheKeysMutexUntilReleased(t *testing.T) {
	// Arrange.
	var set Set[string]

	// Act.
	release := set.Lock("a")

	// Assert.
	if set.For("a").TryLock() {
		t.Fatal("the key's mutex was free while held")
	}
	if !set.For("b").TryLock() {
		t.Fatal("another key's mutex was held by the first key's lock")
	}
	release()
	if !set.For("a").TryLock() {
		t.Fatal("the key's mutex stayed held after its release")
	}
}

// mutexMap is a hand-rolled per-key mutex map: a map whose values are
// *sync.Mutex, which is what a Set replaces.
var mutexMap = regexp.MustCompile(`map\[[^\]]+\]\*sync\.Mutex`)

// TestEveryPerKeyMutexIsASet asserts no daemon source outside this package
// keeps its own map of mutexes, so a per-key lock that mints or keeps its
// locks differently fails here rather than drifting silently.
func TestEveryPerKeyMutexIsASet(t *testing.T) {
	// Arrange.
	root, err := filepath.Abs("..")
	if err != nil {
		t.Fatalf("resolve the daemon's internal tree: %v", err)
	}
	self, err := filepath.Abs(".")
	if err != nil {
		t.Fatalf("resolve this package: %v", err)
	}
	var found []string

	// Act.
	walkErr := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if path == self {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if mutexMap.Match(src) {
			found = append(found, path)
		}
		return nil
	})

	// Assert.
	if walkErr != nil {
		t.Fatalf("walk %s: %v", root, walkErr)
	}
	if len(found) > 0 {
		t.Fatalf("these sources keep their own map of mutexes; use a keyedmutex.Set: %v", found)
	}
}
