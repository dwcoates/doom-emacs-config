package wsm

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

func TestRepositoryWithID(t *testing.T) {
	repos := []Repository{{ID: "r1", Dir: "/one"}, {ID: "r2", Dir: "/two"}}
	tests := []struct {
		name      string
		id        RepoID
		want      Repository
		wantFound bool
	}{
		{name: "the first", id: "r1", want: repos[0], wantFound: true},
		{name: "a later one", id: "r2", want: repos[1], wantFound: true},
		{name: "an unregistered id", id: "r3"},
		{name: "the empty id", id: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: repos.

			// Act.
			got, found := RepositoryWithID(repos, tt.id)

			// Assert.
			if got != tt.want || found != tt.wantFound {
				t.Fatalf("RepositoryWithID(%q) = (%+v, %v), want (%+v, %v)", tt.id, got, found, tt.want, tt.wantFound)
			}
		})
	}
}

func TestRepositoryAt(t *testing.T) {
	repos := []Repository{{ID: "r1", Dir: "/one"}, {ID: "r2", Dir: "/two"}}
	tests := []struct {
		name      string
		dir       string
		want      Repository
		wantFound bool
	}{
		{name: "the first", dir: "/one", want: repos[0], wantFound: true},
		{name: "a later one", dir: "/two", want: repos[1], wantFound: true},
		{name: "an unregistered dir", dir: "/three"},
		{name: "a spelling it does not normalize", dir: "/one/"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: repos.

			// Act.
			got, found := RepositoryAt(repos, tt.dir)

			// Assert.
			if got != tt.want || found != tt.wantFound {
				t.Fatalf("RepositoryAt(%q) = (%+v, %v), want (%+v, %v)", tt.dir, got, found, tt.want, tt.wantFound)
			}
		})
	}
}

func TestLookupsOverAnEmptyRegistryFindNothing(t *testing.T) {
	// Arrange / Act.
	_, byID := RepositoryWithID(nil, "r1")
	_, byDir := RepositoryAt(nil, "/one")

	// Assert.
	if byID || byDir {
		t.Fatalf("found by id = %v, by dir = %v, want nothing in an empty registry", byID, byDir)
	}
}

// handRolledLookup matches a loop over a repository list (named `repos` or
// `repositories`, as every site names ListRepositories' answer) whose body
// opens by comparing each repository's ID or Dir: the shape RepositoryWithID
// and RepositoryAt replaced at every site.
var handRolledLookup = regexp.MustCompile(`for _, (\w+) := range (?:repos|repositories) \{\s*if (?:string\()?(\w+)\.(?:ID|Dir)\)? == `)

// lookupHome is the one file allowed the loop: the lookups themselves.
const lookupHome = "repositoryfind.go"

// TestTheLookupShapeIsDetected keeps the scan below honest: it must match the
// very shape it forbids, or it passes vacuously.
func TestTheLookupShapeIsDetected(t *testing.T) {
	// Arrange.
	sample := "for _, repository := range repositories {\n\t\tif repository.ID == repo {"

	// Act.
	match := handRolledLookup.FindStringSubmatch(sample)

	// Assert.
	if match == nil || match[1] != match[2] {
		t.Fatalf("the scan does not recognize a hand-rolled repository lookup: %q", sample)
	}
}

// TestNoProductionCodeHandRollsARepositoryLookup pins that every site that
// finds a repository in a listed registry goes through RepositoryWithID or
// RepositoryAt, so a new site that loops by hand fails here instead of
// drifting from the shared lookup.
func TestNoProductionCodeHandRollsARepositoryLookup(t *testing.T) {
	// Arrange: the daemon's module root, two directories up.
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("resolve the module root: %v", err)
	}

	// Act.
	var offenders []string
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		if path == filepath.Join(root, "internal", "wsm", lookupHome) {
			return nil
		}
		data, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for _, match := range handRolledLookup.FindAllStringSubmatch(string(data), -1) {
			if match[1] == match[2] {
				offenders = append(offenders, path)
			}
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walk %s: %v", root, err)
	}
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled repository lookups in %v; use wsm.RepositoryWithID or wsm.RepositoryAt", offenders)
	}
}
