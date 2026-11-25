package wsm

import (
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

func TestRepositoryNamed(t *testing.T) {
	repos := []Repository{{ID: "r1", Dir: "/one"}, {ID: "r2", Dir: "/two"}}
	tests := []struct {
		name      string
		id        RepoID
		dir       string
		want      Repository
		wantFound bool
	}{
		{name: "by id", id: "r2", want: repos[1], wantFound: true},
		{name: "by dir", dir: "/one", want: repos[0], wantFound: true},
		{name: "by both agreeing", id: "r1", dir: "/one", want: repos[0], wantFound: true},
		{name: "by both disagreeing", id: "r1", dir: "/two"},
		{name: "an unregistered id", id: "r3"},
		{name: "an unregistered dir", dir: "/three"},
		{name: "neither", id: "", dir: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, found := RepositoryNamed(repos, tt.id, tt.dir)

			// Assert.
			if got != tt.want || found != tt.wantFound {
				t.Fatalf("RepositoryNamed(%q, %q) = (%+v, %v), want (%+v, %v)", tt.id, tt.dir, got, found, tt.want, tt.wantFound)
			}
		})
	}
}

func TestLookupsOverAnEmptyRegistryFindNothing(t *testing.T) {
	// Arrange / Act.
	_, byID := RepositoryWithID(nil, "r1")
	_, byDir := RepositoryAt(nil, "/one")
	_, byRef := RepositoryNamed(nil, "r1", "/one")

	// Assert.
	if byID || byDir || byRef {
		t.Fatalf("found by id = %v, by dir = %v, by ref = %v, want nothing in an empty registry", byID, byDir, byRef)
	}
}
