package reuse

import (
	"strings"
	"testing"

	"agentrepl/testrun/roster"
)

const lockJSON = `{"packages": {
  "": {"name": "shim"},
  "node_modules/a": {"version": "1.0.0", "resolved": "r/a", "integrity": "sha-a"},
  "node_modules/opt": {"version": "2.0.0", "resolved": "r/o", "integrity": "sha-o", "optional": true}
}}`

func TestNodeModulesMismatch(t *testing.T) {
	tests := []struct {
		name      string
		installed string
		want      string
	}{
		{"the tree is the lock's", `{"packages": {"node_modules/a": {"version": "1.0.0", "resolved": "r/a", "integrity": "sha-a"}, "node_modules/opt": {"version": "2.0.0", "resolved": "r/o", "integrity": "sha-o"}}}`, ""},
		{"an optional package may be absent", `{"packages": {"node_modules/a": {"version": "1.0.0", "resolved": "r/a", "integrity": "sha-a"}}}`, ""},
		{"a package at another version", `{"packages": {"node_modules/a": {"version": "0.9.0", "resolved": "r/a", "integrity": "sha-old"}}}`, "node_modules/a is installed at 0.9.0 but package-lock.json names 1.0.0"},
		{"a required package missing", `{"packages": {}}`, "node_modules/a is in package-lock.json but not installed"},
		{"a package the lock does not name", `{"packages": {"node_modules/a": {"version": "1.0.0", "resolved": "r/a", "integrity": "sha-a"}, "node_modules/x": {"version": "1"}}}`, "node_modules/x is installed but not in package-lock.json"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			root := writeTree(t, map[string]string{
				"pkg/package-lock.json":               lockJSON,
				"pkg/node_modules/.package-lock.json": tt.installed,
			})

			// Act
			got, err := NodeModulesMismatch(root, "pkg/node_modules")

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("NodeModulesMismatch = %q, %v; want %q", got, err, tt.want)
			}
		})
	}
}

func TestNodeModulesMismatchNeedsBothLocks(t *testing.T) {
	// Arrange
	root := writeTree(t, map[string]string{"pkg/package-lock.json": lockJSON})

	// Act
	_, err := NodeModulesMismatch(root, "pkg/node_modules")

	// Assert
	if err == nil || !strings.Contains(err.Error(), ".package-lock.json") {
		t.Fatalf("err = %v, want the missing installed lock named", err)
	}
}

func TestARunOnAStaleInstallRecordsNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	write(t, f.d.Repo, fixtureModule+"/web/package-lock.json", lockJSON)
	write(t, f.d.Repo, fixtureModule+"/web/node_modules/.package-lock.json", `{"packages": {}}`)
	s := lockSuite
	s.Reuse = &roster.Reuse{Generated: []string{"web/node_modules/"}}
	v := f.decide(t, Reusing, s)

	// Act
	f.pass(t, Reusing, v)

	// Assert
	if _, ok, _ := f.d.Store.Lookup("lock", v[0].Key.Digest); ok {
		t.Fatal("a run on packages its key does not name was recorded")
	}
	if len(f.log.errs) != 1 || !strings.Contains(f.log.errs[0], "it ran on packages its key does not name") {
		t.Fatalf("errors = %v", f.log.errs)
	}
}
