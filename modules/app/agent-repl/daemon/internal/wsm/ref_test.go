package wsm

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	workspacev1 "agentrepl/proto/workspace/v1"
)

func TestRefNamesTheWorkspaceByIdAndDir(t *testing.T) {
	tests := []struct {
		name string
		ref  func() *workspacev1.WorkspaceRef
	}{
		{name: "a workspace record", ref: func() *workspacev1.WorkspaceRef {
			return Workspace{ID: "w-1", Dir: "/repos/one/w-1", Name: "one", Branch: "branch/w-1"}.Ref()
		}},
		{name: "a family member", ref: func() *workspacev1.WorkspaceRef {
			return FamilyRef{ID: "w-1", Dir: "/repos/one/w-1"}.Ref()
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the fixture is the case.

			// Act.
			got := tc.ref()

			// Assert.
			if got.GetId() != "w-1" || got.GetDir() != "/repos/one/w-1" {
				t.Fatalf("Ref() = %v, want id w-1 and dir /repos/one/w-1", got)
			}
		})
	}
}

func TestRefAnswersAFreshMessageEachCall(t *testing.T) {
	// Arrange.
	ws := Workspace{ID: "w-1", Dir: "/d"}

	// Act.
	first, second := ws.Ref(), ws.Ref()
	first.Dir = "/changed"

	// Assert: a caller mutating its ref never reaches another's.
	if second.GetDir() != "/d" {
		t.Fatalf("second Ref().Dir = %q, want /d", second.GetDir())
	}
}

// handRolledRef is a workspace.v1.WorkspaceRef composed from a record's id and
// dir by hand, which Ref replaces.
var handRolledRef = regexp.MustCompile(`WorkspaceRef\{Id: string\([\w.*()]+\.ID\), Dir: [\w.*()]+\.Dir\}`)

func TestNoDaemonSourceComposesAWorkspaceRefByHand(t *testing.T) {
	// Arrange: the daemon module's root, two levels above this package.
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("Abs: %v", err)
	}
	self, err := filepath.Abs("ref.go")
	if err != nil {
		t.Fatalf("Abs: %v", err)
	}

	// Act.
	var found []string
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || path == self {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for _, hit := range handRolledRef.FindAll(src, -1) {
			found = append(found, path+": "+string(hit))
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walk %s: %v", root, err)
	}

	// Assert.
	if len(found) > 0 {
		t.Fatalf("hand-composed workspace refs, use Workspace.Ref or FamilyRef.Ref:\n%s", strings.Join(found, "\n"))
	}
}

func TestTheHandRolledRefPatternMatchesTheShapeItForbids(t *testing.T) {
	tests := []struct {
		name string
		src  string
		want bool
	}{
		{name: "a record's fields", src: `&workspacev1.WorkspaceRef{Id: string(record.ID), Dir: record.Dir}`, want: true},
		{name: "a nested field", src: `&workspacev1.WorkspaceRef{Id: string(p.Target.ID), Dir: p.Target.Dir}`, want: true},
		{name: "an id alone", src: `&workspacev1.WorkspaceRef{Id: string(*ws.Parent)}`, want: false},
		{name: "the helper", src: `record.Ref()`, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the fixture is the case.

			// Act.
			got := handRolledRef.MatchString(tc.src)

			// Assert.
			if got != tc.want {
				t.Fatalf("match(%q) = %v, want %v", tc.src, got, tc.want)
			}
		})
	}
}
