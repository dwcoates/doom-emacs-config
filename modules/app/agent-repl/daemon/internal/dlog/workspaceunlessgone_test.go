package dlog

import (
	"errors"
	"os"
	"strings"
	"testing"
)

func TestWorkspaceUnlessGone(t *testing.T) {
	tests := []struct {
		name        string
		lookupErr   error
		wantErr     bool
		wantCentral bool
	}{
		{name: "a resolvable directory answers its workspace logger"},
		{name: "a removed directory answers the central logger naming it", lookupErr: ErrWorkspaceDirGone, wantCentral: true},
		{name: "any other refusal is the caller's", lookupErr: errors.New("permission denied"), wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := NewTestSurfaces()
			if tt.lookupErr != nil {
				s.BindWorkspaceIDs(func(string) (string, error) { return "", tt.lookupErr })
			}

			// Act
			log, err := WorkspaceUnlessGone(s, "/tmp/ws")

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("WorkspaceUnlessGone err = %v, want error %v", err, tt.wantErr)
			}
			if tt.wantErr {
				return
			}
			log.Info("probe", "a record", nil)
			rec, ok := FindRecord(s.Records(), RecordQuery{Operation: "probe"})
			if !ok {
				t.Fatalf("records = %+v, want the probe", s.Records())
			}
			_, central := rec.Context[KeyUnroutableWorkspace]
			if central != tt.wantCentral {
				t.Fatalf("record %+v central = %v, want %v", rec, central, tt.wantCentral)
			}
		})
	}
}

func TestTheResolversRouteARemovedDirectoryThroughWorkspaceUnlessGone(t *testing.T) {
	for _, file := range []string{"../resolve/feed/resolver.go", "../resolve/footer/resolver.go"} {
		t.Run(file, func(t *testing.T) {
			// Arrange
			b, err := os.ReadFile(file)
			if err != nil {
				t.Fatal(err)
			}
			src := string(b)

			// Act, Assert: the helper is taken, and no hand-rolled branch on
			// the removed directory stands beside it.
			if !strings.Contains(src, "dlog.WorkspaceUnlessGone(") || strings.Contains(src, "dlog.ErrWorkspaceDirGone") {
				t.Fatalf("%s does not route a removed directory through dlog.WorkspaceUnlessGone alone", file)
			}
		})
	}
}
