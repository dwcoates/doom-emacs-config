package metrics

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agentrepl/logging/buildreport"
)

func fixedSelf(build string) func() (buildreport.Report, error) {
	return func() (buildreport.Report, error) { return buildreport.Report{PID: 1, Build: build}, nil }
}

func TestOpenSelf(t *testing.T) {
	tests := []struct {
		name       string
		root       func(t *testing.T) string
		self       func() (buildreport.Report, error)
		wantErr    string
		wantOff    bool
		wantTarget bool
	}{
		{name: "off without a root", root: func(*testing.T) string { return "" }, self: fixedSelf("abc"), wantOff: true},
		{name: "opens and advertises", root: func(t *testing.T) string { return t.TempDir() }, self: fixedSelf("abc"), wantTarget: true},
		{
			name:    "an unknown running build is refused",
			root:    func(t *testing.T) string { return t.TempDir() },
			self:    func() (buildreport.Report, error) { return buildreport.Report{}, errors.New("no executable") },
			wantErr: `determine the running build of "store": no executable`,
		},
		{
			name: "a target that cannot be written closes the listener",
			root: func(t *testing.T) string {
				root := t.TempDir()
				if err := os.WriteFile(filepath.Join(root, TargetsDir), nil, 0o644); err != nil {
					t.Fatal(err)
				}
				return root
			},
			self:    fixedSelf("abc"),
			wantErr: `create the targets directory for "store"`,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			root := tt.root(t)
			sink := &errSink{}

			// Act
			s, err := openSelf(ProgramStore, root, sink.record, tt.self)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to contain %q", err, tt.wantErr)
				}
				if s != nil {
					t.Fatalf("a refused surface was answered")
				}
				return
			}
			if err != nil {
				t.Fatalf("openSelf: %v", err)
			}
			if tt.wantOff {
				if s != nil {
					t.Fatalf("an off surface was answered")
				}
				return
			}
			t.Cleanup(func() { _ = s.Close() })
			status, body := get(t, "http://"+s.Addr()+Path)
			if status != 200 || !strings.Contains(body, `agentrepl_build_info{build="abc",program="store"} 1`) {
				t.Fatalf("scrape = %d:\n%s", status, body)
			}
			if _, err := os.Stat(TargetPath(root, ProgramStore)); err != nil {
				t.Fatalf("no target advertised: %v", err)
			}
		})
	}
}
