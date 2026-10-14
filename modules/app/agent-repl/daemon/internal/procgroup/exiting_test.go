package procgroup

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestParseStatExiting(t *testing.T) {
	tests := []struct {
		name    string
		stat    string
		want    bool
		wantErr string
	}{
		{name: "a running process", stat: "42 (node) S 1 42 42 0 -1 4194560 100 0 0 0", want: false},
		{name: "a process marked PF_EXITING", stat: "42 (node) S 1 42 42 0 -1 4194564 100 0 0 0", want: true},
		{name: "a zombie", stat: "42 (node) Z 1 42 42 0 -1 4194560 100 0 0 0", want: true},
		{name: "a dead process", stat: "42 (node) X 1 42 42 0 -1 4194560 100 0 0 0", want: true},
		{name: "a command name holding spaces and parentheses", stat: "42 (a (b) c) R 1 42 42 0 -1 4194560 1 0 0 0", want: false},
		{name: "a line with no command name", stat: "42 node R", wantErr: "no command name"},
		{name: "a short line", stat: "42 (node) R 1 2", wantErr: "is short"},
		{name: "non-numeric flags", stat: "42 (node) R 1 42 42 0 -1 x 1 0 0 0", wantErr: "flags"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := parseStatExiting(tt.stat)

			// Assert.
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("parseStatExiting error = %v, want one containing %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("parseStatExiting = %v, %v; want %v", got, err, tt.want)
			}
		})
	}
}

// TestOnlyProcgroupReadsAProcessState pins that every read of one process's
// kernel state in the daemon module goes through this package (ReadState,
// Exiting), so the platforms' readings live in one place. Listing processes
// (the harness's stray sweep) is another question and is not this one.
func TestOnlyProcgroupReadsAProcessState(t *testing.T) {
	// Arrange
	var offenders []string

	// Act
	err := filepath.WalkDir(filepath.Join("..", ".."), func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || filepath.Base(filepath.Dir(path)) == "procgroup" {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(src), `"kern.proc.pid"`) || strings.Contains(string(src), `"/proc/" + strconv.Itoa(pid) + "/stat"`) || strings.Contains(string(src), `"/proc/%d/stat"`) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon module: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("%v read the kernel's process table themselves instead of through procgroup", offenders)
	}
}
