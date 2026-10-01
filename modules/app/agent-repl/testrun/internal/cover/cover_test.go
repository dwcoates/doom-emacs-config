package cover

import (
	"bytes"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

type call struct {
	dir  string
	args []string
}

func fakeTool(report string, fail map[string]error) (Runner, *[]call) {
	var calls []call
	return func(dir string, args ...string) ([]byte, error) {
		calls = append(calls, call{dir, args})
		if err := fail[args[1]]; err != nil {
			return nil, err
		}
		if args[1] == "cover" {
			return []byte(report), nil
		}
		return nil, nil
	}, &calls
}

func covRoot(t *testing.T, dirs ...string) string {
	t.Helper()
	root := t.TempDir()
	for _, d := range dirs {
		if err := os.MkdirAll(filepath.Join(root, d), 0o755); err != nil {
			t.Fatal(err)
		}
	}
	return root
}

func TestReportMergesEveryPackageDirectory(t *testing.T) {
	// Arrange
	root := covRoot(t, "001", "000")
	tool, calls := fakeTool("a.go:1:\tF\t100.0%\ntotal:\t(statements)\t80.0%\n", nil)
	var out bytes.Buffer

	// Act
	err := Report(tool, &out, "lock", "/m", root)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	merge := (*calls)[0]
	wantArgs := []string{"tool", "covdata", "textfmt",
		"-i=" + filepath.Join(root, "000") + "," + filepath.Join(root, "001"),
		"-o=" + filepath.Join(root, "lock.coverprofile")}
	if merge.dir != "/m" || !reflect.DeepEqual(merge.args, wantArgs) {
		t.Fatalf("merge call = %+v, want %v in /m", merge, wantArgs)
	}
	if !strings.HasSuffix(out.String(), "[agent-repl-coverage] lock: total:\t(statements)\t80.0%\n") {
		t.Fatalf("report output = %q", out.String())
	}
}

func TestReportFailures(t *testing.T) {
	tests := []struct {
		name    string
		dirs    []string
		report  string
		fail    map[string]error
		wantErr string
	}{
		{name: "no package directories", dirs: nil, report: "", wantErr: "holds no package coverage directories"},
		{name: "the merge fails", dirs: []string{"000"}, fail: map[string]error{"covdata": errors.New("bad counters")}, wantErr: "merge lock's coverage: bad counters"},
		{name: "the function report fails", dirs: []string{"000"}, fail: map[string]error{"cover": errors.New("no profile")}, wantErr: "lock's function report: no profile"},
		{name: "a report without a total", dirs: []string{"000"}, report: "a.go:1:\tF\t100.0%\n", wantErr: "coverage summary is malformed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			root := covRoot(t, tt.dirs...)
			tool, _ := fakeTool(tt.report, tt.fail)
			var out bytes.Buffer

			// Act
			err := Report(tool, &out, "lock", "/m", root)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
			}
			if out.Len() != 0 {
				t.Fatalf("a failed report printed %q", out.String())
			}
		})
	}
}

func TestReportOfAMissingRootFails(t *testing.T) {
	// Arrange
	tool, _ := fakeTool("", nil)

	// Act
	err := Report(tool, &bytes.Buffer{}, "lock", "/m", filepath.Join(t.TempDir(), "none"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "cover: read") {
		t.Fatalf("err = %v", err)
	}
}
