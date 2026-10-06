package run

import (
	"bufio"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"strings"
	"testing"
)

// A `go build` of a main package inside a git checkout may ask git to stamp the
// binary with the revision it was built from (-buildvcs=auto). NO TEST RUNS
// REAL GIT (owner rule), so every go build the tests and their harnesses run
// passes -buildvcs=false on the same line. This scan holds the module's Go and
// elisp sources to that.
var goBuildCall = regexp.MustCompile(`"go",\s*"build"|:=\s*\[\]string\{"build"[,}]|call-process go [^"]*"build"`)

func TestEveryGoBuildATestRunsRefusesTheVCSStamp(t *testing.T) {
	// Arrange
	_, self, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller could not locate this file")
	}
	module := filepath.Clean(filepath.Join(filepath.Dir(self), "..", "..", ".."))
	var sources []string
	err := filepath.WalkDir(module, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() && (d.Name() == "node_modules" || d.Name() == ".git") {
			return filepath.SkipDir
		}
		if !d.IsDir() && (strings.HasSuffix(path, ".go") || strings.HasSuffix(path, ".el")) {
			sources = append(sources, path)
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(sources) == 0 {
		t.Fatal("found no sources to scan: the layout this test assumes moved")
	}

	// Act
	var stamped []string
	found := 0
	for _, path := range sources {
		if path == self {
			continue
		}
		f, err := os.Open(path)
		if err != nil {
			t.Fatal(err)
		}
		sc := bufio.NewScanner(f)
		sc.Buffer(make([]byte, 1024*1024), 1024*1024)
		for n := 1; sc.Scan(); n++ {
			line := sc.Text()
			if !goBuildCall.MatchString(line) {
				continue
			}
			found++
			if !strings.Contains(line, "-buildvcs=false") {
				rel, _ := filepath.Rel(module, path)
				stamped = append(stamped, rel+":"+strconv.Itoa(n)+": "+strings.TrimSpace(line))
			}
		}
		if err := sc.Err(); err != nil {
			t.Fatalf("read %s: %v", path, err)
		}
		f.Close()
	}

	// Assert
	if found == 0 {
		t.Fatal("found no go build call at all: the pattern no longer matches how the harnesses build")
	}
	if len(stamped) > 0 {
		t.Fatalf("a go build without -buildvcs=false may run real git to stamp the binary; pass -buildvcs=false on the same line:\n%s", strings.Join(stamped, "\n"))
	}
}

func TestTheGoBuildPatternMatchesEachWayTheHarnessesBuild(t *testing.T) {
	tests := []struct {
		line  string
		match bool
	}{
		{`exec.Command("go", "build", "-o", out, ".")`, true},
		{`[]string{"go", "build", "-o", fake, "."}`, true},
		{`buildArgs := []string{"build"}`, true},
		{`args := []string{"build", "-buildvcs=false"}`, true},
		{`(call-process go nil log nil "build" "-o" output ".")`, true},
		{`argv = []string{"go", "test", "-c", "-o", p.Bin}`, false},
		{`Deps: []string{"build"}`, false},
	}
	for _, tt := range tests {
		t.Run(tt.line, func(t *testing.T) {
			// Act / Assert
			if got := goBuildCall.MatchString(tt.line); got != tt.match {
				t.Fatalf("match = %v, want %v", got, tt.match)
			}
		})
	}
}
