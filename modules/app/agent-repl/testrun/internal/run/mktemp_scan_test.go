package run

import (
	"bufio"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"strings"
	"testing"
)

// macOS's mktemp IGNORES TMPDIR: with no template (or with -t) it creates in
// the user temp directory whatever TMPDIR says, so the temp root OSExec hands
// a unit cannot hold it. A shell script must name its parent itself,
// `mktemp -d "${TMPDIR:-/tmp}/name.XXXXXX"`. This scan holds every script the
// suites run to that; the e2e sandbox's scripts run on Linux, whose mktemp
// honors TMPDIR, and are not scanned.
var bareMktemp = regexp.MustCompile(`\bmktemp((\s+-[A-Za-z]+)*)\s*($|[);|&]|-t\b)`)

func TestNoSuiteScriptMakesATempFileWithoutNamingItsParent(t *testing.T) {
	// Arrange
	_, self, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller could not locate this file")
	}
	module := filepath.Clean(filepath.Join(filepath.Dir(self), "..", "..", ".."))
	repo := filepath.Clean(filepath.Join(module, "..", "..", ".."))
	var scripts []string
	for _, dir := range []string{filepath.Join(module, "bin"), filepath.Join(module, "scripts"), filepath.Join(repo, ".githooks"), filepath.Join(repo, ".claude")} {
		matches, err := filepath.Glob(filepath.Join(dir, "*.sh"))
		if err != nil {
			t.Fatal(err)
		}
		scripts = append(scripts, matches...)
	}
	if len(scripts) == 0 {
		t.Fatal("found no scripts to scan: the layout this test assumes moved")
	}

	// Act
	var bare []string
	for _, path := range scripts {
		f, err := os.Open(path)
		if err != nil {
			t.Fatal(err)
		}
		sc := bufio.NewScanner(f)
		sc.Buffer(make([]byte, 1024*1024), 1024*1024)
		for n := 1; sc.Scan(); n++ {
			line := strings.TrimSpace(sc.Text())
			if strings.HasPrefix(line, "#") {
				continue
			}
			if bareMktemp.MatchString(line) {
				rel, _ := filepath.Rel(repo, path)
				bare = append(bare, rel+":"+strconv.Itoa(n)+": "+line)
			}
		}
		if err := sc.Err(); err != nil {
			t.Fatalf("read %s: %v", path, err)
		}
		f.Close()
	}

	// Assert
	if len(bare) > 0 {
		t.Fatalf("mktemp without a named parent writes to the user temp directory on macOS, whatever TMPDIR says; name it, e.g. mktemp -d \"${TMPDIR:-/tmp}/name.XXXXXX\":\n%s", strings.Join(bare, "\n"))
	}
}

func TestTheBareMktempPatternMatchesOnlyCallsWithoutAParent(t *testing.T) {
	tests := []struct {
		line string
		bare bool
	}{
		{`d="$(mktemp -d)"`, true},
		{`f="$(mktemp)"`, true},
		{`d="$(mktemp -d -t foo)"`, true},
		{`mktemp -d`, true},
		{`d="$(mktemp -d "${TMPDIR:-/tmp}/x.XXXXXX")"`, false},
		{`f="$(mktemp "$TMP/case.XXXXXX")"`, false},
		{`d="$(mktemp -d /tmp/bounce-test.XXXXXX)"`, false},
	}
	for _, tt := range tests {
		t.Run(tt.line, func(t *testing.T) {
			// Act / Assert
			if got := bareMktemp.MatchString(tt.line); got != tt.bare {
				t.Fatalf("bare = %v, want %v", got, tt.bare)
			}
		})
	}
}
