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

// A grep that exits at its first match (-q) must never be fed by a pipe. Under
// pipefail the writer is killed by SIGPIPE when grep exits before it has
// written everything, the pipeline answers 141, and a check whose output was
// correct fails -- only under load, when the writer is descheduled mid-write
// (2026-10-06: the logs harness failed 10 of 24 runs six-wide). Text a script
// already holds, or a command's captured output, goes through grep_in
// (bin/lib-grep-in.sh), which feeds grep from a here-string.
var pipedQuietGrep = regexp.MustCompile(`(^|[^|])\|\s*grep\b[^|;&)]*\s-[A-Za-z]*q`)

func TestNoSuiteScriptPipesIntoAQuietGrep(t *testing.T) {
	// Arrange
	_, self, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller could not locate this file")
	}
	module := filepath.Clean(filepath.Join(filepath.Dir(self), "..", "..", ".."))
	repo := filepath.Clean(filepath.Join(module, "..", "..", ".."))
	var scripts []string
	for _, glob := range []string{
		filepath.Join(module, "bin", "*.sh"),
		filepath.Join(module, "scripts", "*.sh"),
		filepath.Join(repo, ".githooks", "*.sh"),
		filepath.Join(repo, ".claude", "*.sh"),
		filepath.Join(repo, ".claude", "skills", "*", "*.sh"),
		filepath.Join(repo, "bin", "*.sh"),
	} {
		matches, err := filepath.Glob(glob)
		if err != nil {
			t.Fatal(err)
		}
		scripts = append(scripts, matches...)
	}
	if len(scripts) == 0 {
		t.Fatal("found no scripts to scan: the layout this test assumes moved")
	}

	// Act
	var piped []string
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
			if pipedQuietGrep.MatchString(line) {
				rel, _ := filepath.Rel(repo, path)
				piped = append(piped, rel+":"+strconv.Itoa(n)+": "+line)
			}
		}
		if err := sc.Err(); err != nil {
			t.Fatalf("read %s: %v", path, err)
		}
		f.Close()
	}

	// Assert
	if len(piped) > 0 {
		t.Fatalf("a pipe into an early-exiting grep fails under pipefail whenever grep matches first (the writer dies of SIGPIPE); use grep_in \"$text\" -q PATTERN from bin/lib-grep-in.sh:\n%s", strings.Join(piped, "\n"))
	}
}

func TestThePipedQuietGrepPatternMatchesOnlyPipesIntoAQuietGrep(t *testing.T) {
	tests := []struct {
		line  string
		piped bool
	}{
		{`printf '%s\n' "$out" | grep -q foo`, true},
		{`echo "$out" | grep -Eq "$re"`, true},
		{`cmd | grep -qv bar`, true},
		{`"$bin" help | grep -Fxq word`, true},
		{`ps -o command= | tr ' ' '\n' | grep -q "^X="`, true},
		{`grep_in "$out" -q foo`, false},
		{`[ -f x ] || grep -q foo file`, false},
		{`grep -q foo "$file"`, false},
		{`cmd | grep -v bar >/dev/null`, false},
		{`cmd | grep foo | wc -l`, false},
	}
	for _, tt := range tests {
		t.Run(tt.line, func(t *testing.T) {
			// Act / Assert
			if got := pipedQuietGrep.MatchString(tt.line); got != tt.piped {
				t.Fatalf("piped = %v, want %v", got, tt.piped)
			}
		})
	}
}
