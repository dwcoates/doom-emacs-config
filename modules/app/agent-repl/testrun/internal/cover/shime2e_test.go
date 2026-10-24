package cover

import (
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

// writeFile writes body at path, making its directory.
func writeFile(t *testing.T, path, body string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
}

// fakeC8 writes SUMMARY as c8's json-summary report, recording each call.
type fakeC8 struct {
	summary string
	err     error
	calls   [][]string
}

func (f *fakeC8) run(dir, name string, args ...string) ([]byte, error) {
	f.calls = append(f.calls, append([]string{dir, name}, args...))
	if f.err != nil {
		return nil, f.err
	}
	for _, a := range args {
		if out, ok := strings.CutPrefix(a, "--reports-dir="); ok {
			if err := os.MkdirAll(out, 0o755); err != nil {
				return nil, err
			}
			return nil, os.WriteFile(filepath.Join(out, "coverage-summary.json"), []byte(f.summary), 0o644)
		}
	}
	return nil, errors.New("no --reports-dir")
}

const shimSummaryJSON = `{
 "total": {"statements": {"covered": 99, "total": 99}},
 "/w/agent-shim/claude/shim/src/a.ts": {"statements": {"covered": 3, "total": 4}},
 "/w/agent-shim/claude/shim/src/b.ts": {"statements": {"covered": 0, "total": 4}},
 "/w/agent-shim/claude/shim/node_modules/x/index.js": {"statements": {"covered": 9, "total": 9}},
 "/w/elsewhere/c.ts": {"statements": {"covered": 5, "total": 5}}
}`

func TestShimE2EReachRendersAndSumsTheShimsOwnSources(t *testing.T) {
	// Arrange
	root, reports := t.TempDir(), t.TempDir()
	c8 := filepath.Join(t.TempDir(), "c8")
	writeFile(t, c8, "")
	writeFile(t, filepath.Join(root, "e2e", "shim", "coverage-1.json"), "{}")
	f := &fakeC8{summary: shimSummaryJSON}

	// Act
	lines, err := ShimE2EReach(f.run, c8, root, reports)

	// Assert
	if err != nil || len(lines) != 1 || !strings.HasPrefix(lines[0], "37.5% (3/8 statements over 2 files)") {
		t.Fatalf("ShimE2EReach = %q, %v; want the shim's own 3 of 8 statements", lines, err)
	}
	want := []string{filepath.Join(root, "e2e"), c8, "report", "--temp-directory=" + filepath.Join(root, "e2e", "shim"),
		"--reports-dir=" + filepath.Join(reports, "0"), "--reporter=json-summary", "--reporter=html"}
	if len(f.calls) != 1 || !slices.Equal(f.calls[0], want) {
		t.Fatalf("c8 ran %q, want %q: from the directory above the profiles", f.calls, want)
	}
}

func TestShimE2EReachOfARunWithNoE2EShimAnswersNone(t *testing.T) {
	// Arrange: a counter directory and an empty shim directory, no profile.
	root := t.TempDir()
	writeFile(t, filepath.Join(root, "daemon", "covmeta.1"), "")
	if err := os.MkdirAll(filepath.Join(root, "e2e", "shim"), 0o755); err != nil {
		t.Fatal(err)
	}
	f := &fakeC8{}

	// Act
	lines, err := ShimE2EReach(f.run, "/no/c8", root, t.TempDir())

	// Assert
	if err != nil || lines != nil || f.calls != nil {
		t.Fatalf("ShimE2EReach = %q, %v, c8 ran %q; want nothing measured and nothing run", lines, err, f.calls)
	}
}

func TestShimE2EReachRefuses(t *testing.T) {
	cases := []struct {
		name    string
		c8      bool
		summary string
		runErr  error
		want    string
	}{
		{name: "c8 not installed", c8: false, want: "c8 is not installed"},
		{name: "c8 failed", c8: true, runErr: errors.New("exit 1"), want: "raw v8 profiles remain there"},
		{name: "no shim source survived the remap", c8: true, summary: `{"total": {"statements": {"covered": 1, "total": 1}}}`, want: "no agent-shim source survived"},
		{name: "a summary that is no JSON", c8: true, summary: "not json", want: "decode the shim's c8 summary"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			root := t.TempDir()
			writeFile(t, filepath.Join(root, "e2e", "shim", "coverage-1.json"), "{}")
			c8 := filepath.Join(t.TempDir(), "c8")
			if tc.c8 {
				writeFile(t, c8, "")
			}
			f := &fakeC8{summary: tc.summary, err: tc.runErr}

			// Act
			_, err := ShimE2EReach(f.run, c8, root, t.TempDir())

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("ShimE2EReach = %v, want an error containing %q", err, tc.want)
			}
		})
	}
}
