package suites

import (
	"maps"
	"slices"
	"strconv"
	"strings"
	"testing"
)

func TestParseVitestNamesJoinsEachTestsPathAsItsPatternMatchesIt(t *testing.T) {
	// Arrange
	out := []byte(`[{"name":"feed > draws a row","file":"/pkg/test/feed.test.ts"},{"name":"boots","file":"/pkg/test/boot.test.ts"}]`)

	// Act
	got, err := parseVitestNames("/pkg", out)

	// Assert
	want := map[string][]string{"test/feed.test.ts": {"feed draws a row"}, "test/boot.test.ts": {"boots"}}
	if err != nil || !maps.EqualFunc(got, want, slices.Equal) {
		t.Fatalf("parseVitestNames = %v, %v; want %v", got, err, want)
	}
}

func TestParseVitestNamesRefuses(t *testing.T) {
	tests := []struct {
		name string
		out  string
		want string
	}{
		{name: "output that is no list", out: "No test files found", want: "printed no test list"},
		{name: "a test with no name", out: `[{"name":"","file":"/pkg/a.test.ts"}]`, want: "no name or file"},
		{name: "a test outside the package", out: `[{"name":"x","file":"/elsewhere/a.test.ts"}]`, want: "is not under /pkg"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := parseVitestNames("/pkg", []byte(tt.out))

			// Assert
			if err == nil || got != nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("parseVitestNames = %v, %v; want an error saying %q", got, err, tt.want)
			}
		})
	}
}

func TestParseERTNamesReadsOnlyItsLines(t *testing.T) {
	// Arrange
	out := []byte("Loading...\nTESTRUN-NAME test-a.el test-a-one\nTESTRUN-NAME test-a.el test-a-two\n")

	// Act
	got, err := parseERTNames(out, []string{"test-a.el", "test-b.el"})

	// Assert
	want := map[string][]string{"test-a.el": {"test-a-one", "test-a-two"}}
	if err != nil || !maps.EqualFunc(got, want, slices.Equal) {
		t.Fatalf("parseERTNames = %v, %v; want %v", got, err, want)
	}
}

func TestParseERTNamesRefuses(t *testing.T) {
	tests := []struct {
		name string
		out  string
		want string
	}{
		{name: "a line naming no test", out: "TESTRUN-NAME test-a.el\n", want: "unreadable line"},
		{name: "a file it was not asked to list", out: "TESTRUN-NAME test-z.el z\n", want: "not asked to list"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := parseERTNames([]byte(tt.out), []string{"test-a.el"})

			// Assert
			if err == nil || got != nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("parseERTNames = %v, %v; want an error saying %q", got, err, tt.want)
			}
		})
	}
}

func TestTheERTDriverListsTheTestsOfTheFilesItIsAsked(t *testing.T) {
	tests := []struct {
		name     string
		files    map[string]string
		list     []string
		wantExit int
		wantOut  []string
		wantNot  string
	}{
		{
			name:     "each asked file's tests print, and no other file's",
			files:    map[string]string{"test-a.el": "(ert-deftest fixture-a-one () (should t))\n(ert-deftest fixture-a-two () (should nil))\n", "test-b.el": "(ert-deftest fixture-b () (should t))\n"},
			list:     []string{"test-a.el"},
			wantOut:  []string{"TESTRUN-NAME test-a.el fixture-a-one", "TESTRUN-NAME test-a.el fixture-a-two"},
			wantNot:  "fixture-b",
			wantExit: 0,
		},
		{
			name:     "a file that cannot load breaks the listing",
			files:    map[string]string{"test-a.el": "(ert-deftest fixture-a-one ("},
			list:     []string{"test-a.el"},
			wantOut:  []string{"listing (test-a.el) failed"},
			wantExit: 2,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := ertFixture(t, tt.files)

			// Act
			out, code := runDriverForm(t, dir, "(agent-repl-testrun-ert-list "+strconv.Quote(dir+"/")+" "+lispList(tt.list)+")")

			// Assert
			if code != tt.wantExit {
				t.Fatalf("exit = %d, want %d; output:\n%s", code, tt.wantExit, out)
			}
			for _, w := range tt.wantOut {
				if !strings.Contains(out, w) {
					t.Fatalf("output lacks %q:\n%s", w, out)
				}
			}
			if tt.wantNot != "" && strings.Contains(out, tt.wantNot) {
				t.Fatalf("output names %q, a file it was not asked to list:\n%s", tt.wantNot, out)
			}
		})
	}
}
