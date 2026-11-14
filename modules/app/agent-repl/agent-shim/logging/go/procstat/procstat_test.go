package procstat

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

func TestParseStat(t *testing.T) {
	cases := []struct {
		name       string
		stat       string
		wantComm   string
		wantFields []string
		wantErr    string
	}{
		{name: "an ordinary line", stat: "42 (node) S 1 42", wantComm: "node", wantFields: []string{"S", "1", "42"}},
		{name: "a command name holding spaces and parentheses", stat: "42 (a (b) c) R 1", wantComm: "a (b) c", wantFields: []string{"R", "1"}},
		{name: "a line with no command name", stat: "42 node R", wantErr: "no command name"},
		{name: "a close before the open", stat: "42 )node( R", wantErr: "no command name"},
		{name: "a line with nothing after its command name", stat: "42 (node) ", wantErr: "no state after its command name"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := ParseStat(tc.stat)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("ParseStat error = %v, want one containing %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got.Comm != tc.wantComm || !slices.Equal(got.Fields, tc.wantFields) {
				t.Fatalf("ParseStat = %+v, %v; want comm %q fields %v", got, err, tc.wantComm, tc.wantFields)
			}
		})
	}
}

func TestStatLineField(t *testing.T) {
	// Arrange
	line, err := ParseStat("42 (node) S 1 42")
	if err != nil {
		t.Fatal(err)
	}
	cases := []struct {
		name    string
		n       int
		want    string
		wantErr bool
	}{
		{name: "the state is field 3", n: 3, want: "S"},
		{name: "the ppid is field 4", n: 4, want: "1"},
		{name: "the last field present", n: 5, want: "42"},
		{name: "a field past the line's end is refused", n: 6, wantErr: true},
		{name: "a field before the state is not after the command", n: 2, wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := line.Field(tc.n)

			// Assert
			if (err != nil) != tc.wantErr || got != tc.want {
				t.Fatalf("Field(%d) = (%q, %v), want %q (error %v)", tc.n, got, err, tc.want, tc.wantErr)
			}
		})
	}
}

func TestParseCmdline(t *testing.T) {
	cases := []struct {
		name string
		raw  string
		want []string
	}{
		{name: "argv split on its NULs", raw: "node\x00main.js\x00--listen\x00/s/ws.sock\x00", want: []string{"node", "main.js", "--listen", "/s/ws.sock"}},
		{name: "an empty argument inside argv is kept", raw: "a\x00\x00b\x00", want: []string{"a", "", "b"}},
		{name: "a rewritten title's NUL padding is dropped", raw: "node (vitest)\x00\x00\x00\x00", want: []string{"node (vitest)"}},
		{name: "a single argument stands alone", raw: "/bin/x\x00", want: []string{"/bin/x"}},
		{name: "a kernel thread has no argv", raw: "", want: []string{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ParseCmdline([]byte(tc.raw))

			// Assert
			if !slices.Equal(got, tc.want) {
				t.Fatalf("ParseCmdline = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestEveryProcFileIsParsedByProcstat pins that no other Go source of
// agent-repl splits a /proc stat line at its command name by hand, and that
// every source reading a cmdline decodes it here: every program reads them
// through this package.
func TestEveryProcFileIsParsedByProcstat(t *testing.T) {
	// Arrange: this package sits at agent-shim/logging/go/procstat.
	root := filepath.Join("..", "..", "..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() && d.Name() == "node_modules" {
			return filepath.SkipDir
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || filepath.Base(filepath.Dir(path)) == "procstat" {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		text := string(src)
		// A /proc path's cmdline, not pprof's endpoint of the same name.
		readsCmdline := strings.Contains(text, `"/cmdline"`) || strings.Contains(text, `, "cmdline")`)
		if readsCmdline && !strings.Contains(text, "procstat.ParseCmdline(") {
			offenders = append(offenders, path)
			return nil
		}
		for line := range strings.SplitSeq(text, "\n") {
			if strings.Contains(line, "LastIndexByte(") && strings.Contains(line, "')'") {
				offenders = append(offenders, path)
				break
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk agent-repl: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("%v parse a /proc file by hand instead of through procstat", offenders)
	}
}
