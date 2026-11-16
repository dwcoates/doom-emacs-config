package procstat

import (
	"errors"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strings"
	"testing"
	"time"
)

// newTree writes a /proc fixture: the host's stat and each file named in
// files, keyed by its path under the root.
func newTree(t *testing.T, files map[string]string) Tree {
	t.Helper()
	root := t.TempDir()
	all := map[string]string{"stat": "cpu  1 2 3 4\nbtime 1700000000\n"}
	maps.Copy(all, files)
	for path, content := range all {
		full := filepath.Join(root, path)
		if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(full, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return New(root)
}

// stat42 is a process started 250 ticks (2.5s) after boot.
const stat42 = "42 (node) S 1 42 42 0 -1 0 0 0 0 0 150 50 7 3 20 0 1 0 250 0 0"

func TestTreeListsEveryNumericEntry(t *testing.T) {
	// Arrange
	tree := newTree(t, map[string]string{"42/stat": stat42, "7/stat": stat42, "self/stat": stat42})

	// Act
	pids, err := tree.PIDs()

	// Assert
	slices.Sort(pids)
	if err != nil || !slices.Equal(pids, []int{7, 42}) {
		t.Fatalf("PIDs = (%v, %v), want 7 and 42", pids, err)
	}
}

func TestTreeListingAMissingRootIsAnError(t *testing.T) {
	// Act
	_, err := New(filepath.Join(t.TempDir(), "absent")).PIDs()

	// Assert
	if err == nil || errors.Is(err, ErrGone) {
		t.Fatalf("PIDs = %v, want an error that is not ErrGone", err)
	}
}

func TestTreeReadsOfAnExitedProcessAreGone(t *testing.T) {
	// Arrange
	tree := newTree(t, nil)

	// Act
	_, statErr := tree.Stat(42)
	_, statusErr := tree.Status(42)
	_, cmdErr := tree.Cmdline(42)
	_, envErr := tree.Environ(42)

	// Assert
	for _, err := range []error{statErr, statusErr, cmdErr, envErr} {
		if !errors.Is(err, ErrGone) {
			t.Fatalf("reads = %v %v %v %v, want ErrGone from each", statErr, statusErr, cmdErr, envErr)
		}
	}
}

func TestTreeReadKeepsAnyOtherCause(t *testing.T) {
	// Arrange: a directory where a file should be fails to read, not as gone.
	tree := newTree(t, map[string]string{"42/environ/x": ""})

	// Act
	_, err := tree.Environ(42)

	// Assert
	if err == nil || errors.Is(err, ErrGone) || !strings.Contains(err.Error(), "read environ of pid 42") {
		t.Fatalf("Environ = %v, want the read's own failure", err)
	}
}

func TestTreeReadsAProcess(t *testing.T) {
	// Arrange
	tree := newTree(t, map[string]string{
		"42/stat":    stat42,
		"42/status":  "Name:\tnode\nUid:\t1000\t1000\t1000\t1000\nVmRSS:\t  2048 kB\n",
		"42/cmdline": "node\x00main.js\x00",
		"42/environ": "HOME=/home/u\x00MARK=1\x00",
	})

	// Act
	line, statErr := tree.Stat(42)
	status, statusErr := tree.Status(42)
	argv, cmdErr := tree.Cmdline(42)
	env, envErr := tree.Environ(42)

	// Assert
	if err := errors.Join(statErr, statusErr, cmdErr, envErr); err != nil {
		t.Fatal(err)
	}
	if line.Comm != "node" || line.Fields[0] != "S" {
		t.Fatalf("Stat = %+v, want node in state S", line)
	}
	if status["Uid"] != "1000\t1000\t1000\t1000" || status["VmRSS"] != "2048 kB" {
		t.Fatalf("Status = %q", status)
	}
	if !slices.Equal(argv, []string{"node", "main.js"}) || !slices.Equal(env, []string{"HOME=/home/u", "MARK=1"}) {
		t.Fatalf("Cmdline = %q, Environ = %q", argv, env)
	}
}

func TestTreeStartIsBootPlusTheStartTicks(t *testing.T) {
	// Arrange
	tree := newTree(t, map[string]string{"42/stat": stat42})
	line, err := tree.Stat(42)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	start, err := tree.Start(line)

	// Assert
	want := time.Unix(1700000000, 0).Add(2500 * time.Millisecond)
	if err != nil || !start.Equal(want) {
		t.Fatalf("Start = (%v, %v), want %v", start, err, want)
	}
}

func TestTreeStartRefusesAHostStatWithNoBootTime(t *testing.T) {
	// Arrange
	tree := newTree(t, map[string]string{"stat": "cpu 1 2 3 4\n", "42/stat": stat42})
	line, err := tree.Stat(42)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	_, err = tree.Start(line)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no btime") {
		t.Fatalf("Start = %v, want the missing btime named", err)
	}
}

func TestTreeStatRefusesAMalformedLine(t *testing.T) {
	// Arrange
	tree := newTree(t, map[string]string{"42/stat": "42 node S"})

	// Act
	_, err := tree.Stat(42)

	// Assert
	if err == nil || errors.Is(err, ErrGone) || !strings.Contains(err.Error(), "pid 42") {
		t.Fatalf("Stat = %v, want a parse failure naming pid 42", err)
	}
}

func TestGone(t *testing.T) {
	cases := []struct {
		name string
		err  error
		want bool
	}{
		{name: "procstat's own", err: ErrGone, want: true},
		{name: "a missing file", err: fs.ErrNotExist, want: true},
		{name: "a permission refusal is not", err: fs.ErrPermission, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Gone(tc.err)

			// Assert
			if got != tc.want {
				t.Fatalf("Gone(%v) = %v, want %v", tc.err, got, tc.want)
			}
		})
	}
}

func TestParseKeyed(t *testing.T) {
	// Act
	got := ParseKeyed([]byte("VmRSS:\t  2048 kB\nbtime 1700000000\nread_bytes: 4096\n\nkey with space: v\n"))

	// Assert
	want := map[string]string{"VmRSS": "2048 kB", "btime": "1700000000", "read_bytes": "4096", "key": "with space: v"}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("ParseKeyed = %q, want %q", got, want)
	}
}

func TestStatLineUint(t *testing.T) {
	// Arrange
	line, err := ParseStat("42 (node) S 1 x")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	ppid, ppidErr := line.Uint(4)
	_, badErr := line.Uint(5)
	_, missingErr := line.Uint(6)

	// Assert
	if ppid != 1 || ppidErr != nil || badErr == nil || !strings.Contains(badErr.Error(), "stat field 5") || missingErr == nil {
		t.Fatalf("Uint = (%d, %v), field 5 %v, field 6 %v", ppid, ppidErr, badErr, missingErr)
	}
}

func TestKiB(t *testing.T) {
	cases := []struct {
		name    string
		fields  map[string]string
		want    uint64
		wantErr string
	}{
		{name: "kilobytes in bytes", fields: map[string]string{"VmRSS": "2 kB"}, want: 2048},
		{name: "a missing field", fields: map[string]string{}, wantErr: "no VmRSS field"},
		{name: "a value that is no number", fields: map[string]string{"VmRSS": "lots kB"}, wantErr: `VmRSS "lots kB"`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := KiB(tc.fields, "VmRSS")

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("KiB error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("KiB = (%d, %v), want %d", got, err, tc.want)
			}
		})
	}
}
