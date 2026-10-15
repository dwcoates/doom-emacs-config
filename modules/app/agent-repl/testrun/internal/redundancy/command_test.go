package redundancy

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"
)

type bufLog struct{ bytes.Buffer }

func (l *bufLog) Infof(f string, a ...any) {
	l.WriteString("INFO " + strings.TrimSpace(sprintf(f, a...)) + "\n")
}
func (l *bufLog) Errorf(f string, a ...any) {
	l.WriteString("ERROR " + strings.TrimSpace(sprintf(f, a...)) + "\n")
}

func sprintf(f string, a ...any) string { return fmt.Sprintf(f, a...) }

func TestSplitListDropsEmptyEntries(t *testing.T) {
	if got := SplitList(" a, ,b,"); !reflect.DeepEqual(got, []string{"a", "b"}) {
		t.Fatalf("SplitList = %v", got)
	}
}

func TestReadDeleteSet(t *testing.T) {
	tests := []struct {
		name, body string
		missing    bool
		want       []string
		wantErr    string
	}{
		{name: "comments and blank lines are skipped", body: "# rows\nA\n\n  B  \n", want: []string{"A", "B"}},
		{name: "a set naming nothing", body: "# none\n", wantErr: "names no case"},
		{name: "a missing file", missing: true, wantErr: "read -delete"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "d.txt")
			if !tt.missing {
				if err := os.WriteFile(path, []byte(tt.body), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := ReadDeleteSet(path)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("ReadDeleteSet = %v, %v", got, err)
			}
		})
	}
}

func TestCommonRegisterDefaultsAndRefusals(t *testing.T) {
	tests := []struct {
		name    string
		argv    []string
		wantErr string
	}{
		{name: "defaults", argv: []string{"-out", "/o"}},
		{name: "no -out", argv: nil, wantErr: "-out is required"},
		{name: "an unknown universe", argv: []string{"-out", "/o", "-universe", "lines"}, wantErr: "unknown universe"},
		{name: "a negative budget", argv: []string{"-out", "/o", "-max-coverage-drop", "-1"}, wantErr: "must not be negative"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var c Common
			fs := flag.NewFlagSet("x", flag.ContinueOnError)
			finish := c.Register(fs)

			// Act
			err := fs.Parse(tt.argv)
			if err == nil {
				err = finish()
			}

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || c.Universe != KillsOnly || c.Budget.MaxDrop != DefaultMaxDrop || c.MaxLoad != 32 || c.QuietWait != time.Hour || c.Overhead != DefaultOverhead {
				t.Fatalf("common = %+v, %v", c, err)
			}
		})
	}
}

func TestRefusedAtNormalPriority(t *testing.T) {
	// Arrange
	t.Setenv(EnvBackground, "")
	var log bufLog

	// Act / Assert
	if code := RefusedAtNormalPriority(&log); code != 78 || !strings.Contains(log.String(), "ERROR redundancy: refused at normal priority") {
		t.Fatalf("code %d, log %q", code, log.String())
	}
	t.Setenv(EnvBackground, "1")
	if code := RefusedAtNormalPriority(&log); code != 0 {
		t.Fatalf("code %d at background priority", code)
	}
}

func TestRunWritesBothOutputsAndRemovesItsScratch(t *testing.T) {
	// Arrange
	out := t.TempDir()
	var log bufLog
	var scratch string
	c := Common{Out: out, Budget: Budget{MaxDrop: 100}}

	// Act
	code := Run(&log, c, "x", func(s string) (*Measurement, error) {
		scratch = s
		return reportFixture(), nil
	})

	// Assert
	if code != 0 {
		t.Fatalf("code %d, log %q", code, log.String())
	}
	for _, f := range []string{"measurement.json", "report.md"} {
		if _, err := os.Stat(filepath.Join(out, f)); err != nil {
			t.Errorf("%s: %v", f, err)
		}
	}
	if _, err := os.Stat(scratch); !os.IsNotExist(err) {
		t.Errorf("scratch %s outlived the run (%v)", scratch, err)
	}
}

func TestRunLogsAFailedMeasurementOnceAndWritesNothing(t *testing.T) {
	// Arrange
	out := t.TempDir()
	var log bufLog

	// Act
	code := Run(&log, Common{Out: out}, "./p", func(string) (*Measurement, error) { return nil, errors.New("boom") })

	// Assert
	if code != 1 || strings.Count(log.String(), "ERROR") != 1 || !strings.Contains(log.String(), "ERROR redundancy: measure ./p: boom") {
		t.Fatalf("code %d, log %q", code, log.String())
	}
	if _, err := os.Stat(filepath.Join(out, "measurement.json")); !os.IsNotExist(err) {
		t.Fatalf("a failed measurement wrote measurement.json (%v)", err)
	}
}

// TestNoAdapterRollsItsOwnCommandScaffold holds every adapter to the shared
// pieces: a second deletion-file reader, list splitter or scratch directory
// would drift from these.
func TestNoAdapterRollsItsOwnCommandScaffold(t *testing.T) {
	shapes := []string{"func readDeleteSet(", "func splitList(", `"agent-repl-redundancy-"`, "func WriteOutputs("}
	err := filepath.WalkDir(".", func(path string, d os.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || path == "command.go" {
			return err
		}
		b, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for _, s := range shapes {
			if strings.Contains(string(b), s) {
				t.Errorf("%s holds %s; use redundancy's", path, s)
			}
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
}

func TestFindRootAnswersTheRealPathBehindASymlink(t *testing.T) {
	// Arrange: a root reached through a symlinked directory.
	real := t.TempDir()
	if err := os.MkdirAll(filepath.Join(real, "testrun", "roster"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(real, "testrun", "roster", "roster.go"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(t.TempDir(), "link")
	if err := os.Symlink(real, link); err != nil {
		t.Fatal(err)
	}
	want, err := filepath.EvalSymlinks(real)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := FindRoot(filepath.Join(link, "testrun"))

	// Assert
	if err != nil || got != want {
		t.Fatalf("FindRoot = %q, %v; want %q", got, err, want)
	}
}

func TestRealPathResolvesASymlinkedDirectory(t *testing.T) {
	// Arrange
	real, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(t.TempDir(), "link")
	if err := os.Symlink(real, link); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := RealPath(link)

	// Assert
	if err != nil || got != real {
		t.Fatalf("RealPath = %q, %v, want %q", got, err, real)
	}
}

func TestRealPathRefusesAMissingDirectory(t *testing.T) {
	// Arrange
	missing := filepath.Join(t.TempDir(), "gone")

	// Act
	_, err := RealPath(missing)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "redundancy: resolve "+missing) {
		t.Fatalf("err = %v, want the directory named", err)
	}
}
