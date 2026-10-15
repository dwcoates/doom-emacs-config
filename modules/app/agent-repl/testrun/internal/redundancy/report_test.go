package redundancy

import (
	"bytes"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"
)

// reportFixture: Big alone kills m1 and m2, so kills-only keeps it and
// drops Line and Dup. Under strict, Line alone reaches b2 and Dup reaches b1
// for less than Big, so strict keeps Line and Dup and drops Big.
func reportFixture() *Measurement {
	return &Measurement{
		Adapter: "go", Subject: "d/p/a_test.go", Scope: []string{"./p"},
		Cases: []Case{
			{ID: "Big", Cost: 2 * ms, Kills: []string{"m1", "m2"}, Blocks: []string{"b1"}},
			{ID: "Line", Cost: 2 * ms, Kills: []string{"m1"}, Blocks: []string{"b2"}},
			{ID: "Dup", Cost: 1 * ms, Kills: []string{"m2"}, Blocks: []string{"b1"}},
			{ID: "Log", Cost: 1 * ms, Kills: []string{"m3"}, Pinned: "asserts a log record: Log -> FindRecord"},
		},
		Mutants: []MutantResult{
			{Key: "a.go:1:1:< -> <=", Status: "killed", KilledBy: []string{"Big"}},
			{Key: "a.go:2:1:< -> >=", Status: "survived"},
			{Key: "a.go:3:1:true -> false", Status: "unreached"},
			{Key: "a.go:4:1:+ -> -", Status: "dropped"},
		},
		Anomalies: []string{"X failed under M, which it does not reach"},
		Hangs:     []string{"Big hung under M past its 1s bound"},
		Cost:      Cost{Wall: 90 * time.Second, CPU: 30 * time.Second, Processes: 12, Phases: []Phase{{Name: "mutant runs", Wall: time.Minute}}},
	}
}

func TestReportUnderKillsOnly(t *testing.T) {
	// Arrange
	var out bytes.Buffer

	// Act
	err := Report(&out, reportFixture(), KillsOnly, 0)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"- Universe: kills-only (every killed mutant the whole set catches stays caught)",
		"- Cases: 4, pinned: 1, removable: 2, kept: 2",
		"- Case runtime: 6ms total, 3ms kept, 3ms saved",
		"- killed: 1\n- survived: 1\n- unreached: 1\n- dropped: 1\n- score over reached mutants: 50.0%",
		"| `Line` | 2ms | dominated by Big |",
		"| `Dup` | 1ms | dominated by Big |",
		"- Removable under strict only: 1, saving 2ms\n  - `Big`",
		"- Removable under kills-only only: 2, saving 3ms\n  - `Dup`\n  - `Line`",
		"## Anomalies\n\n- X failed under M, which it does not reach",
		"## Kills by hang (1)\n\n- Big hung under M past its 1s bound",
		"- `Log`: asserts a log record: Log -> FindRecord",
		"- Wall: 1m30s, CPU: 30s, processes: 12\n- mutant runs: 1m0s",
	} {
		if !strings.Contains(out.String(), want) {
			t.Errorf("the report lacks %q:\n%s", want, out.String())
		}
	}
}

func TestReportUnderStrictListsWhatKillsOnlyWouldAlsoRemove(t *testing.T) {
	// Arrange
	var out bytes.Buffer

	// Act
	err := Report(&out, reportFixture(), Strict, 0)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"- Cases: 4, pinned: 1, removable: 1, kept: 3",
		"| `Big` | 2ms | Dup, Line |",
		"## Under kills-only instead",
		"- Removable under kills-only only: 2, saving 3ms\n  - `Dup`\n  - `Line`",
	} {
		if !strings.Contains(out.String(), want) {
			t.Errorf("the report lacks %q:\n%s", want, out.String())
		}
	}
}

func TestReportRefusesAMeasurementWithACaseTwice(t *testing.T) {
	// Arrange
	m := reportFixture()
	m.Cases = append(m.Cases, m.Cases[0])

	// Act
	err := Report(&bytes.Buffer{}, m, KillsOnly, 0)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "measured twice") {
		t.Fatalf("err = %v", err)
	}
}

type failWriter struct{}

func (failWriter) Write([]byte) (int, error) { return 0, errors.New("disk full") }

func TestReportSurfacesAFailedWrite(t *testing.T) {
	// Act
	err := Report(failWriter{}, reportFixture(), KillsOnly, 0)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "write the report: disk full") {
		t.Fatalf("err = %v", err)
	}
}

func TestMeasurementRoundTrips(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "m.json")
	want := reportFixture()

	// Act
	err := WriteMeasurement(path, want)
	got, rerr := ReadMeasurement(path)

	// Assert
	if err != nil || rerr != nil {
		t.Fatal(err, rerr)
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("read back %+v\nwant %+v", got, want)
	}
}

func TestReadMeasurementRefuses(t *testing.T) {
	tests := []struct {
		name string
		body *string
		want string
	}{
		{name: "a missing file", want: "read"},
		{name: "a file that is not JSON", body: ptr("{"), want: "decode"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "m.json")
			if tt.body != nil {
				if err := os.WriteFile(path, []byte(*tt.body), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			_, err := ReadMeasurement(path)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func ptr(s string) *string { return &s }

type logCapture struct{ errs []string }

func (l *logCapture) Errorf(format string, args ...any) {
	l.errs = append(l.errs, strings.TrimSpace(strings.ReplaceAll(format, "%v", "")))
	_ = args
}

func TestReportCommand(t *testing.T) {
	path := filepath.Join(t.TempDir(), "m.json")
	if err := WriteMeasurement(path, reportFixture()); err != nil {
		t.Fatal(err)
	}
	tests := []struct {
		name     string
		argv     []string
		wantCode int
		wantOut  string
		wantErrs int
	}{
		{name: "redraws a saved measurement", argv: []string{"-in", path, "-universe", "strict"}, wantCode: 0, wantOut: "- Universe: strict"},
		{name: "refuses no -in", argv: nil, wantCode: 2, wantErrs: 1},
		{name: "refuses an unknown universe", argv: []string{"-in", path, "-universe", "lines"}, wantCode: 2, wantErrs: 1},
		{name: "fails on an unreadable measurement", argv: []string{"-in", path + ".gone"}, wantCode: 1, wantErrs: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var out bytes.Buffer
			log := &logCapture{}

			// Act
			code := ReportCommand(log, tt.argv, &out)

			// Assert
			if code != tt.wantCode || len(log.errs) != tt.wantErrs {
				t.Fatalf("code %d, errors %v; want %d, %d", code, log.errs, tt.wantCode, tt.wantErrs)
			}
			if !strings.Contains(out.String(), tt.wantOut) {
				t.Fatalf("out = %q, want %q", out.String(), tt.wantOut)
			}
		})
	}
}
