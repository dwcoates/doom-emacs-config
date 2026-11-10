package gocache

import (
	"bytes"
	"context"
	"errors"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	"agentrepl/testrun/internal/run"
)

// now is when every guard test reads the clock first.
var now = time.Date(2026, 11, 10, 10, 51, 0, 0, time.UTC)

// clock reads now until a trim moves it on.
type clock struct {
	mu sync.Mutex
	t  time.Time
}

func (c *clock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.t
}

func (c *clock) advance(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.t = c.t.Add(d)
}

// trimTook is how long every fake trim takes on the test clock.
const trimTook = 19 * time.Second

// fixture is a guard over a temp cache whose fake trim writes trim.txt as
// cmd/go does: its start time, once the walk completes.
type fixture struct {
	g      *Guard
	clk    *clock
	trims  *int
	out    *bytes.Buffer
	errOut *bytes.Buffer
}

func newFixture(t *testing.T, record *string) fixture {
	t.Helper()
	dir := t.TempDir()
	if record != nil {
		writeRecord(t, dir, *record)
	}
	clk := &clock{t: now}
	trims := new(int)
	out, errOut := &bytes.Buffer{}, &bytes.Buffer{}
	g := &Guard{
		Dir:   dir,
		Clock: clk,
		Bound: TrimBound,
		Log:   &run.Log{Out: out, Err: errOut},
		Trim: func(_ context.Context, d string) error {
			*trims++
			began := clk.Now()
			clk.advance(trimTook)
			return os.WriteFile(filepath.Join(d, TrimFile), []byte(strconv.FormatInt(began.Unix(), 10)), 0o644)
		},
	}
	return fixture{g: g, clk: clk, trims: trims, out: out, errOut: errOut}
}

func writeRecord(t *testing.T, dir, record string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(dir, TrimFile), []byte(record), 0o644); err != nil {
		t.Fatal(err)
	}
}

func unix(t time.Time) *string {
	s := strconv.FormatInt(t.Unix(), 10)
	return &s
}

func literal(s string) *string { return &s }

func TestDeadlineTrimsOnlyWhenTheTrimFallsDueWithinTheBound(t *testing.T) {
	bound := time.Second
	cases := []struct {
		name string
		// record is trim.txt's content; nil is no record.
		record   *string
		wantTrim bool
	}{
		{name: "a trim an hour old is not due", record: unix(now.Add(-time.Hour)), wantTrim: false},
		{name: "a trim due just after the deadline is not due", record: unix(now.Add(-TrimInterval + bound + time.Second)), wantTrim: false},
		{name: "a trim due exactly at the deadline is due", record: unix(now.Add(-TrimInterval + bound)), wantTrim: true},
		{name: "a trim due inside the bound is due", record: unix(now.Add(-TrimInterval + bound/2)), wantTrim: true},
		{name: "a trim a day old is due", record: unix(now.Add(-TrimInterval)), wantTrim: true},
		{name: "no record is due", record: nil, wantTrim: true},
		{name: "an unparsable record is due", record: literal("yesterday\n"), wantTrim: true},
		{name: "a record just under an hour ahead is not due", record: unix(now.Add(FutureSlack - time.Second)), wantTrim: false},
		{name: "a record an hour ahead reads as corrupt and is due", record: unix(now.Add(FutureSlack)), wantTrim: true},
		{name: "a record with surrounding whitespace is read", record: literal(" " + *unix(now.Add(-time.Hour)) + "\n"), wantTrim: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t, tc.record)

			// Act
			deadline, err := f.g.Deadline("daemon:internal/shimsocket:build", bound)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			if got := *f.trims == 1; got != tc.wantTrim || *f.trims > 1 {
				t.Fatalf("trims = %d, want trimmed %v", *f.trims, tc.wantTrim)
			}
			want := now.Add(bound)
			if tc.wantTrim {
				want = now.Add(trimTook + bound)
			}
			if !deadline.Equal(want) {
				t.Fatalf("deadline = %v, want %v: counted from the moment the unit is clear to start", deadline, want)
			}
			if f.errOut.Len() != 0 {
				t.Fatalf("errors logged: %q", f.errOut.String())
			}
		})
	}
}

func TestDeadlineLogsTheTrimItRunsAndHowLongItTook(t *testing.T) {
	// Arrange
	f := newFixture(t, unix(now.Add(-TrimInterval)))

	// Act
	if _, err := f.g.Deadline("daemon:internal/wssummary:build", 729*time.Millisecond); err != nil {
		t.Fatal(err)
	}

	// Assert
	for _, want := range []string{
		"go build cache: cmd/go's daily trim of " + f.g.Dir + " (last trim 2026-11-09T10:51:00Z) falls due before unit daemon:internal/wssummary:build's time limit of 729ms ends, so the run trims it first, bounded at 57s",
		"go build cache: trimmed " + f.g.Dir + " in 19s; its next trim falls due at 2026-11-11T10:51:00Z",
	} {
		if !strings.Contains(f.out.String(), want) {
			t.Fatalf("log = %q, want %q", f.out.String(), want)
		}
	}
}

func TestDeadlineLogsNothingWhenTheTrimIsNotDue(t *testing.T) {
	// Arrange
	f := newFixture(t, unix(now.Add(-time.Hour)))

	// Act
	if _, err := f.g.Deadline("a", time.Second); err != nil {
		t.Fatal(err)
	}

	// Assert
	if f.out.Len() != 0 || f.errOut.Len() != 0 {
		t.Fatalf("log = %q / %q, want nothing", f.out.String(), f.errOut.String())
	}
}

func TestDeadlineFails(t *testing.T) {
	cases := []struct {
		name string
		// arrange breaks the fixture.
		arrange func(t *testing.T, f fixture)
		wantErr string
	}{
		{
			name: "when the trim fails",
			arrange: func(_ *testing.T, f fixture) {
				f.g.Trim = func(context.Context, string) error { return errors.New("exit status 1: go: no such tool") }
			},
			wantErr: "trimming DIR before unit a failed: exit status 1: go: no such tool",
		},
		{
			name: "when the trim runs past its bound",
			arrange: func(_ *testing.T, f fixture) {
				f.g.Bound = time.Nanosecond
				f.g.Trim = func(ctx context.Context, _ string) error {
					<-ctx.Done()
					return errors.New("signal: killed")
				}
			},
			wantErr: "trimming DIR before unit a did not finish within its bound of 1ns: signal: killed",
		},
		{
			name: "when the trim leaves its record stale",
			arrange: func(_ *testing.T, f fixture) {
				f.g.Trim = func(context.Context, string) error { return nil }
			},
			wantErr: "cmd/go trimmed DIR before unit a, but its trim record still says 2026-11-09T10:51:00Z, so the trim falls due within the unit's time limit of 1s",
		},
		{
			name: "when the record cannot be read",
			arrange: func(t *testing.T, f fixture) {
				path := filepath.Join(f.g.Dir, TrimFile)
				if err := os.Remove(path); err != nil {
					t.Fatal(err)
				}
				if err := os.Mkdir(path, 0o755); err != nil {
					t.Fatal(err)
				}
			},
			wantErr: "read the trim record DIR/trim.txt",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a due trim, broken as the case says.
			f := newFixture(t, unix(now.Add(-TrimInterval)))
			tc.arrange(t, f)

			// Act
			deadline, err := f.g.Deadline("a", time.Second)

			// Assert
			want := strings.ReplaceAll(tc.wantErr, "DIR", f.g.Dir)
			if err == nil || !strings.Contains(err.Error(), want) {
				t.Fatalf("Deadline = %v, %v; want the error %q", deadline, err, want)
			}
			if !deadline.IsZero() {
				t.Fatalf("deadline = %v, want none with the error", deadline)
			}
		})
	}
}

func TestDeadlinePanicsWithoutATrimBound(t *testing.T) {
	// Arrange
	f := newFixture(t, nil)
	f.g.Bound = 0
	defer func() {
		// Assert
		if r := recover(); r == nil || !strings.Contains(r.(string), "has no trim bound") {
			t.Fatalf("recovered %v, want the missing bound refused", r)
		}
	}()

	// Act
	f.g.Deadline("a", time.Second)
}

// TestGoTrimHasCmdGoTrimTheCache pins the contract the guard rests on, with
// the toolchain the test itself runs on: `go build -n unsafe` trims a due
// cache and records the trim.
func TestGoTrimHasCmdGoTrimTheCache(t *testing.T) {
	// Arrange: a cache last trimmed in 1970.
	dir := t.TempDir()
	writeRecord(t, dir, "1000")
	before := time.Now().Unix()

	// Act
	if err := GoTrim(context.Background(), dir); err != nil {
		t.Fatal(err)
	}

	// Assert
	data, err := os.ReadFile(filepath.Join(dir, TrimFile))
	if err != nil {
		t.Fatal(err)
	}
	got, err := strconv.ParseInt(strings.TrimSpace(string(data)), 10, 64)
	if err != nil || got < before {
		t.Fatalf("trim record = %q, want a trim at or after %d", data, before)
	}
}

func TestGoTrimFailsWhereGoCannotRun(t *testing.T) {
	// Arrange
	dir := filepath.Join(t.TempDir(), "missing")

	// Act
	err := GoTrim(context.Background(), dir)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "go build -n unsafe in "+dir) {
		t.Fatalf("GoTrim = %v, want the failure named", err)
	}
}

func TestDir(t *testing.T) {
	cases := []struct {
		name    string
		gocache string
		want    string
		wantErr string
	}{
		{name: "answers the cache go uses", gocache: "/tmp/agent-repl-gocache-test", want: "/tmp/agent-repl-gocache-test"},
		{name: "refuses a disabled cache", gocache: "off", wantErr: `go env GOCACHE answered "off"`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			env := append(os.Environ(), "GOCACHE="+tc.gocache)

			// Act
			got, err := Dir(env)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("Dir = %q, %v; want the error %q", got, err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("Dir = %q, %v; want %q", got, err, tc.want)
			}
		})
	}
}
