package dlog

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"
)

func TestTestLoggerCapturesEveryLevel(t *testing.T) {
	// Arrange.
	log := NewTestLogger()

	// Act.
	log.Debug("daemon.pkg.a", "a", nil)
	log.Info("daemon.pkg.b", "b", nil)
	log.Warn("daemon.pkg.c", "c", nil)
	log.Error("daemon.pkg.d", "d", nil)

	// Assert.
	records := log.Records()
	want := []string{"debug", "info", "warn", "error"}
	if len(records) != len(want) {
		t.Fatalf("records = %d, want %d", len(records), len(want))
	}
	for i, level := range want {
		if records[i].Level != level {
			t.Fatalf("record %d level = %q, want %q", i, records[i].Level, level)
		}
	}
}

func TestTestLoggerWithSharesTheCaptureBuffer(t *testing.T) {
	// Arrange.
	log := NewTestLogger()

	// Act.
	log.With(Context{"bound": true}).Info("daemon.pkg.a", "a", Context{"per": 1})

	// Assert.
	records := log.Records()
	if len(records) != 1 {
		t.Fatalf("records = %d, want 1", len(records))
	}
	if records[0].Context["bound"] != true || records[0].Context["per"] != 1 {
		t.Fatalf("context = %v, want both the bound and the per-record keys", records[0].Context)
	}
}

func TestTestSurfacesWorkspaceStampsIdentity(t *testing.T) {
	// Arrange.
	s := NewTestSurfaces()
	dir := t.TempDir()
	want, err := syntheticWorkspaceID(dir)
	if err != nil {
		t.Fatalf("syntheticWorkspaceID: %v", err)
	}

	// Act.
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	log.Info("daemon.pkg.a", "a", nil)

	// Assert.
	records := s.Records()
	if len(records) != 1 {
		t.Fatalf("records = %d, want 1", len(records))
	}
	if records[0].Context[KeyWorkspaceID] != want {
		t.Fatalf("workspace_id = %v, want %q", records[0].Context[KeyWorkspaceID], want)
	}
}

func TestTestSurfacesCapturesClientRecords(t *testing.T) {
	// Arrange.
	s := NewTestSurfaces()
	rec := ClientRecord{ClientKind: RuntimeWebapp, Level: LevelInfo, Operation: "webapp.a", Message: "m"}

	// Act.
	if err := s.ClientLog("/workspace", rec); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	got := s.ClientRecords()
	if len(got) != 1 || got[0].Dir != "/workspace" || got[0].Record.Operation != "webapp.a" {
		t.Fatalf("ClientRecords = %+v", got)
	}
}

func TestTestSurfacesCapturesEvictions(t *testing.T) {
	// Arrange.
	s := NewTestSurfaces()

	// Act.
	if err := s.Evict("/workspace"); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert.
	if got := s.Evicted(); len(got) != 1 || got[0] != "/workspace" {
		t.Fatalf("Evicted = %v", got)
	}
}

func TestTestSurfacesRefusesAShimSinkBorrow(t *testing.T) {
	// Arrange.
	s := NewTestSurfaces()

	// Act.
	_, err := s.ShimSink("/workspace")

	// Assert: no test process should inherit an invented descriptor.
	if err == nil {
		t.Fatalf("TestSurfaces handed out a shim sink")
	}
}

// teeCapture records every record a TestSurfaces tee is handed.
type teeCapture struct{ got []WorkspaceRecord }

// OnWorkspaceRecord implements RecordTee.
func (c *teeCapture) OnWorkspaceRecord(rec WorkspaceRecord) { c.got = append(c.got, rec) }

func TestTestSurfacesTeesAWorkspaceLoggersWarn(t *testing.T) {
	// Arrange.
	s := NewTestSurfaces()
	tee := &teeCapture{}
	s.BindRecordTee(tee)
	log, err := s.Workspace(t.TempDir())
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Act.
	log.With(Context{"k": "v"}).Warn("daemon.pkg.verb", "m", nil)

	// Assert.
	if len(tee.got) != 1 || tee.got[0].Level != LevelWarn || tee.got[0].WorkspaceID == "" {
		t.Fatalf("tee got %+v, want the workspace's warn record", tee.got)
	}
}

func TestTestSurfacesDoesNotTeeTheGlobalLogger(t *testing.T) {
	// Arrange.
	s := NewTestSurfaces()
	tee := &teeCapture{}
	s.BindRecordTee(tee)

	// Act.
	s.Global().Error("daemon.pkg.verb", "m", nil)

	// Assert.
	if len(tee.got) != 0 {
		t.Fatalf("tee got %+v, want nothing from the global logger", tee.got)
	}
}

func TestFindRecord(t *testing.T) {
	records := []Record{
		{Level: "info", Operation: "op.a", Message: "first"},
		{Level: "error", Operation: "op.a", Message: "second"},
		{Level: "error", Operation: "op.b", Message: "second"},
	}
	tests := []struct {
		name      string
		query     RecordQuery
		wantFound bool
		wantIndex int
	}{
		{name: "every field named matches the one record carrying all three", query: RecordQuery{Level: "error", Operation: "op.b", Message: "second"}, wantFound: true, wantIndex: 2},
		{name: "an empty level matches any level", query: RecordQuery{Operation: "op.a", Message: "second"}, wantFound: true, wantIndex: 1},
		{name: "an empty operation matches any operation", query: RecordQuery{Level: "info", Message: "first"}, wantFound: true, wantIndex: 0},
		{name: "an empty message matches any message", query: RecordQuery{Level: "error", Operation: "op.a"}, wantFound: true, wantIndex: 1},
		{name: "the first of several matches is answered", query: RecordQuery{Message: "second"}, wantFound: true, wantIndex: 1},
		{name: "a query nothing matches finds nothing", query: RecordQuery{Level: "warn"}, wantFound: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, found := FindRecord(records, tc.query)

			// Assert
			if found != tc.wantFound {
				t.Fatalf("found = %t, want %t", found, tc.wantFound)
			}
			if found && got.Message != records[tc.wantIndex].Message || found && got.Operation != records[tc.wantIndex].Operation {
				t.Fatalf("record = %+v, want %+v", got, records[tc.wantIndex])
			}
		})
	}
}

func TestFindRecordInNoRecordsFindsNothing(t *testing.T) {
	// Act
	got, found := FindRecord(nil, RecordQuery{})

	// Assert
	if found || got.Operation != "" {
		t.Fatalf("FindRecord(nil) = %+v, %t; want nothing", got, found)
	}
}

func TestFilterRecords(t *testing.T) {
	records := []Record{
		{Level: "info", Operation: "op.a", Message: "first"},
		{Level: "error", Operation: "op.a", Message: "second"},
		{Level: "error", Operation: "op.b", Message: "third"},
	}
	tests := []struct {
		name  string
		query RecordQuery
		want  []string
	}{
		{name: "every match is answered, in order", query: RecordQuery{Level: "error"}, want: []string{"second", "third"}},
		{name: "every field named must match", query: RecordQuery{Level: "error", Operation: "op.a"}, want: []string{"second"}},
		{name: "an empty query matches every record", query: RecordQuery{}, want: []string{"first", "second", "third"}},
		{name: "a query nothing matches answers none", query: RecordQuery{Operation: "op.c"}, want: nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := FilterRecords(records, tc.query)

			// Assert
			var messages []string
			for _, r := range got {
				messages = append(messages, r.Message)
			}
			if len(messages) != len(tc.want) {
				t.Fatalf("messages = %v, want %v", messages, tc.want)
			}
			for i := range messages {
				if messages[i] != tc.want[i] {
					t.Fatalf("messages = %v, want %v", messages, tc.want)
				}
			}
		})
	}
}

func TestTestLoggerAwait(t *testing.T) {
	want := RecordQuery{Operation: "daemon.test.op", Message: "reached"}
	cases := []struct {
		name string
		// before logs before Await is called; after, once it is waiting.
		before, after func(l Logger)
		wantErr       error
	}{
		{name: "a record already captured answers at once",
			before: func(l Logger) { l.Info("daemon.test.op", "reached", nil) }},
		{name: "a record appended while waiting answers then",
			after: func(l Logger) { l.Info("daemon.test.op", "reached", nil) }},
		{name: "a record appended through a derived logger answers",
			after: func(l Logger) { l.With(Context{"k": "v"}).Info("daemon.test.op", "reached", nil) }},
		{name: "only records that do not match leave it waiting until its context ends",
			before:  func(l Logger) { l.Info("daemon.test.op", "elsewhere", nil) },
			wantErr: context.Canceled},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			l := NewTestLogger()
			if tc.before != nil {
				tc.before(l)
			}
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			if tc.wantErr != nil {
				// NOTHING THAT MATCHES WILL COME: the context's end is the
				// answer, given at once rather than after a window.
				cancel()
			}
			type answer struct {
				r   Record
				err error
			}
			got := make(chan answer, 1)

			// Act
			go func() {
				r, err := l.Await(ctx, want)
				got <- answer{r, err}
			}()
			if tc.after != nil {
				tc.after(l)
			}
			a := <-got

			// Assert
			if !errors.Is(a.err, tc.wantErr) {
				t.Fatalf("Await = %v, want %v", a.err, tc.wantErr)
			}
			if tc.wantErr == nil && a.r.Message != "reached" {
				t.Fatalf("Await answered %+v, want the matching record", a.r)
			}
		})
	}
}

func TestTestLoggerAwaitCountAnswersOnceEnoughAreCaptured(t *testing.T) {
	// Arrange
	l := NewTestLogger()
	l.Info("daemon.test.op", "reached", nil)
	got := make(chan int, 1)
	go func() {
		records, _ := l.AwaitCount(context.Background(), RecordQuery{Message: "reached"}, 2)
		got <- len(records)
	}()

	// Act
	l.Info("daemon.test.op", "reached", nil)

	// Assert
	if n := <-got; n != 2 {
		t.Fatalf("AwaitCount answered %d records, want 2", n)
	}
}

func TestTestLoggerAwaitCountNamesHowManyWereCaptured(t *testing.T) {
	// Arrange
	l := NewTestLogger()
	l.Info("daemon.test.op", "reached", nil)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	_, err := l.AwaitCount(ctx, RecordQuery{Message: "reached"}, 2)

	// Assert
	if !errors.Is(err, context.Canceled) || !strings.Contains(err.Error(), "1 of 2") {
		t.Fatalf("AwaitCount = %v, want the context's end naming 1 of 2 captured", err)
	}
}

func TestTestLoggerAwaitCountRefusesANonPositiveCount(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("AwaitCount(0) did not panic")
		}
	}()

	// Act
	_, _ = NewTestLogger().AwaitCount(context.Background(), RecordQuery{}, 0)
}
