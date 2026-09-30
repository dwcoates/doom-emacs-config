package dlog

import (
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestLoggerRecordsEachLevel(t *testing.T) {
	tests := []struct {
		name  string
		emit  func(Logger)
		level string
	}{
		{name: "debug", level: LevelDebug, emit: func(l Logger) { l.Debug("daemon.pkg.verb", "m", nil) }},
		{name: "info", level: LevelInfo, emit: func(l Logger) { l.Info("daemon.pkg.verb", "m", nil) }},
		{name: "warn", level: LevelWarn, emit: func(l Logger) { l.Warn("daemon.pkg.verb", "m", nil) }},
		{name: "error", level: LevelError, emit: func(l Logger) { l.Error("daemon.pkg.verb", "m", nil) }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, runLogPath := testSurfaces(t)

			// Act.
			tc.emit(s.Global())

			// Assert.
			records := readRecords(t, runLogPath)
			if len(records) != 1 {
				t.Fatalf("records = %d, want 1", len(records))
			}
			if records[0]["level"] != tc.level {
				t.Fatalf("level = %v, want %q", records[0]["level"], tc.level)
			}
		})
	}
}

func TestLoggerWithStampsEveryRecord(t *testing.T) {
	// Arrange.
	s, runLogPath := testSurfaces(t)

	// Act.
	s.Global().With(Context{KeyRequestID: "req-1", "component": "boot"}).
		Info("daemon.pkg.verb", "m", Context{"per_record": true})

	// Assert.
	records := readRecords(t, runLogPath)
	if records[0]["request_id"] != "req-1" {
		t.Fatalf("request_id = %v, want req-1", records[0]["request_id"])
	}
	ctx, _ := records[0]["context"].(map[string]any)
	if ctx["component"] != "boot" || ctx["per_record"] != true {
		t.Fatalf("context = %v, want the bound and the per-record keys", ctx)
	}
}

func TestLoggerWithDoesNotMutateItsParent(t *testing.T) {
	// Arrange.
	s, runLogPath := testSurfaces(t)
	parent := s.Global()

	// Act.
	parent.With(Context{"child_only": true})
	parent.Info("daemon.pkg.verb", "m", nil)

	// Assert.
	records := readRecords(t, runLogPath)
	ctx, _ := records[0]["context"].(map[string]any)
	if _, leaked := ctx["child_only"]; leaked {
		t.Fatalf("the derived logger's context leaked into its parent: %v", ctx)
	}
}

func TestLoggerPerRecordContextOverridesTheBoundOne(t *testing.T) {
	// Arrange.
	s, runLogPath := testSurfaces(t)

	// Act.
	s.Global().With(Context{"phase": "boot"}).Info("daemon.pkg.verb", "m", Context{"phase": "serve"})

	// Assert.
	records := readRecords(t, runLogPath)
	ctx, _ := records[0]["context"].(map[string]any)
	if ctx["phase"] != "serve" {
		t.Fatalf("phase = %v, want the per-record value", ctx["phase"])
	}
}

func TestLoggerReportsADegradedMirrorDurably(t *testing.T) {
	// Arrange: a terminal wedged inside its first write, and a queue of one.
	terminal := newBlockingWriter()
	runLogPath := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	s, err := openSurfaces(runLogPath, LevelDebug, terminal)
	if err != nil {
		t.Fatalf("openSurfaces: %v", err)
	}
	defer func() { close(terminal.release); s.Close() }()
	s.mirror.close()
	s.mirror = newMirror(terminal, 1)
	log := s.Global()
	log.Info("daemon.pkg.first", "m", nil)
	<-terminal.entered
	log.Info("daemon.pkg.queued", "m", nil)
	log.Info("daemon.pkg.dropped", "m", nil)

	// Act: the next record carries the report.
	log.Info("daemon.pkg.next", "m", nil)

	// Assert.
	if !hasOperation(readRecords(t, runLogPath), "daemon.dlog.terminal_mirror_degraded") {
		t.Fatalf("a dropped terminal record was never reported durably")
	}
}

func TestLoggerSurvivesAPoisonedDestination(t *testing.T) {
	// Arrange: a workspace sink poisoned out from under its logger.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	_, sk, err := s.resolve(dir, "daemon")
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	sk.poisonLocked(io.ErrClosedPipe)

	// Act, Assert: the emitter must not panic; the failure goes to the one
	// permitted emergency output instead.
	log.Error("daemon.pkg.verb", "m", nil)
}

// TestTheEmergencyOutputIsItselfARecord pins the shape of the LAST RESORT.
// Every reader in the system parses a log line as a record, so two lines of
// prose -- "LOG SINK FAILURE: <cause>" and "unpersisted record: <the json>" --
// made the one output that fires when everything else has failed the one
// output nothing could read: 24 unparseable lines in the 2026-09-13 sweep,
// each carrying a real record nobody could group, level or attribute.
func TestTheEmergencyOutputIsItselfARecord(t *testing.T) {
	tests := []struct {
		name            string
		line            []byte
		wantUnpersisted string
	}{
		{
			name:            "a record the sink refused",
			line:            []byte("{\"message\":\"the original\"}\n"),
			wantUnpersisted: "{\"message\":\"the original\"}",
		},
		{
			name: "a failure with no record to carry",
			line: nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			s, _ := testSurfaces(t)
			r, w, err := os.Pipe()
			if err != nil {
				t.Fatalf("os.Pipe: %v", err)
			}
			saved := os.Stderr
			os.Stderr = w
			t.Cleanup(func() { os.Stderr = saved; r.Close() })

			// Act.
			s.emergency(RuntimeDaemon, io.ErrClosedPipe, tt.line)
			w.Close()

			// Assert.
			raw, err := io.ReadAll(r)
			if err != nil {
				t.Fatalf("read the emergency output: %v", err)
			}
			lines := strings.Split(strings.TrimSuffix(string(raw), "\n"), "\n")
			if len(lines) != 1 {
				t.Fatalf("the emergency output is %d lines, want exactly one: %q", len(lines), string(raw))
			}
			var rec struct {
				Level     string            `json:"level"`
				Operation string            `json:"operation"`
				Context   map[string]string `json:"context"`
			}
			if err := json.Unmarshal([]byte(lines[0]), &rec); err != nil {
				t.Fatalf("the emergency output is not a record: %v (%q)", err, lines[0])
			}
			if rec.Operation != "daemon.dlog.sink_failure" || rec.Level != LevelError {
				t.Fatalf("emergency record = %s/%s, want daemon.dlog.sink_failure/error", rec.Level, rec.Operation)
			}
			if rec.Context["unpersisted_record"] != tt.wantUnpersisted {
				t.Fatalf("unpersisted_record = %q, want %q", rec.Context["unpersisted_record"], tt.wantUnpersisted)
			}
		})
	}
}

// captureTee records every record the tee is handed, and optionally runs a
// probe at the moment of hand-off.
type captureTee struct {
	got   []WorkspaceRecord
	probe func(WorkspaceRecord)
}

// OnWorkspaceRecord implements RecordTee.
func (c *captureTee) OnWorkspaceRecord(rec WorkspaceRecord) {
	c.got = append(c.got, rec)
	if c.probe != nil {
		c.probe(rec)
	}
}

func TestRecordTeeTakesAWorkspaceLoggersWarnAndErrorRecords(t *testing.T) {
	tests := []struct {
		name  string
		emit  func(Logger)
		level string
	}{
		{name: "warn", level: LevelWarn, emit: func(l Logger) { l.Warn("daemon.pkg.verb", "the message", nil) }},
		{name: "error", level: LevelError, emit: func(l Logger) { l.Error("daemon.pkg.verb", "the message", nil) }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, _ := testSurfaces(t)
			tee := &captureTee{}
			s.BindRecordTee(tee)
			dir := t.TempDir()
			log, err := s.Workspace(dir)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}

			// Act.
			tc.emit(log)

			// Assert.
			want := WorkspaceRecord{WorkspaceID: mintedTestID(dir), Level: tc.level, Operation: "daemon.pkg.verb", Message: "the message"}
			if len(tee.got) != 1 || tee.got[0] != want {
				t.Fatalf("tee got %+v, want exactly %+v", tee.got, want)
			}
		})
	}
}

func TestRecordTeeSkipsLevelsBelowWarn(t *testing.T) {
	tests := []struct {
		name string
		emit func(Logger)
	}{
		{name: "debug", emit: func(l Logger) { l.Debug("daemon.pkg.verb", "m", nil) }},
		{name: "info", emit: func(l Logger) { l.Info("daemon.pkg.verb", "m", nil) }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, _ := testSurfaces(t)
			tee := &captureTee{}
			s.BindRecordTee(tee)
			log, err := s.Workspace(t.TempDir())
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}

			// Act.
			tc.emit(log)

			// Assert.
			if len(tee.got) != 0 {
				t.Fatalf("tee got %+v, want nothing below warn", tee.got)
			}
		})
	}
}

func TestRecordTeeSkipsTheRunLog(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	tee := &captureTee{}
	s.BindRecordTee(tee)

	// Act.
	s.Global().Error("daemon.pkg.verb", "m", nil)

	// Assert.
	if len(tee.got) != 0 {
		t.Fatalf("tee got %+v, want nothing from a logger bound to no workspace", tee.got)
	}
}

func TestRecordTeeTakesADerivedWorkspaceLoggersRecords(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	tee := &captureTee{}
	s.BindRecordTee(tee)
	dir := t.TempDir()
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Act.
	log.With(Context{"k": "v"}).Warn("daemon.pkg.verb", "m", nil)

	// Assert.
	if len(tee.got) != 1 || tee.got[0].WorkspaceID != mintedTestID(dir) {
		t.Fatalf("tee got %+v, want the derived logger's record under its workspace", tee.got)
	}
}

func TestRecordTeeTakesARecordOnlyAfterItIsDurable(t *testing.T) {
	// Arrange: the tee reads the workspace sink at the moment it is handed
	// the record.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	durable := false
	s.BindRecordTee(&captureTee{probe: func(WorkspaceRecord) {
		durable = hasOperation(workspaceRecords(t, dir, "daemon"), "daemon.pkg.verb")
	}})
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Act.
	log.Warn("daemon.pkg.verb", "m", nil)

	// Assert.
	if !durable {
		t.Fatal("the tee was handed a record its workspace sink did not yet carry")
	}
}

func TestRecordTeeSkipsARecordTheLevelThresholdDrops(t *testing.T) {
	// Arrange.
	runLogPath := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	s, err := openSurfaces(runLogPath, LevelError, io.Discard)
	if err != nil {
		t.Fatalf("openSurfaces: %v", err)
	}
	t.Cleanup(func() { s.Close() })
	bindTestWorkspaceIDs(s)
	tee := &captureTee{}
	s.BindRecordTee(tee)
	log, err := s.Workspace(t.TempDir())
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Act.
	log.Warn("daemon.pkg.verb", "m", nil)

	// Assert.
	if len(tee.got) != 0 {
		t.Fatalf("tee got %+v, want nothing for a record never persisted", tee.got)
	}
}

func TestRecordTeeUnboundTakesNothing(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	tee := &captureTee{}
	s.BindRecordTee(tee)
	s.BindRecordTee(nil)
	dir := t.TempDir()
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Act.
	log.Warn("daemon.pkg.verb", "m", nil)

	// Assert.
	if len(tee.got) != 0 {
		t.Fatalf("an unbound tee got %+v", tee.got)
	}
	if !hasOperation(workspaceRecords(t, dir, "daemon"), "daemon.pkg.verb") {
		t.Fatal("the record was not persisted with no tee bound")
	}
}
