package dlog

import (
	"io"
	"path/filepath"
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
	s, err := openSurfaces(runLogPath, true, terminal)
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
