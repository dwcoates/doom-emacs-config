package dlog

import "testing"

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
