package main

import (
	"net"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"agentrepl/logging/buildreport"

	"agent-repld/internal/dlog"
	"agent-repld/internal/stateroot"
	"agent-repld/internal/tempdirs/tempdirstest"
)

// shortDir is a directory whose path fits a unix socket's length limit.
func shortDir(t *testing.T) string { return tempdirstest.ShortDir(t, "wf") }

func records(log *dlog.TestLogger, level, op string) []dlog.Record {
	return dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: level, Operation: op})
}

func TestWaitForAnswersWhetherTheGroupFinishedWithinItsBound(t *testing.T) {
	tests := []struct {
		name  string
		hold  bool
		bound time.Duration
		want  bool
	}{
		{"a finished group is done", false, time.Second, true},
		{"a group still running past the bound is not", true, 20 * time.Millisecond, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var wg sync.WaitGroup
			release := make(chan struct{})
			if tt.hold {
				wg.Add(1)
				go func() { <-release; wg.Done() }()
				defer close(release)
			}

			// Act
			got := waitFor(&wg, tt.bound)

			// Assert
			if got != tt.want {
				t.Fatalf("waitFor() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestListenServesOnTheInheritedListener(t *testing.T) {
	// Arrange: a bound socket handed over as a file descriptor.
	ln, err := net.Listen("unix", filepath.Join(shortDir(t), "h.sock"))
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	t.Cleanup(func() { _ = ln.Close() })
	file, err := ln.(*net.UnixListener).File()
	if err != nil {
		t.Fatalf("File(): %v", err)
	}
	log := dlog.NewTestLogger()

	// Act
	got, err := listen(options{listenFD: int(file.Fd())}, log)

	// Assert
	if err != nil {
		t.Fatalf("listen() error = %v", err)
	}
	_ = got.Close()
	if r := records(log, dlog.LevelInfo, "daemon.persistentwifi.service.listen"); len(r) != 1 {
		t.Fatalf("INFO records = %+v, want the one inherited-listener record", r)
	}
}

func TestListenRefusesAnInheritedDescriptorThatIsNoListenerAndRecordsIt(t *testing.T) {
	// Arrange: a regular file's descriptor.
	file, err := os.Open(os.DevNull)
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	log := dlog.NewTestLogger()

	// Act
	_, err = listen(options{listenFD: int(file.Fd())}, log)

	// Assert
	if err == nil {
		t.Fatal("listen() accepted a descriptor that is no listener")
	}
	if r := records(log, dlog.LevelError, "daemon.persistentwifi.service.listen"); len(r) != 1 {
		t.Fatalf("ERROR records = %+v, want the one refusal", r)
	}
}

func TestListenBindsItsOwnSocketWithNoHolder(t *testing.T) {
	// Arrange
	socket := filepath.Join(shortDir(t), "own.sock")
	log := dlog.NewTestLogger()

	// Act
	ln, err := listen(options{socket: socket, listenFD: -1}, log)

	// Assert
	if err != nil {
		t.Fatalf("listen() error = %v", err)
	}
	defer func() { _ = ln.Close() }()
	if ln.Addr().String() != socket {
		t.Fatalf("listening on %s, want %s", ln.Addr(), socket)
	}
}

func TestListenRefusesASocketAnotherServiceHoldsAndRecordsIt(t *testing.T) {
	// Arrange: a live listener already holds the path.
	socket := filepath.Join(shortDir(t), "taken.sock")
	held, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	defer func() { _ = held.Close() }()
	log := dlog.NewTestLogger()

	// Act
	_, err = listen(options{socket: socket, listenFD: -1}, log)

	// Assert
	if err == nil {
		t.Fatal("listen() bound a socket another service holds")
	}
	if r := records(log, dlog.LevelError, "daemon.persistentwifi.service.listen"); len(r) != 1 {
		t.Fatalf("ERROR records = %+v, want the one refusal", r)
	}
}

func TestListenFailsAndRecordsWhenTheHolderIsAbsent(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()

	// Act
	_, err := listen(options{holder: filepath.Join(shortDir(t), "no-holder.sock"), listenFD: -1}, log)

	// Assert
	if err == nil {
		t.Fatal("listen() took a listener from a holder that is not there")
	}
	if r := records(log, dlog.LevelError, "daemon.persistentwifi.service.listen"); len(r) != 1 {
		t.Fatalf("ERROR records = %+v, want the one failed take", r)
	}
}

func TestAnnounceBuildWritesTheServicesReport(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	log := dlog.NewTestLogger()
	getenv := func(k string) string {
		if k == buildreport.DirEnv {
			return dir
		}
		return ""
	}

	// Act
	announceBuild(log, getenv)

	// Assert
	if _, err := os.Stat(buildreport.Path(dir, buildreport.ServicePersistentWifi)); err != nil {
		t.Fatalf("the report was not written: %v", err)
	}
	if r := records(log, dlog.LevelInfo, "daemon.persistentwifi.service.buildreport"); len(r) != 1 {
		t.Fatalf("INFO records = %+v, want the one report record", r)
	}
}

func TestAnnounceBuildRecordsAReportItCannotWrite(t *testing.T) {
	// Arrange: the report directory is a file.
	blocker := filepath.Join(t.TempDir(), "file")
	if err := os.WriteFile(blocker, nil, 0o600); err != nil {
		t.Fatalf("write: %v", err)
	}
	log := dlog.NewTestLogger()

	// Act
	announceBuild(log, func(k string) string {
		if k == buildreport.DirEnv {
			return filepath.Join(blocker, "run")
		}
		return ""
	})

	// Assert
	if r := records(log, dlog.LevelError, "daemon.persistentwifi.service.buildreport"); len(r) != 1 {
		t.Fatalf("ERROR records = %+v, want the one failed write", r)
	}
}

func TestRunLogIsBesideTheThawGuardsInTheStateRoot(t *testing.T) {
	// Arrange
	layout, err := stateroot.Root(t.TempDir(), "")
	if err != nil {
		t.Fatalf("Root(): %v", err)
	}

	// Act
	got := runLog(layout)

	// Assert
	if want := filepath.Join(layout.LogsDir(), "persistent-wifi.run.log"); got != want {
		t.Fatalf("runLog() = %q, want %q", got, want)
	}
}
