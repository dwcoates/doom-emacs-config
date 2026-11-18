package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
	"agentrepl/logging/recordlog"
	"agentrepl/logging/sockethold"
	"agentrepl/logging/sockethold/activate"
)

// exchangeBound bounds a Take against a holder this test runs, a kernel-local
// exchange; it guards against a hang and is never a wait.
const exchangeBound = 5 * time.Second

// records decodes every record the holder wrote.
func records(t *testing.T, sink *bytes.Buffer) []map[string]any {
	t.Helper()
	var out []map[string]any
	for _, line := range strings.Split(strings.TrimSpace(sink.String()), "\n") {
		if line == "" {
			continue
		}
		var rec map[string]any
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("record %q is not JSON: %v", line, err)
		}
		out = append(out, rec)
	}
	return out
}

func hasRecord(recs []map[string]any, level, operation, message string) bool {
	for _, r := range recs {
		if r["level"] == level && r["operation"] == operation && r["message"] == message {
			return true
		}
	}
	return false
}

// socketFile binds a unix socket at PATH and answers its descriptor, the
// path kept: a service manager's socket outlives every listener on it.
func socketFile(t *testing.T, path string) *os.File {
	t.Helper()
	ln, err := net.ListenUnix("unix", &net.UnixAddr{Name: path, Net: "unix"})
	if err != nil {
		t.Fatal(err)
	}
	ln.SetUnlinkOnClose(false)
	f, err := ln.File()
	if err != nil {
		t.Fatal(err)
	}
	ln.Close()
	return f
}

func newLog(sink *bytes.Buffer) *recordlog.Logger {
	return recordlog.New(runtimeName, sink, sink, sharedlogging.FixedWindow(sharedlogging.LevelDebug), time.Now)
}

func TestRunHandsAServerItsHeldSocket(t *testing.T) {
	// Arrange
	dir, err := os.MkdirTemp("/tmp", "shr")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)
	control, data := filepath.Join(dir, "c.sock"), filepath.Join(dir, "d.sock")
	sockets := []activate.Socket{{Name: "control", File: socketFile(t, control)}, {Name: "store", File: socketFile(t, data)}}
	var sink bytes.Buffer
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() {
		done <- run(ctx, "control", []string{"store"}, func([]string) ([]activate.Socket, error) { return sockets, nil }, newLog(&sink))
	}()

	// Act
	takeCtx, takeCancel := context.WithTimeout(context.Background(), exchangeBound)
	ln, takeErr := sockethold.Take(takeCtx, control, "store")
	takeCancel()
	_, refused := func() (net.Listener, error) {
		c, cc := context.WithTimeout(context.Background(), exchangeBound)
		defer cc()
		return sockethold.Take(c, control, "nope")
	}()
	cancel()
	runErr := <-done

	// Assert
	if takeErr != nil {
		t.Fatalf("Take: %v", takeErr)
	}
	ln.Close()
	if runErr != nil {
		t.Fatalf("run = %v, want a clean exit on its signal", runErr)
	}
	if !errors.Is(refused, sockethold.ErrRefused) {
		t.Fatalf("Take of an unheld socket = %v, want ErrRefused", refused)
	}
	recs := records(t, &sink)
	for _, want := range [][3]string{
		{"info", "sockethold.grant", "handed a server its held socket"},
		{"error", "sockethold.grant", "a request for a held socket was not granted"},
		{"info", "sockethold.run", "the holder is exiting on its service manager's signal"},
	} {
		if !hasRecord(recs, want[0], want[1], want[2]) {
			t.Fatalf("records = %v, want %v", recs, want)
		}
	}
}

func TestRunRecordsEveryFailureToStart(t *testing.T) {
	dir, err := os.MkdirTemp("/tmp", "shf")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)
	cases := []struct {
		name     string
		control  string
		hold     []string
		activate func([]string) ([]activate.Socket, error)
		message  string
	}{
		{name: "no socket to hold", control: "control", message: "the holder was given no control socket or no socket to hold",
			activate: func([]string) ([]activate.Socket, error) { return nil, nil }},
		{name: "no sockets from the service manager", control: "control", hold: []string{"store"}, message: "the service manager handed the holder no sockets",
			activate: func([]string) ([]activate.Socket, error) { return nil, errors.New("no such job") }},
		{name: "no control socket handed", control: "control", hold: []string{"store"}, message: "the service manager handed no control socket",
			activate: func([]string) ([]activate.Socket, error) {
				return []activate.Socket{{Name: "store", File: socketFile(t, filepath.Join(dir, "a.sock"))}}, nil
			}},
		{name: "a held socket not handed", control: "control", hold: []string{"store"}, message: "the service manager handed no socket to hold",
			activate: func([]string) ([]activate.Socket, error) {
				return []activate.Socket{{Name: "control", File: socketFile(t, filepath.Join(dir, "b.sock"))}}, nil
			}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var sink bytes.Buffer

			// Act
			err := run(context.Background(), tc.control, tc.hold, tc.activate, newLog(&sink))

			// Assert
			if err == nil {
				t.Fatal("run = nil, want the failure answered")
			}
			if !hasRecord(records(t, &sink), "error", "sockethold.run", tc.message) {
				t.Fatalf("records = %v, want %q at ERROR", records(t, &sink), tc.message)
			}
		})
	}
}
