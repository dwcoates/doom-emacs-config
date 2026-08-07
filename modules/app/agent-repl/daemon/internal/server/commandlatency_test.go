package server

import (
	"bytes"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/frontend"
)

// readWorkspaceDaemonRecords decodes every JSONL record the recorder persisted
// into one workspace's canonical daemon target.
func readWorkspaceDaemonRecords(t *testing.T, workspace string) []dlog.Record {
	t.Helper()
	raw, err := os.ReadFile(filepath.Join(workspace, ".claude", "emacs", "daemon.log"))
	if err != nil {
		t.Fatalf("read workspace daemon log: %v", err)
	}
	var records []dlog.Record
	for _, line := range strings.Split(strings.TrimSpace(string(raw)), "\n") {
		if line == "" {
			continue
		}
		var record dlog.Record
		if err := json.Unmarshal([]byte(line), &record); err != nil {
			t.Fatalf("decode record %q: %v", line, err)
		}
		records = append(records, record)
	}
	return records
}

func newTestLatencyRecorder(t *testing.T) (frontend.CommandLatencyRecorder, *dlog.TargetManager, *bytes.Buffer) {
	t.Helper()
	targets := dlog.NewTargetManager()
	t.Cleanup(func() {
		if err := targets.Close(); err != nil {
			t.Fatalf("close targets: %v", err)
		}
	})
	global := &bytes.Buffer{}
	recorder, err := NewTargetCommandLatencyRecorder(targets, dlog.New(global, io.Discard, false), io.Discard, false)
	if err != nil {
		t.Fatalf("build recorder: %v", err)
	}
	return recorder, targets, global
}

func sampleFor(workspace string, ack, threshold time.Duration) frontend.CommandLatencySample {
	return frontend.CommandLatencySample{
		Workspace:  workspace,
		RequestID:  "request-1",
		Command:    "open_workspace",
		ClientKind: "host",
		QueueDepth: 3,
		Ack:        ack,
		Processing: ack / 2,
		Threshold:  threshold,
		Ok:         true,
	}
}

func TestRecordCommandLatencySeverityFollowsTheThreshold(t *testing.T) {
	tests := []struct {
		name          string
		ack           time.Duration
		threshold     time.Duration
		wantLevel     dlog.Level
		wantVerbosity dlog.Verbosity
	}{
		{
			name:          "a fast command is verbose debug detail",
			ack:           50 * time.Millisecond,
			threshold:     2 * time.Second,
			wantLevel:     dlog.LevelDebug,
			wantVerbosity: dlog.Verbose,
		},
		{
			name:          "a slow ack warns without anything enabled in advance",
			ack:           3 * time.Second,
			threshold:     2 * time.Second,
			wantLevel:     dlog.LevelWarn,
			wantVerbosity: dlog.Normal,
		},
		{
			name:          "exactly at the threshold has crossed it",
			ack:           2 * time.Second,
			threshold:     2 * time.Second,
			wantLevel:     dlog.LevelWarn,
			wantVerbosity: dlog.Normal,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			workspace := t.TempDir()
			recorder, _, _ := newTestLatencyRecorder(t)

			// Act.
			if err := recorder.RecordCommandLatency(sampleFor(workspace, tc.ack, tc.threshold)); err != nil {
				t.Fatalf("RecordCommandLatency = %v, want nil", err)
			}

			// Assert.
			records := readWorkspaceDaemonRecords(t, workspace)
			if len(records) != 1 {
				t.Fatalf("persisted records = %d, want exactly one", len(records))
			}
			if records[0].Level != tc.wantLevel || records[0].Verbosity != tc.wantVerbosity {
				t.Fatalf("record level/verbosity = %s/%s, want %s/%s",
					records[0].Level, records[0].Verbosity, tc.wantLevel, tc.wantVerbosity)
			}
		})
	}
}

func TestRecordCommandLatencyCarriesTheLifecycleContext(t *testing.T) {
	// Arrange.
	workspace := t.TempDir()
	recorder, _, _ := newTestLatencyRecorder(t)

	// Act.
	if err := recorder.RecordCommandLatency(sampleFor(workspace, 3*time.Second, 2*time.Second)); err != nil {
		t.Fatalf("RecordCommandLatency = %v, want nil", err)
	}

	// Assert.
	record := readWorkspaceDaemonRecords(t, workspace)[0]
	want := map[string]any{
		"command":       "open_workspace",
		"client_kind":   "host",
		"queue_depth":   float64(3),
		"duration_ms":   float64(3000),
		"processing_ms": float64(1500),
		"threshold_ms":  float64(2000),
		"ok":            true,
	}
	for key, value := range want {
		if record.Context[key] != value {
			t.Fatalf("context[%q] = %#v, want %#v", key, record.Context[key], value)
		}
	}
}

func TestRecordCommandLatencyUsesTheStableOperationName(t *testing.T) {
	// Arrange.
	workspace := t.TempDir()
	recorder, _, _ := newTestLatencyRecorder(t)

	// Act.
	if err := recorder.RecordCommandLatency(sampleFor(workspace, time.Second, 2*time.Second)); err != nil {
		t.Fatalf("RecordCommandLatency = %v, want nil", err)
	}

	// Assert. Operators query the operation, never the message text.
	if got := readWorkspaceDaemonRecords(t, workspace)[0].Operation; got != CommandLatencyOperation {
		t.Fatalf("operation = %q, want %q", got, CommandLatencyOperation)
	}
}

func TestRecordCommandLatencyRoutesAWorkspaceCommandToItsWorkspace(t *testing.T) {
	// Arrange.
	workspace := t.TempDir()
	recorder, _, global := newTestLatencyRecorder(t)

	// Act.
	if err := recorder.RecordCommandLatency(sampleFor(workspace, 3*time.Second, 2*time.Second)); err != nil {
		t.Fatalf("RecordCommandLatency = %v, want nil", err)
	}

	// Assert. A workspace-owned record must never land in the global sink.
	resolved, err := dlog.WorkspaceFromDirectory(workspace)
	if err != nil {
		t.Fatal(err)
	}
	record := readWorkspaceDaemonRecords(t, workspace)[0]
	if record.WorkspaceDirectory != resolved.Directory || record.WorkspaceID != resolved.ID {
		t.Fatalf("record workspace attribution = %q/%q, want %q/%q",
			record.WorkspaceDirectory, record.WorkspaceID, resolved.Directory, resolved.ID)
	}
	if global.Len() != 0 {
		t.Fatalf("global sink = %q, want a workspace-owned record to stay out of it", global.String())
	}
}

func TestRecordCommandLatencyRoutesAWorkspacelessCommandGlobally(t *testing.T) {
	// Arrange. Daemon health, shutdown and roster publication genuinely have
	// no workspace.
	recorder, _, global := newTestLatencyRecorder(t)
	sample := sampleFor("", 3*time.Second, 2*time.Second)
	sample.Command = "daemon_health"

	// Act.
	if err := recorder.RecordCommandLatency(sample); err != nil {
		t.Fatalf("RecordCommandLatency = %v, want nil", err)
	}

	// Assert.
	var record dlog.Record
	if err := json.Unmarshal(bytes.TrimSpace(global.Bytes()), &record); err != nil {
		t.Fatalf("decode global record: %v", err)
	}
	if record.Operation != CommandLatencyOperation || record.WorkspaceDirectory != "" {
		t.Fatalf("global record = %+v, want an unattributed command latency record", record)
	}
}

func TestRecordCommandLatencyRefusesAnUnresolvableWorkspace(t *testing.T) {
	// Arrange.
	recorder, _, global := newTestLatencyRecorder(t)

	// Act. A workspace the daemon cannot resolve is a routing invariant
	// violation, never permission to write the record globally.
	err := recorder.RecordCommandLatency(sampleFor(filepath.Join(t.TempDir(), "gone"), time.Second, 2*time.Second))

	// Assert.
	if err == nil {
		t.Fatal("RecordCommandLatency for an unresolvable workspace = nil, want a loud refusal")
	}
	if global.Len() != 0 {
		t.Fatalf("global sink = %q, want no demotion of a workspace-owned record", global.String())
	}
}

func TestNewTargetCommandLatencyRecorderRequiresItsDependencies(t *testing.T) {
	tests := []struct {
		name     string
		targets  *dlog.TargetManager
		global   *dlog.Logger
		terminal io.Writer
	}{
		{name: "no target manager", targets: nil, global: dlog.New(io.Discard, io.Discard, false), terminal: io.Discard},
		{name: "no global logger", targets: dlog.NewTargetManager(), global: nil, terminal: io.Discard},
		{name: "no terminal", targets: dlog.NewTargetManager(), global: dlog.New(io.Discard, io.Discard, false), terminal: nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			_, err := NewTargetCommandLatencyRecorder(tc.targets, tc.global, tc.terminal, false)

			// Assert.
			if err == nil {
				t.Fatal("NewTargetCommandLatencyRecorder = nil error, want a loud missing-dependency refusal")
			}
		})
	}
}
