package dlog

import (
	"encoding/json"
	"os"
	"path/filepath"
	"regexp"
	"testing"
	"time"
)

// vocabTimestampPattern is the cross-language timestamp contract from
// proto/vocab/log-timestamp.json — the seam that makes a divergence between
// Go, TypeScript and elisp fail loudly instead of quietly.
func vocabTimestampPattern(t *testing.T) *regexp.Regexp {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	// daemon/internal/dlog -> modules/app/agent-repl
	path := filepath.Join(dir, "..", "..", "..", "proto", "vocab", "log-timestamp.json")
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	var vocab struct {
		Pattern string `json:"pattern"`
	}
	if err := json.Unmarshal(raw, &vocab); err != nil {
		t.Fatalf("parse %s: %v", path, err)
	}
	if vocab.Pattern == "" {
		t.Fatalf("%s carries no pattern", path)
	}
	return regexp.MustCompile(vocab.Pattern)
}

func TestRecordTimestampMatchesVocabPattern(t *testing.T) {
	// Arrange.
	pattern := vocabTimestampPattern(t)
	// A whole second is the instant that once rendered short and sorted wrong.
	at := time.Date(2026, 7, 28, 16, 34, 56, 0, time.UTC)

	// Act.
	rec := newRecord(at, RuntimeDaemon, LevelInfo, "daemon.dlog.test", "m", nil, 42)

	// Assert.
	if !pattern.MatchString(rec.Timestamp) {
		t.Fatalf("Timestamp = %q, does not match the vocab pattern %s", rec.Timestamp, pattern)
	}
}

func TestRecordCarriesEveryRequiredField(t *testing.T) {
	// Arrange.
	at := time.Date(2026, 7, 28, 16, 34, 56, 789000000, time.UTC)

	// Act.
	line := newRecord(at, RuntimeDaemon, LevelWarn, "daemon.dlog.test", "a message",
		Context{"branch": "taken"}, 4242).marshal()

	// Assert.
	var got map[string]any
	if err := json.Unmarshal(line, &got); err != nil {
		t.Fatalf("the record is not one JSON object: %v (%s)", err, line)
	}
	for _, field := range []string{"timestamp", "runtime", "level", "verbosity", "operation", "message", "context", "pid"} {
		if _, ok := got[field]; !ok {
			t.Fatalf("record is missing the required field %q: %s", field, line)
		}
	}
	if got["runtime"] != RuntimeDaemon || got["level"] != LevelWarn || got["message"] != "a message" {
		t.Fatalf("record fields = %v", got)
	}
}

func TestRecordIsExactlyOneLine(t *testing.T) {
	// Arrange, Act.
	line := newRecord(time.Now(), RuntimeDaemon, LevelInfo, "daemon.dlog.test",
		"a message\nwith an embedded newline", Context{"k": "v\nv"}, 1).marshal()

	// Assert.
	if n := len(line); line[n-1] != '\n' {
		t.Fatalf("record does not end in a newline: %q", line)
	}
	if body := line[:len(line)-1]; regexp.MustCompile(`\n`).Match(body) {
		t.Fatalf("record body carries a raw newline, so it is not one JSONL line: %q", body)
	}
}

func TestRecordVerbosityFollowsLevel(t *testing.T) {
	tests := []struct {
		name  string
		level string
		want  string
	}{
		{name: "debug is the verbose class", level: LevelDebug, want: VerbosityVerbose},
		{name: "info is normal", level: LevelInfo, want: VerbosityNormal},
		{name: "warn is normal", level: LevelWarn, want: VerbosityNormal},
		{name: "error is normal", level: LevelError, want: VerbosityNormal},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			rec := newRecord(time.Now(), RuntimeDaemon, tc.level, "daemon.dlog.test", "m", nil, 1)

			// Assert.
			if rec.Verbosity != tc.want {
				t.Fatalf("Verbosity = %q, want %q", rec.Verbosity, tc.want)
			}
		})
	}
}

func TestRecordPromotesIdentityOutOfContext(t *testing.T) {
	tests := []struct {
		name  string
		key   string
		field func(record) string
	}{
		{name: "workspace_dir", key: KeyWorkspaceDir, field: func(r record) string { return r.WorkspaceDir }},
		{name: "workspace_id", key: KeyWorkspaceID, field: func(r record) string { return r.WorkspaceID }},
		{name: "agent_repl_session_id", key: KeyAgentReplSessionID, field: func(r record) string { return r.AgentReplSessionID }},
		{name: "claude_session_id", key: KeyClaudeSessionID, field: func(r record) string { return r.ClaudeSessionID }},
		{name: "request_id", key: KeyRequestID, field: func(r record) string { return r.RequestID }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			rec := newRecord(time.Now(), RuntimeDaemon, LevelInfo, "daemon.dlog.test", "m",
				Context{tc.key: "value"}, 1)

			// Assert.
			if got := tc.field(rec); got != "value" {
				t.Fatalf("%s field = %q, want %q", tc.key, got, "value")
			}
			if _, still := rec.Context[tc.key]; still {
				t.Fatalf("%s stayed inside context; identifiers belong in their dedicated field", tc.key)
			}
		})
	}
}

func TestRecordContextIsAnObjectWhenEmpty(t *testing.T) {
	// Arrange, Act.
	line := newRecord(time.Now(), RuntimeDaemon, LevelInfo, "daemon.dlog.test", "m", nil, 1).marshal()

	// Assert.
	var got map[string]json.RawMessage
	if err := json.Unmarshal(line, &got); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	if string(got["context"]) != "{}" {
		t.Fatalf("context = %s, want {} — context is a required object, never null", got["context"])
	}
}

func TestRecordSurvivesAnUnencodableContextValue(t *testing.T) {
	// Arrange: a channel cannot be JSON-encoded.
	ctx := Context{"good": "kept", "bad": make(chan int)}

	// Act.
	line := newRecord(time.Now(), RuntimeDaemon, LevelError, "daemon.dlog.test", "m", ctx, 1).marshal()

	// Assert: the record survives rather than being lost to its own diagnostics.
	var got struct {
		Context map[string]any `json:"context"`
	}
	if err := json.Unmarshal(line, &got); err != nil {
		t.Fatalf("the record was lost to an unencodable context value: %v (%s)", err, line)
	}
	if got.Context["good"] != "kept" {
		t.Fatalf("context = %v, want the encodable value kept", got.Context)
	}
}

func TestRecordOmitsPidForAForwardedRecord(t *testing.T) {
	// Arrange, Act: pid 0 means "this record is not ours to attribute".
	line := newRecord(time.Now(), RuntimeWebapp, LevelInfo, "webapp.test", "m", nil, 0).marshal()

	// Assert.
	var got map[string]any
	if err := json.Unmarshal(line, &got); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	if _, ok := got["pid"]; ok {
		t.Fatalf("a forwarded record carries pid %v; it must carry the sender's identity instead", got["pid"])
	}
}
