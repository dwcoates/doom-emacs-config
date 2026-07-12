package session

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/protocol"
)

// --- TranscriptPath / DefaultClaudeConfigDir --------------------------------

func TestTranscriptPathEncodesCwdSlug(t *testing.T) {
	tests := []struct {
		name string
		cwd  string
		want string
	}{
		{
			name: "dots and slashes become dashes",
			cwd:  "/Users/dodgecoates/.config/doom",
			want: filepath.Join("cfg", "projects", "-Users-dodgecoates--config-doom", "uuid-1.jsonl"),
		},
		{
			name: "underscores become dashes",
			cwd:  "/tmp/my_proj",
			want: filepath.Join("cfg", "projects", "-tmp-my-proj", "uuid-1.jsonl"),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := TranscriptPath("cfg", tt.cwd, "uuid-1")
			// Assert
			if got != tt.want {
				t.Fatalf("TranscriptPath(%q) = %q, want %q", tt.cwd, got, tt.want)
			}
		})
	}
}

func TestDefaultClaudeConfigDirHonorsEnvOverride(t *testing.T) {
	// Arrange
	t.Setenv("CLAUDE_CONFIG_DIR", "/custom/claude")
	// Act / Assert
	if got := DefaultClaudeConfigDir(); got != "/custom/claude" {
		t.Fatalf("DefaultClaudeConfigDir() = %q, want /custom/claude", got)
	}
}

func TestDefaultClaudeConfigDirFallsBackToHome(t *testing.T) {
	// Arrange
	t.Setenv("CLAUDE_CONFIG_DIR", "")
	home, err := os.UserHomeDir()
	if err != nil {
		t.Skipf("no home dir in test environment: %v", err)
	}
	// Act / Assert
	if got, want := DefaultClaudeConfigDir(), filepath.Join(home, ".claude"); got != want {
		t.Fatalf("DefaultClaudeConfigDir() = %q, want %q", got, want)
	}
}

// --- BuildReplayFrames -------------------------------------------------------

// buildFrames runs BuildReplayFrames over the given transcript lines with
// a fresh translator, returning the frames and the translator for state
// assertions.
func buildFrames(t *testing.T, lines ...string) ([]protocol.L2Frame, *Translator) {
	t.Helper()
	tr := NewTranslator()
	frames := BuildReplayFrames(tr, strings.NewReader(strings.Join(lines, "\n")))
	return frames, tr
}

func TestBuildReplayFramesUserStringContent(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":"hello there"}}`)
	// Assert
	wantTypes(t, frames, "user-turn")
	turn := frames[0].(*protocol.UserTurnFrame)
	if turn.RequestID != "replay-1" {
		t.Fatalf("RequestID = %q, want replay-1", turn.RequestID)
	}
	var blocks []map[string]string
	if err := json.Unmarshal(turn.Content, &blocks); err != nil || len(blocks) != 1 || blocks[0]["text"] != "hello there" {
		t.Fatalf("content = %s (err %v), want one text block", turn.Content, err)
	}
}

func TestBuildReplayFramesUserTurnCarriesTranscriptTimestamp(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"user","timestamp":"2026-05-24T12:34:56.789Z","message":{"role":"user","content":"hello there"}}`)
	// Assert — the prompt's own send time, not the (much later) resume time.
	wantTypes(t, frames, "user-turn")
	if got := frames[0].Env().TS; got != "2026-05-24T12:34:56.789Z" {
		t.Fatalf("TS = %q, want the transcript entry's timestamp", got)
	}
}

func TestBuildReplayFramesUserTurnBlocksCarryTranscriptTimestamp(t *testing.T) {
	// Arrange / Act — array-form content takes a different build path.
	frames, _ := buildFrames(t,
		`{"type":"user","timestamp":"2026-05-24T01:02:03.000Z","message":{"role":"user","content":[{"type":"text","text":"block text"}]}}`)
	// Assert
	wantTypes(t, frames, "user-turn")
	if got := frames[0].Env().TS; got != "2026-05-24T01:02:03.000Z" {
		t.Fatalf("TS = %q, want the transcript entry's timestamp", got)
	}
}

func TestBuildReplayFramesUserTurnWithoutTimestampLeavesTSUnstamped(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":"hello there"}}`)
	// Assert — an empty TS is the hub's cue to stamp its own.
	wantTypes(t, frames, "user-turn")
	if got := frames[0].Env().TS; got != "" {
		t.Fatalf("TS = %q, want empty so the hub stamps it", got)
	}
}

func TestBuildReplayFramesUserEmptyStringSkipped(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":""}}`)
	// Assert
	wantTypes(t, frames)
}

func TestBuildReplayFramesUserTextBlocks(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":[{"type":"text","text":"block text"}]}}`)
	// Assert
	wantTypes(t, frames, "user-turn")
	turn := frames[0].(*protocol.UserTurnFrame)
	if !strings.Contains(string(turn.Content), "block text") {
		t.Fatalf("content = %s, want the original text block", turn.Content)
	}
}

func TestBuildReplayFramesToolResultBlock(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"toolu_1","content":"ok","is_error":true}]}}`)
	// Assert
	wantTypes(t, frames, "tool-use-result")
	result := frames[0].(*protocol.ToolUseResultFrame)
	if result.ToolUseID != "toolu_1" || !result.IsError {
		t.Fatalf("tool result = %+v, want toolu_1 with is_error", result)
	}
}

func TestBuildReplayFramesMixedUserEntryOrdersToolResultFirst(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"toolu_1","content":"ok"},{"type":"text","text":"and a question"}]}}`)
	// Assert
	wantTypes(t, frames, "tool-use-result", "user-turn")
}

func TestBuildReplayFramesAssistantTextBlock(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"assistant","message":{"id":"msg_1","model":"claude-fable-5","content":[{"type":"text","text":"answer"}]}}`)
	// Assert
	wantTypes(t, frames, "text-start", "text-delta", "text-end")
	if start := frames[0].(*protocol.TextStartFrame); start.MessageID != "msg_1" {
		t.Fatalf("MessageID = %q, want msg_1", start.MessageID)
	}
}

func TestBuildReplayFramesAssistantThinkingBlock(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"assistant","message":{"id":"msg_1","content":[{"type":"thinking","thinking":"hmm","signature":"sig-1"}]}}`)
	// Assert
	wantTypes(t, frames, "thinking-start", "thinking-delta", "thinking-end")
	if end := frames[2].(*protocol.ThinkingEndFrame); end.Signature != "sig-1" {
		t.Fatalf("Signature = %q, want sig-1", end.Signature)
	}
}

func TestBuildReplayFramesAssistantToolUseRegistersRenderMeta(t *testing.T) {
	// Arrange: a Bash tool_use followed by its result in a later entry.
	// Act
	frames, _ := buildFrames(t,
		`{"type":"assistant","message":{"id":"msg_1","content":[{"type":"tool_use","id":"toolu_1","name":"Bash","input":{"command":"ls"}}]}}`,
		`{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"toolu_1","content":"file.txt"}]}}`)
	// Assert: the result frame carries the render hint only a
	// registered tool_use can produce.
	wantTypes(t, frames, "tool-use-start", "tool-use-input-end", "tool-use-result")
	result := frames[2].(*protocol.ToolUseResultFrame)
	if result.Render == nil || result.Render.Kind != "bash" {
		t.Fatalf("Render = %+v, want bash render hint", result.Render)
	}
}

func TestBuildReplayFramesCapturesModel(t *testing.T) {
	// Arrange / Act
	_, tr := buildFrames(t,
		`{"type":"assistant","message":{"id":"msg_1","model":"claude-fable-5","content":[]}}`)
	// Assert
	if tr.Model != "claude-fable-5" {
		t.Fatalf("Model = %q, want claude-fable-5", tr.Model)
	}
}

func TestBuildReplayFramesEmitsTrailingUsage(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"assistant","message":{"id":"msg_1","content":[{"type":"text","text":"a"}],"usage":{"input_tokens":100,"output_tokens":7}}}`)
	// Assert: block trio plus the trailing usage frame.
	wantTypes(t, frames, "text-start", "text-delta", "text-end", "usage")
	usage := frames[3].(*protocol.UsageFrame)
	if usage.Usage.InputTokens != 100 || usage.MessageID != "msg_1" {
		t.Fatalf("usage frame = %+v, want input 100 on msg_1", usage)
	}
}

func TestBuildReplayFramesSkipsSidechainEntries(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"assistant","isSidechain":true,"message":{"id":"msg_1","content":[{"type":"text","text":"subagent"}]}}`)
	// Assert
	wantTypes(t, frames)
}

func TestBuildReplayFramesSkipsMetaEntries(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"user","isMeta":true,"message":{"role":"user","content":"caveat text"}}`)
	// Assert
	wantTypes(t, frames)
}

func TestBuildReplayFramesSkipsNonConversationTypes(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"queue-operation","operation":"enqueue"}`,
		`{"type":"attachment","attachment":{"type":"hook_success"}}`,
		`{"type":"last-prompt","lastPrompt":"continue"}`,
		`{"type":"system","subtype":"stop_hook_summary"}`,
		`{"type":"summary","summary":"old topic"}`)
	// Assert
	wantTypes(t, frames)
}

func TestBuildReplayFramesSkipsMalformedLineAndContinues(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{not json`,
		`{"type":"user","message":{"role":"user","content":"still here"}}`)
	// Assert
	wantTypes(t, frames, "user-turn")
}

func TestBuildReplayFramesLeavesTurnInactive(t *testing.T) {
	// Arrange / Act
	_, tr := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":"hello"}}`,
		`{"type":"assistant","message":{"id":"msg_1","content":[{"type":"text","text":"hi"}]}}`)
	// Assert: replay must not leave the session looking mid-turn.
	if tr.TurnActive() {
		t.Fatal("TurnActive() = true after replay, want false")
	}
}

// --- SeedFromTranscript -------------------------------------------------------

func TestSeedFromTranscriptRetainsFramesForReplay(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	path := filepath.Join(dir, "uuid-1.jsonl")
	transcript := `{"type":"user","message":{"role":"user","content":"hello"}}` + "\n" +
		`{"type":"assistant","message":{"id":"msg_1","model":"claude-fable-5","content":[{"type":"text","text":"hi"}]}}` + "\n"
	if err := os.WriteFile(path, []byte(transcript), 0o600); err != nil {
		t.Fatal(err)
	}
	shim := newFakeShim()
	sess := New(Config{ID: "s_test", Shim: shim, CWD: dir, Logf: func(string, ...any) {}})
	// Act
	if err := sess.SeedFromTranscript(path, "uuid-1"); err != nil {
		t.Fatalf("SeedFromTranscript: %v", err)
	}
	// Assert: a client attaching afterwards replays the seed from the
	// hello's resume_from_seq watermark (§2.10).
	client := NewClient()
	sess.Attach(client)
	var hello struct {
		Type          string `json:"type"`
		ResumeFromSeq int64  `json:"resume_from_seq"`
	}
	if err := json.Unmarshal(<-client.Send, &hello); err != nil || hello.Type != "hello" {
		t.Fatalf("first frame = %+v (err %v), want hello", hello, err)
	}
	if hello.ResumeFromSeq == 0 {
		t.Fatal("hello.resume_from_seq = 0, want a seeded ring watermark")
	}
	req := []byte(`{"type":"replay-request","from_seq":` + strconv.FormatInt(hello.ResumeFromSeq, 10) + `}`)
	if err := sess.HandleClientFrame(client, req); err != nil {
		t.Fatalf("replay-request: %v", err)
	}
	var types []string
	for len(types) < 4 { // user-turn + text trio
		var env struct {
			Type string `json:"type"`
		}
		data := <-client.Send
		if err := json.Unmarshal(data, &env); err != nil {
			t.Fatalf("bad frame %s: %v", data, err)
		}
		types = append(types, env.Type)
	}
	want := []string{"user-turn", "text-start", "text-delta", "text-end"}
	for i := range want {
		if types[i] != want[i] {
			t.Fatalf("replayed frame types = %v, want %v", types, want)
		}
	}
}

// seedAndReplayFirstFrame seeds sess from a one-entry transcript and
// returns the single frame a freshly attached client replays back.
func seedAndReplayFirstFrame(t *testing.T, sess *Session, entry string) map[string]any {
	t.Helper()
	path := filepath.Join(t.TempDir(), "uuid-1.jsonl")
	if err := os.WriteFile(path, []byte(entry+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := sess.SeedFromTranscript(path, "uuid-1"); err != nil {
		t.Fatalf("SeedFromTranscript: %v", err)
	}
	client := NewClient()
	sess.Attach(client)
	var hello struct {
		ResumeFromSeq int64 `json:"resume_from_seq"`
	}
	if err := json.Unmarshal(<-client.Send, &hello); err != nil {
		t.Fatalf("unmarshal hello: %v", err)
	}
	req := []byte(`{"type":"replay-request","from_seq":` + strconv.FormatInt(hello.ResumeFromSeq, 10) + `}`)
	if err := sess.HandleClientFrame(client, req); err != nil {
		t.Fatalf("replay-request: %v", err)
	}
	var frame map[string]any
	if err := json.Unmarshal(<-client.Send, &frame); err != nil {
		t.Fatalf("unmarshal replayed frame: %v", err)
	}
	return frame
}

// clockedSession is a session whose frame clock is pinned to a time
// distinct from any transcript timestamp under test.
func clockedSession(t *testing.T) *Session {
	t.Helper()
	return New(Config{
		ID:    "s_test",
		Shim:  newFakeShim(),
		Now:   func() time.Time { return time.Date(2026, 6, 1, 9, 0, 0, 0, time.UTC) },
		Logf:  func(string, ...any) {},
		Model: "claude-fable-5",
	})
}

func TestSeedFromTranscriptReplaysUserTurnAtItsOriginalTime(t *testing.T) {
	// Arrange
	sess := clockedSession(t)
	// Act
	frame := seedAndReplayFirstFrame(t, sess,
		`{"type":"user","timestamp":"2026-05-24T12:34:56.789Z","message":{"role":"user","content":"hello"}}`)
	// Assert — the hub kept the prompt's send time instead of resume time.
	if frame["type"] != "user-turn" || frame["ts"] != "2026-05-24T12:34:56.789Z" {
		t.Fatalf("replayed frame = %v, want a user-turn stamped at its transcript time", frame)
	}
}

func TestSeedFromTranscriptStampsHubTimeWhenTranscriptEntryHasNoTimestamp(t *testing.T) {
	// Arrange
	sess := clockedSession(t)
	// Act
	frame := seedAndReplayFirstFrame(t, sess,
		`{"type":"user","message":{"role":"user","content":"hello"}}`)
	// Assert
	if frame["ts"] != "2026-06-01T09:00:00.000Z" {
		t.Fatalf("ts = %v, want the hub clock's stamp", frame["ts"])
	}
}

func TestSeedFromTranscriptStampsClaudeSessionID(t *testing.T) {
	// Arrange
	shim := newFakeShim()
	sess := New(Config{ID: "s_test", Shim: shim, Logf: func(string, ...any) {}})
	// Act: transcript path does not exist.
	err := sess.SeedFromTranscript(filepath.Join(t.TempDir(), "missing.jsonl"), "uuid-9")
	// Assert: the failure surfaces AND the resume identity still sticks.
	if err == nil {
		t.Fatal("SeedFromTranscript on a missing file returned nil error")
	}
	if got := sess.Info().ClaudeSessionID; got != "uuid-9" {
		t.Fatalf("ClaudeSessionID = %q, want uuid-9", got)
	}
}

func TestSeedFromTranscriptSetsModelInHello(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	path := filepath.Join(dir, "uuid-1.jsonl")
	transcript := `{"type":"assistant","message":{"id":"msg_1","model":"claude-fable-5","content":[]}}` + "\n"
	if err := os.WriteFile(path, []byte(transcript), 0o600); err != nil {
		t.Fatal(err)
	}
	shim := newFakeShim()
	sess := New(Config{ID: "s_test", Shim: shim, Logf: func(string, ...any) {}})
	// Act
	if err := sess.SeedFromTranscript(path, "uuid-1"); err != nil {
		t.Fatalf("SeedFromTranscript: %v", err)
	}
	// Assert
	if got := sess.Info().Model; got != "claude-fable-5" {
		t.Fatalf("Model = %q, want claude-fable-5", got)
	}
}
