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

// userTurnText returns the joined text of a user-turn frame's blocks.
func userTurnText(t *testing.T, frame protocol.L2Frame) string {
	t.Helper()
	turn, ok := frame.(*protocol.UserTurnFrame)
	if !ok {
		t.Fatalf("frame = %T, want *protocol.UserTurnFrame", frame)
	}
	var blocks []struct {
		Text string `json:"text"`
	}
	if err := json.Unmarshal(turn.Content, &blocks); err != nil {
		t.Fatalf("unmarshal content %s: %v", turn.Content, err)
	}
	var texts []string
	for _, b := range blocks {
		texts = append(texts, b.Text)
	}
	return strings.Join(texts, "\n")
}

func TestBuildReplayFramesCollapsesSlashCommandEnvelope(t *testing.T) {
	tests := []struct {
		name    string
		content string
		want    string
	}{
		{
			name:    "argless command reads as the typed command",
			content: `<command-name>/clear</command-name>\n            <command-message>clear</command-message>\n            <command-args></command-args>`,
			want:    "/clear",
		},
		{
			name:    "command with args reads as command plus args",
			content: `<command-name>/model</command-name>\n<command-message>model</command-message>\n<command-args>fable</command-args>`,
			want:    "/model fable",
		},
		{
			name:    "message-first tag order still collapses",
			content: `<command-message>workspace</command-message>\n<command-name>/workspace</command-name>\n<command-args>merge it</command-args>`,
			want:    "/workspace merge it",
		},
		{
			name:    "prompt quoting the tags is left verbatim",
			content: `i see <command-name>/clear</command-name> in the output`,
			want:    `i see <command-name>/clear</command-name> in the output`,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			line := `{"type":"user","message":{"role":"user","content":"` + tt.content + `"}}`
			// Act
			frames, _ := buildFrames(t, line)
			// Assert
			wantTypes(t, frames, "user-turn")
			if got := userTurnText(t, frames[0]); got != tt.want {
				t.Fatalf("user-turn text = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestBuildReplayFramesSkipsNamelessCommandEnvelope(t *testing.T) {
	// Arrange / Act — an envelope whose name collapses to nothing has no
	// prompt to render, so it gets no bubble at all.
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":"<command-name></command-name>\n<command-args></command-args>"}}`)
	// Assert
	wantTypes(t, frames)
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
	// Assert — the replayed history also teaches the client the model that
	// produced it, so a resumed session's topbar is right on arrival.
	wantTypes(t, frames, "model-changed", "text-start", "text-delta", "text-end")
	if start := frames[1].(*protocol.TextStartFrame); start.MessageID != "msg_1" {
		t.Fatalf("MessageID = %q, want msg_1", start.MessageID)
	}
}

func TestBuildReplayFramesAssistantTextCarriesTranscriptTimestamp(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"assistant","timestamp":"2026-05-24T14:33:00.000Z","message":{"id":"msg_1","content":[{"type":"text","text":"answer"}]}}`)
	// Assert — the response's own emit time, so its bubble is not stamped with
	// the (much later) resume time.
	wantTypes(t, frames, "text-start", "text-delta", "text-end")
	if got := frames[0].Env().TS; got != "2026-05-24T14:33:00.000Z" {
		t.Fatalf("TS = %q, want the transcript entry's timestamp", got)
	}
}

func TestBuildReplayFramesAssistantWithoutTimestampLeavesTSUnstamped(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"assistant","message":{"id":"msg_1","content":[{"type":"text","text":"answer"}]}}`)
	// Assert — an empty TS is the hub's cue to stamp its own.
	wantTypes(t, frames, "text-start", "text-delta", "text-end")
	if got := frames[0].Env().TS; got != "" {
		t.Fatalf("TS = %q, want empty so the hub stamps it", got)
	}
}

func TestBuildReplayFramesToolResultCarriesTranscriptTimestamp(t *testing.T) {
	// Arrange / Act — a tool result is translated, so it takes the same
	// pre-stamping path the assistant blocks do.
	frames, _ := buildFrames(t,
		`{"type":"user","timestamp":"2026-05-24T14:31:00.000Z","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"t1","content":"ok"}]}}`)
	// Assert
	wantTypes(t, frames, "tool-use-result")
	if got := frames[0].Env().TS; got != "2026-05-24T14:31:00.000Z" {
		t.Fatalf("TS = %q, want the transcript entry's timestamp", got)
	}
}

func TestBuildReplayFramesAdoptsTheLastAssistantModel(t *testing.T) {
	// Arrange / Act — the model moved mid-conversation.
	_, tr := buildFrames(t,
		`{"type":"assistant","message":{"id":"m1","model":"opus","content":[{"type":"text","text":"a"}]}}`,
		`{"type":"assistant","message":{"id":"m2","model":"haiku","content":[{"type":"text","text":"b"}]}}`)
	// Assert — the mirror lands on the LAST model, not the first.
	if tr.Model != "haiku" {
		t.Errorf("mirror = %q, want haiku", tr.Model)
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

// --- sidechain (subagents/agent-*.jsonl) replay -------------------------------

// buildFramesWithAgents replays MAIN plus a subagents dir holding one
// agent-<id>.jsonl per AGENTS entry.
func buildFramesWithAgents(t *testing.T, mainLines []string, agents map[string][]string) ([]protocol.L2Frame, *Translator) {
	t.Helper()
	sub := filepath.Join(t.TempDir(), "subagents")
	if err := os.MkdirAll(sub, 0o755); err != nil {
		t.Fatal(err)
	}
	for id, lines := range agents {
		path := filepath.Join(sub, "agent-"+id+".jsonl")
		if err := os.WriteFile(path, []byte(strings.Join(lines, "\n")), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	tr := NewTranslator()
	return BuildReplayFramesWithAgents(tr, strings.NewReader(strings.Join(mainLines, "\n")), sub), tr
}

// The main-transcript shape every sidechain test starts from: an Agent
// call whose result announces the sidechain's agentId.
var sidechainMain = []string{
	`{"type":"assistant","timestamp":"2026-05-24T10:00:00.000Z","message":{"id":"m1","role":"assistant","model":"claude-fable-5","content":[{"type":"tool_use","id":"tk1","name":"Agent","input":{"description":"d","prompt":"do the thing"}}],"usage":{"input_tokens":1,"output_tokens":1}}}`,
	`{"type":"user","timestamp":"2026-05-24T10:00:01.000Z","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"tk1","content":"Async agent launched successfully. agentId: abc1"}]}}`,
}

var sidechainAgentLines = []string{
	`{"type":"user","timestamp":"2026-05-24T10:00:02.000Z","message":{"role":"user","content":"do the thing"}}`,
	`{"type":"assistant","timestamp":"2026-05-24T10:00:03.000Z","message":{"id":"am1","role":"assistant","model":"claude-haiku-4-5","content":[{"type":"text","text":"working"}],"usage":{"input_tokens":1,"output_tokens":1}}}`,
}

func firstParentedTextStart(frames []protocol.L2Frame) *protocol.TextStartFrame {
	for _, f := range frames {
		if ts, ok := f.(*protocol.TextStartFrame); ok && ts.ParentToolUseID != "" {
			return ts
		}
	}
	return nil
}

func TestSidechainReplayLinksBackgroundAgentByAnnouncedID(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFramesWithAgents(t, sidechainMain, map[string][]string{"abc1": sidechainAgentLines})
	// Assert — the agent's prose replays under the call that spawned it.
	ts := firstParentedTextStart(frames)
	if ts == nil || ts.ParentToolUseID != "tk1" {
		t.Fatalf("parented text-start = %+v", ts)
	}
}

func TestSidechainReplayPromptMatchesForegroundAgent(t *testing.T) {
	// Arrange — the result carries the agent's ANSWER, no agentId.
	main := []string{
		sidechainMain[0],
		`{"type":"user","timestamp":"2026-05-24T10:00:01.000Z","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"tk1","content":"the answer"}]}}`,
	}
	// Act — the file's first user prompt equals the Agent input's prompt.
	frames, _ := buildFramesWithAgents(t, main, map[string][]string{"zzz9": sidechainAgentLines})
	// Assert
	ts := firstParentedTextStart(frames)
	if ts == nil || ts.ParentToolUseID != "tk1" {
		t.Fatalf("parented text-start = %+v", ts)
	}
}

func TestSidechainReplaySkipsUnlinkableFile(t *testing.T) {
	// Arrange — no agentId announcement and a prompt matching nothing.
	agent := []string{
		`{"type":"user","timestamp":"2026-05-24T10:00:02.000Z","message":{"role":"user","content":"some other prompt"}}`,
		sidechainAgentLines[1],
	}
	main := []string{
		sidechainMain[0],
		`{"type":"user","timestamp":"2026-05-24T10:00:01.000Z","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"tk1","content":"the answer"}]}}`,
	}
	// Act
	frames, _ := buildFramesWithAgents(t, main, map[string][]string{"zzz9": agent})
	// Assert — unparented frames would pollute the feed, so none replay.
	if ts := firstParentedTextStart(frames); ts != nil {
		t.Fatalf("unexpected parented frames: %+v", ts)
	}
}

func TestSidechainReplayEmitsNoUserTurnForTheInjectedPrompt(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFramesWithAgents(t, sidechainMain, map[string][]string{"abc1": sidechainAgentLines})
	// Assert — the prompt is the spawning call's input, not a user bubble.
	for _, f := range frames {
		if _, ok := f.(*protocol.UserTurnFrame); ok {
			t.Fatalf("sidechain replay produced a user-turn: %+v", f)
		}
	}
}

func TestSidechainReplayDoesNotFlipTheModelMirror(t *testing.T) {
	// Arrange / Act — the agent file reports a haiku model.
	_, tr := buildFramesWithAgents(t, sidechainMain, map[string][]string{"abc1": sidechainAgentLines})
	// Assert — a subagent's model must not masquerade as the session's.
	if tr.Model != "claude-fable-5" {
		t.Fatalf("mirror = %q, want claude-fable-5", tr.Model)
	}
}

func TestSidechainReplayInterleavesByTimestamp(t *testing.T) {
	// Arrange — a main text block written AFTER the agent's (10:00:05).
	main := append(append([]string{}, sidechainMain...),
		`{"type":"assistant","timestamp":"2026-05-24T10:00:05.000Z","message":{"id":"m2","role":"assistant","model":"claude-fable-5","content":[{"type":"text","text":"after"}],"usage":{"input_tokens":1,"output_tokens":1}}}`)
	// Act
	frames, _ := buildFramesWithAgents(t, main, map[string][]string{"abc1": sidechainAgentLines})
	// Assert — the agent's 10:00:03 prose lands before the 10:00:05 main text.
	agentAt, mainAt := -1, -1
	for i, f := range frames {
		if ts, ok := f.(*protocol.TextStartFrame); ok {
			if ts.ParentToolUseID != "" {
				agentAt = i
			} else {
				mainAt = i
			}
		}
	}
	if agentAt == -1 || mainAt == -1 || agentAt > mainAt {
		t.Fatalf("agentAt = %d, mainAt = %d, want agent first", agentAt, mainAt)
	}
}

func TestSidechainReplayToolResultsCarryTheParent(t *testing.T) {
	// Arrange — the agent runs its own tool and gets a result back.
	agent := append(append([]string{}, sidechainAgentLines...),
		`{"type":"assistant","timestamp":"2026-05-24T10:00:04.000Z","message":{"id":"am2","role":"assistant","model":"claude-haiku-4-5","content":[{"type":"tool_use","id":"gt1","name":"Grep","input":{"pattern":"x"}}],"usage":{"input_tokens":1,"output_tokens":1}}}`,
		`{"type":"user","timestamp":"2026-05-24T10:00:05.000Z","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"gt1","content":"no matches"}]}}`)
	// Act
	frames, _ := buildFramesWithAgents(t, sidechainMain, map[string][]string{"abc1": agent})
	// Assert — the nested call and its result both replay.
	var start, result bool
	for _, f := range frames {
		if s, ok := f.(*protocol.ToolUseStartFrame); ok && s.ToolUseID == "gt1" && s.ParentToolUseID == "tk1" {
			start = true
		}
		if r, ok := f.(*protocol.ToolUseResultFrame); ok && r.ToolUseID == "gt1" {
			result = true
		}
	}
	if !start || !result {
		t.Fatalf("start = %v, result = %v, want both", start, result)
	}
}

func TestSidechainReplayMissingDirMatchesPlainReplay(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act — a session that never spawned agents has no subagents dir.
	frames := BuildReplayFramesWithAgents(tr, strings.NewReader(strings.Join(sidechainMain, "\n")),
		filepath.Join(t.TempDir(), "subagents"))
	// Assert — exactly the main chain's frames, nothing dropped or added.
	plain := BuildReplayFrames(NewTranslator(), strings.NewReader(strings.Join(sidechainMain, "\n")))
	if len(frames) != len(plain) {
		t.Fatalf("len = %d, want %d", len(frames), len(plain))
	}
}

func TestSubagentsDirForDerivesTheSessionDir(t *testing.T) {
	// Arrange / Act
	got := subagentsDirFor(filepath.Join("p", "projects", "slug", "uuid-1.jsonl"))
	// Assert
	want := filepath.Join("p", "projects", "slug", "uuid-1", "subagents")
	if got != want {
		t.Fatalf("subagentsDirFor = %q, want %q", got, want)
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

// --- closeReplayTurns (turn-closing result frames) ---------------------------

func TestBuildReplayFramesClosesCompletedTurnWithResult(t *testing.T) {
	// Arrange / Act — a prompt the agent answered is a completed turn,
	// which the live stream closes with a result the transcript omits.
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":"q"}}`,
		`{"type":"assistant","message":{"id":"m1","content":[{"type":"text","text":"a"}]}}`)
	// Assert — the synthetic result trails the turn's final text, so the
	// webapp draws its green final-response border on a resumed session.
	wantTypes(t, frames, "user-turn", "text-start", "text-delta", "text-end", "result")
}

func TestBuildReplayFramesSyntheticResultIsSuccess(t *testing.T) {
	// Arrange / Act
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":"q"}}`,
		`{"type":"assistant","message":{"id":"m1","content":[{"type":"text","text":"a"}]}}`)
	// Assert — only a "success" result marks a turn's answer a final
	// response (the green border keys off exactly that subtype).
	result, ok := frames[len(frames)-1].(*protocol.ResultFrame)
	if !ok {
		t.Fatalf("last frame = %T, want *protocol.ResultFrame", frames[len(frames)-1])
	}
	if result.Subtype != "success" {
		t.Fatalf("result subtype = %q, want success", result.Subtype)
	}
}

func TestBuildReplayFramesResultClosesPriorTurnBeforeNextPrompt(t *testing.T) {
	// Arrange / Act — two answered turns.
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":"q1"}}`,
		`{"type":"assistant","message":{"id":"m1","content":[{"type":"text","text":"a1"}]}}`,
		`{"type":"user","message":{"role":"user","content":"q2"}}`,
		`{"type":"assistant","message":{"id":"m2","content":[{"type":"text","text":"a2"}]}}`)
	// Assert — each turn's answer is closed by its own result: one before
	// the second prompt, one at end-of-input.
	wantTypes(t, frames,
		"user-turn", "text-start", "text-delta", "text-end", "result",
		"user-turn", "text-start", "text-delta", "text-end", "result")
}

func TestBuildReplayFramesIncompleteTrailingPromptGetsNoResult(t *testing.T) {
	// Arrange / Act — a trailing prompt the agent never answered is an
	// incomplete turn, so no result closes it (the live stream emits one
	// only when a turn ends).
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":"q1"}}`,
		`{"type":"assistant","message":{"id":"m1","content":[{"type":"text","text":"a1"}]}}`,
		`{"type":"user","message":{"role":"user","content":"q2 unanswered"}}`)
	// Assert — turn 1 is closed; the dangling q2 is not.
	wantTypes(t, frames,
		"user-turn", "text-start", "text-delta", "text-end", "result",
		"user-turn")
}

func TestBuildReplayFramesBackToBackPromptsGetNoResultBetween(t *testing.T) {
	// Arrange / Act — a prompt with no assistant response before the next
	// prompt never became an answered turn.
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":"q1"}}`,
		`{"type":"user","message":{"role":"user","content":"q2"}}`)
	// Assert — neither prompt gets a result.
	wantTypes(t, frames, "user-turn", "user-turn")
}

func TestBuildReplayFramesToolOnlyTurnGetsResult(t *testing.T) {
	// Arrange / Act — a turn that only ran a tool (no final text) is still
	// a completed turn, exactly as the live stream reports it.
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":"q1"}}`,
		`{"type":"assistant","message":{"id":"m1","content":[{"type":"tool_use","id":"toolu_1","name":"Bash","input":{"command":"ls"}}]}}`,
		`{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"toolu_1","content":"ok"}]}}`,
		`{"type":"user","message":{"role":"user","content":"q2 unanswered"}}`)
	// Assert — the tool-only turn is closed with a result before q2.
	wantTypes(t, frames,
		"user-turn", "tool-use-start", "tool-use-input-end", "tool-use-result", "result",
		"user-turn")
}

func TestBuildReplayFramesResultSplitsToolResultFromFollowingPrompt(t *testing.T) {
	// Arrange / Act — one user entry carries both the prior turn's tool
	// result and a fresh prompt; the closing result must land between them.
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":"q1"}}`,
		`{"type":"assistant","message":{"id":"m1","content":[{"type":"tool_use","id":"toolu_1","name":"Bash","input":{"command":"ls"}}]}}`,
		`{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"toolu_1","content":"ok"},{"type":"text","text":"q2"}]}}`)
	// Assert — tool result (closes the tool), then result (closes turn 1),
	// then the new prompt opens turn 2.
	wantTypes(t, frames,
		"user-turn", "tool-use-start", "tool-use-input-end",
		"tool-use-result", "result", "user-turn")
}

func TestBuildReplayFramesSyntheticResultCarriesClosingTurnTimestamp(t *testing.T) {
	// Arrange / Act — the result must be dated at the turn's end so the
	// webapp's "since previous final" timing is measured correctly.
	frames, _ := buildFrames(t,
		`{"type":"user","timestamp":"2026-05-24T12:00:00.000Z","message":{"role":"user","content":"q"}}`,
		`{"type":"assistant","timestamp":"2026-05-24T12:00:05.000Z","message":{"id":"m1","content":[{"type":"text","text":"a"}]}}`)
	// Assert — the result takes the closing assistant entry's timestamp.
	result := frames[len(frames)-1].(*protocol.ResultFrame)
	if got := result.Env().TS; got != "2026-05-24T12:00:05.000Z" {
		t.Fatalf("result TS = %q, want the closing turn's last frame time", got)
	}
}

func TestBuildReplayFramesResultPrecedesTrailingUsage(t *testing.T) {
	// Arrange / Act — a turn whose assistant message carries usage.
	frames, _ := buildFrames(t,
		`{"type":"user","message":{"role":"user","content":"q"}}`,
		`{"type":"assistant","message":{"id":"m1","content":[{"type":"text","text":"a"}],"usage":{"input_tokens":100,"output_tokens":7}}}`)
	// Assert — the turn-closing result comes before the trailing usage snapshot.
	wantTypes(t, frames,
		"user-turn", "text-start", "text-delta", "text-end", "result", "usage")
}

// --- SeedFromTranscript -------------------------------------------------------

func TestSeedFromTranscriptHelloReportsNoActiveTurn(t *testing.T) {
	// Arrange — a trailing prompt the agent never answered (Emacs killed
	// right after the prompt was sent). closeReplayTurns closes only answered
	// turns, so this incomplete turn gets no synthetic `result` and the seeded
	// ring ends on a dangling user-turn. The hello must still tell attaching
	// clients that nothing is running, else the topbar timer counts from this
	// prompt's (potentially days-old) stamp after restoration.
	dir := t.TempDir()
	path := filepath.Join(dir, "uuid-1.jsonl")
	transcript := `{"type":"user","message":{"role":"user","content":"first"}}` + "\n" +
		`{"type":"assistant","message":{"id":"msg_1","model":"claude-fable-5","content":[{"type":"text","text":"answer"}]}}` + "\n" +
		`{"type":"user","message":{"role":"user","content":"unanswered follow-up"}}` + "\n"
	if err := os.WriteFile(path, []byte(transcript), 0o600); err != nil {
		t.Fatal(err)
	}
	shim := newFakeShim()
	sess := New(Config{ID: "s_test", Shim: shim, CWD: dir, Logf: func(string, ...any) {}})
	if err := sess.SeedFromTranscript(path, "uuid-1"); err != nil {
		t.Fatalf("SeedFromTranscript: %v", err)
	}
	// Act
	client := NewClient()
	sess.Attach(client)
	var hello struct {
		Type       string `json:"type"`
		TurnActive bool   `json:"turn_active"`
	}
	if err := json.Unmarshal(<-client.Send, &hello); err != nil || hello.Type != "hello" {
		t.Fatalf("first frame = %+v (err %v), want hello", hello, err)
	}
	// Assert
	if hello.TurnActive {
		t.Fatal("hello turn_active = true after transcript seed, want false")
	}
}

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
	want := []string{"user-turn", "model-changed", "text-start", "text-delta", "text-end"}
	var types []string
	for len(types) < len(want) { // user-turn + the model it was answered on + text trio
		var env struct {
			Type string `json:"type"`
		}
		data := <-client.Send
		if err := json.Unmarshal(data, &env); err != nil {
			t.Fatalf("bad frame %s: %v", data, err)
		}
		types = append(types, env.Type)
	}
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

func TestNoteResumeUnavailableRingsRecoverableError(t *testing.T) {
	// Arrange
	shim := newFakeShim()
	sess := New(Config{ID: "s_test", Shim: shim, Logf: func(string, ...any) {}})
	// Act
	sess.NoteResumeUnavailable("uuid-gone", "/cfg/projects/-w/uuid-gone.jsonl")
	// Assert: the frame is retained for later attachers, not just live ones.
	client := NewClient()
	sess.Attach(client)
	<-client.Send // hello
	if err := sess.HandleClientFrame(client, []byte(`{"type":"replay-request","from_seq":1}`)); err != nil {
		t.Fatalf("replay-request: %v", err)
	}
	var frame struct {
		Type        string `json:"type"`
		Code        string `json:"code"`
		Message     string `json:"message"`
		Recoverable bool   `json:"recoverable"`
	}
	if err := json.Unmarshal(<-client.Send, &frame); err != nil {
		t.Fatalf("bad frame: %v", err)
	}
	if frame.Type != "error" || frame.Code != "resume_unavailable" || !frame.Recoverable {
		t.Fatalf("frame = %+v, want recoverable resume_unavailable error", frame)
	}
	if !strings.Contains(frame.Message, "uuid-gone") {
		t.Fatalf("message %q does not name the resume target", frame.Message)
	}
}

func TestClaudeConfigDirPrefersThePerSessionDir(t *testing.T) {
	// Arrange — the daemon's own env names a DIFFERENT account.
	t.Setenv("CLAUDE_CONFIG_DIR", "/daemon/.claude")
	// Act
	got := ClaudeConfigDir("/home/u/.claude-chesscom")
	// Assert
	if got != "/home/u/.claude-chesscom" {
		t.Fatalf("ClaudeConfigDir = %q, want the session's own dir", got)
	}
}

func TestClaudeConfigDirFallsBackToTheDaemonDefault(t *testing.T) {
	// Arrange
	t.Setenv("CLAUDE_CONFIG_DIR", "/daemon/.claude")
	// Act — a session that carries no dir of its own.
	got := ClaudeConfigDir("")
	// Assert
	if got != "/daemon/.claude" {
		t.Fatalf("ClaudeConfigDir = %q, want the daemon default", got)
	}
}
