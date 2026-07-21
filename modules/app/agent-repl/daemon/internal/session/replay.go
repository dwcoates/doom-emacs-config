// Transcript replay: seed a resumed session's retained-frame ring with
// the prior conversation so attaching webapp clients render history
// immediately.
//
// The CLI restores context on --resume but re-emits NOTHING through the
// stream (empirically verified; see shim/src/fake-query.ts), and a new
// session's §2.10 replay window starts empty — without this seeding, a
// resumed session renders as a blank conversation (and a zero token
// counter) until its first live turn. That bites every binding
// recreation: daemon restart, Emacs restart, vterm→gui frontend switch.
//
// The seed is built from the resumed session's transcript JSONL under
// the Claude config dir (~/.claude/projects/<cwd-slug>/<uuid>.jsonl),
// translated through the session's own Translator so the frames are
// exactly what the live stream would have produced (§2.3–§2.6), plus a
// trailing §2.8 usage frame so the webapp's token counter reflects the
// resumed context size.

package session

import (
	"bufio"
	"encoding/json"
	"errors"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"

	"claude-repld/internal/protocol"
)

// DefaultClaudeConfigDir returns the Claude CLI config root: the
// CLAUDE_CONFIG_DIR override when set, else ~/.claude.
func DefaultClaudeConfigDir() string {
	if dir := os.Getenv("CLAUDE_CONFIG_DIR"); dir != "" {
		return dir
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return filepath.Join(home, ".claude")
}

// ClaudeConfigDir resolves the config root for ONE session: its
// per-session dir when Emacs supplied one (agent-repl--compute-config-dir
// picks the account from the project dir — ~/.claude-chesscom under
// $MULTI_REPO_ROOT, ~/.claude elsewhere), else the daemon-wide default.
//
// Every transcript lookup MUST route through here rather than
// DefaultClaudeConfigDir: a session whose CLI writes into
// ~/.claude-chesscom has no transcript under ~/.claude, so resolving
// against the daemon's own env would fail the resume-viability gate and
// silently downgrade a resume into a fresh conversation.
func ClaudeConfigDir(dir string) string {
	if dir != "" {
		return dir
	}
	return DefaultClaudeConfigDir()
}

var transcriptSlugRe = regexp.MustCompile(`[^A-Za-z0-9]`)

// TranscriptPath returns the transcript JSONL path for claudeSessionID
// rooted at cwd, mirroring the CLI's project-dir encoding (every
// non-alphanumeric byte of the absolute cwd becomes "-").
func TranscriptPath(configDir, cwd, claudeSessionID string) string {
	slug := transcriptSlugRe.ReplaceAllString(cwd, "-")
	return filepath.Join(configDir, "projects", slug, claudeSessionID+".jsonl")
}

// transcriptEntry is the subset of one transcript JSONL line the replay
// builder inspects. Timestamp is the CLI's ISO8601 stamp for the entry;
// it rides through onto the replayed user-turn envelope so the webapp
// renders each historical prompt at the time it was actually sent
// rather than at resume time.
type transcriptEntry struct {
	Type        string          `json:"type"`
	IsSidechain bool            `json:"isSidechain"`
	IsMeta      bool            `json:"isMeta"`
	Timestamp   string          `json:"timestamp"`
	Message     json.RawMessage `json:"message"`
	// Error is the SDK's assistant-message error verdict, recorded beside
	// Message on an assistant entry that was an API-level failure; empty
	// otherwise. Carried through so a rehydrated session reddens the same
	// error bubbles a live one does.
	Error string `json:"error,omitempty"`
}

// transcriptUserMessage decodes a user entry's message envelope.
type transcriptUserMessage struct {
	Content json.RawMessage `json:"content"`
}

// transcriptUserBlock is one block of a user message's array-form
// content. Tool results carry ToolUseID/Content/IsError; every other
// block type passes through raw.
type transcriptUserBlock struct {
	Type      string          `json:"type"`
	ToolUseID string          `json:"tool_use_id"`
	Content   json.RawMessage `json:"content"`
	IsError   bool            `json:"is_error"`
}

// transcriptAssistantMeta is the assistant message metadata the builder
// lifts (the content blocks themselves go through the Translator's
// assistant-message path verbatim).
type transcriptAssistantMeta struct {
	ID    string          `json:"id"`
	Model string          `json:"model"`
	Usage *protocol.Usage `json:"usage"`
}

// replayState carries the accumulators one replay pass threads through
// its entries: the synthetic user-turn counter, the last assistant
// metadata (for the trailing usage frame), and the background-agent ids
// each Task/Agent result announced (agentId → spawning tool_use_id),
// which is what links a subagents/agent-<id>.jsonl file back to the
// card that spawned it.
type replayState struct {
	seq      int
	lastMeta transcriptAssistantMeta
	agentIDs map[string]string
}

func newReplayState() *replayState {
	return &replayState{agentIDs: map[string]string{}}
}

// replayBatch is one transcript entry's frames plus the entry's own
// timestamp, the unit the sidechain merge interleaves by.
type replayBatch struct {
	ts     string
	frames []protocol.L2Frame
}

// buildReplayBatches walks the main-transcript JSONL on r, one batch per
// usable entry, mutating t exactly as live translation would.
func buildReplayBatches(t *Translator, r io.Reader, st *replayState) []replayBatch {
	var batches []replayBatch
	reader := bufio.NewReader(r)
	for {
		line, err := reader.ReadBytes('\n')
		if len(line) > 0 {
			if frames, ts := replayEntryFrames(t, line, st); len(frames) > 0 {
				batches = append(batches, replayBatch{ts: ts, frames: frames})
			}
		}
		if err != nil {
			break
		}
	}
	return batches
}

// flattenWithUsage joins the batches and closes with the §2.8 usage
// frame mirroring the last MAIN assistant message, so the webapp's token
// counter shows the resumed context size.
func flattenWithUsage(batches []replayBatch, st *replayState) []protocol.L2Frame {
	var frames []protocol.L2Frame
	for _, b := range batches {
		frames = append(frames, b.frames...)
	}
	// Synthetic turn-end results land on the FLATTENED, time-merged list,
	// so a turn's boundary is computed over the same order clients see.
	frames = closeReplayTurns(frames)
	// No model assignment here: each assistant entry is translated through
	// t.OnEvent, which adopts the model it reports, so the mirror has
	// already landed on the LAST main-chain entry's model by this point.
	if st.lastMeta.Usage != nil {
		frames = append(frames, &protocol.UsageFrame{
			Envelope:  protocol.Envelope{Type: "usage"},
			MessageID: st.lastMeta.ID,
			Usage:     *st.lastMeta.Usage,
		})
	}
	return frames
}

// BuildReplayFrames translates the transcript JSONL on r into the L2
// frames the live stream would have produced, mutating t exactly as
// live translation would (block ids, tool metadata for render hints,
// model). Inline sidechain entries (legacy CLI transcripts, no reliable
// parent linkage) and meta entries are skipped; malformed lines are
// skipped without aborting the remainder. Current-CLI sidechains live
// in per-agent files, which BuildReplayFramesWithAgents merges in.
func BuildReplayFrames(t *Translator, r io.Reader) []protocol.L2Frame {
	st := newReplayState()
	return flattenWithUsage(buildReplayBatches(t, r, st), st)
}

// BuildReplayFramesWithAgents is BuildReplayFrames plus the session's
// sidechain files: every subagents/agent-<id>.jsonl under subagentsDir
// that can be linked to the Task/Agent call that spawned it replays as
// parented frames, interleaved with the main chain by entry timestamp.
// Files with no discoverable spawner are skipped like malformed lines —
// unparented frames would pollute the main feed instead of nesting.
func BuildReplayFramesWithAgents(t *Translator, r io.Reader, subagentsDir string) []protocol.L2Frame {
	st := newReplayState()
	batches := buildReplayBatches(t, r, st)
	batches = append(batches, sidechainBatches(t, subagentsDir, st)...)
	sort.SliceStable(batches, func(i, j int) bool { return batches[i].ts < batches[j].ts })
	return flattenWithUsage(batches, st)
}

// closeReplayTurns inserts a synthetic §2.4 result frame at each turn
// boundary of the replayed frame list, so a resumed session's history
// carries the same turn-end markers the live stream would have. The
// transcript records no result event (the CLI's result is stream-only;
// see this file's header), so without this a replayed turn's final text
// block is followed by no result — and the webapp keys the green
// final-response border off exactly that text→result adjacency
// (finalResponses in webapp/src/render.ts). The border therefore
// vanished on every binding recreation (§2.10) until the first live turn.
//
// A result closes the PRIOR turn, so one is inserted immediately before
// each user-turn that opens a NEW turn, and once more at end-of-input to
// close the last turn. Only a turn that actually produced an assistant
// response is closed: a bare prompt the agent never answered (a lone
// trailing prompt, or two prompts back to back) is an incomplete turn,
// not a completed one, and gets no result — mirroring the live stream,
// which emits a result only when a turn ends.
func closeReplayTurns(frames []protocol.L2Frame) []protocol.L2Frame {
	out := make([]protocol.L2Frame, 0, len(frames)+2)
	turnOpen := false     // a real user-turn has opened a turn not yet closed
	turnAnswered := false // that open turn has produced assistant content
	lastTS := ""          // TS of the last frame emitted, to date the result
	closeTurn := func() {
		if turnOpen && turnAnswered {
			out = append(out, replayResult(lastTS))
		}
		turnOpen = false
		turnAnswered = false
	}
	for _, f := range frames {
		switch f.(type) {
		case *protocol.UserTurnFrame:
			closeTurn() // close the prior turn before the new one opens
			turnOpen = true
		case *protocol.TaskNotificationFrame:
			// A task notification wakes a promptless turn exactly as a
			// prompt wakes a prompted one — the live stream closes the
			// prior turn with a real result before the woken turn runs —
			// so it is a turn boundary too. Without it, a replayed
			// notification-woken turn (a gns-sockets bridge respawn, a
			// Monitor firing) would merge into the turn above it.
			closeTurn()
			turnOpen = true
		case *protocol.ResultFrame:
			// The only result in the pre-close stream is the aborted one
			// replayUserFrames emits for an interrupt marker, and it IS the
			// interrupted turn's close. Consume the pending close so the
			// end-of-turn logic does not append a second, success result
			// after it (which would double-close the turn).
			turnOpen = false
			turnAnswered = false
		default:
			if isResponseFrame(f) {
				turnAnswered = true
			}
		}
		out = append(out, f)
		if ts := f.Env().TS; ts != "" {
			lastTS = ts
		}
	}
	closeTurn() // close the final turn at end-of-input
	return out
}

// isResponseFrame reports whether a frame is the agent doing something
// within its turn — text, thinking, a tool call, or a tool result — as
// against pure metadata (model-changed) or the user's own turn.
//
// Two callers share the one definition of "the agent answered", so the
// replayed feed and the live one agree on it: closeReplayTurns treats a turn
// carrying at least one of these as answered (the live stream would have
// closed it with a result), and Translator.NoteBroadcast treats the first one
// of a live turn as the point of no return for retracting that turn.
func isResponseFrame(f protocol.L2Frame) bool {
	switch f.(type) {
	case *protocol.TextStartFrame,
		*protocol.ThinkingStartFrame,
		*protocol.ToolUseStartFrame,
		*protocol.ToolUseResultFrame:
		return true
	default:
		return false
	}
}

// replayResult builds the synthetic turn-closing result frame (§2.4).
// It reports subtype "success" — a turn preserved in the transcript with
// a response is one the CLI ran to completion — which is what marks the
// turn's answer a final response the webapp draws its green border
// around.
func replayResult(ts string) protocol.L2Frame {
	return replayResultSubtype(ts, "success")
}

// replayInterruptResult builds the synthetic "aborted" result the webapp
// renders as the yellow interrupt badge (ResultChip in
// webapp/src/render.ts). It stands in for the CLI's synthetic
// `[Request interrupted by user]` user-role marker so an interrupted turn
// is denoted ONE way — the yellow badge — never ALSO as a prompt bubble
// echoing that marker text. The live path already produces only the badge
// (the shim stamps subtype "aborted" and drops the marker's user
// envelope); this restores that single-badge behavior on replay.
func replayInterruptResult(ts string) protocol.L2Frame {
	return replayResultSubtype(ts, "aborted")
}

// replayResultSubtype builds a synthetic turn-closing result of the given
// subtype, pre-stamped with ts so the hub keeps it (§2.1); it carries no
// duration/usage/cost, none of which the transcript records.
func replayResultSubtype(ts, subtype string) protocol.L2Frame {
	return &protocol.ResultFrame{
		Envelope: protocol.Envelope{Type: "result", TS: ts},
		Subtype:  subtype,
	}
}

// replayEntryFrames translates one transcript line; unusable lines
// yield no frames. The returned ts is the entry's own timestamp.
func replayEntryFrames(t *Translator, line []byte, st *replayState) ([]protocol.L2Frame, string) {
	var entry transcriptEntry
	if err := json.Unmarshal(line, &entry); err != nil {
		return nil, ""
	}
	if entry.IsSidechain || entry.IsMeta {
		return nil, ""
	}
	switch entry.Type {
	case "assistant":
		var meta transcriptAssistantMeta
		if err := json.Unmarshal(entry.Message, &meta); err == nil {
			st.lastMeta = meta
		}
		frames := t.OnEvent(&protocol.L1Event{Type: "assistant-message", Message: entry.Message, Error: entry.Error})
		return stampReplay(frames, entry.Timestamp), entry.Timestamp
	case "user":
		return replayUserFrames(t, entry.Message, entry.Timestamp, st), entry.Timestamp
	default:
		return nil, ""
	}
}

// stampReplay pre-stamps translated frames with the transcript entry's own
// time (§2.1), which the hub then preserves instead of stamping the (much
// later) replay time. Without it a resumed session's response bubbles all
// read as having been written the moment the webapp attached.
func stampReplay(frames []protocol.L2Frame, ts string) []protocol.L2Frame {
	for _, f := range frames {
		f.Env().TS = ts
	}
	return frames
}

// The CLI does not store a slash command as the user typed it. It
// rewrites the turn into a tagged envelope before writing the
// transcript line (tag order varies by CLI version):
//
//	<command-name>/model</command-name>
//	<command-message>model</command-message>
//	<command-args>fable</command-args>
//
// The live stream never carries that shape: a §2.3 user-turn is echoed
// from the daemon's own user-message command, so it holds the literal
// "/model fable" the user typed. Collapsing the envelope back to that
// typed form is what keeps a replayed command rendering the way it
// rendered live (raw tags in the bubble otherwise, and no /clear
// context divider).
var (
	commandNameRe = regexp.MustCompile(`(?s)<command-name>(.*?)</command-name>`)
	commandArgsRe = regexp.MustCompile(`(?s)<command-args>(.*?)</command-args>`)
)

// slashCommandText collapses a transcript slash-command envelope to the
// text the user typed. Any other text passes through verbatim: the
// leading tag is what marks an envelope, so a prompt that merely quotes
// the tags mid-sentence is left alone.
func slashCommandText(s string) string {
	if !strings.HasPrefix(strings.TrimSpace(s), "<command-") {
		return s
	}
	name := commandNameRe.FindStringSubmatch(s)
	if name == nil {
		return s
	}
	typed := strings.TrimSpace(name[1])
	if args := commandArgsRe.FindStringSubmatch(s); args != nil {
		if a := strings.TrimSpace(args[1]); a != "" {
			return typed + " " + a
		}
	}
	return typed
}

// A backgrounded Task/Agent result announces the sidechain file its
// output lands in: "Async agent launched successfully. agentId: <hex>".
var agentIDRe = regexp.MustCompile(`agentId:\s*([0-9a-fA-F]+)`)

// replayUserFrames maps one user transcript entry to frames: tool_result
// blocks become tool-use-result frames (through the Translator, so
// render hints fire for tools it has seen), and the remaining content
// becomes one §2.3 user-turn frame. Two synthetic markers the CLI writes
// as user text are diverted rather than rendered as prompts: a
// task-notification becomes a §2.6 notification frame, and an interrupt
// marker becomes the yellow "aborted" result badge (isInterruptText).
// Subagent-spawning results also bank their announced agentId so the
// sidechain merge can find its parent.
func replayUserFrames(t *Translator, message json.RawMessage, ts string, st *replayState) []protocol.L2Frame {
	var msg transcriptUserMessage
	if err := json.Unmarshal(message, &msg); err != nil {
		return nil
	}
	var text string
	if err := json.Unmarshal(msg.Content, &text); err == nil {
		if isTaskNotificationText(text) {
			return replayTaskNotification(t, text, ts)
		}
		if isInterruptText(text) {
			return []protocol.L2Frame{replayInterruptResult(ts)}
		}
		text = slashCommandText(text)
		if text == "" {
			return nil
		}
		norm, _ := json.Marshal([]map[string]string{{"type": "text", "text": text}})
		return []protocol.L2Frame{replayUserTurn(norm, ts, st)}
	}
	var rawBlocks []json.RawMessage
	if err := json.Unmarshal(msg.Content, &rawBlocks); err != nil {
		return nil
	}
	var frames []protocol.L2Frame
	var turnBlocks []json.RawMessage
	for _, raw := range rawBlocks {
		var block transcriptUserBlock
		if err := json.Unmarshal(raw, &block); err != nil {
			continue
		}
		if block.Type == "tool_result" {
			if m := agentIDRe.FindStringSubmatch(contentText(block.Content)); m != nil {
				st.agentIDs[m[1]] = block.ToolUseID
			}
			frames = append(frames, replayToolResult(t, block, ts, "")...)
			continue
		}
		if block.Type == "text" {
			var tb struct {
				Text string `json:"text"`
			}
			if err := json.Unmarshal(raw, &tb); err == nil {
				if isTaskNotificationText(tb.Text) {
					frames = append(frames, replayTaskNotification(t, tb.Text, ts)...)
					continue
				}
				if isInterruptText(tb.Text) {
					frames = append(frames, replayInterruptResult(ts))
					continue
				}
			}
		}
		turnBlocks = append(turnBlocks, raw)
	}
	if len(turnBlocks) > 0 {
		content, _ := json.Marshal(turnBlocks)
		frames = append(frames, replayUserTurn(content, ts, st))
	}
	return frames
}

// isTaskNotificationText mirrors the shim's live detection (session.ts
// mapUserMessage): a user text carrying a <task-notification> payload is
// the harness's background-work completion signal, not the user's words.
func isTaskNotificationText(text string) bool {
	return strings.Contains(text, "<task-notification>")
}

// isInterruptText reports whether a user text is the CLI's synthetic
// interrupt marker — the user-role entry it writes when a turn is
// interrupted (`[Request interrupted by user]`, and the tool-use variant
// `[Request interrupted by user for tool use]`). It is not the user's
// words: the interrupt is already denoted by the yellow "aborted" result
// badge, so this marker must never ALSO surface as a prompt bubble.
func isInterruptText(text string) bool {
	return strings.HasPrefix(strings.TrimSpace(text), "[Request interrupted by user")
}

// replayTaskNotification maps one replayed task-notification text to the
// same §2.6 task-notification frame the live shim event produces, routed
// through the Translator so the correlation tags parse identically. The
// live stream never renders these as user turns — the shim drops the
// user-role envelope — so a replayed user-turn bubble full of raw
// notification XML was a replay/live divergence, and it also miscast the
// promptless turn the notification wakes as a user-prompted one (which
// the webapp's gns-sockets fold keys off; see webapp/src/gns.ts).
func replayTaskNotification(t *Translator, text, ts string) []protocol.L2Frame {
	return stampReplay(taskNotificationFrames(t, text), ts)
}

// taskNotificationFrames maps a raw <task-notification> payload through
// the translator exactly as the live shim event does — the ONE mapping
// the live path, the replay path, and the transcript watch
// (notifwatch.go) all share, so the three can never diverge on how a
// completion parses.
func taskNotificationFrames(t *Translator, text string) []protocol.L2Frame {
	data, err := json.Marshal(map[string]string{"text": text})
	if err != nil {
		return nil
	}
	return t.OnEvent(&protocol.L1Event{
		Type:    "system",
		Subtype: "task_notification",
		Data:    data,
	})
}

// replayToolResult translates one transcript tool_result block through
// the Translator (so render hints fire for tools it has seen), stamped
// with the entry's own time. PARENT is the owning subagent call, empty
// on the main chain — the one decoding both the main and sidechain
// walks share.
func replayToolResult(t *Translator, block transcriptUserBlock, ts, parent string) []protocol.L2Frame {
	return stampReplay(t.OnEvent(&protocol.L1Event{
		Type:            "tool-result",
		ToolUseID:       block.ToolUseID,
		IsError:         block.IsError,
		Content:         block.Content,
		ParentToolUseID: parent,
	}), ts)
}

// sidechainBatches replays every linkable subagents/agent-<id>.jsonl
// under dir. A file links to its spawner via the announced agentId
// (background agents) or by its first user prompt matching a replayed
// Task/Agent input's prompt (foreground agents, claimed at most once);
// files with neither are skipped.
func sidechainBatches(t *Translator, dir string, st *replayState) []replayBatch {
	des, err := os.ReadDir(dir)
	if err != nil {
		return nil
	}
	var batches []replayBatch
	claimed := map[string]bool{}
	for _, id := range st.agentIDs {
		claimed[id] = true
	}
	for _, de := range des {
		id, ok := sidechainFileID(de.Name())
		if !ok {
			continue
		}
		data, err := os.ReadFile(filepath.Join(dir, de.Name()))
		if err != nil {
			continue
		}
		entryLines := transcriptLines(data)
		parent := st.agentIDs[id]
		if parent == "" {
			parent = promptMatchParent(t, entryLines, claimed)
		}
		if parent == "" {
			continue
		}
		claimed[parent] = true
		for _, line := range entryLines {
			if frames, ts := sidechainEntryFrames(t, line, parent); len(frames) > 0 {
				batches = append(batches, replayBatch{ts: ts, frames: frames})
			}
		}
	}
	return batches
}

// sidechainFileID extracts <id> from an agent-<id>.jsonl filename.
func sidechainFileID(name string) (string, bool) {
	id, ok := strings.CutPrefix(name, "agent-")
	if !ok {
		return "", false
	}
	id, ok = strings.CutSuffix(id, ".jsonl")
	if !ok || id == "" {
		return "", false
	}
	return id, true
}

// transcriptLines splits a whole JSONL file into its non-blank lines.
func transcriptLines(data []byte) [][]byte {
	var lines [][]byte
	for _, l := range splitLines(string(data)) {
		if strings.TrimSpace(l) != "" {
			lines = append(lines, []byte(l))
		}
	}
	return lines
}

// promptMatchParent links a sidechain file to the replayed Task/Agent
// call whose input prompt equals the file's first user text — the only
// linkage a FOREGROUND agent leaves behind (background agents announce
// an agentId instead). Each spawning call is claimed at most once, so
// parallel agents with identical prompts each land on their own card
// (which one is arbitrary, and interchangeable by construction).
func promptMatchParent(t *Translator, entryLines [][]byte, claimed map[string]bool) string {
	prompt := firstUserText(entryLines)
	if prompt == "" {
		return ""
	}
	for id, meta := range t.tools {
		if claimed[id] || (meta.name != "Task" && meta.name != "Agent") {
			continue
		}
		var in struct {
			Prompt string `json:"prompt"`
		}
		if err := json.Unmarshal(meta.input, &in); err != nil {
			continue
		}
		if in.Prompt != "" && in.Prompt == prompt {
			return id
		}
	}
	return ""
}

// firstUserText returns the first user entry's text content: the prompt
// the spawning call injected into the sidechain.
func firstUserText(entryLines [][]byte) string {
	for _, line := range entryLines {
		var entry transcriptEntry
		if err := json.Unmarshal(line, &entry); err != nil || entry.Type != "user" {
			continue
		}
		var msg transcriptUserMessage
		if err := json.Unmarshal(entry.Message, &msg); err != nil {
			return ""
		}
		var text string
		if err := json.Unmarshal(msg.Content, &text); err == nil {
			return text
		}
		var blocks []transcriptUserBlock
		if err := json.Unmarshal(msg.Content, &blocks); err == nil {
			for _, b := range blocks {
				if b.Type == "text" {
					var s string
					if err := json.Unmarshal(b.Content, &s); err == nil {
						return s
					}
				}
			}
		}
		return ""
	}
	return ""
}

// sidechainEntryFrames translates one sidechain transcript line under
// its spawning call. Assistant entries replay in full (their blocks all
// carry the parent); user entries contribute ONLY tool_result blocks —
// the injected prompt is the spawning call's own input, never a user
// bubble in the feed.
func sidechainEntryFrames(t *Translator, line []byte, parent string) ([]protocol.L2Frame, string) {
	var entry transcriptEntry
	if err := json.Unmarshal(line, &entry); err != nil {
		return nil, ""
	}
	if entry.IsMeta {
		return nil, ""
	}
	switch entry.Type {
	case "assistant":
		frames := t.OnEvent(&protocol.L1Event{
			Type:            "assistant-message",
			Message:         entry.Message,
			ParentToolUseID: parent,
		})
		return stampReplay(frames, entry.Timestamp), entry.Timestamp
	case "user":
		var msg transcriptUserMessage
		if err := json.Unmarshal(entry.Message, &msg); err != nil {
			return nil, ""
		}
		var rawBlocks []json.RawMessage
		if err := json.Unmarshal(msg.Content, &rawBlocks); err != nil {
			return nil, ""
		}
		var frames []protocol.L2Frame
		for _, raw := range rawBlocks {
			var block transcriptUserBlock
			if err := json.Unmarshal(raw, &block); err != nil || block.Type != "tool_result" {
				continue
			}
			frames = append(frames, replayToolResult(t, block, entry.Timestamp, parent)...)
		}
		return frames, entry.Timestamp
	default:
		return nil, ""
	}
}

// replayUserTurn builds the §2.3 user-turn frame for replayed content.
// The request id is synthetic: replayed turns never had a daemon-issued
// user-message command. The envelope is pre-stamped with the transcript
// entry's own timestamp so the hub keeps it (§2.1) instead of stamping
// resume time; an entry without one falls through to the hub's stamp.
func replayUserTurn(content json.RawMessage, ts string, st *replayState) protocol.L2Frame {
	st.seq++
	return &protocol.UserTurnFrame{
		Envelope:  protocol.Envelope{Type: "user-turn", TS: ts},
		RequestID: replayRequestID(st.seq),
		Content:   content,
	}
}

func replayRequestID(n int) string {
	return "replay-" + strconv.Itoa(n)
}

// SeedFromTranscript stamps the resumed identity onto the session and
// retains the transcript's replay frames so clients attaching later see
// the prior conversation (§2.10). Call before Run so live frames land
// after the seed. The claudeSessionID stamp happens even when the
// transcript cannot be read — the resume target is authoritative
// regardless — and the open/read failure is returned for the caller to
// surface (the session itself stays fully usable, only history
// rendering is degraded).
func (s *Session) SeedFromTranscript(path, claudeSessionID string) error {
	f, err := os.Open(path)
	s.mu.Lock()
	defer s.mu.Unlock()
	if claudeSessionID != "" {
		s.translator.ClaudeSessionID = claudeSessionID
		// The resume target is a durable id in its own right; the
		// registry must hold it even if system:init later reports a
		// successor uuid (which re-notifies).
		s.notifyRegistrarLocked()
	}
	if err != nil {
		return err
	}
	defer func() {
		if cerr := f.Close(); cerr != nil && !errors.Is(cerr, os.ErrClosed) {
			s.logf("session %s: close transcript %s: %v", s.ID, path, cerr)
		}
	}()
	s.broadcastLocked(BuildReplayFramesWithAgents(s.translator, f, subagentsDirFor(path)))
	// The replay adopts the transcript's last main-chain model into the
	// mirror, so write it through here: a resumed session whose record
	// predates the write-through (or drifted before the restart) is
	// corrected to the model it is actually resuming on.
	s.notifyRegistrarLocked()
	return nil
}

// subagentsDirFor returns the per-session sidechain directory the CLI
// writes beside a transcript: <uuid>/subagents under the project dir,
// one agent-<id>.jsonl per spawned subagent.
func subagentsDirFor(path string) string {
	return filepath.Join(strings.TrimSuffix(path, ".jsonl"), "subagents")
}
