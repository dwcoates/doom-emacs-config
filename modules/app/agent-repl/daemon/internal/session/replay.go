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
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
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

// BuildReplayFrames translates the transcript JSONL on r into the L2
// frames the live stream would have produced, mutating t exactly as
// live translation would (block ids, tool metadata for render hints,
// model). Sidechain (subagent) and meta entries are skipped; malformed
// lines are skipped without aborting the remainder. The trailing usage
// frame mirrors the last assistant message's usage so the webapp's
// token counter shows the resumed context size.
func BuildReplayFrames(t *Translator, r io.Reader) []protocol.L2Frame {
	var frames []protocol.L2Frame
	var lastMeta transcriptAssistantMeta
	reader := bufio.NewReader(r)
	replaySeq := 0
	for {
		line, err := reader.ReadBytes('\n')
		if len(line) > 0 {
			frames = append(frames, replayEntryFrames(t, line, &replaySeq, &lastMeta)...)
		}
		if err != nil {
			break
		}
	}
	// No model assignment here: each assistant entry is translated through
	// t.OnEvent, which adopts the model it reports, so the mirror has
	// already landed on the LAST main-chain entry's model by this point.
	if lastMeta.Usage != nil {
		frames = append(frames, &protocol.UsageFrame{
			Envelope:  protocol.Envelope{Type: "usage"},
			MessageID: lastMeta.ID,
			Usage:     *lastMeta.Usage,
		})
	}
	return frames
}

// replayEntryFrames translates one transcript line; unusable lines
// yield no frames.
func replayEntryFrames(t *Translator, line []byte, replaySeq *int, lastMeta *transcriptAssistantMeta) []protocol.L2Frame {
	var entry transcriptEntry
	if err := json.Unmarshal(line, &entry); err != nil {
		return nil
	}
	if entry.IsSidechain || entry.IsMeta {
		return nil
	}
	switch entry.Type {
	case "assistant":
		var meta transcriptAssistantMeta
		if err := json.Unmarshal(entry.Message, &meta); err == nil {
			*lastMeta = meta
		}
		frames := t.OnEvent(&protocol.L1Event{Type: "assistant-message", Message: entry.Message})
		return stampReplay(frames, entry.Timestamp)
	case "user":
		return replayUserFrames(t, entry.Message, entry.Timestamp, replaySeq)
	default:
		return nil
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

// replayUserFrames maps one user transcript entry to frames: tool_result
// blocks become tool-use-result frames (through the Translator, so
// render hints fire for tools it has seen), and the remaining content
// becomes one §2.3 user-turn frame.
func replayUserFrames(t *Translator, message json.RawMessage, ts string, replaySeq *int) []protocol.L2Frame {
	var msg transcriptUserMessage
	if err := json.Unmarshal(message, &msg); err != nil {
		return nil
	}
	var text string
	if err := json.Unmarshal(msg.Content, &text); err == nil {
		text = slashCommandText(text)
		if text == "" {
			return nil
		}
		norm, _ := json.Marshal([]map[string]string{{"type": "text", "text": text}})
		return []protocol.L2Frame{replayUserTurn(norm, ts, replaySeq)}
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
			frames = append(frames, stampReplay(t.OnEvent(&protocol.L1Event{
				Type:      "tool-result",
				ToolUseID: block.ToolUseID,
				IsError:   block.IsError,
				Content:   block.Content,
			}), ts)...)
			continue
		}
		turnBlocks = append(turnBlocks, raw)
	}
	if len(turnBlocks) > 0 {
		content, _ := json.Marshal(turnBlocks)
		frames = append(frames, replayUserTurn(content, ts, replaySeq))
	}
	return frames
}

// replayUserTurn builds the §2.3 user-turn frame for replayed content.
// The request id is synthetic: replayed turns never had a daemon-issued
// user-message command. The envelope is pre-stamped with the transcript
// entry's own timestamp so the hub keeps it (§2.1) instead of stamping
// resume time; an entry without one falls through to the hub's stamp.
func replayUserTurn(content json.RawMessage, ts string, replaySeq *int) protocol.L2Frame {
	*replaySeq++
	return &protocol.UserTurnFrame{
		Envelope:  protocol.Envelope{Type: "user-turn", TS: ts},
		RequestID: replayRequestID(*replaySeq),
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
	s.broadcastLocked(BuildReplayFrames(s.translator, f))
	// The replay adopts the transcript's last main-chain model into the
	// mirror, so write it through here: a resumed session whose record
	// predates the write-through (or drifted before the restart) is
	// corrected to the model it is actually resuming on.
	s.notifyRegistrarLocked()
	return nil
}

// NoteResumeUnavailable retains a recoverable in-band error frame
// telling every attaching client that the requested resume target could
// not be honored and the session started as a FRESH conversation. Used
// by the create path when it drops a --resume whose transcript does not
// exist in this daemon's config dir (spawning anyway would hard-kill
// the CLI at startup); the drop must be visible in the webapp, not just
// the daemon log.
func (s *Session) NoteResumeUnavailable(resumeID, path string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.broadcastLocked([]protocol.L2Frame{&protocol.ErrorFrame{
		Envelope: protocol.Envelope{Type: "error"},
		Code:     "resume_unavailable",
		Message: fmt.Sprintf(
			"resume target %s has no transcript at %s — started a fresh conversation instead",
			resumeID, path),
		Recoverable: true,
	}})
}
