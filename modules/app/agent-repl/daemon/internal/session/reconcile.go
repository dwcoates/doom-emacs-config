// Model drift reconciliation (§2.12).
//
// The daemon's model is a MIRROR of a value the CLI owns, and every
// mirror can drift. The event path — system:init, an acked set-model, the
// model named on each main-chain assistant message — is what normally
// keeps it true, but each of those is a PUSH: a dropped, malformed, or
// never-sent event leaves the mirror confidently wrong, and a wrong model
// in the topbar is the kind of wrong nobody notices until it has cost
// them a turn.
//
// So the mirror is also PULLED, on a timer, from a source that owes
// nothing to the event stream: the CLI's own transcript JSONL. That is
// the same file the Emacs mode-line has always trusted, which is exactly
// why reconciling against it means the two frontends cannot disagree.
//
// Silence is the steady state. The check broadcasts only on genuine
// drift, so a healthy session pays one tail read per tick and puts no
// frame on the wire. A `reconcile`-origin frame is therefore evidence
// that the push path MISSED something, and it is visible precisely so it
// can be noticed — this is a self-healing loop, not a fallback that
// papers over a broken event path.

package session

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"time"

	"claude-repld/internal/protocol"
)

// DefaultModelReconcileInterval is how often a live session re-derives
// its model from the transcript.
const DefaultModelReconcileInterval = 30 * time.Second

// transcriptTailBytes is how much of the transcript's tail the model
// check reads. Transcripts grow without bound and the answer is always at
// the end, so this is a tail read, never a full scan. Mirrors the Emacs
// mode-line's own `agent-repl-model-scan-bytes`.
const transcriptTailBytes = 32768

// LastTranscriptModel returns the model of the last MAIN-CHAIN assistant
// message in the transcript at PATH, or "" when the tail holds none.
//
// Main-chain only: a sidechain entry is a subagent's message and names
// the SUBAGENT's model (a Haiku Explore under an Opus session), so
// trusting it would report the subagent's model as the session's.
//
// Malformed lines are skipped rather than fatal — a transcript is append
// -only and its final line can be a half-flushed write.
// Named returns are load-bearing: they are what lets the deferred Close
// actually SURFACE a close failure instead of dropping it on the floor.
func LastTranscriptModel(path string) (model string, err error) {
	f, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer func() {
		cerr := f.Close()
		if cerr == nil || errors.Is(cerr, os.ErrClosed) {
			return
		}
		// The read already produced an answer, so the close failure cannot
		// change it — but it is reported rather than swallowed, and it only
		// overwrites a nil err so a real read failure keeps precedence.
		if err == nil {
			err = fmt.Errorf("close transcript %s: %w", path, cerr)
		}
	}()

	info, err := f.Stat()
	if err != nil {
		return "", fmt.Errorf("stat transcript %s: %w", path, err)
	}
	offset := max(info.Size()-transcriptTailBytes, 0)
	if _, err := f.Seek(offset, io.SeekStart); err != nil {
		return "", fmt.Errorf("seek transcript %s: %w", path, err)
	}
	tail, err := io.ReadAll(f)
	if err != nil {
		return "", fmt.Errorf("read transcript %s: %w", path, err)
	}
	// A non-zero offset almost certainly lands mid-line; that first
	// fragment is not a JSON object and must not be parsed as one.
	if offset > 0 {
		if nl := bytes.IndexByte(tail, '\n'); nl >= 0 {
			tail = tail[nl+1:]
		} else {
			tail = nil
		}
	}
	return lastAssistantModel(tail), nil
}

// lastAssistantModel scans NDJSON transcript lines and returns the model
// of the last main-chain assistant entry.
func lastAssistantModel(tail []byte) string {
	model := ""
	scanner := bufio.NewScanner(bytes.NewReader(tail))
	scanner.Buffer(make([]byte, 0, 64*1024), transcriptTailBytes)
	for scanner.Scan() {
		var entry transcriptEntry
		if err := json.Unmarshal(scanner.Bytes(), &entry); err != nil {
			continue
		}
		if entry.Type != "assistant" || entry.IsSidechain || entry.IsMeta {
			continue
		}
		var meta transcriptAssistantMeta
		if err := json.Unmarshal(entry.Message, &meta); err != nil {
			continue
		}
		if meta.Model != "" {
			model = meta.Model
		}
	}
	return model
}

// ReconcileModel re-derives the session's model from its transcript and
// broadcasts a model-changed frame when the mirror has drifted. A no-op
// when the mirror already agrees, so the steady state is silent.
//
// A session whose claude_session_id has not arrived yet (no system:init)
// has no transcript path to read, and is SKIPPED rather than guessed at.
func (s *Session) ReconcileModel() {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.terminal || s.translator.ClaudeSessionID == "" {
		return
	}
	path := TranscriptPath(
		ClaudeConfigDir(s.configDir),
		s.translator.CWD,
		s.translator.ClaudeSessionID,
	)
	model, err := LastTranscriptModel(path)
	if err != nil {
		// Never swallowed: a transcript the daemon cannot read is a
		// reconciler that cannot do its job, and the session is running
		// without its safety net until this is fixed.
		s.logf("session %s: model reconcile could not read %s: %v", s.ID, path, err)
		return
	}
	was := s.translator.Model
	frame := s.translator.SetModel(model, "reconcile")
	if frame == nil {
		return
	}
	s.logf("session %s: model drift — mirror said %q, transcript says %q; adopting the transcript",
		s.ID, was, model)
	s.broadcastLocked([]protocol.L2Frame{frame})
}

// runModelReconciler drives ReconcileModel until the session ends. TICKS
// is the caller's clock; a nil channel means the session mints its own at
// INTERVAL. A non-positive INTERVAL with no TICKS disables the check
// entirely (fake sessions, which have no transcript to reconcile against).
// runDone bounds the reconciler to ONE shim's run, not to the session's
// whole life: a hibernated session has no CLI to reconcile a model
// against, and its Run has already returned. Reviving starts a fresh
// reconciler with a fresh runDone, so the two never coexist.
func (s *Session) runModelReconciler(runDone <-chan struct{}, ticks <-chan time.Time, interval time.Duration) {
	if ticks == nil {
		if interval <= 0 {
			return
		}
		ticker := time.NewTicker(interval)
		defer ticker.Stop()
		ticks = ticker.C
	}
	for {
		select {
		case <-runDone:
			return
		case _, ok := <-ticks:
			if !ok {
				return
			}
			s.ReconcileModel()
		}
	}
}
