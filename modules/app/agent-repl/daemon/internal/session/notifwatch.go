// Transcript notification watch (§2.6 settling, delivery-independent).
//
// A completion notification that WAKES an idle session does not reliably
// traverse the SDK stream: the CLI writes the wake straight into the
// session transcript as a user entry (origin kind "task-notification") and
// streams only the assistant turn it starts — sometimes nothing at all —
// so the shim's user-message scan, its only live detection, never fires
// and no badge ever settles. Mid-turn completions ride streamed user
// messages and settle fine, which is why short-lived shells LOOKED
// reliable while background agents (which outlive their turn) never
// resolved.
//
// The transcript is the one channel that provably always carries the
// wake, so the daemon tails its own session transcript and feeds each
// newly appended notification entry through the SAME translator path a
// live shim event takes — identical frame, identical task settle,
// identical client write, for every async type alike (the payload is
// task-id-keyed and nothing downstream branches on kind). An entry whose
// task the daemon already settled is skipped, so the working mid-turn
// delivery never produces duplicate frames.
package session

import (
	"encoding/json"
	"io"
	"os"
	"strings"
	"time"
)

const (
	// DefaultNotifWatchInterval is the transcript poll cadence. A tick on
	// a quiet transcript is one stat call, so the watch is cheap to keep
	// always-on rather than armed per task.
	DefaultNotifWatchInterval = 1500 * time.Millisecond
	// notifReadMax caps one tick's read; a bigger backlog carries over.
	notifReadMax = 4 << 20
	// notifCarryMax caps the partial-line carry. A notification entry is
	// tiny, so a carry this large is some other entry's mega-line; it is
	// dropped and the scan resyncs at the next newline (an unparseable
	// fragment can never emit a frame, so a false settle is impossible).
	notifCarryMax = 1 << 20
	// notifMarker is the payload signature a wake entry always contains.
	notifMarker = "<task-notification>"
)

// notifEntry is the slice of a transcript line the watch reads: enough to
// recognize a user-role notification entry and dedupe it.
type notifEntry struct {
	Type    string `json:"type"`
	UUID    string `json:"uuid"`
	Message struct {
		Content json.RawMessage `json:"content"`
	} `json:"message"`
}

// notifEntryText extracts ENTRY's text: a plain string content (the shape
// wake entries carry) or the concatenated text blocks of an array content.
func notifEntryText(content json.RawMessage) string {
	var s string
	if json.Unmarshal(content, &s) == nil {
		return s
	}
	var blocks []struct {
		Type string `json:"type"`
		Text string `json:"text"`
	}
	if json.Unmarshal(content, &blocks) != nil {
		return ""
	}
	var parts []string
	for _, b := range blocks {
		if b.Type == "text" && b.Text != "" {
			parts = append(parts, b.Text)
		}
	}
	return strings.Join(parts, "\n")
}

// runNotifWatch drives notifWatchTick until the session's Run returns.
// TICKS is the test seam; absent one, INTERVAL paces a real ticker and a
// non-positive interval disables the watch (mirroring runModelReconciler).
func (s *Session) runNotifWatch(runDone <-chan struct{}, ticks <-chan time.Time, interval time.Duration) {
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
			s.notifWatchTick()
		}
	}
}

// notifWatchTick reads the transcript's growth since the last tick and
// emits a task-notification frame batch for each newly appended wake
// entry the live stream failed to deliver.
//
// The watch starts at the transcript's TAIL: history is the replay path's
// business (replayTaskNotification already maps it on every attach), so
// re-emitting it here would double frames for nothing. A changed
// transcript path (a /clear or resume minted a new claude session id)
// re-tails the new file for the same reason.
func (s *Session) notifWatchTick() {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.terminal || s.translator.ClaudeSessionID == "" {
		return
	}
	path := TranscriptPath(ClaudeConfigDir(s.configDir), s.translator.CWD, s.translator.ClaudeSessionID)
	if path != s.notifPath {
		s.notifPath = path
		s.notifCarry = ""
		if fi, err := os.Stat(path); err == nil {
			s.notifOffset = fi.Size()
		} else {
			// Not written yet: the first appended byte is line one.
			s.notifOffset = 0
		}
		return
	}
	f, err := os.Open(path)
	if err != nil {
		return
	}
	defer f.Close()
	fi, err := f.Stat()
	if err != nil || fi.Size() <= s.notifOffset {
		return
	}
	if _, err := f.Seek(s.notifOffset, io.SeekStart); err != nil {
		return
	}
	buf, err := io.ReadAll(io.LimitReader(f, notifReadMax))
	if len(buf) == 0 || (err != nil && err != io.EOF) {
		return
	}
	s.notifOffset += int64(len(buf))
	lines := strings.Split(s.notifCarry+string(buf), "\n")
	s.notifCarry = lines[len(lines)-1]
	if len(s.notifCarry) > notifCarryMax {
		s.notifCarry = ""
	}
	for _, line := range lines[:len(lines)-1] {
		s.emitTranscriptNotificationLocked(line)
	}
}

// emitTranscriptNotificationLocked recognizes one transcript line as a
// wake notification entry and broadcasts its frame batch, exactly as the
// live shim event path would have. Callers hold s.mu.
func (s *Session) emitTranscriptNotificationLocked(line string) {
	// The raw-line prefilter is bracket-less on purpose: a JSON encoder may
	// write `<` literally (the CLI's JSON.stringify) or as < (Go's
	// html-escaping default), and the cheap gate must pass both. The
	// DECODED text below re-checks the full marker authoritatively.
	if !strings.Contains(line, "task-notification") {
		return
	}
	var entry notifEntry
	if json.Unmarshal([]byte(line), &entry) != nil || entry.Type != "user" {
		return
	}
	text := notifEntryText(entry.Message.Content)
	if !strings.Contains(text, notifMarker) {
		return
	}
	if entry.UUID != "" {
		if _, seen := s.notifSeen[entry.UUID]; seen {
			return
		}
		s.notifSeen[entry.UUID] = struct{}{}
	}
	// A notification the live stream DID deliver has already settled its
	// task; re-broadcasting it would only double frames. An unknown task
	// id still emits — a completion is never swallowed.
	if rec, ok := s.taskPaths[tagValue(text, "task-id")]; ok && rec.done {
		return
	}
	frames := taskNotificationFrames(s.translator, text)
	s.broadcastLocked(frames)
	s.superviseTailersLocked(frames)
}
