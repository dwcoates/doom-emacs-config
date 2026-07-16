package server

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/session"
)

// postCreateKeepingShim POSTs body to /sessions and returns the new
// session id WITHOUT consuming the spawned shim, leaving it on h.shims for
// the caller to take via awaitShim. The package's own postCreate swallows
// the shim; these tests must hold the handle, because a supersede is
// proven by the command forwarded to the OLDER session's shim.
func postCreateKeepingShim(t *testing.T, h *harness, body string) string {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json", bytes.NewBufferString(body))
	if err != nil {
		t.Fatalf("POST /sessions: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusCreated {
		t.Fatalf("POST /sessions status = %d, want 201", resp.StatusCode)
	}
	var out struct {
		SessionID string `json:"session_id"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		t.Fatalf("decode create response: %v", err)
	}
	return out.SessionID
}

// createResumed POSTs a create that resumes claudeSessionID, returning the
// new session's id. cfg is the CLAUDE_CONFIG_DIR the transcript lives
// under; cwd must be "/w" to match writeTranscript's fixed project slug.
func createResumed(t *testing.T, h *harness, cfg, cwd, resume string) string {
	t.Helper()
	return postCreateKeepingShim(t, h, fmt.Sprintf(`{"cwd":%q,"config_dir":%q,"resume":%q}`, cwd, cfg, resume))
}

// createFresh POSTs a create for a NEW conversation in cfg/cwd — the same
// workspace and account createResumed uses, but resuming nothing, so it
// carries no claude_session_id.
func createFresh(t *testing.T, h *harness, cfg, cwd string) string {
	t.Helper()
	return postCreateKeepingShim(t, h, fmt.Sprintf(`{"cwd":%q,"config_dir":%q}`, cwd, cfg))
}

// recvSupersede asserts shim was told to shut down for the supersede
// reason. A live session goes terminal only once its real CLI exits, so
// the forwarded command is what proves the supersede landed.
func recvSupersede(t *testing.T, shim *fakeShim) {
	t.Helper()
	select {
	case line := <-shim.sent:
		if !strings.Contains(string(line), "shutdown") || !strings.Contains(string(line), supersedeReason) {
			t.Fatalf("forwarded = %s, want a shutdown carrying %q", line, supersedeReason)
		}
	case <-time.After(recvTimeout):
		t.Fatal("the older holder was never told to shut down")
	}
}

// awaitTerminal waits for id to go terminal, returning its death reason.
func awaitTerminal(t *testing.T, h *harness, id string) string {
	t.Helper()
	deadline := time.Now().Add(recvTimeout)
	for time.Now().Before(deadline) {
		if sess := h.srv.lookup(id); sess != nil {
			if info := sess.Info(); info.Terminal {
				return info.DeathReason
			}
		}
		time.Sleep(time.Millisecond)
	}
	t.Fatalf("session %s never went terminal", id)
	return ""
}

// transcriptHarness boots a recorded-spawn server with a transcript on
// disk for each uuid, and returns the config dir holding them.
func transcriptHarness(t *testing.T, uuids ...string) (*harness, string) {
	t.Helper()
	cfg := t.TempDir()
	for _, uuid := range uuids {
		writeTranscript(t, cfg, uuid)
	}
	h, _, _ := rehydrationHarness(t, false)
	return h, cfg
}

func TestResumeSupersedesAnOlderSessionOnTheSameTranscript(t *testing.T) {
	// Arrange — an older session already live on uuid-1's transcript.
	h, cfg := transcriptHarness(t, "uuid-1")
	createResumed(t, h, cfg, "/w", "uuid-1")
	older := h.awaitShim(t)

	// Act — a newer create resumes the very same transcript.
	newer := createResumed(t, h, cfg, "/w", "uuid-1")
	h.awaitShim(t)

	// Assert — the older CLI was told to stand down, leaving one writer.
	recvSupersede(t, older)
	if sess := h.srv.lookup(newer); sess == nil || sess.Info().Terminal {
		t.Fatalf("the newest resume must survive, got %+v", sess)
	}
}

func TestResumeLeavesASessionOnADifferentTranscriptAlone(t *testing.T) {
	// Arrange — a session on uuid-1, which the new resume does not touch.
	h, cfg := transcriptHarness(t, "uuid-1", "uuid-2")
	createResumed(t, h, cfg, "/w", "uuid-1")
	other := h.awaitShim(t)

	// Act — resume a DIFFERENT conversation.
	createResumed(t, h, cfg, "/w", "uuid-2")
	h.awaitShim(t)

	// Assert — no shared file, so nothing is superseded.
	expectNoForward(t, other)
}

func TestResumeUnderADifferentAccountIsNotAConflict(t *testing.T) {
	// Arrange — the SAME uuid under two config dirs is two distinct files
	// with two distinct writers, which is a real arrangement here: one uuid
	// exists under both ~/.claude and ~/.claude-chesscom on this machine.
	h, personal := transcriptHarness(t, "uuid-1")
	work := t.TempDir()
	writeTranscript(t, work, "uuid-1")
	createResumed(t, h, personal, "/w", "uuid-1")
	other := h.awaitShim(t)

	// Act — same uuid, different account.
	createResumed(t, h, work, "/w", "uuid-1")
	h.awaitShim(t)

	// Assert — different paths, so the personal session keeps its CLI.
	expectNoForward(t, other)
}

func TestCreateWithoutResumeSupersedesNothing(t *testing.T) {
	// Arrange — a live session holding uuid-1.
	h, cfg := transcriptHarness(t, "uuid-1")
	createResumed(t, h, cfg, "/w", "uuid-1")
	held := h.awaitShim(t)

	// Act — a fresh conversation contends for no transcript at all.
	createFresh(t, h, cfg, "/w")
	h.awaitShim(t)

	// Assert
	expectNoForward(t, held)
}

func TestSupersedeSparesAFreshSessionInTheSameWorkspace(t *testing.T) {
	// Arrange — a fresh session in the same workspace and account has
	// adopted no transcript yet, so it is not contending for the one about
	// to be resumed.
	h, cfg := transcriptHarness(t, "uuid-1")
	createFresh(t, h, cfg, "/w")
	fresh := h.awaitShim(t)

	// Act
	createResumed(t, h, cfg, "/w", "uuid-1")
	h.awaitShim(t)

	// Assert
	expectNoForward(t, fresh)
}

func TestSupersedeSkipsAnAlreadyTerminalHolder(t *testing.T) {
	// Arrange — the previous holder is already dead, so the supersede pass
	// must leave it alone rather than overwrite the death reason recording
	// how it actually ended.
	h, cfg := transcriptHarness(t, "uuid-1")
	dead := createResumed(t, h, cfg, "/w", "uuid-1")
	deadShim := h.awaitShim(t)
	deadShim.end() // the CLI process dies; Run observes the stream close
	before := awaitTerminal(t, h, dead)

	// Act
	createResumed(t, h, cfg, "/w", "uuid-1")
	h.awaitShim(t)

	// Assert — its original death reason survives untouched.
	if got := h.srv.lookup(dead).Info().DeathReason; got != before {
		t.Fatalf("death reason = %q, want the original %q", got, before)
	}
}

func TestTranscriptOwnerDistinguishesAccountsForOneUUID(t *testing.T) {
	// Arrange / Act — the same uuid and cwd under two config dirs.
	personal := transcriptOwner("/home/u/.claude", "/w", "uuid-1")
	work := transcriptOwner("/home/u/.claude-chesscom", "/w", "uuid-1")

	// Assert — identity is the resolved file, never the uuid alone.
	if personal == work {
		t.Fatalf("two accounts collapsed to one owner: %s", personal)
	}
}

func TestTranscriptOwnerMatchesTheReplayTranscriptPath(t *testing.T) {
	// Arrange / Act — the owner key must name the file replay actually
	// reads, or supersede would be comparing a key nothing else uses.
	got := transcriptOwner("/cfg", "/w", "uuid-1")
	want := session.TranscriptPath(session.ClaudeConfigDir("/cfg"), "/w", "uuid-1")

	// Assert
	if got != want {
		t.Fatalf("owner = %s, want %s", got, want)
	}
}
