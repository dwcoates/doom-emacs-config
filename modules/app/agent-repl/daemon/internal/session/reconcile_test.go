package session

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"
)

// writeTranscript lays a transcript JSONL down where TranscriptPath will
// look for it, and returns the config dir rooting it.
func writeTranscript(t *testing.T, cwd, claudeSessionID string, lines ...string) string {
	t.Helper()
	configDir := t.TempDir()
	path := TranscriptPath(configDir, cwd, claudeSessionID)
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir transcript dir: %v", err)
	}
	body := ""
	for _, l := range lines {
		body += l + "\n"
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatalf("write transcript: %v", err)
	}
	return configDir
}

func assistantLine(model string) string {
	return `{"type":"assistant","message":{"id":"m","model":"` + model + `","content":[{"type":"text","text":"hi"}]}}`
}

func sidechainLine(model string) string {
	return `{"type":"assistant","isSidechain":true,"message":{"id":"m","model":"` + model + `","content":[]}}`
}

// --- LastTranscriptModel ----------------------------------------------------------

func TestLastTranscriptModelReadsTheFinalAssistantModel(t *testing.T) {
	// Arrange — the model moved mid-conversation.
	dir := writeTranscript(t, "/w", "uuid",
		assistantLine("opus"),
		assistantLine("haiku"))
	// Act
	got, err := LastTranscriptModel(TranscriptPath(dir, "/w", "uuid"))
	// Assert
	if err != nil || got != "haiku" {
		t.Errorf("model = %q, err = %v, want haiku", got, err)
	}
}

func TestLastTranscriptModelIgnoresSidechainEntries(t *testing.T) {
	// Arrange — a Haiku subagent ran AFTER the last main-chain message.
	dir := writeTranscript(t, "/w", "uuid",
		assistantLine("opus"),
		sidechainLine("haiku"))
	// Act
	got, _ := LastTranscriptModel(TranscriptPath(dir, "/w", "uuid"))
	// Assert — the subagent's model is not the session's model.
	if got != "opus" {
		t.Errorf("model = %q, want opus (a sidechain model leaked)", got)
	}
}

func TestLastTranscriptModelSkipsMalformedLines(t *testing.T) {
	// Arrange — a half-flushed final write is normal for an append-only log.
	dir := writeTranscript(t, "/w", "uuid",
		assistantLine("opus"),
		`{"type":"assistant","message":{"id":"m","mod`)
	// Act
	got, err := LastTranscriptModel(TranscriptPath(dir, "/w", "uuid"))
	// Assert
	if err != nil || got != "opus" {
		t.Errorf("model = %q, err = %v, want opus", got, err)
	}
}

func TestLastTranscriptModelReadsOnlyTheTailOfAHugeTranscript(t *testing.T) {
	// Arrange — the answer must come from the END, and the scan must not
	// walk an unbounded file to find it. The opus line is buried far
	// beyond the tail window; the haiku line is the last one.
	filler := strings.Repeat(`{"type":"user","message":{"content":"x"}}`+"\n", 2000)
	dir := t.TempDir()
	path := TranscriptPath(dir, "/w", "uuid")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	body := assistantLine("opus") + "\n" + filler + assistantLine("haiku") + "\n"
	if len(body) <= transcriptTailBytes {
		t.Fatalf("fixture is only %d bytes; it must exceed the %d-byte tail window", len(body), transcriptTailBytes)
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	// Act
	got, err := LastTranscriptModel(path)
	// Assert
	if err != nil || got != "haiku" {
		t.Errorf("model = %q, err = %v, want haiku", got, err)
	}
}

func TestLastTranscriptModelErrorsOnAMissingTranscript(t *testing.T) {
	// Arrange — a transcript the daemon cannot read means the reconciler
	// cannot do its job, which is reported, never silently treated as "no
	// model" (that would blank a perfectly good mirror).
	// Act
	_, err := LastTranscriptModel(filepath.Join(t.TempDir(), "nope.jsonl"))
	// Assert
	if err == nil {
		t.Error("err = nil, want a read failure")
	}
}

func TestLastTranscriptModelReturnsEmptyWhenNoAssistantEntryExists(t *testing.T) {
	// Arrange — a conversation with no assistant turn yet.
	dir := writeTranscript(t, "/w", "uuid", `{"type":"user","message":{"content":"hi"}}`)
	// Act
	got, err := LastTranscriptModel(TranscriptPath(dir, "/w", "uuid"))
	// Assert
	if err != nil || got != "" {
		t.Errorf("model = %q, err = %v, want empty", got, err)
	}
}

// --- Session.ReconcileModel -------------------------------------------------------

// reconcilable builds a live session whose transcript says TRANSCRIPTMODEL
// and whose mirror says MIRRORMODEL, with the reconciler on an injected
// clock so the check fires on demand rather than on a wall clock.
func reconcilable(t *testing.T, mirrorModel, transcriptModel string) (*Session, *fakeShim, chan time.Time) {
	t.Helper()
	configDir := writeTranscript(t, "/w", "uuid", assistantLine(transcriptModel))
	shim := newFakeShim()
	ticks := make(chan time.Time)
	sess := New(Config{
		ID:                  "s_test",
		Shim:                shim,
		CWD:                 "/w",
		ConfigDir:           configDir,
		Logf:                func(string, ...any) {},
		ModelReconcileTicks: ticks,
	})
	sess.translator.ClaudeSessionID = "uuid"
	sess.translator.Model = mirrorModel
	return sess, shim, ticks
}

func TestReconcileModelBroadcastsWhenTheMirrorHasDrifted(t *testing.T) {
	// Arrange — the push path missed a switch: the mirror still says opus.
	sess, shim, _ := reconcilable(t, "opus", "haiku")
	defer shim.end()
	client := NewClient()
	sess.Attach(client)
	<-client.Send // hello
	// Act
	sess.ReconcileModel()
	// Assert
	frame := recvFrame(t, client)
	if frame["type"] != "model-changed" || frame["model"] != "haiku" || frame["origin"] != "reconcile" {
		t.Errorf("frame = %v, want model-changed haiku origin reconcile", frame)
	}
}

func TestReconcileModelWritesTheDriftedModelThrough(t *testing.T) {
	// Arrange — a drift the push path missed, on a session with a
	// registrar: healing the live topbar must ALSO update the durable
	// record so a restart resumes on the reconciled model.
	configDir := writeTranscript(t, "/w", "uuid", assistantLine("haiku"))
	shim := newFakeShim()
	defer shim.end()
	reg := &recordingRegistrar{}
	sess := New(Config{
		ID:                  "s_test",
		Shim:                shim,
		CWD:                 "/w",
		ConfigDir:           configDir,
		Logf:                func(string, ...any) {},
		ModelReconcileTicks: make(chan time.Time),
		Registrar:           reg,
	})
	sess.translator.ClaudeSessionID = "uuid"
	sess.translator.Model = "opus"
	// Act
	sess.ReconcileModel()
	// Assert
	if !slices.Contains(reg.snapshot(), "model s_test haiku") {
		t.Fatalf("calls = %v, want a model write-through", reg.snapshot())
	}
}

func TestReconcileModelAdoptsTheTranscriptModelIntoTheMirror(t *testing.T) {
	// Arrange
	sess, shim, _ := reconcilable(t, "opus", "haiku")
	defer shim.end()
	// Act
	sess.ReconcileModel()
	// Assert
	if got := sess.Info().Model; got != "haiku" {
		t.Errorf("mirror = %q, want haiku", got)
	}
}

func TestReconcileModelIsSilentWhenTheMirrorAgrees(t *testing.T) {
	// Arrange — the steady state, which must put NOTHING on the wire.
	sess, shim, _ := reconcilable(t, "haiku", "haiku")
	defer shim.end()
	client := NewClient()
	sess.Attach(client)
	<-client.Send // hello
	// Act
	sess.ReconcileModel()
	// Assert
	select {
	case data := <-client.Send:
		t.Errorf("broadcast %s, want silence", data)
	default:
	}
}

func TestReconcileModelSkipsASessionWithNoClaudeSessionID(t *testing.T) {
	// Arrange — before system:init there is no transcript path to read, so
	// the check is skipped rather than guessed at.
	sess, shim, _ := reconcilable(t, "opus", "haiku")
	defer shim.end()
	sess.translator.ClaudeSessionID = ""
	// Act
	sess.ReconcileModel()
	// Assert — the mirror is left exactly as it was.
	if got := sess.Info().Model; got != "opus" {
		t.Errorf("mirror = %q, want opus (reconciled without a transcript path)", got)
	}
}

func TestReconcileModelLeavesTheMirrorAloneWhenTheTranscriptIsUnreadable(t *testing.T) {
	// Arrange — a missing transcript must not blank a good mirror.
	shim := newFakeShim()
	defer shim.end()
	var logged []string
	sess := New(Config{
		ID:        "s_test",
		Shim:      shim,
		CWD:       "/w",
		ConfigDir: t.TempDir(), // no transcript written
		Logf:      func(f string, a ...any) { logged = append(logged, f) },
	})
	sess.translator.ClaudeSessionID = "uuid"
	sess.translator.Model = "opus"
	// Act
	sess.ReconcileModel()
	// Assert
	if got := sess.Info().Model; got != "opus" {
		t.Errorf("mirror = %q, want opus", got)
	}
	if len(logged) == 0 {
		t.Error("the unreadable transcript was swallowed, not logged")
	}
}

func TestRunModelReconcilerChecksOnEveryTick(t *testing.T) {
	// Arrange — the periodic check, driven by an injected clock so this is
	// deterministic rather than a race against a wall-clock ticker.
	sess, shim, ticks := reconcilable(t, "opus", "haiku")
	go sess.Run()
	client := NewClient()
	sess.Attach(client)
	<-client.Send // hello
	// Act — one tick.
	ticks <- time.Time{}
	// Assert — the drift was caught without any shim event at all.
	frame := recvFrame(t, client)
	if frame["type"] != "model-changed" || frame["origin"] != "reconcile" {
		t.Errorf("frame = %v, want model-changed origin reconcile", frame)
	}
	shim.end()
}

func TestRunModelReconcilerStopsWithTheSession(t *testing.T) {
	// Arrange — the reconciler must not outlive the shim it checks.
	sess, shim, ticks := reconcilable(t, "opus", "haiku")
	go sess.Run()
	// Act — end the session, then tick.
	shim.end()
	<-sess.Done()
	// Assert — a send on the tick channel must not block forever, which it
	// would if the reconciler goroutine were still selecting on it... and
	// must not panic or broadcast either. A closed session simply ignores it.
	select {
	case ticks <- time.Time{}:
		// The reconciler may have taken this tick before observing done;
		// either way ReconcileModel no-ops on a terminal session.
	case <-time.After(recvTimeout):
		t.Fatal("tick blocked: the reconciler goroutine outlived the session")
	default:
		// Nobody is listening: the goroutine has already exited.
	}
}
