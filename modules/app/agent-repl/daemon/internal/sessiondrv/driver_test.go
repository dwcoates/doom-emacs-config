package sessiondrv

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/shimclient"
)

// --- fakes ------------------------------------------------------------------

type fakeSeqStore struct{ seq map[string]uint64 }

func (f *fakeSeqStore) LastSeq(id string) uint64       { return f.seq[id] }
func (f *fakeSeqStore) SetLastSeq(id string, s uint64) { f.seq[id] = s }

type fakeSpawner struct {
	mu      sync.Mutex
	calls   []string
	stopped []string
	err     error
}

func (s *fakeSpawner) EnsureShim(_ context.Context, sessionID, _ string) error {
	s.mu.Lock()
	s.calls = append(s.calls, sessionID)
	s.mu.Unlock()
	return s.err
}

func (s *fakeSpawner) StopShim(sessionID string) error {
	s.mu.Lock()
	s.stopped = append(s.stopped, sessionID)
	s.mu.Unlock()
	return nil
}

type fakeLocator struct{ m map[string]string }

func (l fakeLocator) Locate(ws string) (string, bool) { id, ok := l.m[ws]; return id, ok }

type fakeClient struct {
	cfg        shimclient.Config
	mu         sync.Mutex
	prompts    []string
	origins    []string
	modes      []string
	interrupts int
}

func (c *fakeClient) Run(ctx context.Context) error {
	<-ctx.Done()
	return nil
}
func (c *fakeClient) SubmitPrompt(_ context.Context, text, origin, mode string) error {
	c.mu.Lock()
	c.prompts = append(c.prompts, text)
	c.origins = append(c.origins, origin)
	c.modes = append(c.modes, mode)
	c.mu.Unlock()
	return nil
}
func (c *fakeClient) Interrupt(_ context.Context, _ bool) error {
	c.mu.Lock()
	c.interrupts++
	c.mu.Unlock()
	return nil
}

// newTestManager builds a Manager whose clients are fakes, capturing the last
// built fake so a test can inspect what the driver sent it.
func newTestManager(t *testing.T, locator SessionLocator, spawner Spawner) (*Manager, func() *fakeClient) {
	t.Helper()
	var mu sync.Mutex
	var last *fakeClient
	m, err := New(Config{
		Push:            &fakePusher{},
		SSM:             &fakeApplier{},
		Spawner:         spawner,
		Locator:         locator,
		SeqStore:        &fakeSeqStore{seq: map[string]uint64{}},
		ProtocolVersion: "1",
		socketPath:      func(id string) string { return "/tmp/session-" + id + ".sock" },
		newClient: func(cfg shimclient.Config) sessionClient {
			fc := &fakeClient{cfg: cfg}
			mu.Lock()
			last = fc
			mu.Unlock()
			return fc
		},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	return m, func() *fakeClient { mu.Lock(); defer mu.Unlock(); return last }
}

func TestNewRejectsMissingDeps(t *testing.T) {
	_, err := New(Config{})
	if err == nil {
		t.Fatal("New with no deps must error")
	}
}

func TestSubmitPromptBringsUpAndSends(t *testing.T) {
	// Arrange.
	spawner := &fakeSpawner{}
	m, lastClient := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)

	// Act.
	if err := m.SubmitPrompt(context.Background(), "ws", "hello", "default"); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert: shim ensured once, prompt forwarded with origin "frontend".
	if len(spawner.calls) != 1 || spawner.calls[0] != "s1" {
		t.Fatalf("expected EnsureShim(s1) once, got %v", spawner.calls)
	}
	fc := lastClient()
	if fc == nil || len(fc.prompts) != 1 || fc.prompts[0] != "hello" {
		t.Fatalf("expected prompt 'hello' forwarded, got %+v", fc)
	}
	if fc.origins[0] != "frontend" {
		t.Errorf("origin: got %q, want frontend", fc.origins[0])
	}
}

func TestSubmitPromptUnknownWorkspaceErrors(t *testing.T) {
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{}}, &fakeSpawner{})
	if err := m.SubmitPrompt(context.Background(), "ghost", "x", ""); err == nil {
		t.Fatal("prompting a workspace with no live session must error")
	}
}

func TestSubmitPromptReusesLiveSession(t *testing.T) {
	// Arrange.
	spawner := &fakeSpawner{}
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)

	// Act: two prompts to the same workspace.
	_ = m.SubmitPrompt(context.Background(), "ws", "a", "")
	_ = m.SubmitPrompt(context.Background(), "ws", "b", "")

	// Assert: brought up once (reused).
	if len(spawner.calls) != 1 {
		t.Fatalf("expected 1 bring-up for a reused workspace, got %d", len(spawner.calls))
	}
}

func TestMetapromptRefireFoldsOncePerResume(t *testing.T) {
	// Arrange: a resumed session whose cwd carries a metaprompt file.
	cwd := t.TempDir()
	writeMetaprompt(t, cwd)
	spawner := &fakeSpawner{}
	m, lastClient := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)

	// Bring the session up, then arm via a resume SessionStarted, as the
	// consumer's onSessionStarted would on the live stream.
	if err := m.SubmitPrompt(context.Background(), "ws", "first", ""); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	d, err := m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	m.armMetaprompt(d, &corev1.SessionStarted{Source: corev1.SessionSource_SESSION_SOURCE_RESUME, Cwd: cwd})

	// Act: the NEXT two prompts.
	_ = m.SubmitPrompt(context.Background(), "ws", "second", "")
	_ = m.SubmitPrompt(context.Background(), "ws", "third", "")

	// Assert: exactly the "second" prompt carries the directive; "third" does not.
	fc := lastClient()
	if len(fc.prompts) != 3 {
		t.Fatalf("expected 3 prompts, got %d", len(fc.prompts))
	}
	if fc.prompts[1] == "second" {
		t.Error("second prompt should have the metaprompt directive prepended")
	}
	if !strings.Contains(fc.prompts[1], "second") {
		t.Errorf("second prompt should still contain the user text; got %q", fc.prompts[1])
	}
	if fc.prompts[2] != "third" {
		t.Errorf("third prompt must NOT re-fire the directive; got %q", fc.prompts[2])
	}
}

func TestInterruptRequiresLiveSession(t *testing.T) {
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{}}, &fakeSpawner{})
	if err := m.Interrupt(context.Background(), "ws", true); err == nil {
		t.Fatal("interrupt with no live session must error")
	}
}

func TestAnswerPermissionRoutesToRegistry(t *testing.T) {
	// Arrange.
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{}}, &fakeSpawner{})
	ch, release := m.reg.await("r1", "ws")
	defer release()

	// Act.
	if err := m.AnswerPermission(context.Background(), "ws", "r1", true, "", nil); err != nil {
		t.Fatalf("AnswerPermission: %v", err)
	}

	// Assert.
	select {
	case resp := <-ch:
		if resp.GetDecision() != corev1.PermissionDecision_PERMISSION_DECISION_ALLOW {
			t.Errorf("decision: got %v, want ALLOW", resp.GetDecision())
		}
	case <-time.After(time.Second):
		t.Fatal("AnswerPermission did not resolve the parked waiter")
	}
}

func TestEnsureShimFailurePropagates(t *testing.T) {
	spawner := &fakeSpawner{err: errBringUp}
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)
	if err := m.SubmitPrompt(context.Background(), "ws", "x", ""); err == nil {
		t.Fatal("a spawn failure must surface as a loud error, not be swallowed")
	}
}

var errBringUp = errors.New("bring-up failed")
