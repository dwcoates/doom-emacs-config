package sessioncontroller

import (
	"context"
	"errors"
	"net"
	"reflect"
	"runtime"
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/shimclient"
)

// --- fakes ------------------------------------------------------------------

type fakeSeqStore struct{ seq map[string]uint64 }

func (f *fakeSeqStore) LastSeq(id string) uint64       { return f.seq[id] }
func (f *fakeSeqStore) SetLastSeq(id string, s uint64) { f.seq[id] = s }

// fakeClearCompactStore is an in-memory ClearCompactStore. It keeps the real store's MONOTONIC
// contract so a test cannot pass against a fake that is more forgiving than
// production. Its map is what a "daemon restart" hands to a fresh Manager.
type fakeClearCompactStore struct {
	mu  sync.Mutex
	seq map[string]uint64
}

func newFakeClearCompactStore() *fakeClearCompactStore {
	return &fakeClearCompactStore{seq: map[string]uint64{}}
}

func (f *fakeClearCompactStore) NewestClearOrCompactSeq(id string) uint64 {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.seq[id]
}

func (f *fakeClearCompactStore) SetNewestClearOrCompactSeq(id string, s uint64) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.seq[id] = max(f.seq[id], s)
}

type fakeSpawner struct {
	mu      sync.Mutex
	calls   []string
	stopped []string
	// stopPIDs records the announced pid each StopShim was handed (0 = none).
	stopPIDs []int32
	err      error
	// stopErr, when set, makes every stop fail.
	stopErr error
	// resume is the vendor conversation pointer each session would be spawned
	// with.
	resume map[string]string
	// staleDropped is reported by the NEXT EnsureShim, standing in for the
	// spawner's validate-before-resume repair.
	staleDropped string
	// drops is a tripwire proving recovery never clears durable identity through
	// the legacy concrete-only DropResume method.
	drops []string
	// dropErr, when set, makes every drop fail.
	dropErr error
	// entered / gate, when set, park every EnsureShim: entry is announced on
	// entered and the call blocks until gate closes. That is how a test holds
	// a bring-up mid-flight (past its entry closed-check) without a sleep.
	entered chan struct{}
	gate    chan struct{}
}

func (s *fakeSpawner) EnsureShim(_ context.Context, sessionID string) (SpawnResult, error) {
	s.mu.Lock()
	s.calls = append(s.calls, sessionID)
	res := SpawnResult{StaleResumeDropped: s.staleDropped, Resumed: s.resume[sessionID]}
	s.staleDropped = ""
	entered, gate := s.entered, s.gate
	s.mu.Unlock()
	if entered != nil {
		entered <- struct{}{}
	}
	if gate != nil {
		<-gate
	}
	return res, s.err
}

// DropResume clears the fake's resume pointer. It remains outside the Spawner
// interface as a test tripwire for any accidental legacy recovery call.
func (s *fakeSpawner) DropResume(sessionID string) (string, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.dropErr != nil {
		return "", s.dropErr
	}
	dropped := s.resume[sessionID]
	s.drops = append(s.drops, sessionID)
	if s.resume != nil {
		delete(s.resume, sessionID)
	}
	return dropped, nil
}

func (s *fakeSpawner) StopShim(sessionID string, hintPID int32) error {
	s.mu.Lock()
	s.stopped = append(s.stopped, sessionID)
	s.stopPIDs = append(s.stopPIDs, hintPID)
	s.mu.Unlock()
	return s.stopErr
}

// dropCalls returns every accidental legacy DropResume call.
func (s *fakeSpawner) dropCalls() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.drops...)
}

// stoppedSessions returns every session the spawner was asked to stop, taken
// under the lock so a teardown goroutine cannot race the read.
func (s *fakeSpawner) stoppedSessions() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.stopped...)
}

// stopHints returns the announced pids each stop was told about, so a test can
// prove the surviving-shim handle actually reaches the spawner.
func (s *fakeSpawner) stopHints() []int32 {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]int32(nil), s.stopPIDs...)
}

// stubSource satisfies the required ConnSource. The fake clients never use it
// (they are handed to newClient directly), but the session controller requires one because
// production cannot drive a session without somewhere for shims to dial in.
type stubSource struct{}

func (stubSource) Next(ctx context.Context, _ string) (net.Conn, *corev1.ShimHello, error) {
	<-ctx.Done()
	return nil, nil, ctx.Err()
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
	// interruptOutcome is the shim verdict the fake acks with. Zero means
	// INTERRUPTED, so a test that does not care about the outcome gets the
	// ordinary successful stop.
	interruptOutcome corev1.InterruptOutcome
	// notReady, when non-nil, blocks AwaitReady until it is closed — the
	// fake's stand-in for a shim that has not finished handshaking yet. Nil
	// (the default) means the connection is already usable, so tests that do
	// not care about bring-up timing are unaffected.
	notReady chan struct{}
	// awaitErr, when set, fails AwaitReady immediately — a handshake that dies
	// rather than one that is merely slow.
	awaitErr error
	// awaitReadyCalls counts AwaitReady round-trips, so a test can prove the
	// readiness wait was TAKEN (rather than inferring it from timing).
	awaitReadyCalls int
	// runResult, when non-nil, makes Run terminate with the received error.
	// This models a protocol failure that evicts the session controller without Hibernate.
	runResult        chan error
	healthStatus     *corev1.HealthStatus
	healthErr        error
	healthRequestIDs []string
	// onSubmit, when set, runs INSIDE SubmitPrompt, before it records or
	// returns. It is how a test observes the world as the shim round-trip
	// begins, which is the only way to prove what was published BEFORE it.
	onSubmit func()
}

type fakeFileDiagnosticPersister struct{}

func (fakeFileDiagnosticPersister) PersistFileDiagnostic(string, string, *corev1.Event, *corev1.FilePlaneDiagnostic) error {
	return nil
}

func (c *fakeClient) Run(ctx context.Context) error {
	if c.runResult != nil {
		select {
		case err := <-c.runResult:
			return err
		case <-ctx.Done():
			return nil
		}
	}
	<-ctx.Done()
	return nil
}
func (c *fakeClient) SubmitPrompt(_ context.Context, text, origin, mode string) error {
	c.mu.Lock()
	hook := c.onSubmit
	c.mu.Unlock()
	if hook != nil {
		hook()
	}
	c.mu.Lock()
	c.prompts = append(c.prompts, text)
	c.origins = append(c.origins, origin)
	c.modes = append(c.modes, mode)
	c.mu.Unlock()
	return nil
}
func (c *fakeClient) AwaitReady(ctx context.Context) error {
	c.mu.Lock()
	ch := c.notReady
	awaitErr := c.awaitErr
	c.awaitReadyCalls++
	c.mu.Unlock()
	if awaitErr != nil {
		return awaitErr
	}
	if ch == nil {
		return nil
	}
	select {
	case <-ch:
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

func (c *fakeClient) Health(_ context.Context, requestID string) (*corev1.HealthStatus, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.healthRequestIDs = append(c.healthRequestIDs, requestID)
	if c.healthErr != nil {
		return nil, c.healthErr
	}
	if c.healthStatus == nil {
		return &corev1.HealthStatus{RequestId: requestID, Healthy: true, Component: "fake-shim"}, nil
	}
	return c.healthStatus, nil
}

func (c *fakeClient) Interrupt(_ context.Context) (corev1.InterruptOutcome, error) {
	c.mu.Lock()
	c.interrupts++
	outcome := c.interruptOutcome
	c.mu.Unlock()
	if outcome == corev1.InterruptOutcome_INTERRUPT_OUTCOME_UNSPECIFIED {
		outcome = corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED
	}
	return outcome, nil
}
func (c *fakeClient) SetModel(_ context.Context, model string) (string, error) { return model, nil }

func TestManagerSetModelRejectsSyntheticBeforeSessionLookup(t *testing.T) {
	// A zero Manager has no locator or shim; reaching either would panic. The
	// marker must stop at this boundary exactly like an empty model.
	m := &Manager{}
	selected, err := m.SetModel(context.Background(), "/workspace", "<synthetic>")
	if err == nil || !strings.Contains(err.Error(), "non-empty model") {
		t.Fatalf("SetModel(<synthetic>) error = %v, want non-empty-model refusal", err)
	}
	if selected != "" {
		t.Fatalf("SetModel(<synthetic>) selected = %q, want empty", selected)
	}
}

// Replay is the shim-mediated bounded history replay. The default fake serves
// an empty, complete one; the replay-specific harness (repull_test.go) swaps in
// a scripted stand-in.
func (c *fakeClient) Replay(_ context.Context, _, _ uint64, _ uint32, _ func(*corev1.Event)) (shimclient.ReplayResult, error) {
	return shimclient.ReplayResult{}, nil
}

// promptTexts returns a copy of the prompts the session controller sent, safe to read while
// the session controller's own goroutines are still running.
func (c *fakeClient) promptTexts() []string {
	c.mu.Lock()
	defer c.mu.Unlock()
	return append([]string(nil), c.prompts...)
}

// interruptCount returns how many interrupts the session controller sent.
func (c *fakeClient) interruptCount() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.interrupts
}

// newTestManager builds a Manager whose clients are fakes, capturing the last
// built fake so a test can inspect what the session controller sent it.
func newTestManager(t *testing.T, locator SessionLocator, spawner Spawner) (*Manager, func() *fakeClient) {
	t.Helper()
	var mu sync.Mutex
	var last *fakeClient
	m, err := New(Config{
		Push:              &fakePusher{},
		SSM:               &fakeApplier{},
		Spawner:           spawner,
		Locator:           locator,
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
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

// newTestManagerNotReady is newTestManager whose clients report NOT-yet-
// connected until notReady is closed — a shim that has been spawned but has not
// finished handshaking, which is the state every cold workspace passes through.
func newTestManagerNotReady(t *testing.T, locator SessionLocator, spawner Spawner, notReady chan struct{}) (*Manager, func() *fakeClient) {
	t.Helper()
	var mu sync.Mutex
	var last *fakeClient
	m, err := New(Config{
		Push:              &fakePusher{},
		SSM:               &fakeApplier{},
		Spawner:           spawner,
		Locator:           locator,
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		newClient: func(cfg shimclient.Config) sessionClient {
			fc := &fakeClient{cfg: cfg, notReady: notReady}
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
	if err := m.SubmitPrompt(context.Background(), "ws", "", "hello", "default"); err != nil {
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
	applier := m.cfg.SSM.(*fakeApplier)
	if len(applier.promptAccepts) != 1 {
		t.Fatalf("prompt accepted edges = %+v, want exactly one", applier.promptAccepts)
	}
	gotEdge := applier.promptAccepts[0]
	if gotEdge.workspace != "ws" || gotEdge.sessionID != "s1" || gotEdge.requestID != "" {
		t.Fatalf("prompt accepted edge = %+v, want ws/s1/empty-request", gotEdge)
	}
	if active, activeErr := m.TurnActive("ws"); activeErr != nil || !active {
		t.Fatalf("TurnActive after prompt acceptance = (%v, %v), want true/nil", active, activeErr)
	}
}

func TestSubmitPromptUnknownWorkspaceErrors(t *testing.T) {
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{}}, &fakeSpawner{})
	if err := m.SubmitPrompt(context.Background(), "ghost", "", "x", ""); err == nil {
		t.Fatal("prompting a workspace with no live session must error")
	}
}

func TestHealthRequiresTheNamedExistingSessionControllerAndForwardsCorrelation(t *testing.T) {
	// Arrange: bring up the session controller through an ordinary command, then change the
	// fake's health result so the test proves Health inspects THAT controller rather
	// than starting another one.
	spawner := &fakeSpawner{}
	m, lastClient := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)
	if err := m.SubmitPrompt(context.Background(), "ws", "", "hello", ""); err != nil {
		t.Fatalf("bring up: %v", err)
	}
	lastClient().healthStatus = &corev1.HealthStatus{RequestId: "health-1", Healthy: true, Component: "claude-shim"}

	// Act.
	status, err := m.Health(context.Background(), "ws", "s1", "health-1")

	// Assert.
	if err != nil || !status.GetHealthy() || status.GetRequestId() != "health-1" {
		t.Fatalf("Health = (%+v, %v)", status, err)
	}
	if len(spawner.calls) != 1 {
		t.Fatalf("health must not start another shim; EnsureShim calls=%v", spawner.calls)
	}
	if got := lastClient().healthRequestIDs; len(got) != 1 || got[0] != "health-1" {
		t.Fatalf("forwarded health ids=%v", got)
	}
}

func TestHealthRejectsNoSessionControllerAndWrongSession(t *testing.T) {
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	if _, err := m.Health(context.Background(), "ws", "s1", "health-1"); err == nil {
		t.Fatal("health without an existing controller must error")
	}
	if err := m.SubmitPrompt(context.Background(), "ws", "", "hello", ""); err != nil {
		t.Fatalf("bring up: %v", err)
	}
	if _, err := m.Health(context.Background(), "ws", "other", "health-2"); err == nil {
		t.Fatal("health for a different session must error")
	}
}

// TestHealthWaitsForABringUpAlreadyInMotion pins the race the probe used to
// lose: createSession acks when the spawn is ISSUED, Emacs probes health a few
// milliseconds later, and the shim attaches a few milliseconds after that.
func TestHealthWaitsForABringUpAlreadyInMotion(t *testing.T) {
	// Arrange: the session controller EXISTS (Ensure started it) but has not handshaked.
	notReady := make(chan struct{})
	m, lastClient := newTestManagerNotReady(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{}, notReady)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}

	// Act: probe while the shim is still connecting, then let it connect.
	type answer struct {
		status *corev1.HealthStatus
		err    error
	}
	done := make(chan answer, 1)
	go func() {
		s, err := m.Health(context.Background(), "ws", "s1", "health-1")
		done <- answer{s, err}
	}()
	select {
	case a := <-done:
		t.Fatalf("Health answered %+v/%v while the shim was still connecting; it must wait", a.status, a.err)
	case <-time.After(20 * time.Millisecond):
	}
	close(notReady)

	// Assert: healthy, once the connection the probe waited for exists.
	select {
	case a := <-done:
		if a.err != nil || !a.status.GetHealthy() || a.status.GetRequestId() != "health-1" {
			t.Fatalf("Health = (%+v, %v)", a.status, a.err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("Health never answered after the shim connected")
	}
	if got := lastClient().healthRequestIDs; len(got) != 1 || got[0] != "health-1" {
		t.Fatalf("forwarded health ids=%v", got)
	}
}

// TestHealthWithNoSessionControllerStillFailsImmediately keeps the loud instant failure
// for a workspace nothing is driving: the wait covers a bring-up in motion, and
// there is no bring-up here to wait for.
func TestHealthWithNoSessionControllerStillFailsImmediately(t *testing.T) {
	// Arrange: no Ensure, no submit — nothing has ever created this workspace's session controller.
	m, lastClient := newTestManagerNotReady(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{}, make(chan struct{}))

	// Act.
	done := make(chan error, 1)
	go func() { _, err := m.Health(context.Background(), "ws", "s1", "health-1"); done <- err }()

	// Assert: answered at once, loudly, and no client was ever built for it.
	select {
	case err := <-done:
		if err == nil || !strings.Contains(err.Error(), "no live session") {
			t.Fatalf("err = %v, want a loud no-live-session failure", err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("Health hung on a workspace with no session controller; it must fail at once")
	}
	if fc := lastClient(); fc != nil {
		t.Fatalf("health must not have built a client, got %+v", fc)
	}
}

// TestHealthNamesTheLinkItStoppedAt pins the sentinel each failure carries, one
// case per link: a create nack reports the DEEPEST hop it reached, and it can
// only do that if the hops are distinguishable by errors.Is rather than by
// message text.
func TestHealthNamesTheLinkItStoppedAt(t *testing.T) {
	tests := []struct {
		name string
		// arrange brings the manager to the state under test and returns the
		// context the probe runs under.
		arrange func(t *testing.T) (*Manager, context.Context)
		want    error
		unwant  error
	}{
		{
			name: "nothing ever drove the workspace",
			arrange: func(t *testing.T) (*Manager, context.Context) {
				m, _ := newTestManagerNotReady(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{}, make(chan struct{}))
				return m, context.Background()
			},
			want:   ErrNoLiveSessionController,
			unwant: ErrShimNotReady,
		},
		{
			name: "controller exists but never completed its handshake",
			arrange: func(t *testing.T) (*Manager, context.Context) {
				m, _ := newTestManagerNotReady(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{}, make(chan struct{}))
				if err := m.Ensure("ws"); err != nil {
					t.Fatalf("Ensure: %v", err)
				}
				ctx, cancel := context.WithTimeout(context.Background(), 30*time.Millisecond)
				t.Cleanup(cancel)
				return m, ctx
			},
			want:   ErrShimNotReady,
			unwant: ErrNoLiveSessionController,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m, ctx := tc.arrange(t)

			// Act.
			_, err := m.Health(ctx, "ws", "s1", "health-1")

			// Assert.
			if !errors.Is(err, tc.want) {
				t.Fatalf("err = %v, want it to carry %v", err, tc.want)
			}
			if errors.Is(err, tc.unwant) {
				t.Fatalf("err = %v also carries %v; the links must stay disjoint", err, tc.unwant)
			}
		})
	}
}

// TestHealthSurfacesItsOwnDeadlineDuringBringUp proves the wait is bounded by
// the PROBE'S context: a shim that never attaches ends the probe with that
// deadline, never with a hang.
func TestHealthSurfacesItsOwnDeadlineDuringBringUp(t *testing.T) {
	// Arrange: a session controller that never becomes ready.
	m, _ := newTestManagerNotReady(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{}, make(chan struct{}))
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Millisecond)
	defer cancel()

	// Act.
	_, err := m.Health(ctx, "ws", "s1", "health-1")

	// Assert: the deadline itself, named with the session it was probing.
	if err == nil {
		t.Fatal("Health must fail when the probe's deadline expires before readiness")
	}
	if !errors.Is(err, context.DeadlineExceeded) || !strings.Contains(err.Error(), "health-1") {
		t.Fatalf("err = %v, want the probe's deadline, correlated", err)
	}
}

// TestHealthOnAReadySessionControllerAnswersWithoutBlocking pins that the wait costs an
// already-connected session nothing: one non-blocking readiness check, then the
// same round-trip as before.
func TestHealthOnAReadySessionControllerAnswersWithoutBlocking(t *testing.T) {
	// Arrange: a live, connected controller.
	m, lastClient := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	if err := m.SubmitPrompt(context.Background(), "ws", "", "hello", ""); err != nil {
		t.Fatalf("bring up: %v", err)
	}
	fc := lastClient()
	fc.mu.Lock()
	before := fc.awaitReadyCalls
	fc.mu.Unlock()

	// Act.
	status, err := m.Health(context.Background(), "ws", "s1", "health-1")

	// Assert: answered, having consulted readiness exactly once more.
	if err != nil || !status.GetHealthy() {
		t.Fatalf("Health = (%+v, %v)", status, err)
	}
	fc.mu.Lock()
	after := fc.awaitReadyCalls
	fc.mu.Unlock()
	if after != before+1 {
		t.Fatalf("AwaitReady calls: got %d, want %d", after, before+1)
	}
}

func TestSubmitPromptReusesLiveSession(t *testing.T) {
	// Arrange.
	spawner := &fakeSpawner{}
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)

	// Act: two prompts to the same workspace.
	_ = m.SubmitPrompt(context.Background(), "ws", "", "a", "")
	_ = m.SubmitPrompt(context.Background(), "ws", "", "b", "")

	// Assert: brought up once (reused).
	if len(spawner.calls) != 1 {
		t.Fatalf("expected 1 bring-up for a reused workspace, got %d", len(spawner.calls))
	}
}

func TestResumedSessionPromptsAreForwardedVerbatim(t *testing.T) {
	// Arrange: a resumed session, which used to have a read-directive folded
	// into its first prompt. Its guidelines now ride in the system prompt the
	// shim built at spawn (agent-shim/claude/shim/src/metaprompt.ts), so the
	// daemon has nothing to re-establish and rewrites nothing.
	spawner := &fakeSpawner{}
	m, lastClient := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)
	if err := m.SubmitPrompt(context.Background(), "ws", "", "first", ""); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	d, err := m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	m.onTurnBoundary(d, false)
	d.consumer.Apply(&corev1.Event{
		SessionId: "s1",
		Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{
			Source: corev1.SessionSource_SESSION_SOURCE_RESUME,
			Cwd:    t.TempDir(),
		}},
	})

	// Act: the next two prompts.
	_ = m.SubmitPrompt(context.Background(), "ws", "", "second", "")
	m.onTurnBoundary(d, false)
	_ = m.SubmitPrompt(context.Background(), "ws", "", "third", "")

	// Assert: every prompt reaches the shim exactly as the user typed it.
	fc := lastClient()
	want := []string{"first", "second", "third"}
	if len(fc.prompts) != len(want) {
		t.Fatalf("expected %d prompts, got %d", len(want), len(fc.prompts))
	}
	for i, w := range want {
		if fc.prompts[i] != w {
			t.Errorf("prompt %d = %q, want %q verbatim", i, fc.prompts[i], w)
		}
	}
}

func TestInterruptRequiresLiveSession(t *testing.T) {
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{}}, &fakeSpawner{})
	if err := m.Interrupt(context.Background(), "ws"); err == nil {
		t.Fatal("interrupt with no live session must error")
	}
}

// --- Interrupt outcome (F4) -------------------------------------------------
//
// The shim's own verdict decides whether a stop failed. From outside, a stop
// that could not be delivered and a turn that had already ended arrive as the
// same silence, and the second used to be reported as the first.

func TestInterruptReportsNoErrorWhenTheTurnWasStopped(t *testing.T) {
	// Arrange.
	m, lastClient := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	if err := m.SubmitPrompt(context.Background(), "ws", "", "hello", ""); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	lastClient().interruptOutcome = corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED

	// Act.
	err := m.Interrupt(context.Background(), "ws")

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt(INTERRUPTED) = %v, want nil", err)
	}
}

func TestInterruptReportsNoErrorWhenTheTurnHadAlreadyEnded(t *testing.T) {
	// Arrange: the outcome that exists precisely so this stops being painted
	// as a failed stop. The user asked for the turn to be over and it is.
	m, lastClient := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	if err := m.SubmitPrompt(context.Background(), "ws", "", "hello", ""); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	lastClient().interruptOutcome = corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE

	// Act.
	err := m.Interrupt(context.Background(), "ws")

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt(ALREADY_COMPLETE) = %v, want nil; a no-op stop is success", err)
	}
	applier := m.cfg.SSM.(*fakeApplier)
	if len(applier.alreadyCompletes) != 1 ||
		applier.alreadyCompletes[0] != (alreadyCompleteCall{workspace: "ws", sessionID: "s1"}) {
		t.Fatalf("already-complete reconciliations = %+v, want ws/s1 exactly once", applier.alreadyCompletes)
	}
	if active, activeErr := m.TurnActive("ws"); activeErr != nil || active {
		t.Fatalf("TurnActive after ALREADY_COMPLETE = (%v, %v), want false/nil", active, activeErr)
	}
}

func TestAlreadyCompleteWithholdsFooterWindowWhenStateReconciliationFails(t *testing.T) {
	// Arrange — the shim can truthfully answer ALREADY_COMPLETE while the
	// state database fails. Publishing the footer chip then would recreate the
	// contradiction this path exists to prevent.
	m, lastClient := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	progress := &fakeProgress{}
	m.cfg.Progress = progress
	if err := m.SubmitPrompt(context.Background(), "ws", "", "hello", ""); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	applier := m.cfg.SSM.(*fakeApplier)
	applier.alreadyCompleteErr = errors.New("state write failed")
	lastClient().interruptOutcome = corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE

	// Act.
	err := m.Interrupt(context.Background(), "ws")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "state write failed") {
		t.Fatalf("Interrupt error = %v, want reconciliation failure", err)
	}
	if notes := progress.interruptNotes(); len(notes) != 0 {
		t.Fatalf("interrupt windows = %+v, want none when state reconciliation failed", notes)
	}
}

func TestInterruptReportsAnUndeliverableStopAsAFailure(t *testing.T) {
	// Arrange: the ONLY outcome that reads as a failure.
	m, lastClient := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	if err := m.SubmitPrompt(context.Background(), "ws", "", "hello", ""); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	lastClient().interruptOutcome = corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED

	// Act.
	err := m.Interrupt(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, errclass.ErrInterruptUndelivered) {
		t.Fatalf("Interrupt(FAILED) = %v, want ErrInterruptUndelivered", err)
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
	if err := m.SubmitPrompt(context.Background(), "ws", "", "x", ""); err == nil {
		t.Fatal("a spawn failure must surface as a loud error, not be swallowed")
	}
}

var errBringUp = errors.New("bring-up failed")

// --- permHandler permission-item push (S8) ---------------------------------

// newTestPermHandler builds a permHandler over a fresh registry and a
// fakePusher-backed consumer, returning all three for assertions.
func newTestPermHandler() (permHandler, *permRegistry, *fakePusher) {
	push := &fakePusher{}
	reg := newPermRegistry(nil)
	cons := newConsumer("ws", "s1", push, &fakeApplier{}, nil, newFakeClearCompactStore(), nil, nil, nil, nil, nil, nil)
	return permHandler{reg: reg, cons: cons, logf: func(string, ...any) {}}, reg, push
}

// waitForPermWaiter blocks until reg has a pending waiter for id under ws,
// yielding the scheduler so the blocked handler goroutine can park its waiter.
// It is a deterministic rendezvous (HandlePermission registers the waiter
// synchronously before blocking on the answer channel), not a timed sleep.
func waitForPermWaiter(reg *permRegistry, ws, id string) {
	for {
		for _, got := range reg.idsForWorkspace(ws) {
			if got == id {
				return
			}
		}
		runtime.Gosched()
	}
}

func TestHandlePermissionPushesPendingThenAllowed(t *testing.T) {
	// Arrange.
	ph, reg, push := newTestPermHandler()
	req := &corev1.PermissionRequest{RequestId: "r1", ToolName: "Bash"}

	// Act: run the blocking handler, wait for its waiter, then allow.
	done := make(chan *corev1.PermissionResponse, 1)
	go func() { done <- ph.HandlePermission("s1", req) }()
	waitForPermWaiter(reg, "ws", "r1")
	if err := reg.answer("r1", true, "", nil); err != nil {
		t.Fatalf("answer: %v", err)
	}
	<-done

	// Assert: pending then allowed on uuid r1, plus a PERMISSION render-state.
	got := push.permissionResolutions("r1")
	want := []corev1.PermissionItem_Resolution{
		corev1.PermissionItem_RESOLUTION_PENDING,
		corev1.PermissionItem_RESOLUTION_ALLOWED,
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("resolutions = %v, want %v", got, want)
	}
	if len(push.state) == 0 || push.state[0].GetState() != frontendv1.RenderState_RENDER_STATE_PERMISSION {
		t.Fatalf("expected a PERMISSION workspace-state push, got %v", push.state)
	}
}

func TestHandlePermissionPushesDeniedWithMessage(t *testing.T) {
	// Arrange.
	ph, reg, push := newTestPermHandler()
	req := &corev1.PermissionRequest{RequestId: "r2", ToolName: "Bash"}

	// Act.
	done := make(chan *corev1.PermissionResponse, 1)
	go func() { done <- ph.HandlePermission("s1", req) }()
	waitForPermWaiter(reg, "ws", "r2")
	if err := reg.answer("r2", false, "not allowed", nil); err != nil {
		t.Fatalf("answer: %v", err)
	}
	<-done

	// Assert: pending then denied, and the denied item carries the deny message.
	got := push.permissionResolutions("r2")
	want := []corev1.PermissionItem_Resolution{
		corev1.PermissionItem_RESOLUTION_PENDING,
		corev1.PermissionItem_RESOLUTION_DENIED,
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("resolutions = %v, want %v", got, want)
	}
	if msg := lastPermissionDenyMessage(push, "r2"); msg != "not allowed" {
		t.Fatalf("deny message = %q, want %q", msg, "not allowed")
	}
}

func TestHandlePermissionAbandonedOnTeardown(t *testing.T) {
	// Arrange.
	ph, reg, push := newTestPermHandler()
	req := &corev1.PermissionRequest{RequestId: "r3", ToolName: "Bash"}

	// Act: the handler blocks; a teardown fail abandons it (nil response).
	done := make(chan *corev1.PermissionResponse, 1)
	go func() { done <- ph.HandlePermission("s1", req) }()
	waitForPermWaiter(reg, "ws", "r3")
	reg.fail("connection teardown")
	if resp := <-done; resp != nil {
		t.Fatalf("abandoned permission must return a nil response, got %v", resp)
	}

	// Assert: pending then abandoned on uuid r3.
	got := push.permissionResolutions("r3")
	want := []corev1.PermissionItem_Resolution{
		corev1.PermissionItem_RESOLUTION_PENDING,
		corev1.PermissionItem_RESOLUTION_ABANDONED,
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("resolutions = %v, want %v", got, want)
	}
}

// --- session-scoped hibernation ---------------------------------------------
//
// Several registry records can share one workspace cwd (a stale duplicate, a
// superseded resume, an orphan awaiting reap), so "stand down THIS record's
// shim" is a different question from "stand down the workspace's shim".
// HibernateSession answers the first one; answering it with the
// workspace-keyed Hibernate is what killed a healthy session on 2026-07-25.

func TestHibernateSessionStopsTheMatchingSession(t *testing.T) {
	// Arrange: bring up s1 for ws.
	spawner := &fakeSpawner{}
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)
	if err := m.SubmitPrompt(context.Background(), "ws", "", "hi", ""); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Act: stand down the session that IS live.
	if err := m.HibernateSession("ws", "s1"); err != nil {
		t.Fatalf("HibernateSession: %v", err)
	}

	// Assert.
	if len(spawner.stopped) != 1 || spawner.stopped[0] != "s1" {
		t.Fatalf("stopped = %v, want [s1]", spawner.stopped)
	}
}

func TestHibernateSessionStopsOnlyTheRequestedDifferentSession(t *testing.T) {
	// Arrange: s1 is the live session controller for ws.
	spawner := &fakeSpawner{}
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)
	if err := m.SubmitPrompt(context.Background(), "ws", "", "hi", ""); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Act: stand down a STALE record that shares the cwd (the orphan reap).
	err := m.HibernateSession("ws", "s_orphan")

	// Assert: the stale process handle is reachable by id; the live shim is
	// untouched even though both records share the workspace.
	if err != nil {
		t.Fatalf("HibernateSession: %v", err)
	}
	if !reflect.DeepEqual(spawner.stopped, []string{"s_orphan"}) {
		t.Fatalf("stopped = %v, want [s_orphan] (s1 must survive)", spawner.stopped)
	}
}

func TestHibernateSessionKeepsTheSessionControllerLiveOnAMismatch(t *testing.T) {
	// Arrange: s1 live for ws.
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	if err := m.SubmitPrompt(context.Background(), "ws", "", "hi", ""); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Act: a mismatched stand-down must not evict the byWS entry either.
	_ = m.HibernateSession("ws", "s_orphan")

	// Assert: ws still resolves to its live session controller.
	d, err := m.existing("ws")
	if err != nil {
		t.Fatalf("existing after mismatched HibernateSession: %v", err)
	}
	if d.sessionID != "s1" {
		t.Fatalf("live session = %s, want s1", d.sessionID)
	}
}

func TestHibernateSessionStopsAnEvictedSessionsProcess(t *testing.T) {
	// Arrange: no byWS controller remains, but the spawner still owns the process
	// handle created before the session controller failed.
	spawner := &fakeSpawner{}
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)

	// Act.
	err := m.HibernateSession("ws", "s1")

	// Assert: session identity is sufficient; absence from byWS must not make
	// the child process unreachable.
	if err != nil {
		t.Fatalf("HibernateSession: %v", err)
	}
	if !reflect.DeepEqual(spawner.stopped, []string{"s1"}) {
		t.Fatalf("stopped = %v, want [s1]", spawner.stopped)
	}
}

func TestTerminalSessionControllerErrorStopsShimAfterEviction(t *testing.T) {
	// Arrange: a client whose Run loop terminates independently, reproducing
	// the sequence-regression path that left the shim parked and unreachable.
	spawner := &fakeSpawner{}
	runResult := make(chan error, 1)
	var client *fakeClient
	m, err := New(Config{
		Push:              &fakePusher{},
		SSM:               &fakeApplier{},
		Spawner:           spawner,
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		newClient: func(cfg shimclient.Config) sessionClient {
			client = &fakeClient{cfg: cfg, runResult: runResult}
			return client
		},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	if err := m.SubmitPrompt(context.Background(), "ws", "", "hi", ""); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Act.
	runResult <- errors.New("sequence regression")

	// Assert: Run's terminal return evicts the session controller and stops its exact shim.
	deadline := time.Now().Add(2 * time.Second)
	for {
		spawner.mu.Lock()
		stopped := append([]string(nil), spawner.stopped...)
		spawner.mu.Unlock()
		if reflect.DeepEqual(stopped, []string{"s1"}) {
			break
		}
		if time.Now().After(deadline) {
			t.Fatalf("stopped = %v, want [s1]", stopped)
		}
		runtime.Gosched()
	}
	if _, err := m.existing("ws"); err == nil {
		t.Fatal("controller still present after terminal Run error")
	}
}

func TestHibernateStillStopsWhicheverSessionIsLive(t *testing.T) {
	// Arrange: the workspace-scoped variant keeps its identity-blind behavior
	// (the idle sweep and daemon shutdown depend on it).
	spawner := &fakeSpawner{}
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)
	if err := m.SubmitPrompt(context.Background(), "ws", "", "hi", ""); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Act
	if err := m.Hibernate("ws"); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}

	// Assert
	if len(spawner.stopped) != 1 || spawner.stopped[0] != "s1" {
		t.Fatalf("stopped = %v, want [s1]", spawner.stopped)
	}
}

// --- bring-up readiness ------------------------------------------------------
//
// The idle sweep hibernates every workspace that is not mid-turn, so a prompt
// after any pause lands on a workspace whose shim must be spawned first. The
// spawn is asynchronous: without a readiness gate the prompt was handed to a
// connection that did not exist yet and came back "no live shim connection"
// roughly 500ms before the shim finished connecting.

func TestSubmitPromptWaitsForTheShimToBecomeDriveable(t *testing.T) {
	// Arrange: a client that is NOT connected yet, as a freshly spawned shim.
	spawner := &fakeSpawner{}
	notReady := make(chan struct{})
	m, lastClient := newTestManagerNotReady(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner, notReady)

	// Act: submit while the shim is still coming up.
	done := make(chan error, 1)
	go func() { done <- m.SubmitPrompt(context.Background(), "ws", "", "hello", "") }()

	// Assert: it must not have been rejected — it is waiting, not failing.
	select {
	case err := <-done:
		t.Fatalf("SubmitPrompt returned %v while the shim was still connecting", err)
	case <-time.After(20 * time.Millisecond):
	}

	// Act: the shim finishes connecting.
	close(notReady)

	// Assert: the prompt is delivered, not dropped.
	select {
	case err := <-done:
		if err != nil {
			t.Fatalf("SubmitPrompt: %v", err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("SubmitPrompt never completed after the shim connected")
	}
	if fc := lastClient(); fc == nil || len(fc.promptTexts()) != 1 {
		t.Fatalf("expected the prompt to reach the shim, got %+v", fc)
	}
}

func TestSubmitPromptFailsWhenTheShimNeverConnects(t *testing.T) {
	// Arrange: a shim that never becomes driveable.
	m, _ := newTestManagerNotReady(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{}, make(chan struct{}))

	// Act: the caller's context is the failure bound.
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Millisecond)
	defer cancel()
	err := m.SubmitPrompt(ctx, "ws", "", "hello", "")

	// Assert: surfaced loudly, naming the workspace — never a silent drop.
	if err == nil {
		t.Fatal("SubmitPrompt must fail when the shim never connects")
	}
	if !strings.Contains(err.Error(), "ws") {
		t.Fatalf("err = %v, want it to name the workspace", err)
	}
}

func TestEnsureDoesNotWaitForReadiness(t *testing.T) {
	// Arrange: Ensure is the eager create-path bring-up. It must NOT block on
	// the handshake, or a workspace restore would serialize behind N of them.
	m, _ := newTestManagerNotReady(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{}, make(chan struct{}))

	// Act / Assert: returns promptly even though the shim never connects.
	done := make(chan error, 1)
	go func() { done <- m.Ensure("ws") }()
	select {
	case err := <-done:
		if err != nil {
			t.Fatalf("Ensure: %v", err)
		}
	case <-time.After(time.Second):
		t.Fatal("Ensure blocked on the shim handshake; it must only start the shim")
	}
}

// TestResyncRoundTripReplaysTheRetainedHistory is the integration proof of the
// backfill-to-GUI path: a freshly-mounted frontend asks from seq 0 and receives
// the daemon's whole retained conversation history back as ConversationDeltas.
//
// It runs the REAL chain — Manager.Resync, the per-session consumer's retained
// ring, and internal/frontend's conversation translation — against a recording
// Pusher. That is exactly the hop that used to answer with silence, because
// nothing ever sent a ResyncCmd.
func TestResyncRoundTripReplaysTheRetainedHistory(t *testing.T) {
	// Arrange: a live session controller whose ring holds three streamed assistant messages.
	h := newRepullHarness(t, &replayClient{})
	d := h.controller(t)
	for i, uuid := range []string{"u1", "u2", "u3"} {
		d.consumer.Consume(assistantEvent(t, uint64(6108+i), uuid))
	}
	h.push.mu.Lock()
	h.push.convo = nil // drop the LIVE pushes; only the replay should remain
	h.push.mu.Unlock()

	// Act: the fresh frontend knows nothing, so it asks from the very start.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert: every retained item comes back, in order, stamped with its seq.
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var gotUUIDs []string
	var gotSeqs []uint64
	for _, cd := range h.push.convo {
		gotSeqs = append(gotSeqs, cd.GetThroughSeq())
		for _, it := range cd.GetItems() {
			gotUUIDs = append(gotUUIDs, it.GetUuid())
		}
	}
	if !reflect.DeepEqual(gotUUIDs, []string{"u1", "u2", "u3"}) {
		t.Fatalf("replayed item uuids = %v, want [u1 u2 u3]", gotUUIDs)
	}
	if !reflect.DeepEqual(gotSeqs, []uint64{6108, 6109, 6110}) {
		t.Fatalf("replayed through_seqs = %v, want [6108 6109 6110]", gotSeqs)
	}
}

func TestTaskCatalogsSnapshotsEveryLiveSessionIncludingAnEmptyRoster(t *testing.T) {
	// Arrange — one live session controller with no task events yet. The empty catalog is
	// necessary to clear stale frontend state after reconnect.
	h := newRepullHarness(t, &replayClient{})
	d := h.controller(t)

	// Act.
	empty := h.m.TaskCatalogs()
	d.consumer.Apply(&corev1.Event{
		SessionId: "s1",
		Seq:       1,
		Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{
			TaskId:      "t1",
			Kind:        corev1.TaskKind_TASK_KIND_AGENT,
			Description: "investigate",
		}},
	})
	populated := h.m.TaskCatalogs()

	// Assert.
	if len(empty) != 1 || empty[0].GetWorkspace() != "ws" || empty[0].GetSessionId() != "s1" {
		t.Fatalf("empty catalogs = %v, want one catalog for ws/s1", empty)
	}
	if len(empty[0].GetTasks()) != 0 {
		t.Fatalf("empty catalog tasks = %v, want none", empty[0].GetTasks())
	}
	if len(populated) != 1 || len(populated[0].GetTasks()) != 1 {
		t.Fatalf("populated catalogs = %v, want one task", populated)
	}
	if got := populated[0].GetTasks()[0].GetTaskId(); got != "t1" {
		t.Fatalf("task id = %q, want t1", got)
	}
}
