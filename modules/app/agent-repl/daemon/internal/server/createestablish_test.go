package server

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
)

// probeHealthRouter answers each establishment probe with a scripted verdict,
// correlated to the request id it was asked under (an uncorrelated answer is
// itself a failure the gate must catch, so the correlation cannot be faked away
// by a fixed id). gate, when non-nil, holds the probe until the test releases
// it, which is how "no ack before the verdict" is proven without a sleep.
type probeHealthRouter struct {
	mu       sync.Mutex
	requests []string

	healthy   bool
	component string
	reason    string
	err       error
	// answerID, when non-empty, overrides the echoed request id so a
	// mis-correlated verdict can be scripted.
	answerID string
	gate     chan struct{}
	// entered receives once per probe taken, so a test can order callers on the
	// probe actually arriving rather than on elapsed time.
	entered chan struct{}
}

func (r *probeHealthRouter) Health(ctx context.Context, workspace, sessionID, requestID string) (*corev1.HealthStatus, error) {
	r.mu.Lock()
	r.requests = append(r.requests, requestID)
	gate := r.gate
	r.mu.Unlock()
	if r.entered != nil {
		r.entered <- struct{}{}
	}
	if gate != nil {
		select {
		case <-gate:
		case <-ctx.Done():
			return nil, fmt.Errorf("probe abandoned: %w", ctx.Err())
		}
	}
	if r.err != nil {
		return nil, r.err
	}
	id := requestID
	if r.answerID != "" {
		id = r.answerID
	}
	return &corev1.HealthStatus{RequestId: id, Healthy: r.healthy, Component: r.component, Reason: r.reason}, nil
}

func (r *probeHealthRouter) probeCount() int {
	r.mu.Lock()
	defer r.mu.Unlock()
	return len(r.requests)
}

// establishHandler builds a command handler whose create core and health router
// are the two things under test here.
func establishHandler(t *testing.T, sessions *fakeSessionCmds, router SessionHealthRouter) *commandHandler {
	t.Helper()
	return establishHandlerWithPrompts(t, &fakePrompts{}, sessions, router)
}

// fakeResumes is a ConversationResumeResolver that reports whatever it is told
// to, so a create test can pin the resolution without a registry on disk. The
// zero value resolves nothing, which is the "brand-new workspace" answer.
type fakeResumes struct {
	uuid string
	// observed is what ObservedClaudeSessionID reports.
	observed string
	// asked records every (configDir, cwd) the create path resolved, so a test
	// can prove the daemon consulted its own records rather than the caller.
	asked [][2]string
}

func (f *fakeResumes) ResolveResume(configDir, cwd string) (string, bool) {
	f.asked = append(f.asked, [2]string{configDir, cwd})
	return f.uuid, f.uuid != ""
}

// observed is what the ack should report; separate from uuid so a test can
// distinguish "what we resumed" from "what we landed on".
func (f *fakeResumes) ObservedClaudeSessionID(string) string { return f.observed }

// establishHandlerWithPrompts is establishHandler with the prompt router left
// to the caller, so a test can watch what the create sends down the model path.
func establishHandlerWithPrompts(t *testing.T, prompts *fakePrompts, sessions *fakeSessionCmds, router SessionHealthRouter) *commandHandler {
	t.Helper()
	h, err := newCommandHandler(prompts, &fakeMerges{}, &fakeLifecycle{}, nil, sessions, nil, nil, nil,
		CommandHandlerConfig{Health: HealthConfig{Router: router}, Resumes: &fakeResumes{}})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	return h
}

// TestCreateSessionAppliesTheRequestedStartingModel is the whole point of
// carrying a model on the create: the session must already be on it when the
// create acks, because the caller may submit a turn the moment it does.
func TestCreateSessionAppliesTheRequestedStartingModel(t *testing.T) {
	// Arrange
	prompts := &fakePrompts{}
	h := establishHandlerWithPrompts(t, prompts, &fakeSessionCmds{}, &probeHealthRouter{healthy: true})

	// Act
	_, err := h.CreateSession(context.Background(), "/w", "r1",
		&frontendv1.CreateSessionCmd{Cwd: "/w", Model: "opus"})

	// Assert
	if err != nil {
		t.Fatalf("CreateSession with a model = %v, want nil", err)
	}
	if len(prompts.models) != 1 || prompts.models[0] != "opus" {
		t.Fatalf("create sent models %#v, want exactly [opus]", prompts.models)
	}
}

// TestCreateSessionWithoutAModelLeavesTheModelPathUntouched keeps the field
// genuinely optional: an empty model must not become a model request.
func TestCreateSessionWithoutAModelLeavesTheModelPathUntouched(t *testing.T) {
	// Arrange
	prompts := &fakePrompts{}
	h := establishHandlerWithPrompts(t, prompts, &fakeSessionCmds{}, &probeHealthRouter{healthy: true})

	// Act
	_, err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"})

	// Assert
	if err != nil {
		t.Fatalf("CreateSession without a model = %v, want nil", err)
	}
	if len(prompts.models) != 0 {
		t.Fatalf("create sent models %#v, want none", prompts.models)
	}
}

// TestCreateSessionTreatsAPlaceholderModelAsUnspecified: "<synthetic>" means
// the caller has no selection, so it must behave like an absent field.  Passing
// it through would normalize to "" and make SetModel refuse an empty model,
// failing a create that asked for nothing.
func TestCreateSessionTreatsAPlaceholderModelAsUnspecified(t *testing.T) {
	// Arrange
	prompts := &fakePrompts{}
	h := establishHandlerWithPrompts(t, prompts, &fakeSessionCmds{}, &probeHealthRouter{healthy: true})

	// Act
	_, err := h.CreateSession(context.Background(), "/w", "r1",
		&frontendv1.CreateSessionCmd{Cwd: "/w", Model: "<synthetic>"})

	// Assert
	if err != nil {
		t.Fatalf("CreateSession with a placeholder model = %v, want nil", err)
	}
	if len(prompts.models) != 0 {
		t.Fatalf("placeholder model sent %#v down the model path, want none", prompts.models)
	}
}

// TestCreateSessionFailsWhenTheShimRefusesTheRequestedModel: acking ok would
// hand back a live session quietly running a model nobody asked for.
func TestCreateSessionFailsWhenTheShimRefusesTheRequestedModel(t *testing.T) {
	// Arrange
	prompts := &fakePrompts{err: errors.New("shim refused the model")}
	h := establishHandlerWithPrompts(t, prompts, &fakeSessionCmds{}, &probeHealthRouter{healthy: true})

	// Act
	_, err := h.CreateSession(context.Background(), "/w", "r1",
		&frontendv1.CreateSessionCmd{Cwd: "/w", Model: "opus"})

	// Assert
	if err == nil {
		t.Fatal("CreateSession must fail when the requested model is refused")
	}
	if !strings.Contains(err.Error(), "opus") {
		t.Fatalf("CreateSession error = %v, want the refused model named", err)
	}
}

// TestCreateSessionAcksOnlyAfterTheShimAnswersHealthy is the gate's central
// claim: the command does not return while the verdict is outstanding, and
// returns ok the moment it lands healthy.
func TestCreateSessionAcksOnlyAfterTheShimAnswersHealthy(t *testing.T) {
	// Arrange: a probe held open until this test releases it.
	release := make(chan struct{})
	router := &probeHealthRouter{healthy: true, gate: release}
	h := establishHandler(t, &fakeSessionCmds{}, router)

	// Act: the create runs while the verdict is outstanding.
	acked := make(chan error, 1)
	go func() {
		_, err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"})
		acked <- err
	}()

	// Assert: no ack while the shim has not answered.
	select {
	case err := <-acked:
		t.Fatalf("create acked (err=%v) before the shim's health verdict landed", err)
	case <-time.After(50 * time.Millisecond):
	}
	close(release)
	select {
	case err := <-acked:
		if err != nil {
			t.Fatalf("create must ack ok once the shim answers healthy, got %v", err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("create never acked after the shim answered healthy")
	}
}

// TestCreateSessionNackNamesTheDeepestLink walks the taxonomy, one case per
// link. The nack's job is to say WHICH hop is pending; a nack that only says
// the create failed sends its reader back to the logs.
func TestCreateSessionNackNamesTheDeepestLink(t *testing.T) {
	tests := []struct {
		name     string
		router   *probeHealthRouter
		sessions *fakeSessionCmds
		wantLink string
		wantType errclass.Type
	}{
		{
			name:     "no shim was ever brought up",
			router:   &probeHealthRouter{err: fmt.Errorf("session-controller: no live session for workspace %q: %w", "/w", errclass.ErrNoLiveSessionController)},
			wantLink: "no shim was ever brought up",
			wantType: errclass.TypeShimNotSpawned,
		},
		{
			name:     "the handshake never completed",
			router:   &probeHealthRouter{err: fmt.Errorf("session-controller: health: %w: %w", errclass.ErrShimNotReady, context.DeadlineExceeded)},
			wantLink: "never completed its handshake",
			wantType: errclass.TypeShimHandshakeIncomplete,
		},
		{
			name:     "the shim has no live connection",
			router:   &probeHealthRouter{err: fmt.Errorf("probe: %w", errclass.ErrShimNotConnected)},
			wantLink: "no live connection",
			wantType: errclass.TypeShimNotConnected,
		},
		{
			name:     "the shim never answered the probe",
			router:   &probeHealthRouter{err: fmt.Errorf("probe: %w", errclass.ErrShimAckTimeout)},
			wantLink: "did not answer the health probe in time",
			wantType: errclass.TypeShimAckTimeout,
		},
		{
			name:     "the shim's own verdict is unhealthy",
			router:   &probeHealthRouter{healthy: false, component: "shim-store", reason: "store subscription not settled"},
			wantLink: "reported itself unhealthy",
			wantType: errclass.TypeShimUnhealthy,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := establishHandler(t, &fakeSessionCmds{}, tc.router)

			// Act.
			_, err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"})

			// Assert: the link is named, and the classifier agrees with it.
			if err == nil {
				t.Fatal("create must nack when the session never establishes")
			}
			if !strings.Contains(err.Error(), tc.wantLink) {
				t.Fatalf("err = %v, want it to name %q", err, tc.wantLink)
			}
			got, ok := errclass.Sentinel(err)
			if !ok || got != tc.wantType {
				t.Fatalf("classified as (%q, %v), want %q", got, ok, tc.wantType)
			}
		})
	}
}

func TestExplicitResumeCreateClassifiesPostCreateEstablishmentFailures(t *testing.T) {
	queryFailure := &queryTerminationTestError{
		err: errors.New("resumed query terminated during readiness"),
		detail: &frontendv1.QueryTerminationFailure{
			AgentReplSessionId: "s_test",
			QueryInstanceId:    "query-resume",
			VendorIdentity: &frontendv1.QueryTerminationFailure_VendorSessionId{
				VendorSessionId: "claude-resume",
			},
		},
	}
	genericFailure := errors.New("health route failed during resumed readiness")
	tests := []struct {
		name  string
		cause error
		check func(*testing.T, *frontendv1.SessionResumeFailure)
	}{
		{
			name:  "typed query termination",
			cause: queryFailure,
			check: func(t *testing.T, detail *frontendv1.SessionResumeFailure) {
				t.Helper()
				got := detail.GetQueryTermination()
				if got == nil || got.GetQueryInstanceId() != "query-resume" || got.GetVendorSessionId() != "claude-resume" {
					t.Fatalf("query termination = %v", got)
				}
			},
		},
		{
			name:  "generic readiness failure",
			cause: genericFailure,
			check: func(t *testing.T, detail *frontendv1.SessionResumeFailure) {
				t.Helper()
				got := detail.GetBringUpFailure()
				if got == nil || !strings.Contains(got.GetCause(), genericFailure.Error()) {
					t.Fatalf("bring-up failure = %v, want cause containing %q", got, genericFailure)
				}
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — the create core returns a durable daemon session id, then
			// the correlated health/readiness gate reports the scripted failure.
			h := establishHandler(t, &fakeSessionCmds{}, &probeHealthRouter{err: tc.cause})

			// Act.
			_, err := h.CreateSession(context.Background(), "/w", "request-1", &frontendv1.CreateSessionCmd{
				Cwd:                     "/w",
				ConfigDir:               "/cfg",
				ResumeMode:              frontendv1.ResumeMode_RESUME_MODE_EXPLICIT,
				ExplicitClaudeSessionId: "claude-resume",
			})
			failure := errclass.Command(nil, err)
			detail := failure.GetSessionResume()

			// Assert.
			if !errors.Is(err, tc.cause) {
				t.Fatalf("CreateSession error = %v, want original chain to contain %v", err, tc.cause)
			}
			if failure.GetErrorType() != string(errclass.TypeSessionResumeFailed) || detail == nil || detail.GetCreate() == nil {
				t.Fatalf("failure = %v, want typed create resume failure", failure)
			}
			if detail.GetAgentReplSessionId() != "s_test" || detail.GetClaudeSessionId() != "claude-resume" || detail.GetCwd() != "/w" || detail.GetConfigDir() != "/cfg" || detail.GetResolvedConfigDir() != "/cfg" {
				t.Fatalf("failure evidence = %v", detail)
			}
			tc.check(t, detail)
		})
	}
}

func TestExplicitResumeCreateClassifiesCreateCoreBringUpFailure(t *testing.T) {
	// Arrange — the core has already assigned the durable daemon session id
	// when its eager shim bring-up fails.
	cause := errors.New("create core could not spawn resumed shim")
	router := &probeHealthRouter{healthy: true}
	h := establishHandler(t, &fakeSessionCmds{err: cause}, router)

	// Act.
	_, err := h.CreateSession(context.Background(), "/w", "request-1", &frontendv1.CreateSessionCmd{
		Cwd:                     "/w",
		ConfigDir:               "/cfg",
		ResumeMode:              frontendv1.ResumeMode_RESUME_MODE_EXPLICIT,
		ExplicitClaudeSessionId: "claude-resume",
	})
	failure := errclass.Command(nil, err)
	detail := failure.GetSessionResume()

	// Assert.
	if !errors.Is(err, cause) {
		t.Fatalf("CreateSession error = %v, want original core failure", err)
	}
	if failure.GetErrorType() != string(errclass.TypeSessionResumeFailed) || detail == nil || detail.GetCreate() == nil || detail.GetBringUpFailure() == nil {
		t.Fatalf("failure = %v, want typed create bring-up failure", failure)
	}
	if detail.GetAgentReplSessionId() != "s_test" || detail.GetClaudeSessionId() != "claude-resume" || detail.GetCwd() != "/w" || detail.GetConfigDir() != "/cfg" || detail.GetResolvedConfigDir() != "/cfg" {
		t.Fatalf("failure evidence = %v", detail)
	}
	if !strings.Contains(detail.GetBringUpFailure().GetCause(), cause.Error()) {
		t.Fatalf("bring-up failure = %v, want cause containing %q", detail.GetBringUpFailure(), cause)
	}
	if got := router.probeCount(); got != 0 {
		t.Fatalf("health probes = %d, want none after create-core failure", got)
	}
}

// TestCreateSessionNackCarriesTheShimsOwnReason keeps the shim's verdict verbatim
// in the nack: the component and reason are the only part a human can act on, and
// the daemon never re-words them.
func TestCreateSessionNackCarriesTheShimsOwnReason(t *testing.T) {
	// Arrange.
	router := &probeHealthRouter{healthy: false, component: "shim-store", reason: "store subscription not settled"}
	h := establishHandler(t, &fakeSessionCmds{}, router)

	// Act.
	_, err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"})

	// Assert.
	if err == nil {
		t.Fatal("an unhealthy verdict must nack the create")
	}
	for _, want := range []string{"shim-store", "store subscription not settled"} {
		if !strings.Contains(err.Error(), want) {
			t.Fatalf("err = %v, want it to carry %q", err, want)
		}
	}
}

// TestCreateSessionNackOnTheCallersDeadline proves the wait is bounded by the
// COMMAND's context and reports that bound rather than hanging on a wedged
// bring-up.
func TestCreateSessionNackOnTheCallersDeadline(t *testing.T) {
	// Arrange: a probe that never answers.
	router := &probeHealthRouter{healthy: true, gate: make(chan struct{})}
	h := establishHandler(t, &fakeSessionCmds{}, router)
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Millisecond)
	defer cancel()

	// Act.
	_, err := h.CreateSession(ctx, "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"})

	// Assert.
	if !errors.Is(err, errclass.ErrSessionNotEstablished) {
		t.Fatalf("err = %v, want the establishment-deadline sentinel", err)
	}
	if got, ok := errclass.Sentinel(err); !ok || got != errclass.TypeSessionNotEstablished {
		t.Fatalf("classified as (%q, %v), want %q", got, ok, errclass.TypeSessionNotEstablished)
	}
}

func TestExplicitResumeCallerDeadlineCarriesTypedContinuityEvidence(t *testing.T) {
	// Arrange: the shared round remains alive after this caller's bound ends.
	release := make(chan struct{})
	router := &probeHealthRouter{healthy: true, gate: release, entered: make(chan struct{}, 1)}
	h := establishHandler(t, &fakeSessionCmds{}, router)
	cmd := &frontendv1.CreateSessionCmd{
		Cwd:                     "/w",
		ConfigDir:               "/cfg",
		ResumeMode:              frontendv1.ResumeMode_RESUME_MODE_EXPLICIT,
		ExplicitClaudeSessionId: "claude-resume",
	}
	leader := make(chan error, 1)
	go func() { _, err := h.CreateSession(context.Background(), "/w", "leader", cmd); leader <- err }()
	awaitProbe(t, router)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := h.CreateSession(ctx, "/w", "follower", cmd)
	close(release)
	if leaderErr := <-leader; leaderErr != nil {
		t.Fatalf("leader: %v", leaderErr)
	}

	// Assert.
	failure := errclass.Command(nil, err)
	detail := failure.GetSessionResume()
	if !errors.Is(err, errclass.ErrSessionNotEstablished) || failure.GetErrorType() != string(errclass.TypeSessionResumeFailed) || detail == nil || detail.GetCreate() == nil || detail.GetBringUpFailure() == nil {
		t.Fatalf("failure = %v err=%v, want typed explicit-resume caller cancellation", failure, err)
	}
	if detail.GetClaudeSessionId() != "claude-resume" || detail.GetCwd() != "/w" || detail.GetConfigDir() != "/cfg" {
		t.Fatalf("continuity evidence = %v", detail)
	}
}

func TestExplicitResumeEnrollmentCancellationCarriesTypedContinuityEvidence(t *testing.T) {
	// Arrange: a different create owns the workspace slot, so the explicit
	// resume can fail before it joins or creates any agent session.
	release := make(chan struct{})
	router := &probeHealthRouter{healthy: true, gate: release, entered: make(chan struct{}, 1)}
	h := establishHandler(t, &fakeSessionCmds{}, router)
	leader := make(chan error, 1)
	go func() {
		_, err := h.CreateSession(context.Background(), "/w", "leader", &frontendv1.CreateSessionCmd{Cwd: "/w", ConfigDir: "/other"})
		leader <- err
	}()
	awaitProbe(t, router)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := h.CreateSession(ctx, "/w", "explicit", &frontendv1.CreateSessionCmd{
		Cwd:                     "/w",
		ConfigDir:               "/cfg",
		ResumeMode:              frontendv1.ResumeMode_RESUME_MODE_EXPLICIT,
		ExplicitClaudeSessionId: "claude-resume",
	})
	close(release)
	if leaderErr := <-leader; leaderErr != nil {
		t.Fatalf("leader: %v", leaderErr)
	}

	// Assert.
	failure := errclass.Command(nil, err)
	detail := failure.GetSessionResume()
	if !errors.Is(err, errclass.ErrSessionNotEstablished) || failure.GetErrorType() != string(errclass.TypeSessionResumeFailed) || detail == nil || detail.GetCreate() == nil || detail.GetBringUpFailure() == nil {
		t.Fatalf("failure = %v err=%v, want typed explicit-resume enrollment cancellation", failure, err)
	}
	if detail.GetClaudeSessionId() != "claude-resume" || detail.GetCwd() != "/w" || detail.GetConfigDir() != "/cfg" {
		t.Fatalf("continuity evidence = %v", detail)
	}
}

// TestCreateSessionNackOnTheEstablishmentsOwnDeadline covers the other bound: a
// caller who waits patiently is still answered, because the ROUND has a deadline
// of its own and a wedged bring-up must nack rather than hold every joined
// caller forever.
func TestCreateSessionNackOnTheEstablishmentsOwnDeadline(t *testing.T) {
	// Arrange: a probe that never answers, under a round bounded far below the
	// caller's own (absent) deadline.
	router := &probeHealthRouter{healthy: true, gate: make(chan struct{})}
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{Health: HealthConfig{Router: router}, EstablishTimeout: 30 * time.Millisecond, Resumes: &fakeResumes{}})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act: no caller deadline at all — only the round's bound can end this.
	nacked := make(chan error, 1)
	go func() {
		_, err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"})
		nacked <- err
	}()

	// Assert.
	select {
	case err := <-nacked:
		if err == nil || !errors.Is(err, context.DeadlineExceeded) {
			t.Fatalf("err = %v, want the round's own deadline reported", err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("a wedged establishment hung instead of nacking on its own deadline")
	}
}

// TestCreateSessionNackWhenTheHealthRouterIsUnwired keeps the unprovable case
// loud: a daemon that cannot ask the shim anything must not claim the session is
// established.
func TestCreateSessionNackWhenTheHealthRouterIsUnwired(t *testing.T) {
	// Arrange.
	h := establishHandler(t, &fakeSessionCmds{}, nil)

	// Act.
	_, err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "session health router is not wired") {
		t.Fatalf("err = %v, want a loud unwired-router nack", err)
	}
}

// TestCreateSessionNackOnAnUncorrelatedVerdict refuses a healthy answer that
// belongs to a different probe: an uncorrelated verdict proves nothing about
// THIS session.
func TestCreateSessionNackOnAnUncorrelatedVerdict(t *testing.T) {
	// Arrange.
	router := &probeHealthRouter{healthy: true, answerID: "somebody-elses-probe"}
	h := establishHandler(t, &fakeSessionCmds{}, router)

	// Act.
	_, err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "cannot be attributed to this create") {
		t.Fatalf("err = %v, want the create refused for a mis-correlated verdict", err)
	}
}

// TestCreateSessionNackDoesNotTearDownTheSession pins that a failed
// establishment REPORTS: the session the create core made stays made, because
// bring-up continues behind the nack.
func TestCreateSessionNackDoesNotTearDownTheSession(t *testing.T) {
	// Arrange.
	sessions := &fakeSessionCmds{}
	h := establishHandler(t, sessions, &probeHealthRouter{err: errclass.ErrShimNotConnected})

	// Act.
	if _, err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"}); err == nil {
		t.Fatal("want a nack")
	}

	// Assert.
	if len(sessions.created) != 1 {
		t.Fatalf("created = %v, want the create to have happened", sessions.created)
	}
	if len(sessions.deleted) != 0 {
		t.Fatalf("deleted = %v, want a nack that reports rather than tears down", sessions.deleted)
	}
}

// TestConcurrentIdenticalCreatesCoalesce is the anti-double-spawn claim: two
// frontends racing the same create produce ONE create and ONE probe, and both
// callers are answered.
func TestConcurrentIdenticalCreatesCoalesce(t *testing.T) {
	// Arrange: hold the probe so both callers are certainly in flight together.
	release := make(chan struct{})
	router := &probeHealthRouter{healthy: true, gate: release, entered: make(chan struct{}, 4)}
	enrolled := enrollments(t)
	sessions := &fakeSessionCmds{}
	h := establishHandler(t, sessions, router)
	cmd := &frontendv1.CreateSessionCmd{Cwd: "/w"}

	// Act: the second caller joins while the first is still establishing. Both
	// orderings are OBSERVED — the leader's probe arriving, then the follower's
	// enrollment completing — so the join cannot be a lucky interleaving.
	first := make(chan error, 1)
	go func() { _, err := h.CreateSession(context.Background(), "/w", "r1", cmd); first <- err }()
	awaitEnrollment(t, enrolled)
	awaitProbe(t, router)
	second := make(chan error, 1)
	go func() { _, err := h.CreateSession(context.Background(), "/w", "r2", cmd); second <- err }()
	awaitEnrollment(t, enrolled)
	close(release)

	// Assert: both answered, one create, one probe.
	for _, ch := range []chan error{first, second} {
		select {
		case err := <-ch:
			if err != nil {
				t.Fatalf("coalesced create failed: %v", err)
			}
		case <-time.After(5 * time.Second):
			t.Fatal("a coalesced caller was never answered")
		}
	}
	if len(sessions.created) != 1 {
		t.Fatalf("created = %v, want exactly one create for two identical concurrent commands", sessions.created)
	}
	if n := router.probeCount(); n != 1 {
		t.Fatalf("probes = %d, want exactly one establishment probe", n)
	}
}

// TestConcurrentCreatesWithDifferentOptsDoNotCoalesce is the other half of the
// rule: a create asking for a DIFFERENT account or resume target is not
// the same request, and answering it with the in-flight one's result would
// silently discard what it asked for.
func TestConcurrentCreatesWithDifferentOptsDoNotCoalesce(t *testing.T) {
	// Arrange.
	release := make(chan struct{})
	router := &probeHealthRouter{healthy: true, gate: release, entered: make(chan struct{}, 4)}
	enrolled := enrollments(t)
	sessions := &fakeSessionCmds{}
	h := establishHandler(t, sessions, router)

	// Act.
	first := make(chan error, 1)
	go func() {
		_, err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w", ConfigDir: "/cfg-a"})
		first <- err
	}()
	awaitEnrollment(t, enrolled)
	awaitProbe(t, router)
	second := make(chan error, 1)
	go func() {
		_, err := h.CreateSession(context.Background(), "/w", "r2", &frontendv1.CreateSessionCmd{Cwd: "/w", ConfigDir: "/cfg-b"})
		second <- err
	}()
	close(release)

	// Assert: two creates, each with the account root it asked for.
	for _, ch := range []chan error{first, second} {
		select {
		case err := <-ch:
			if err != nil {
				t.Fatalf("create failed: %v", err)
			}
		case <-time.After(5 * time.Second):
			t.Fatal("a create was never answered")
		}
	}
	if len(sessions.created) != 2 {
		t.Fatalf("created = %v, want both distinct creates to have run", sessions.created)
	}
	configs := map[string]bool{}
	for _, o := range sessions.created {
		configs[o.ConfigDir] = true
	}
	if !configs["/cfg-a"] || !configs["/cfg-b"] {
		t.Fatalf("created = %v, want one /cfg-a and one /cfg-b create", sessions.created)
	}
}

// TestCreateSessionWithNoCwdSkipsTheProbe keeps the workspace-less create
// working: nothing is brought up for it, so there is no shim to establish and
// probing the empty workspace would fail every such create.
func TestCreateSessionWithNoCwdSkipsTheProbe(t *testing.T) {
	// Arrange.
	router := &probeHealthRouter{healthy: true}
	h := establishHandler(t, &fakeSessionCmds{}, router)

	// Act.
	_, err := h.CreateSession(context.Background(), "", "r1", &frontendv1.CreateSessionCmd{})

	// Assert.
	if err != nil {
		t.Fatalf("err = %v, want a workspace-less create to succeed", err)
	}
	if n := router.probeCount(); n != 0 {
		t.Fatalf("probes = %d, want none for a session with no workspace", n)
	}
}

// TestEstablishmentSlotIsReleasedAfterEachRound proves the coalescing map does
// not leak: a workspace whose establishment finished accepts the next create as
// a fresh leader rather than joining a completed round forever.
func TestEstablishmentSlotIsReleasedAfterEachRound(t *testing.T) {
	// Arrange.
	sessions := &fakeSessionCmds{}
	h := establishHandler(t, sessions, &probeHealthRouter{healthy: true})
	cmd := &frontendv1.CreateSessionCmd{Cwd: "/w"}

	// Act: the same create twice, sequentially.
	for i, req := range []string{"r1", "r2"} {
		if _, err := h.CreateSession(context.Background(), "/w", req, cmd); err != nil {
			t.Fatalf("create %d: %v", i, err)
		}
	}

	// Assert.
	h.establishMu.Lock()
	remaining := len(h.establishing)
	h.establishMu.Unlock()
	if remaining != 0 {
		t.Fatalf("establishing holds %d rounds after both finished; the slot must be released", remaining)
	}
	if len(sessions.created) != 2 {
		t.Fatalf("created = %v, want two sequential creates", sessions.created)
	}
}

// awaitProbe blocks until the router has taken one more probe, so a test orders
// its callers by an EVENT rather than by a delay.
func awaitProbe(t *testing.T, r *probeHealthRouter) {
	t.Helper()
	select {
	case <-r.entered:
	case <-time.After(5 * time.Second):
		t.Fatal("the establishment probe was never issued")
	}
}

// enrollments installs the handler's enrollment seam for the duration of the
// test and returns the channel each completed enrollment lands on. It is what
// makes "the second caller had already joined" an observed fact.
func enrollments(t *testing.T) <-chan string {
	t.Helper()
	ch := make(chan string, 8)
	onEstablishEnroll = func(cwd string) { ch <- cwd }
	t.Cleanup(func() { onEstablishEnroll = nil })
	return ch
}

func awaitEnrollment(t *testing.T, ch <-chan string) {
	t.Helper()
	select {
	case <-ch:
	case <-time.After(5 * time.Second):
		t.Fatal("a caller never enrolled in an establishment round")
	}
}
