// createestablish.go holds the createSession command's ESTABLISHMENT GATE: the
// rule that a create is acked only once the new session's shim answers a health
// probe healthy over the fully wired connection.
//
// Why the gate exists. The create core registers the record and STARTS bring-up
// (sessioncontroller.Ensure returns as soon as the connect loop is launched), so an ack
// written at that point asserts only that a spawn was issued. Emacs papered over
// the gap by polling session health after every create, and that poll lost
// microsecond races — it probed before the shim attached, and before the shim's
// standing store subscription settled — so the create-then-poll shape was
// producing failures that named nothing and healed on retry. The ack now carries
// the meaning the poll was reaching for, and the poll is gone.
//
// What "established" means, exactly: the shim's OWN health verdict for the new
// session is healthy, obtained over the live connection. That is the strongest
// claim the daemon can make without inventing one, and since the bring-up
// handshake completes ALL shim wiring before ShimReady (lock, query, producer,
// standing store subscription at from_seq), a healthy verdict on the other side
// of AwaitReady means every link of the path is up.
//
// What a failure means: the establishment did NOT complete, and the nack names
// the deepest link it reached. Bring-up itself keeps running — the nack reports,
// it never tears down. A session that establishes a moment after its create
// nacked is a session the frontend will find on its next SessionView, which is
// strictly better than a create that destroyed its own in-flight work.
//
// THE ONE OUTCOME THAT IS NEITHER: a create whose bring-up met the revival gate
// (errclass.ErrSessionHibernated). The record is registered and durably asleep,
// no shim was spawned, and there is nothing to establish — so the round ends
// SUCCESSFULLY with the hibernated SessionView the gate is rendered from,
// rather than being classified as a resume failure and nacked. That is the same
// reading WorkspaceOpener.establish and the boot sweep already give the
// sentinel; the create was the last path that still called an ordinary sleep a
// continuity error, which is what turned every hibernated restore into a hard
// createSession failure and a client-side SessionView timeout.
package server

import (
	"context"
	"errors"
	"fmt"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// createEstablishTimeout bounds the whole create-plus-establish round for one
// workspace. It is a FAILURE bound, not a tuned delay: the ack is written the
// instant the verdict lands, and this only decides how long a caller waits
// before being told which link is still pending.
//
// It sits BELOW the Emacs client's own create timeout on purpose. Whichever
// bound fires first is the one the user reads, and the daemon's nack names the
// link while a client-side timeout can only say the daemon went quiet.
const createEstablishTimeout = 20 * time.Second

// sessionEstablishment is one in-flight create-plus-establish round for one
// workspace, shared by every caller that asked for the SAME create while it was
// running. Its fields are written by the leader before done is closed and read
// by every waiter after, so the close is the happens-before edge.
type sessionEstablishment struct {
	// opts is what the leader is creating. A create with DIFFERENT opts is not
	// the same request and must never be answered by this one's result (see
	// beginEstablishment): coalescing a mismatched create would silently
	// discard a caller's model, account, or resume target.
	opts CreateOpts
	done chan struct{}

	sessionID string
	err       error
	// hibernated reports the ONE bring-up outcome that ends this round
	// successfully WITHOUT a shim: the revival gate. It is a separate field
	// from err rather than a nil-error-plus-guess because the two facts a
	// waiter needs — "the create succeeded" and "there is nothing to drive
	// yet" — are different, and collapsing them is what made the establishment
	// probe a hibernated session and nack the create it had just completed.
	hibernated bool
}

// resumeModeFreshRetired is the RETIRED wire value tag 2 (RESUME_MODE_FRESH).
// The generated enum no longer names it — that is the point of reserving the
// tag — so the daemon names the raw number itself in order to REFUSE it. It is
// a named constant rather than a bare 2 at the switch arm because a bare
// number in a resume-mode switch reads like a bug.
const resumeModeFreshRetired = frontendv1.ResumeMode(2)

// resolveResume turns the caller's INTENT into the concrete conversation the
// create will land on.
//
// The caller says what it wants; the daemon decides what that means. CONTINUE
// is resolved here against the registry and the transcripts on disk, so no
// frontend has to remember a vendor uuid across restarts — which is the entire
// point of ResumeMode. See ConversationResolver for why the frontend's copy
// was the wrong authority.
// The second return reports whether the DAEMON resolved the target from its
// own records rather than the caller naming it; see
// CreateOpts.ResumeDaemonResolved for why the resume ladder must tell the two
// apart.
func (h *commandHandler) resolveResume(cmd *frontendv1.CreateSessionCmd) (string, bool, error) {
	mode := cmd.GetResumeMode()
	explicit := cmd.GetExplicitClaudeSessionId()

	// A uuid supplied under any mode but EXPLICIT is a caller that believes it
	// is steering. Ignoring it quietly would land the session somewhere the
	// caller did not ask for and say nothing, so it fails the create instead.
	if mode != frontendv1.ResumeMode_RESUME_MODE_EXPLICIT && explicit != "" {
		return "", false, fmt.Errorf("create_session carries explicit_claude_session_id=%q under resume_mode=%s: a conversation may only be named under RESUME_MODE_EXPLICIT", explicit, mode)
	}

	switch mode {
	case resumeModeFreshRetired:
		// AN OUT-OF-DATE CLIENT, NOT A CONFUSED ONE. Tag 2 was
		// RESUME_MODE_FRESH and it is retired (see the ResumeMode enum): a
		// caller may no longer ask for a workspace's conversation to be
		// replaced. Reading it as CONTINUE would be the daemon quietly
		// answering a different question than the one asked, and reading it as
		// "unknown mode" would tell the user their client is broken when it is
		// merely old. It is named, refused, and the remedy is stated.
		h.logf("frontend cmd: create_session cwd=%s resume_mode=2 REFUSED — RESUME_MODE_FRESH is retired and this daemon will not start a conversation over an existing one",
			cmd.GetCwd())
		return "", false, fmt.Errorf("%w: resume_mode=2 (RESUME_MODE_FRESH) was retired because a workspace's conversation is not caller-replaceable; update the client to send RESUME_MODE_CONTINUE",
			errclass.ErrResumeModeRetired)

	case frontendv1.ResumeMode_RESUME_MODE_EXPLICIT:
		if explicit == "" {
			return "", false, fmt.Errorf("create_session resume_mode=EXPLICIT requires explicit_claude_session_id")
		}
		h.logf("frontend cmd: create_session cwd=%s resume_mode=EXPLICIT uuid=%s — a caller named this conversation, so no resolution is attempted",
			cmd.GetCwd(), explicit)
		return explicit, false, nil

	case frontendv1.ResumeMode_RESUME_MODE_UNSPECIFIED, frontendv1.ResumeMode_RESUME_MODE_CONTINUE:
		// Unwired resolver is a LOUD refusal, never a quiet fresh start:
		// silently beginning a new conversation on top of an intact one is the
		// data-loss this whole mechanism exists to prevent, and it must not be
		// reachable by forgetting to wire a dependency.
		if h.resumes == nil {
			return "", false, fmt.Errorf("create_session resume_mode=%s needs a conversation resolver and none is wired; refusing rather than silently starting a fresh conversation over an existing one", mode)
		}
		resume, ok := h.resumes.ResolveResume(cmd.GetConfigDir(), cmd.GetCwd())
		if ok {
			return resume, true, nil
		}
		// NOTHING RESOLVED. That used to end here with `return "", nil` — a
		// blank conversation, indistinguishable on the wire and in the log from
		// a genuinely new workspace. It is exactly the fallback the ruling
		// removes: "the resolver found nothing" covers a brand-new workspace
		// AND a workspace whose every candidate was excluded (a stale delete
		// tombstone, a pruned record), and only the first of those may start
		// fresh.
		//
		// The evidence decides, and it is the daemon's own durable record. The
		// same check runs again at the spawn chokepoint (ShimSpawner.EnsureShim),
		// which is where the structural gate lives; it runs HERE too because a
		// create that is going to be refused should be refused by the command
		// that asked, naming the workspace, rather than surfacing later as a
		// bring-up failure.
		evidence := h.resumes.ConversationEvidence(cmd.GetConfigDir(), cmd.GetCwd())
		proof, reason := proveFreshEligible(evidence)
		if proof == nil {
			h.logf("frontend cmd: create_session cwd=%s config_dir=%s REFUSED — the resolver named no resumable conversation and %s",
				cmd.GetCwd(), cmd.GetConfigDir(), reason)
			return "", false, unresumableConversation(cmd.GetCwd(), cmd.GetConfigDir(), reason)
		}
		h.logf("frontend cmd: create_session cwd=%s config_dir=%s starts a FRESH conversation — the registry proves this workspace has never run one",
			cmd.GetCwd(), cmd.GetConfigDir())
		return "", false, nil

	default:
		return "", false, fmt.Errorf("create_session carries unknown resume_mode=%d", int32(mode))
	}
}

func (h *commandHandler) CreateSession(ctx context.Context, workspace, requestID string, cmd *frontendv1.CreateSessionCmd) (string, error) {
	h.logf("frontend cmd: create_session ws=%s request_id=%s config_dir=%s resume_mode=%s fake=%v permission_mode=%q allow_ungated=%v model=%q",
		workspace, requestID, cmd.GetConfigDir(), cmd.GetResumeMode(), cmd.GetFake(),
		cmd.GetPermissionMode(), cmd.GetAllowUngated(), cmd.GetModel())
	resume, daemonResolved, err := h.resolveResume(cmd)
	if err != nil {
		return "", fmt.Errorf("frontend cmd: create_session ws=%s request_id=%s: %w", workspace, requestID, err)
	}
	opts := CreateOpts{
		CWD:                  cmd.GetCwd(),
		PermissionMode:       cmd.GetPermissionMode(),
		ConfigDir:            cmd.GetConfigDir(),
		Resume:               resume,
		ResumeDaemonResolved: daemonResolved,
		Fake:                 cmd.GetFake(),
		AllowUngated:         cmd.GetAllowUngated(),
	}
	// A create is keyed by its cwd, and the frame's workspace field is optional
	// on this command — so diagnostics fall back to the cwd rather than naming
	// an empty workspace in every line a failed create leaves behind.
	if workspace == "" {
		workspace = opts.CWD
	}
	// Construct the one exact-resume classifier before enrollment. Failures can
	// occur while waiting for a conflicting round or while this caller waits on
	// a joined round, before any durable agent session id exists; those paths
	// are still failures of the caller's named Claude resume and must carry its
	// continuity evidence.
	resumeEstablishment := createResumeEstablishment(opts, "")
	est, leader, err := h.beginEstablishment(ctx, opts)
	if err != nil {
		return "", fmt.Errorf("frontend cmd: create_session ws=%s request_id=%s: %w", workspace, requestID,
			resumeEstablishment.classify(err))
	}
	if leader {
		go h.runEstablishment(est, workspace, requestID)
	} else {
		h.logf("frontend cmd: create_session ws=%s request_id=%s JOINED the establishment already in flight for this workspace — no second spawn, no second probe",
			workspace, requestID)
	}
	select {
	case <-est.done:
	case <-ctx.Done():
		waitErr := fmt.Errorf("%w: the caller's context ended first (%v); bring-up continues in the background",
			errclass.ErrSessionNotEstablished, ctx.Err())
		return "", fmt.Errorf("frontend cmd: create_session ws=%s request_id=%s: %w",
			workspace, requestID, resumeEstablishment.classify(waitErr))
	}
	if est.err != nil {
		return "", est.err
	}
	// Apply the requested starting model through the SAME path a later change
	// takes, now that the shim is wired and driveable. Doing it here instead of
	// leaving it to the caller closes the window the two-step has: a create acks
	// ESTABLISHED, so the caller may submit a turn immediately, and that turn
	// would run on whatever model the shim defaulted to.
	//
	// A model the shim refuses fails the create. Returning success here would
	// hand back a live session quietly running a model the caller did not ask
	// for, which is worse than a create the caller can retry.
	// Normalized here, not just checked for emptiness: a PLACEHOLDER model
	// ("<synthetic>") normalizes away to "", and it means "unspecified" exactly
	// as an absent field does. Passing it through would make SetModel refuse an
	// empty model and fail a create that asked for nothing at all.
	//
	// A HIBERNATED create has no shim to carry the change, and SetModel would
	// fail the create the revival gate just completed. Said out loud rather
	// than skipped quietly: the caller asked for a model, and it is the
	// revival — not this create — that will apply one.
	if requested := registry.NormalizeModel(cmd.GetModel()); requested != "" {
		if est.hibernated {
			h.logf("frontend cmd: create_session ws=%s request_id=%s session=%s model requested=%q NOT applied: the session met the revival gate at bring-up, so no shim exists to apply it to; the record keeps its persisted model until the user revives it",
				workspace, requestID, est.sessionID, requested)
		} else {
			selected, modelErr := h.SetModel(ctx, workspace, requestID, &frontendv1.SetModelCmd{Model: requested})
			if modelErr != nil {
				return "", fmt.Errorf("frontend cmd: create_session ws=%s request_id=%s session=%s: requested model %q not applied: %w",
					workspace, requestID, est.sessionID, requested, modelErr)
			}
			h.logf("frontend cmd: create_session ws=%s request_id=%s session=%s model requested=%q selected=%q",
				workspace, requestID, est.sessionID, requested, selected)
		}
	}
	// OBSERVABILITY ONLY, and read AFTER establishment so it reflects what the
	// session actually landed on rather than what was asked for. It rides the
	// ack purely so a client can attribute its logs before the first pushed
	// SessionView; nothing may persist it or feed it back (see ResumeMode).
	var observed string
	if h.resumes != nil {
		observed = h.resumes.ObservedClaudeSessionID(est.sessionID)
	}
	// ONE outcome line for the round, and it states which of the two completed
	// creates this was. A hibernated create that logged ESTABLISHED would claim
	// a wired, healthy shim for a session that has none.
	verdict := "ESTABLISHED (shim wired and healthy)"
	if est.hibernated {
		verdict = "HIBERNATED (no shim spawned; the revival gate stands)"
	}
	h.logf("frontend cmd: create_session ws=%s request_id=%s -> session=%s %s observed_claude_session_id=%q",
		workspace, requestID, est.sessionID, verdict, observed)
	return observed, nil
}

// onEstablishEnroll, when set, is called with the workspace each time a caller
// has finished enrolling in a round. It is a TEST SEAM and nothing reads it in
// production: the coalescing claim is about a caller that has already enrolled,
// and enrollment completing is not observable from outside the handler — the
// alternative is a timing guess, which is exactly what this gate exists to
// eliminate.
var onEstablishEnroll func(cwd string)

// beginEstablishment enrolls this caller in an establishment for opts.CWD,
// returning the round to wait on and whether this caller must RUN it.
//
// An identical create already in flight is joined. A DIFFERENT create for the
// same workspace waits for the in-flight one to finish and then runs its own —
// never coalesced, because it is a different request, and never run
// concurrently, because two creates racing on one workspace would supersede each
// other mid-bring-up.
func (h *commandHandler) beginEstablishment(ctx context.Context, opts CreateOpts) (*sessionEstablishment, bool, error) {
	defer func() {
		if onEstablishEnroll != nil {
			onEstablishEnroll(opts.CWD)
		}
	}()
	for {
		h.establishMu.Lock()
		// Lazy under the lock rather than in the constructor: focused harnesses
		// build a commandHandler literal, and a handler that panics depending on
		// how it was constructed is a worse contract than one extra check.
		if h.establishing == nil {
			h.establishing = make(map[string]*sessionEstablishment)
		}
		inflight, busy := h.establishing[opts.CWD]
		if busy && inflight.opts == opts {
			h.establishMu.Unlock()
			return inflight, false, nil
		}
		if !busy {
			est := &sessionEstablishment{opts: opts, done: make(chan struct{})}
			h.establishing[opts.CWD] = est
			h.establishMu.Unlock()
			return est, true, nil
		}
		h.establishMu.Unlock()
		select {
		case <-inflight.done:
		case <-ctx.Done():
			return nil, false, fmt.Errorf("%w: a different create for %q was still in flight when this caller's context ended (%v)",
				errclass.ErrSessionNotEstablished, opts.CWD, ctx.Err())
		}
	}
}

// runEstablishment is the leader's half: create, then prove. It always releases
// the workspace's slot and closes done, so a waiter can never be stranded by a
// failure on this path.
func (h *commandHandler) runEstablishment(est *sessionEstablishment, workspace, requestID string) {
	defer func() {
		h.establishMu.Lock()
		if h.establishing[est.opts.CWD] == est {
			delete(h.establishing, est.opts.CWD)
		}
		h.establishMu.Unlock()
		close(est.done)
	}()
	// The establishment runs under its OWN deadline rather than the leader's
	// context: every joined caller is waiting on this one result, and letting
	// the first caller's cancellation end it would strand the rest.
	bound := h.establishTimeout
	if bound <= 0 {
		bound = createEstablishTimeout
	}
	ctx, cancel := context.WithTimeout(context.Background(), bound)
	defer cancel()

	id, err := h.sessions.CreateSession(ctx, est.opts)
	resumeEstablishment := createResumeEstablishment(est.opts, id)
	if errors.Is(err, errclass.ErrSessionHibernated) {
		// THE REVIVAL GATE IS NOT A RESUME FAILURE. The bring-up found the
		// record past the keep-alive policy's threshold and hibernated it
		// instead of spawning, so the session is registered, durably asleep,
		// and its SessionView has already been pushed by the create core.
		// Classifying this would hand the caller a continuity error card for a
		// session that is simply asleep, and probing its (nonexistent) shim
		// below would nack a create that completed.
		est.sessionID = id
		est.hibernated = true
		return
	}
	if err != nil {
		// The create core may reject the request before assigning an id, or it
		// may return the id of a durable record whose bring-up failed. The resume
		// authority preserves that distinction and attaches every identity the
		// core returned.
		est.err = fmt.Errorf("frontend cmd: create_session ws=%s request_id=%s: %w", workspace, requestID,
			resumeEstablishment.classify(err))
		h.logf("frontend cmd: create_session ws=%s request_id=%s FAILED in the create core: %v", workspace, requestID, err)
		return
	}
	est.sessionID = id
	est.err = resumeEstablishment.classify(h.awaitEstablished(ctx, est.opts.CWD, workspace, id, requestID))
}

// awaitEstablished drives the session-health path ONCE and returns nil only on a
// healthy verdict from the shim itself.
//
// A workspace-less create (no cwd) has no shim to establish — the create core
// registers the record and brings nothing up — so it is established by
// definition. That is stated here rather than left implicit, because the
// alternative reading (probe a workspace that is the empty string) would fail
// every such create with a message about a missing controller.
func (h *commandHandler) awaitEstablished(ctx context.Context, cwd, workspace, sessionID, requestID string) error {
	if cwd == "" {
		h.logf("frontend cmd: create_session ws=%s request_id=%s -> session=%s registered with no cwd; nothing is brought up for a workspace-less session, so there is no shim to establish",
			workspace, requestID, sessionID)
		return nil
	}
	if h.health == nil {
		return fmt.Errorf("frontend cmd: create_session ws=%s request_id=%s: session %s was created but the session health router is not wired, so the daemon cannot prove the shim is established",
			workspace, requestID, sessionID)
	}
	probeID := requestID + ":establish"
	status, err := h.health.Health(ctx, cwd, sessionID, probeID)
	if err != nil {
		h.logf("frontend cmd: create_session ws=%s request_id=%s session=%s NOT ESTABLISHED at %s: %v",
			workspace, requestID, sessionID, establishLink(err), err)
		return fmt.Errorf("frontend cmd: create_session ws=%s request_id=%s: session %s is not established — %s: %w",
			workspace, requestID, sessionID, establishLink(err), err)
	}
	if status.GetRequestId() != probeID {
		return fmt.Errorf("frontend cmd: create_session ws=%s request_id=%s: session %s establishment probe answered for request_id=%q, want %q; the verdict cannot be attributed to this create",
			workspace, requestID, sessionID, status.GetRequestId(), probeID)
	}
	if !status.GetHealthy() {
		reason := status.GetReason()
		if reason == "" {
			reason = "the shim named no reason"
		}
		h.logf("frontend cmd: create_session ws=%s request_id=%s session=%s NOT ESTABLISHED: shim verdict unhealthy component=%q reason=%q",
			workspace, requestID, sessionID, status.GetComponent(), reason)
		return fmt.Errorf("frontend cmd: create_session ws=%s request_id=%s: session %s is not established — %w (component=%q): %s",
			workspace, requestID, sessionID, errclass.ErrShimUnhealthy, status.GetComponent(), reason)
	}
	return nil
}

// establishLinks is the daemon->shim path as an ORDERED ladder, deepest hop
// first. Order is the whole contract: a handshake that ran out of time carries
// both its own link and the deadline, and reporting the deadline would name the
// clock instead of the hop that was still pending.
var establishLinks = []struct {
	err  error
	link string
}{
	{errclass.ErrShimUnhealthy, "the shim answered and reported itself unhealthy"},
	{errclass.ErrShimNotReady, "the shim never completed its handshake, so its lock, query, producer, and store subscription are not all wired"},
	{errclass.ErrShimNack, "the shim rejected the health probe"},
	{errclass.ErrShimAckTimeout, "the shim did not answer the health probe in time"},
	{errclass.ErrShimNotConnected, "the shim has no live connection to this daemon"},
	{errclass.ErrNoLiveSessionController, "no shim was ever brought up for this workspace"},
	{errclass.ErrNotLiveSession, "the workspace is controlled by a different session than the one just created"},
	{errclass.ErrSessionNotEstablished, "the establishment deadline elapsed with no verdict"},
	{context.DeadlineExceeded, "the establishment deadline elapsed with no verdict"},
}

// establishLink names the deepest link a failed establishment reached. An error
// matching nothing is reported as such rather than silently rendered as one of
// the known links: an unnamed link is a gap in this ladder, and saying so is how
// the gap gets closed.
func establishLink(err error) string {
	for _, l := range establishLinks {
		if errors.Is(err, l.err) {
			return l.link
		}
	}
	return "bring-up failed at an unclassified link"
}
