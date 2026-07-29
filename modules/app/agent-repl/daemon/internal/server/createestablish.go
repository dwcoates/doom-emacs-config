// createestablish.go holds the createSession command's ESTABLISHMENT GATE: the
// rule that a create is acked only once the new session's shim answers a health
// probe healthy over the fully wired connection.
//
// Why the gate exists. The create core registers the record and STARTS bring-up
// (sessiondrv.Ensure returns as soon as the connect loop is launched), so an ack
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
}

// CreateSession runs the shared create core for the command's cwd and acks ONLY
// once that session is fully established (see the file comment). Concurrent
// identical creates for one workspace coalesce onto a single establishment: one
// spawn, one probe, every caller answered.
//
// The wait is bounded twice over — by the establishment's own deadline and by
// the CALLER'S context — and neither bound is a hang: a caller whose context
// ends first is told so, and the establishment it was waiting on continues for
// whoever else is waiting.
func (h *commandHandler) CreateSession(ctx context.Context, workspace, requestID string, cmd *frontendv1.CreateSessionCmd) error {
	h.logf("frontend cmd: create_session ws=%s request_id=%s model=%s config_dir=%s resume=%q fake=%v permission_mode=%q allow_ungated=%v",
		workspace, requestID, cmd.GetModel(), cmd.GetConfigDir(), cmd.GetResumeClaudeSessionId(), cmd.GetFake(),
		cmd.GetPermissionMode(), cmd.GetAllowUngated())
	// THE PLACEHOLDER IS NOT A MODEL, and a create carrying it is refused here
	// rather than absorbed. Accepting it wrote `<synthetic>` onto the record,
	// which was then replayed onto the spawn argv on every respawn forever
	// after. Failing the command is what puts the problem in front of the user
	// at the moment it is caused, instead of leaving a session pinned to a
	// value nothing can be launched under.
	if registry.IsPlaceholderModel(cmd.GetModel()) {
		return fmt.Errorf("create_session for %q carries the CLI placeholder %q as its model, which is a marker rather than a model id; send a real model or none at all",
			cmd.GetCwd(), cmd.GetModel())
	}
	opts := CreateOpts{
		CWD:            cmd.GetCwd(),
		Model:          cmd.GetModel(),
		PermissionMode: cmd.GetPermissionMode(),
		ConfigDir:      cmd.GetConfigDir(),
		Resume:         cmd.GetResumeClaudeSessionId(),
		Fake:           cmd.GetFake(),
		AllowUngated:   cmd.GetAllowUngated(),
	}
	// A create is keyed by its cwd, and the frame's workspace field is optional
	// on this command — so diagnostics fall back to the cwd rather than naming
	// an empty workspace in every line a failed create leaves behind.
	if workspace == "" {
		workspace = opts.CWD
	}
	est, leader, err := h.beginEstablishment(ctx, opts)
	if err != nil {
		return fmt.Errorf("frontend cmd: create_session ws=%s request_id=%s: %w", workspace, requestID, err)
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
		return fmt.Errorf("frontend cmd: create_session ws=%s request_id=%s: %w: the caller's context ended first (%v); bring-up continues in the background",
			workspace, requestID, errclass.ErrSessionNotEstablished, ctx.Err())
	}
	if est.err != nil {
		return est.err
	}
	h.logf("frontend cmd: create_session ws=%s request_id=%s -> session=%s ESTABLISHED (shim wired and healthy)",
		workspace, requestID, est.sessionID)
	return nil
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
	if err != nil {
		// The create core failed before any session existed. Typed create
		// rejections (invalid mode, missing resume transcript) travel through
		// unchanged — the client keys real behavior on their text — and only a
		// bring-up failure gains the link name.
		est.err = fmt.Errorf("frontend cmd: create_session ws=%s request_id=%s: %w", workspace, requestID, err)
		h.logf("frontend cmd: create_session ws=%s request_id=%s FAILED in the create core: %v", workspace, requestID, err)
		return
	}
	est.sessionID = id
	est.err = h.awaitEstablished(ctx, est.opts.CWD, workspace, id, requestID)
}

// awaitEstablished drives the session-health path ONCE and returns nil only on a
// healthy verdict from the shim itself.
//
// A workspace-less create (no cwd) has no shim to establish — the create core
// registers the record and brings nothing up — so it is established by
// definition. That is stated here rather than left implicit, because the
// alternative reading (probe a workspace that is the empty string) would fail
// every such create with a message about a missing driver.
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
	{errclass.ErrNoLiveDriver, "no shim was ever brought up for this workspace"},
	{errclass.ErrNotLiveSession, "the workspace is driven by a different session than the one just created"},
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
