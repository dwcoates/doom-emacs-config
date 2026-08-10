package sessioncontroller

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"fmt"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/registry"
)

// revive.go — BRINGING A HIBERNATED SESSION BACK, on the user's stated terms.
//
// Revival is LAZY AND GATED. The daemon nacks every prompt for a hibernated
// session (hibernation.go), so no model use can precede the choice: the webapp
// renders the gate from SessionView.hibernation and sends exactly one
// ReviveSessionCmd. That is the whole point of the feature — a session with six
// hours of accumulated context is expensive to resume, and the user should be
// told the price before paying it rather than after.
//
// THE MODES DIFFER IN WHEN THE GATE COMES DOWN, not in how the session is
// brought up. All of them take the ordinary create/resume path.
//
//   - DIRECT clears the durable hibernation first, then brings up. The gate is
//     gone the moment the record is written, and the session behaves exactly as
//     any other live session does.
//
//   - EVERY CUTTING MODE brings up while the record STILL SAYS HIBERNATED,
//     drives its context cut to completion, and only then clears. Keeping the
//     record is what keeps the gate standing, so "prompts are refused until the
//     cut lands" is the same mechanism that refused them before the revival
//     began rather than a second, parallel gate that could disagree with it.
//
// THE CUTTING MODES ARE ONE PATH, parameterized by reviveCut. The four
// compacting modes differ only in their submitted text — the scoped three carry
// instructions naming what the summary must leave verbatim — and CLEAR differs
// additionally in the axis it waits on and in taking no compaction claim, since
// `/clear` keeps nothing and reads nothing. There is no second cut machinery,
// which is why a new scope is a new command string and nothing else.
//
// AND THAT IS WHY A FAILED CUT LEAVES THE SESSION GATED. The hibernation clear
// is the LAST step and happens only on the completion signal; there is no path
// in which a cut that errored, timed out, or never reported completion ends
// with an ungated session. The session limps into nothing — it stays asleep,
// loudly, and the user can choose again.

// compactFirstBound bounds how long a compact-first revival waits for the
// compaction to complete. It is a FAILURE bound, not a tuned delay: the wait
// ends the instant the compacting axis closes.
//
// Generous, because a compaction over a six-hour conversation is a model call
// across the whole history and is legitimately slow. Expiring it leaves the
// session gated, which is the safe direction.
const compactFirstBound = 10 * time.Minute

// compactCommandText is the prompt an UNSCOPED compaction submits. It is
// ordinary prompt text: sessioncommand.go recognizes it, promptdispatch.go
// forwards it verbatim, and the CLI runs the compaction. The daemon does not
// need a control frame for something the conversation surface already has.
const compactCommandText = "/compact"

// The SCOPED compactions' prompts, which are the same session command with the
// steering the harness already accepts (`/compact <instructions>`) — and which
// sessioncommand.go recognizes as SESSION_COMMAND_COMPACT precisely because
// that entry takes arguments, so a scoped revival compaction earns no more of a
// prompt bubble than an unscoped one does.
//
// EACH ONE NAMES WHAT SURVIVES, not only what goes. An instruction that said
// only "summarize the responses" leaves the harness to guess whether the rest
// is fair game, and the whole point of the scope is that it is not.
const (
	compactResponsesCommandText = compactCommandText +
		" Summarize ONLY the assistant's own response messages. Preserve every user prompt," +
		" every tool call, and every tool result verbatim."
	compactPromptsCommandText = compactCommandText +
		" Summarize ONLY the user's prompt messages. Preserve every assistant response," +
		" every tool call, and every tool result verbatim."
	compactPromptsAndResponsesCommandText = compactCommandText +
		" Summarize ONLY the user's prompt messages and the assistant's own response messages." +
		" Preserve every tool call and every tool result verbatim."
)

// clearCommandText is the prompt a CLEAR revival submits, on exactly the terms
// compactCommandText is submitted under. It carries no argument and must not:
// sessioncommand.go recognizes `/clear` only as the ENTIRE prompt, because the
// command discards the conversation rather than summarizing it.
const clearCommandText = "/clear"

// ReviveMode is the user's revival choice. It has no zero value that means
// anything: the wire oneof makes "no decision" unrepresentable, and so does
// this — Revive refuses a mode it was not given.
//
// ONE VALUE IS ONE WHOLE DECISION, deliberately, rather than a direct/compact
// flag beside a separate scope. A scope only means anything to a compaction, so
// carrying the two apart would make "resume as-is, summarizing the prompts"
// representable — a combination with no meaning that every reader of the pair
// would then have to rule out.
type ReviveMode int

const (
	// ReviveModeUnset is the refused zero.
	ReviveModeUnset ReviveMode = iota
	// ReviveModeDirect resumes the conversation as-is, full accumulated
	// context and all. The deliberate "I know it's big" path.
	ReviveModeDirect
	// ReviveModeCompactAll compacts the whole conversation before accepting any
	// prompt, paying the full-context cost ONCE instead of on every subsequent
	// turn.
	ReviveModeCompactAll
	// ReviveModeCompactResponses summarizes only the assistant's responses and
	// keeps the prompts, the tool calls, and the tool results verbatim.
	ReviveModeCompactResponses
	// ReviveModeCompactPrompts summarizes only the user's prompts and keeps
	// everything the agent produced verbatim.
	ReviveModeCompactPrompts
	// ReviveModeCompactPromptsAndResponses summarizes the conversation's prose —
	// prompts and responses both — and keeps the work verbatim.
	ReviveModeCompactPromptsAndResponses
	// ReviveModeClear discards the conversation before accepting any prompt. It
	// pays the full-context cost NEVER: nothing is summarized and nothing is
	// carried, so the woken session starts from an empty conversation in the
	// same workspace. It is not a fifth scope — a scope says what a summary
	// keeps, and this keeps nothing.
	ReviveModeClear
)

func (m ReviveMode) String() string {
	switch m {
	case ReviveModeDirect:
		return "direct"
	case ReviveModeCompactAll:
		return "compact_all"
	case ReviveModeCompactResponses:
		return "compact_responses"
	case ReviveModeCompactPrompts:
		return "compact_prompts"
	case ReviveModeCompactPromptsAndResponses:
		return "compact_prompts_and_responses"
	case ReviveModeClear:
		return "clear"
	default:
		return "unset"
	}
}

// cutWaiter is ONE gated revival's one-shot expectation of a context cut: the
// func the closing edge fires, and the token that identifies whose expectation
// it is.
//
// The token exists because funcs are not comparable in Go, so a disarm has no
// other way to tell its own waiter from a later one, and clearing whatever it
// happens to find would retire a revival that is still waiting.
type cutWaiter struct {
	fire  func()
	token *struct{}
}

// fireCutWaiter delivers one closing edge to whatever revival is waiting on it,
// and is a no-op when none is. It exists so the two closing edges in
// noteCutCompleted (sinks.go) state the same thing the same way rather than
// each carrying its own nil check.
func (c *consumer) fireCutWaiter(w *cutWaiter) {
	if w.fire != nil {
		w.fire()
	}
}

// reviveCut is the context cut ONE gated revival mode drives, and the whole of
// what differs between the gated modes.
//
// IT IS A DESCRIPTOR RATHER THAN A BRANCH so that adding a cut cannot add a
// second copy of the bring-up-arm-submit-hand-off sequence. Everything that
// sequence guarantees — the gate stands until the closing edge, every failing
// exit drops what it parked, the claim outlives the ack — is written once.
type reviveCut struct {
	// text is the session command submitted as ordinary prompt text.
	text string
	// requestIDPrefix names the submitted turn on the wire and in the log.
	requestIDPrefix string
	// claimsCompaction takes the cold-read alarm's claim over the submitted
	// turn. TRUE for every compaction: a revival compaction runs after the
	// cache has expired by construction, which makes it the likeliest cold read
	// in the daemon. FALSE for `/clear`, which is not a model call at all — it
	// reads nothing, so claiming it would hand the NEXT turn's input cost to a
	// turn that read nothing, the exact misattribution the claim prevents.
	claimsCompaction bool
	// waiter selects the consumer field this cut's own closing edge fires
	// (sinks.go, noteCutCompleted). A compaction and a clear close DIFFERENT
	// axes, and the fields are separate so a cut of one kind can never release
	// a revival that asked for the other.
	waiter func(*consumer) *cutWaiter
}

// compactionCut is the shared shape of all four compacting modes: same waiter,
// same claim, same id prefix, differing only in the steering text.
func compactionCut(text string) reviveCut {
	return reviveCut{
		text:             text,
		requestIDPrefix:  "revive-compact:",
		claimsCompaction: true,
		waiter:           func(c *consumer) *cutWaiter { return &c.compactedWaiter },
	}
}

// cuts reports whether the mode revives by cutting the context first. Every
// mode but direct does, which is why the gated path is reached by exclusion
// rather than by a list a fifth scope could be forgotten from.
func (m ReviveMode) cuts() bool {
	return m != ReviveModeUnset && m != ReviveModeDirect
}

// compacts reports whether the mode's context cut is a COMPACTION — a
// whole-conversation model call that summarizes rather than discards.
//
// IT IS DERIVED FROM THE CUT rather than listing the compacting modes a second
// time. `claimsCompaction` is already the one place that says which cuts are
// model calls over the whole history — it is what takes the cold-read alarm's
// claim — and a fifth compacting scope added to cut() is a scope this answers
// for without being edited. A mode with no cut compacts nothing.
func (m ReviveMode) compacts() bool {
	cut, err := m.cut()
	return err == nil && cut.claimsCompaction
}

// cut is the context cut this mode drives.
//
// A NON-CUTTING MODE IS AN INVARIANT VIOLATION, not a case to default: the only
// caller reaches here after ReviveSession has already routed direct and unset
// elsewhere, so a mode arriving here with no cut is the routing having broken,
// and it says so rather than quietly submitting a whole-conversation `/compact`
// the user never asked for.
func (m ReviveMode) cut() (reviveCut, error) {
	switch m {
	case ReviveModeCompactAll:
		return compactionCut(compactCommandText), nil
	case ReviveModeCompactResponses:
		return compactionCut(compactResponsesCommandText), nil
	case ReviveModeCompactPrompts:
		return compactionCut(compactPromptsCommandText), nil
	case ReviveModeCompactPromptsAndResponses:
		return compactionCut(compactPromptsAndResponsesCommandText), nil
	case ReviveModeClear:
		return reviveCut{
			text:             clearCommandText,
			requestIDPrefix:  "revive-clear:",
			claimsCompaction: false,
			waiter:           func(c *consumer) *cutWaiter { return &c.clearedWaiter },
		}, nil
	default:
		return reviveCut{}, fmt.Errorf("session-controller: revival mode %s has no context cut; only a cutting mode reaches the gated path", m)
	}
}

// ErrRevivalInFlight reports a revival asked for while another is already
// running on the same workspace.
//
// IT IS HIBERNATION'S CLAIM, MIRRORED. Two ReviveSessionCmds can genuinely
// arrive at once — a double click on the gate, a webapp and an editor both
// holding the same stale SessionView — and without a claim both would drive
// their own `/compact` under the SAME request id, and the second would
// overwrite the first's completion waiter, leaving that revival waiting out its
// whole bound for a signal that had already been delivered to a channel nobody
// held.
var ErrRevivalInFlight = errors.New("session-controller: a revival is already in flight for this workspace")

// newReviveCutRequestID mints the identity ONE revival's context cut submits
// under. The session id is carried for readability; the random suffix is what
// makes the id unique across the repeated hibernate/revive cycles a single
// session goes through, and an entropy failure is surfaced rather than papered
// over with a weaker id (newSecureControllerGenerationID's discipline).
func newReviveCutRequestID(prefix, sessionID string) (string, error) {
	var raw [8]byte
	if _, err := rand.Read(raw[:]); err != nil {
		return "", fmt.Errorf("session-controller: mint revive cut request id for session %s: %w", sessionID, err)
	}
	return prefix + sessionID + ":" + hex.EncodeToString(raw[:]), nil
}

// ReviveSession brings a hibernated workspace back under the user's chosen
// mode.
//
// THE ACK IS AT ACCEPTANCE, NOT AT COMPLETION. Returning means "the revival was
// accepted, the session is up, and the compaction has been submitted" — never
// "the compaction finished". A compact-first revival's compaction is a model
// call across the whole conversation and is bounded at compactFirstBound, so
// acking at completion left the user's ReviveSessionCmd unacked for up to ten
// minutes on the one path that has something to report.
//
// THE GATE'S RELEASE HAS ITS OWN CHANNEL and does not need this one: the
// hibernation record is what stands the gate up, and clearing it publishes a
// SessionView the webapp already renders from. The completion wait therefore
// runs on past this return, holding the revival claim so a second revival is
// still nacked until the compaction lands or its bound expires.
func (m *Manager) ReviveSession(ctx context.Context, workspace string, mode ReviveMode) error {
	if mode == ReviveModeUnset {
		return fmt.Errorf("session-controller: refusing to revive workspace %q with no revival mode; the choice between compacting, clearing and resuming as-is is the user's and the daemon does not have a default for it", workspace)
	}
	if m.cfg.Hibernations == nil {
		return fmt.Errorf("session-controller: cannot revive workspace %q: no hibernation registrar is wired", workspace)
	}
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return fmt.Errorf("session-controller: workspace %q has no session to revive", workspace)
	}
	release, detail, err := m.claimRevival(workspace, sessionID)
	if err != nil {
		return err
	}
	// OWNERSHIP OF THE CLAIM MOVES AT HANDOFF. Every exit taken here releases
	// it; once the compaction completion wait is detached, that goroutine owns
	// the release instead, because the claim must outlive this return for as
	// long as the compaction is pending.
	detached := false
	defer func() {
		if !detached {
			// EVERY PRE-HANDOFF EXIT DISPOSES OF WHAT IT PARKED. A compact-first
			// revival brings the session up before it submits, so a prompt can
			// arrive and be parked in the window between the bring-up and a
			// failure — and this release is the last thing that knows a revival
			// was ever in flight. Dropping here is what keeps a refused submit
			// or a closing manager from leaving an entry no path can deliver.
			m.dropRevivalHolds(workspace, sessionID, "the revival did not reach its context cut")
			release()
		}
	}()
	if detail.Cause == "" {
		// NOT AN ERROR TO REPORT AS A FAILURE, but not silently successful
		// either: the user acted on a gate that is no longer standing, and
		// saying so is more useful than pretending to have revived something
		// that was never asleep.
		m.logf("session-controller: revive ws=%q session=%s mode=%s — the session is not hibernated; nothing to revive",
			workspace, sessionID, mode)
		return nil
	}
	m.logf("session-controller: revive BEGIN ws=%q session=%s mode=%s slept_since_ms=%d cause=%s",
		workspace, sessionID, mode, detail.SinceMs, detail.Cause)

	// A COMPACTION THIS SESSION HAS ALREADY HAD IS NOT PERFORMED A SECOND TIME.
	// The ordinary road to this line is warm-compact, hibernate, revive — the
	// warm compaction ran on the cache clock while the user was away, so the
	// conversation the gate offers to compact is already a summary, and
	// compacting it again would read the whole history to produce a worse
	// summary of the same material.
	//
	// IT IS A DOWNGRADE TO DIRECT, NOT A REFUSAL. The user asked for two things
	// — compact this, and bring it back — and exactly one of them is already
	// done. Refusing the whole command would leave the session asleep behind a
	// gate whose compacting choices all decline, which is a worse answer to
	// "revive my session" than simply reviving it.
	//
	// THE CLEAR MODE IS UNTOUCHED. `/clear` is not a compaction: it discards the
	// conversation rather than summarizing it, so a conversation that was
	// compacted an hour ago has lost nothing by also being cleared now.
	if mode.compacts() {
		redundant, gate, err := m.compactionRedundant(workspace)
		if err != nil {
			// THE REVIVAL IS NOT FAILED OVER THIS. An unreadable gate leaves the
			// daemon unable to prove a duplicate, and the user is standing in
			// front of a hibernated session asking for it back; the compaction is
			// what the doubt is about, so the compaction is what is declined.
			m.errorf("session-controller: revive COMPACTION DECLINED ON AN UNREADABLE GATE ws=%q session=%s mode=%s error=%v — the daemon cannot tell whether this conversation has already been compacted, so it is revived DIRECTLY rather than risking a second whole-conversation compaction",
				workspace, sessionID, mode, err)
			mode = ReviveModeDirect
		} else if redundant {
			m.logf("session-controller: revive COMPACTION SKIPPED ws=%q session=%s mode=%s %s — this conversation has already been compacted and nothing has been said to it since, so the requested compaction would read the whole history to summarize a summary; the session is revived DIRECTLY instead",
				workspace, sessionID, mode, compactionRedundantDetail(gate))
			mode = ReviveModeDirect
		}
	}

	if mode == ReviveModeDirect {
		// THE CLEAR COMES FIRST on this path. There is nothing to gate: the
		// user asked to resume as-is, so the moment the record stops saying
		// hibernated the session is an ordinary one.
		if err := m.clearHibernation(workspace, sessionID); err != nil {
			return err
		}
		if _, err := m.ensure(ctx, workspace); err != nil {
			return fmt.Errorf("session-controller: reviving session %s (ws %q) directly: bringing it up: %w", sessionID, workspace, err)
		}
		m.logf("session-controller: revive COMPLETE ws=%q session=%s mode=direct", workspace, sessionID)
		return nil
	}

	// A GATED MODE: bring up while the record STILL SAYS HIBERNATED, so the
	// gate that has been refusing prompts keeps refusing them for free.
	//
	// THE CUT IS RESOLVED BEFORE THE BRING-UP. A mode with no cut is a routing
	// failure, and discovering it after the session is up would leave a session
	// brought up for a cut that can never be submitted.
	cut, err := mode.cut()
	if err != nil {
		m.errorf("session-controller: revive REFUSED ws=%q session=%s mode=%s error=%v — nothing was brought up and the session STAYS GATED",
			workspace, sessionID, mode, err)
		return err
	}
	if _, err := m.ensure(ctx, workspace); err != nil {
		return fmt.Errorf("session-controller: reviving session %s (ws %q) for compaction: bringing it up: %w", sessionID, workspace, err)
	}
	// THE KEEP-ALIVE DEBT IS SETTLED BEFORE THE COMPACTION READS THE HISTORY,
	// and this path is the one where getting it wrong is permanent. A hibernated
	// session's transcript tail is very often keep-alive turns — pings are the
	// last thing that ran before the cache expired and the sleep was taken — and
	// a `/compact` submitted on top of them SUMMARIZES them into the
	// conversation, where no later rewind can reach them.
	//
	// It runs AFTER the bring-up because the rewind needs a session to stop and
	// a transcript identity to truncate, and the controller is resolved AFTER it
	// because a successful settle replaces the one the bring-up produced.
	m.settleKeepAliveResidue(ctx, workspace, "revive:compact-first")
	d, err := m.ensure(ctx, workspace)
	if err != nil {
		return fmt.Errorf("session-controller: reviving session %s (ws %q) for its %s: bringing the rewound conversation up: %w", sessionID, workspace, cut.text, err)
	}
	cutLanded, disarm, err := m.armCutWait(d, cut)
	if err != nil {
		return err
	}
	// The waiter travels with the claim: it is this revival's only route to the
	// completion signal, so it is retired here on every exit taken before the
	// handoff and by the completion goroutine afterwards.
	defer func() {
		if !detached {
			disarm()
		}
	}()
	m.logf("session-controller: revive ws=%q session=%s mode=%s — the session is up and STILL GATED; submitting %q before any prompt is accepted",
		workspace, sessionID, mode, cut.text)

	// THE REVIVAL'S OWN CUT IS THE ONE THING THE GATE LETS THROUGH. It
	// is submitted as submitterRevival, which guardHibernation admits precisely
	// so that the record can stay hibernated — and therefore keep gating the
	// user's prompts — while the compaction runs.
	//
	// ITS REQUEST ID IS UNIQUE PER ATTEMPT, and must be: a request id is now the
	// submitted turn's own identity on the wire (promptdispatch.go), and the
	// durable turn ledger refuses a second start under a name it already holds.
	// A session hibernated and compact-revived twice would otherwise submit its
	// second compaction under the first one's name — the same collision the
	// durable prompt receipt, keyed by request id, would already have taken.
	// The session id stays in the id so a log line still says which session's
	// revival it belongs to without a lookup.
	cutRequestID, err := newReviveCutRequestID(cut.requestIDPrefix, sessionID)
	if err != nil {
		return err
	}
	// THE COLD-READ ALARM'S CLAIM, taken before the submit so the compaction's
	// own terminal result can never arrive with nothing to match it against.
	//
	// A REVIVAL COMPACTION IS THE INCIDENT THIS ALARM WAS BUILT FROM. It runs
	// after the cache has already expired by construction — that expiry is why
	// the session was asleep — so it is the likeliest cold read in the daemon,
	// and covering only the warm path would leave the measured 1.5-million-token
	// case silent. The claim is released on submit failure and at the turn's own
	// end (queue.go), the same lifecycle the keep-alive ping's claim has.
	//
	// A CLEAR TAKES NO CLAIM, for the reason reviveCut.claimsCompaction states:
	// `/clear` is not a model call, so there is no input cost to attribute.
	if cut.claimsCompaction {
		m.mu.Lock()
		claimErr := m.claimDaemonCompactionLocked(d, daemonCompaction{turnID: cutRequestID, kind: compactionRevive})
		m.mu.Unlock()
		if claimErr != nil {
			m.errorf("session-controller: revive compaction NOT CLAIMED ws=%q session=%s turn_id=%s error=%v — the session STAYS GATED and nothing was submitted",
				workspace, sessionID, cutRequestID, claimErr)
			return claimErr
		}
	}
	if err := m.forwardPrompt(ctx, d, cutRequestID, cut.text,
		cut.requestIDPrefix+sessionID, "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, submitterRevival); err != nil {
		// The claim goes with the compaction that never ran. Leaving it standing
		// would hand the NEXT turn's cost to a compaction that was never
		// submitted, which is the misattribution the claim exists to prevent.
		if cut.claimsCompaction {
			m.mu.Lock()
			m.releaseDaemonCompactionLocked(d, cutRequestID)
			m.mu.Unlock()
		}
		m.logf("session-controller: revive CUT SUBMIT FAILED ws=%q session=%s mode=%s cut=%q error=%v — the session STAYS GATED; it was not left half-revived and accepting prompts",
			workspace, sessionID, mode, cut.text, err)
		return fmt.Errorf("session-controller: reviving session %s (ws %q): submitting %q: %w", sessionID, workspace, cut.text, err)
	}

	// THE HANDOFF. The cut is submitted, which is everything the ack reports;
	// the wait for it to land runs on without the command context.
	m.mu.Lock()
	if m.closed {
		m.mu.Unlock()
		m.logf("session-controller: revive CUT WAIT REFUSED ws=%q session=%s mode=%s — the manager is closing, so nothing will observe the cut; the session STAYS GATED and can be revived again",
			workspace, sessionID, mode)
		return fmt.Errorf("session-controller: reviving session %s (ws %q): the manager is closing; the session remains hibernated and can be revived again", sessionID, workspace)
	}
	// Registered with the same WaitGroup Close joins, so the wait cannot
	// outlive the manager and race whatever tears down after it.
	m.exits.Add(1)
	detached = true
	m.mu.Unlock()
	go m.awaitReviveCut(workspace, sessionID, mode, cut, cutRequestID, cutLanded, d.lifetime.done(), disarm, release)

	m.logf("session-controller: revive ACCEPTED ws=%q session=%s mode=%s — %q is submitted and the session STAYS GATED until it lands",
		workspace, sessionID, mode, cut.text)
	return nil
}

// ReviveForMerge is the merge coordinator's explicit direct-revival policy.
// A merge command is an affirmative request to drive this exact workspace, so
// it is distinct from a user revival command, whose direct-versus-compaction
// choice must remain explicit. Clearing the durable gate before bring-up uses
// the same authority as direct revival and prevents the merge's own action
// from being refused after a successful shim handshake.
func (m *Manager) ReviveForMerge(ctx context.Context, workspace string) error {
	if m.cfg.Hibernations == nil {
		return fmt.Errorf("session-controller: cannot revive workspace %q for merge: no hibernation registrar is wired", workspace)
	}
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return fmt.Errorf("session-controller: workspace %q has no session to revive for merge", workspace)
	}
	release, detail, err := m.claimRevival(workspace, sessionID)
	if err != nil {
		return fmt.Errorf("session-controller: claim merge revival for workspace %q session=%s: %w", workspace, sessionID, err)
	}
	defer release()
	if detail.Cause != "" {
		m.logf("session-controller: merge revival BEGIN ws=%q session=%s cause=%s slept_since_ms=%d policy=direct", workspace, sessionID, detail.Cause, detail.SinceMs)
	} else {
		m.logf("session-controller: merge revival ws=%q session=%s branch=already_awake policy=direct", workspace, sessionID)
	}
	// THE CLEAR IS UNCONDITIONAL, and the branch above is diagnosis alone.
	//
	// A RECORD THAT READS AWAKE IS NOT A RECORD THAT WILL STAY AWAKE. The flag
	// is only half of the durable fact: the other half is the last turn end the
	// keep-alive policy measures from, and a session whose flag says awake while
	// that instant is hours old is STALE — hibernateIfStale takes the sleep the
	// moment anything asks for the session, and bringUp asks (sessioncontroller.go).
	// Skipping the clear on the awake branch therefore let the merge's OWN
	// bring-up hibernate the session the merge was reviving, and ensure() came
	// back with ErrHibernated: a merge failed by the very revival meant to make
	// it possible.
	//
	// clearHibernation is what stamps the fresh measuring point (hibernation.go),
	// so running it on both branches is what makes the revival hold against the
	// staleness evaluator rather than merely against the flag. It is idempotent:
	// on an already-awake record it writes the same empty detail the record
	// already carries.
	if err := m.clearHibernation(workspace, sessionID); err != nil {
		m.logf("session-controller: merge revival FAILED ws=%q session=%s stage=clear_hibernation error=%v", workspace, sessionID, err)
		return fmt.Errorf("session-controller: revive workspace %q session=%s for merge: clear hibernation: %w", workspace, sessionID, err)
	}
	if _, err := m.ensure(ctx, workspace); err != nil {
		m.logf("session-controller: merge revival FAILED ws=%q session=%s stage=ensure_driveable error=%v", workspace, sessionID, err)
		return fmt.Errorf("session-controller: revive workspace %q session=%s for merge: ensure driveable: %w", workspace, sessionID, err)
	}
	m.logf("session-controller: merge revival COMPLETE ws=%q session=%s policy=direct", workspace, sessionID)
	return nil
}

// awaitReviveCut is a GATED revival's completion half, run detached from the
// command context that accepted the revival.
//
// IT OWNS THE CLAIM AND THE WAITER for as long as the cut is pending. Releasing
// them at the ack instead would let a second ReviveSessionCmd arm a second
// waiter over this one's, which is precisely the strand armCutWait refuses.
//
// ITS BOUND IS THE DAEMON'S, NOT A CALLER'S. compactFirstBound and the root
// context are what end the wait, so a webapp that disconnected the instant it
// got its ack does not abandon a cut the session is still gated on.
//
// EVERY FAILING EXIT LEAVES THE RECORD UNTOUCHED. The hibernation clear is
// reached only on the completion signal, which is what makes "a failed cut
// leaves the session gated" structural rather than a promise each error path
// has to keep.
//
// AND THE WAIT IS SCOPED TO THE CONTROLLER THAT IS RUNNING THE CUT, which is
// the fourth arm of the select and the reason `exited` is threaded here at all.
// The cut's completion signal can only ever be delivered BY that controller, so
// a controller that dies — a terminal protocol error killing the run loop, a
// teardown cancelling it — has already decided this revival's outcome. Waiting
// out the ten-minute bound afterwards is not patience, it is holding the
// workspace's exclusive revival claim over a signal that provably cannot
// arrive, and it is what left every later ReviveSessionCmd nacked with
// ErrRevivalInFlight until the daemon was bounced.
func (m *Manager) awaitReviveCut(workspace, sessionID string, mode ReviveMode, cut reviveCut, cutRequestID string, landed, controllerExited <-chan struct{}, disarm, release func()) {
	defer m.exits.Done()
	defer release()
	defer disarm()
	// THE COLD-READ CLAIM ENDS WHERE THIS REVIVAL'S OWNERSHIP OF THE COMPACTION
	// ENDS, on every exit, and that is a WIDER boundary than the compaction
	// turn's own end (queue.go releases it there too, and the two are idempotent
	// against each other because both match on the turn id).
	//
	// The turn-end release alone would not be enough: a compaction that lands on
	// the compacting axis without this daemon ever seeing its turn end — the
	// bound expiring, the daemon shutting down mid-compaction — would leave the
	// claim standing, and the NEXT revival of this session would then be refused
	// its own claim by a compaction that is long over.
	//
	// A cut that took no claim releases none: it has nothing of its own to
	// retire, and a release aimed at a turn id it never claimed under would be
	// aimed at whatever the session IS running.
	if cut.claimsCompaction {
		defer func() {
			m.mu.Lock()
			defer m.mu.Unlock()
			if d, live := m.byWS[workspace]; live {
				m.releaseDaemonCompactionLocked(d, cutRequestID)
			}
		}()
	}
	bound := m.reviveCompactBound()
	select {
	case <-landed:
		m.logf("session-controller: revive cut LANDED ws=%q session=%s mode=%s cut=%q — releasing the gate", workspace, sessionID, mode, cut.text)
	case <-m.rootCtx.Done():
		m.logf("session-controller: revive cut ABANDONED ws=%q session=%s mode=%s cut=%q error=%v — the session STAYS GATED",
			workspace, sessionID, mode, cut.text, m.rootCtx.Err())
		m.dropRevivalHolds(workspace, sessionID, "the daemon shut down before "+cut.text+" landed")
		return
	case <-controllerExited:
		m.logf("session-controller: revive cut ORPHANED ws=%q session=%s mode=%s cut=%q — the session controller running it is gone, so the completion signal can never be delivered; the session STAYS GATED and the revival claim is released NOW rather than at the %s bound, so the user can revive again immediately",
			workspace, sessionID, mode, cut.text, bound)
		m.dropRevivalHolds(workspace, sessionID, "the session controller running "+cut.text+" died before it landed")
		return
	case <-time.After(bound):
		m.logf("session-controller: revive cut TIMED OUT ws=%q session=%s mode=%s cut=%q bound=%s — the session STAYS GATED rather than limping into accepting prompts on a conversation that was never cut",
			workspace, sessionID, mode, cut.text, bound)
		m.dropRevivalHolds(workspace, sessionID, cut.text+" did not land within its bound")
		return
	}

	// THE HIBERNATION CLEAR IS THE LAST STEP, reached only on the completion
	// signal. Its own failure is logged by clearHibernation and leaves the gate
	// standing: there is no caller left to return it to, and a gate that could
	// not be retired is the safe direction.
	if err := m.clearHibernation(workspace, sessionID); err != nil {
		m.logf("session-controller: revive GATE RELEASE FAILED ws=%q session=%s mode=%s error=%v — %q landed but the record still claims a sleep; the session STAYS GATED and can be revived again",
			workspace, sessionID, mode, err, cut.text)
		m.dropRevivalHolds(workspace, sessionID, "the gate could not be released after "+cut.text+" landed")
		return
	}
	// THE PARKED PROMPTS ARE RELEASED AFTER THE CLEAR AND NOWHERE ELSE. This is
	// the second half of the delayed-never-dropped contract: the gate refused
	// nothing during the cut, it DELAYED — and this is the instant the delay is
	// over (queue.go, revivalHoldSessionID).
	m.releaseRevivalHolds(workspace, sessionID)
	m.logf("session-controller: revive COMPLETE ws=%q session=%s mode=%s", workspace, sessionID, mode)
}

// revivalHoldSessionLocked reports the session whose in-flight compact-first
// revival must park a prompt arriving for d right now, or "" when none does.
// Caller holds m.mu.
//
// IT IS THE TWO FACTS TOGETHER, and neither alone. The revival claim alone
// would park prompts through a DIRECT revival, which clears the gate first and
// has nothing to delay them for; the hibernation record alone is the ordinary
// gate, whose refusal for an UNDECIDED session is the whole feature and stays
// exactly as it was. Their conjunction names precisely one window — the user has
// chosen compact-first and its compaction has not landed — which is the only
// window in which a prompt is owed a delay instead of a refusal.
func (m *Manager) revivalHoldSessionLocked(d *sessionController) string {
	if !m.reviving[d.workspace] {
		return ""
	}
	if _, asleep := m.hibernatedLocked(d.sessionID); !asleep {
		return ""
	}
	return d.sessionID
}

// revivalParkAdmits reports whether a prompt from who must be admitted to
// workspace's queue as a PARKED entry rather than refused by the revival gate.
//
// IT IS ASKED AHEAD OF THE GATE in submitPromptAs, and it answers the same
// question revivalHoldSessionLocked does — from the workspace rather than from a
// live controller, because the gate is asked before ensure() and there may not
// be one yet. The two producers the gate keeps refusing are named here:
//
//   - submitterRevival is the revival's OWN `/compact`, which the gate admits
//     outright and which must never be parked behind itself;
//   - submitterKeepAlive is a ping, and a hibernated session is outside the
//     keep-alive loop by construction — a ping reaching here is that
//     construction having failed, which is said out loud rather than queued.
//   - submitterWarmCompaction is the daemon's pre-expiry `/compact`, and it is
//     named for the ping's exact reason: a hibernated session is outside the
//     keep-alive policy that schedules it, so one reaching here is that same
//     construction having failed. Parking it would also be absurd on its own
//     terms — it would wait behind a compaction to run a second compaction.
func (m *Manager) revivalParkAdmits(workspace string, who submitter) bool {
	if who == submitterRevival || who == submitterKeepAlive || who == submitterWarmCompaction {
		return false
	}
	if m.cfg.Hibernations == nil {
		return false
	}
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return false
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if !m.reviving[workspace] {
		return false
	}
	_, asleep := m.hibernatedLocked(sessionID)
	return asleep
}

// releaseRevivalHolds is the compaction's aftermath: the gate is down, so the
// prompts it parked become ordinary queued prompts and the front one is
// delivered.
//
// CALLED ONLY AFTER clearHibernation HAS SUCCEEDED. The release is what makes a
// parked prompt deliverable, and delivering one while the record still claimed a
// sleep would meet forwardPrompt's own gate and mark a perfectly good prompt
// ERROR. Ordering the clear first is what makes that unreachable rather than
// merely unlikely.
func (m *Manager) releaseRevivalHolds(workspace, sessionID string) {
	m.mu.Lock()
	d, live := m.byWS[workspace]
	if !live {
		m.mu.Unlock()
		m.logf("session-controller: revival holds NOT RELEASED ws=%q session=%s — the compaction landed but the workspace has no live session controller; any parked prompt went with its queue",
			workspace, sessionID)
		return
	}
	released := d.queue.releaseRevivalHold(sessionID)
	if released == 0 {
		m.mu.Unlock()
		return
	}
	next := d.queue.popFrontDeliverable()
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()
	m.logf("session-controller: revival holds RELEASED ws=%q session=%s released=%d — the compaction landed and the gate is down, so the prompts typed during it are delivered in the order they were typed",
		workspace, sessionID, released)
	m.publish(d.sessionID, view, recs)
	m.noteDrainActivity()
	if next != nil {
		go m.deliver(d, next)
	}
}

// dropRevivalHolds is the parked entry's BOUND, taken on every revival exit
// that leaves the gate standing: the compaction's bound expiring, the daemon
// shutting down mid-compaction, a clear that failed, and a revival that never
// reached its handoff at all.
//
// THE ENTRIES ARE DROPPED, NOT KEPT. The session is still asleep and the claim
// is going away, so nothing will ever release these holds: every delivery path
// refuses a held entry, so keeping them would leave chips rendering "waiting"
// against a queue no boundary can ever drain — a leak that outlives the very
// revival that caused it. Each one is named in the log with the text that was
// lost, because a prompt the user typed and the daemon discarded is not an
// event to record as a count.
//
// The user is not left guessing: the session is still gated, so the next prompt
// meets the ordinary refusal and the revival gate is still there to choose from.
func (m *Manager) dropRevivalHolds(workspace, sessionID, reason string) {
	m.mu.Lock()
	d, live := m.byWS[workspace]
	if !live {
		m.mu.Unlock()
		return
	}
	dropped := d.queue.dropRevivalHeld(sessionID)
	if len(dropped) == 0 {
		m.mu.Unlock()
		return
	}
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()
	for _, e := range dropped {
		m.logf("session-controller: revival-parked prompt DROPPED entry=%s ws=%q session=%s request_id=%s reason=%s text=%q — the compact-first revival ended without opening the gate, so the session is still asleep and nothing can ever deliver this prompt; it is discarded rather than left queued forever",
			e.id, workspace, sessionID, e.requestID, reason, e.text)
	}
	m.logf("session-controller: revival holds DROPPED ws=%q session=%s dropped=%d reason=%s — the gate still stands and the user can revive again",
		workspace, sessionID, len(dropped), reason)
	m.publish(d.sessionID, view, recs)
	m.noteDrainActivity()
}

// refuseRevivalForce is the loud nack for a force aimed at a revival-parked
// prompt. It carries ErrHibernated because that is exactly what is true: the
// session is still asleep, and this prompt runs when the compaction opens the
// gate and not before.
func (m *Manager) refuseRevivalForce(workspace, entryID, sessionID string) error {
	m.logf("session-controller: force REFUSED for a revival-parked queue entry=%s ws=%q session=%s — a compact-first revival's compaction is still pending; forcing the prompt would answer it against the whole uncompacted conversation the user chose this mode to avoid paying for. It is delivered on its own when the compaction lands, and is still cancellable",
		entryID, workspace, sessionID)
	return fmt.Errorf("session-controller: cannot force queued prompt %q on workspace %q: session %s is being revived and its compaction has not landed; the prompt is delivered automatically once it does, and can be cancelled: %w",
		entryID, workspace, sessionID, ErrHibernated)
}

// reviveCompactBound is how long the detached completion wait allows the
// compaction. Production always uses compactFirstBound; only a test assigns an
// override, so the timeout branch can be driven without a ten-minute wait.
func (m *Manager) reviveCompactBound() time.Duration {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.reviveCompactBoundOverride > 0 {
		return m.reviveCompactBoundOverride
	}
	return compactFirstBound
}

// claimRevival takes the exclusive per-workspace revival claim and reports the
// session's durable hibernation detail, read under the SAME acquisition.
//
// IT IS claimHibernation'S SHAPE, and for the identical reason. Revival was a
// check-then-act: it read HibernationOf, released nothing (it held no lock at
// all), and only then brought the session up and submitted `/compact`. Two
// concurrent revivals therefore both saw a hibernated session, both submitted a
// compaction under the same request id, and the second's arm silently replaced
// the first's completion waiter.
//
// The claim is taken under the manager mutex, the same one every prompt
// submission takes, so the read of the durable state and the decision to act on
// it are one act with respect to any other producer.
func (m *Manager) claimRevival(workspace, sessionID string) (release func(), detail registry.HibernationDetail, err error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.reviving == nil {
		m.reviving = map[string]bool{}
	}
	if m.reviving[workspace] {
		m.logf("session-controller: revive REFUSED ws=%q session=%s — a revival is already in flight for this workspace; nothing was brought up and no compaction was submitted",
			workspace, sessionID)
		return nil, registry.HibernationDetail{}, fmt.Errorf("%w: workspace %q", ErrRevivalInFlight, workspace)
	}
	detail, asleep := m.cfg.Hibernations.HibernationOf(sessionID)
	if !asleep {
		detail = registry.HibernationDetail{}
	}
	m.reviving[workspace] = true
	return func() {
		m.mu.Lock()
		delete(m.reviving, workspace)
		m.mu.Unlock()
	}, detail, nil
}

// armCutWait installs the one-shot completion signal a compact-first
// revival waits on, and returns the channel it closes plus the disarm that
// retires it.
//
// Armed BEFORE the compaction is submitted, never after: a compaction that
// completed between the submit and the arm would close an axis nobody was
// listening to, and the revival would wait out its whole bound for an event
// that had already happened.
//
// AN EXISTING WAITER IS A LOUD REFUSAL, NOT AN OVERWRITE. The revival claim
// makes a second arm unreachable, and this is the detection that says so if it
// ever becomes reachable again: overwriting would strand the first revival on a
// channel nothing will ever close, which is exactly the silent hang the bound
// above cannot distinguish from a slow compaction.
func (m *Manager) armCutWait(d *sessionController, cut reviveCut) (<-chan struct{}, func(), error) {
	done := make(chan struct{})
	var once bool
	armed := func() {
		m.mu.Lock()
		defer m.mu.Unlock()
		if once {
			return
		}
		once = true
		close(done)
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	waiter := cut.waiter(d.consumer)
	if waiter.fire != nil {
		m.logf("session-controller: revive ARM REFUSED ws=%q session=%s cut=%q — a completion waiter for this cut is already installed; overwriting it would strand the revival that owns it on a signal nothing will deliver",
			d.workspace, d.sessionID, cut.text)
		return nil, nil, fmt.Errorf("session-controller: refusing to arm a second %q waiter for workspace %q session %s: one is already installed", cut.text, d.workspace, d.sessionID)
	}
	// The token identifies this arm. Funcs are not comparable in Go, and the
	// disarm must be able to tell its own waiter from a later one rather than
	// clearing whatever it finds.
	token := new(struct{})
	*waiter = cutWaiter{fire: armed, token: token}
	disarm := func() {
		m.mu.Lock()
		defer m.mu.Unlock()
		// Cleared only while it is still OURS. A waiter belonging to somebody
		// else is not this revival's to retire.
		if waiter.token == token {
			*waiter = cutWaiter{}
		}
	}
	return done, disarm, nil
}

// HibernateWorkspace is the user-forced hibernation behind
// HibernateWorkspaceCmd. It is the SAME transition the sweeper's two automatic
// causes take, differing only in the cause it records.
//
// A LIVE TURN OR A HELD MERGE LEASE IS A LOUD NACK, not a wait and not a
// discard: the user interrupts first. The daemon never throws away in-flight
// work to satisfy a hibernate, so the refusal is the honest answer rather than
// an inconvenience.
func (m *Manager) HibernateWorkspace(workspace string) error {
	if m.cfg.SSM.MergeLeaseHeld(workspace) {
		err := fmt.Errorf("session-controller: workspace %q is being merged: merge.Coordinator holds the exclusivity lease on its session, so it cannot be hibernated until the merge reaches a terminal phase", workspace)
		m.logf("session-controller: forced hibernation REFUSED ws=%q — the merge lease is held; nothing was stopped",
			workspace)
		return err
	}
	return m.HibernateWithCause(workspace, registry.HibernationDetail{Cause: registry.HibernationCauseForced})
}
