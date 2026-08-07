// Package errclass is the daemon's SINGLE classifier: the one place a raw
// failure becomes a frontend.v1.SystemFailureItem.
//
// The rule it exists to enforce: a fact is classified by the process that is
// the first to hold both the failure and its cause, exactly once, and every
// process downstream renders the result without re-inspecting it. Before this
// package the daemon and the webapp classified the SAME ApiErrorLine by two
// different rules, and a handler error reached a human as raw Go text — the
// daemon knew what kind of failure it held and discarded that knowledge at
// the wire.
//
// # Namespace partition
//
// Every Type minted here is UNPREFIXED. The `client.` prefix is reserved for
// failures a FRONTEND classifies about its own transport — the only facts a
// frontend legitimately owns. A `client.` type emitted by the daemon means a
// frontend's failure was laundered through the daemon, and a bare type
// emitted by a frontend means the frontend classified something the daemon
// already held; both are contract violations detectable by string inspection
// rather than by review, which is the point of the split.
//
// # Class
//
// The class is SEMANTIC, never chromatic. INTERNAL is agent-repl's own
// machinery failing (nothing about the account is implicated, no amount of
// waiting helps). API is the SDK or the vendor refusing or concluding the
// work (releasing it needs a human or the vendor, never a retry). Each
// frontend maps class to color from its own table; the wire never says red.
package errclass

import (
	"errors"
	"fmt"
	"strings"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ssm"
)

// Type is the closed vocabulary of daemon-classified failure types. It is a
// distinct type rather than a bare string so a caller cannot invent a member
// at a call site: every value a frontend can ever see is declared here.
type Type string

// ClientPrefix is the namespace reserved for FRONTEND-classified failures. No
// value in this package may carry it; see the package doc.
const ClientPrefix = "client."

// INTERNAL failure types: agent-repl's own machinery.
const (
	// TypeShimNotConnected — a command needed the shim and there was no live
	// connection to it.
	TypeShimNotConnected Type = "shim.not_connected"
	// TypeShimRejected — the shim NACKed the request. Its raw reason rides
	// source_detail; the Nack itself never becomes a SystemFailureItem on the
	// wire, because it is machinery and stops being machinery here.
	TypeShimRejected Type = "shim.rejected"
	// TypeShimAckTimeout — the shim never acked within the control deadline.
	TypeShimAckTimeout Type = "shim.ack_timeout"
	// TypeShimVersionMismatch — the shim speaks a protocol version this
	// daemon does not.
	TypeShimVersionMismatch Type = "shim.version_mismatch"
	// TypeShimSeqRegression — the shim's event sequence went backwards, so
	// the stream can no longer be trusted to be ordered.
	TypeShimSeqRegression Type = "shim.seq_regression"
	// TypeShimDegraded — the missed-heartbeat window elapsed with no shim
	// traffic. WINDOW-shaped: it resolves when traffic resumes.
	TypeShimDegraded Type = "shim.degraded"
	// TypeShimStoreWriteRejected — the shim reported it could not write to
	// the store, so events are being dropped. WINDOW-shaped.
	TypeShimStoreWriteRejected Type = "shim.store_write_rejected"
	// TypeUnexpectedQueryTermination — the standing SDK query ended without
	// an intentional shim shutdown. The session can no longer accept turns.
	TypeUnexpectedQueryTermination Type = "unexpected_query_termination"
	// The establishment links, in depth order. A createSession acks only
	// once the new session's shim answers a health probe healthy over the wired
	// connection, so a failed create must say WHICH hop of that path it stopped
	// at — "the session did not come up" names nothing a human can act on.
	//
	// TypeShimNotSpawned — no shim process was ever brought up for the
	// workspace, so there is no connection to wait on.
	TypeShimNotSpawned Type = "shim.not_spawned"
	// TypeShimHandshakeIncomplete — the shim connected but never sent
	// ShimReady, so its lock, query, producer, and standing store
	// subscription are not all wired.
	TypeShimHandshakeIncomplete Type = "shim.handshake_incomplete"
	// TypeShimUnhealthy — the shim is wired and answered, and its OWN verdict
	// on its dependencies is unhealthy. The shim's component and reason ride
	// source_detail; the daemon never second-guesses the verdict.
	TypeShimUnhealthy Type = "shim.unhealthy"
	// TypeSessionNotEstablished — the establishment deadline elapsed with no
	// verdict at all. Distinct from the links above: those name a hop that
	// ANSWERED, this one names a wait that ran out.
	TypeSessionNotEstablished Type = "session.not_established"
	// TypeSessionNotLive — the command addressed a session that is no longer
	// the workspace's live one.
	TypeSessionNotLive Type = "session.not_live"
	// TypeSessionDeleted — the session was deleted by the user.
	TypeSessionDeleted Type = "session.deleted"
	// TypeSessionSuperseded — a NEW Claude session (a fresh conversation or a
	// resume) was created for this workspace, so this registry record was
	// stood down. The workspace and every resumed transcript each take
	// exactly one live session, so minting a new one always retires
	// whatever was running before it — see supersedeCreateConflicts.
	TypeSessionSuperseded Type = "session.superseded"
	// TypeSessionReconnectSuperseded — a resync named a session/generation
	// identity that is no longer the workspace's live one. Unlike
	// TypeSessionSuperseded this does NOT mean a new Claude session was
	// created: the SAME Claude session's controller mints a fresh
	// generation on every bring-up (first prompt, hibernation revival,
	// post-crash rebuild, transport retry, or a daemon restart handed back
	// a surviving agent process), so the mismatch can equally mean a new
	// agent process was spawned for the same conversation, or the daemon
	// simply reconnected to one that survived a restart. Because the
	// resync alone cannot tell which of those happened — or whether a new
	// Claude session is in fact what happened — the message names all
	// three possibilities rather than asserting the wrong one.
	TypeSessionReconnectSuperseded Type = "session.reconnect_superseded"
	// TypeSessionShimDied — the session's shim process died, which is what
	// makes the session terminal.
	TypeSessionShimDied Type = "session.shim_died"
	// TypeSessionStartFailed — a bring-up ended without wiring, and the
	// FRESH retry ended the same way. It is what makes `starting` non-terminal:
	// every bring-up now closes on wired or on this.
	TypeSessionStartFailed Type = "session.start_failed"
	// TypeSessionResumeFailed — restoration could not resume the authoritative
	// Claude conversation. The typed SessionResumeFailure detail carries the
	// exact session, Claude UUID, attempted operation, and continuity cause.
	TypeSessionResumeFailed Type = "session.resume_failed"
	// TypeSessionEndedUnclassified — a persisted death reason from an older
	// build that predates this vocabulary. The raw string rides source_detail
	// rather than being guessed at.
	TypeSessionEndedUnclassified Type = "session.ended_unclassified"
	// TypeHistoryRepullInFlight — a history re-pull was already running for
	// this workspace.
	TypeHistoryRepullInFlight Type = "history.repull_in_flight"
	// TypeHistoryReplayTruncated — the re-pull ended before reaching the
	// retained window, so the history it delivered is incomplete.
	TypeHistoryReplayTruncated Type = "history.replay_truncated"
	// TypeInterruptUndelivered — the stop could not be routed to the shim at
	// all. The ONLY interrupt outcome that is a failure: a stop that landed
	// on an already-finished turn is success, not an error.
	TypeInterruptUndelivered Type = "interrupt.undelivered"
	// TypeQueueEntrySessionUnwired — a queue command aimed at a prompt this
	// daemon holds only in the MATERIALIZED parking ledger, whose session has
	// no shim attached to this daemon. It is the force refusal's name: the
	// prompt exists and is reachable by cancel, but delivering it needs a
	// session that is not here, and a client that is shown a named failure can
	// say so instead of rendering the raw sentence as an internal fault.
	TypeQueueEntrySessionUnwired Type = "queue.entry_session_unwired"
	// TypeQueueEntryKeepAliveHeld — a force aimed at a prompt held behind an
	// in-flight cache keep-alive turn. Unlike a drain hold there is no
	// force-through: the ping must finish so the daemon can rewind it out of
	// the transcript before this prompt is submitted, and forcing would make
	// the ping permanent context. The prompt is still cancellable, and it is
	// delivered on its own the moment the ping ends.
	TypeQueueEntryKeepAliveHeld Type = "queue.entry_keep_alive_held"
	// TypeSessionHibernated — a prompt refused because the session is asleep
	// and the user has not chosen a revival mode. It is the revival gate's
	// name, and it is a NAMED refusal rather than an internal fault precisely
	// because it is the expected answer for a hibernated workspace: the client
	// renders the gate from it instead of an error card.
	TypeSessionHibernated Type = "session.hibernated"
	// TypeKeepAliveWindowUnclosed — the durable keep-alive window ledger
	// refused to stamp a ping's end. It is a NAMED failure rather than a log
	// line because an open window has no upper bound: every later item on the
	// workspace is withheld from every rendering until the row is repaired, so
	// the conversation silently stops advancing. Naming it is what turns a
	// permanent blackout into something a human is told about.
	TypeKeepAliveWindowUnclosed Type = "keep_alive.window_unclosed"
	// TypeKeepAliveWindowInverted — the durable keep-alive window ledger
	// refused an end instant that precedes the window's own start. It is a
	// DIFFERENT fault from the unclosed window above and is named separately
	// for that reason: the row is bounded (the ledger clamps it to its own
	// start), so nothing is blacked out, but the interval covers nothing and
	// the ping's id-less file-plane records may therefore render as the user's.
	// What it reports is a disagreement between the clock that stamped the
	// start and the clock that stamped the end.
	TypeKeepAliveWindowInverted Type = "keep_alive.window_inverted"
	// TypeClientLogIdentityStale — a browser log record named an agent-repl or
	// Claude session that is not the one the daemon registry holds for the
	// command's workspace, so the record was refused rather than persisted
	// under an attribution the daemon knows to be wrong. It is an ORDINARY
	// outcome of a page that outlived its session (a rebind, a resume, or a
	// daemon restart), which is exactly why it is named: reaching a human as
	// internal.unclassified logged a routine staleness as a daemon fault on
	// every single rejected record.
	TypeClientLogIdentityStale Type = "client_log.identity_stale"
	// TypeInternalUnclassified — the loud fallthrough. It carries the raw
	// error text and is always logged; a silent fallthrough would let the
	// vocabulary rot without anyone noticing.
	TypeInternalUnclassified Type = "internal.unclassified"
)

// API failure types: the SDK or the vendor refusing or concluding the work.
const (
	TypeAPIAuthenticationFailed Type = "api.authentication_failed"
	TypeAPIBillingError         Type = "api.billing_error"
	TypeAPIRateLimit            Type = "api.rate_limit"
	TypeAPIInvalidRequest       Type = "api.invalid_request"
	TypeAPIServerError          Type = "api.server_error"
	TypeAPIUnknown              Type = "api.unknown"
	TypeAPIOAuthOrgNotAllowed   Type = "api.oauth_org_not_allowed"
	TypeAPIOverloaded           Type = "api.overloaded"
	TypeAPIModelNotFound        Type = "api.model_not_found"
	TypeAPIMaxOutputTokens      Type = "api.max_output_tokens"
	// TypeAPINetworkDown — the request never reached the vendor at all. API
	// class rather than INTERNAL: nothing of agent-repl's own machinery
	// failed, and by the time this is minted the SDK's retries are already
	// exhausted, so trying again is not the answer.
	TypeAPINetworkDown Type = "api.network_down"
	// TypeAPIRequestFailed — a terminal ApiErrorLine whose status matched
	// nothing known. The request failed and the retries are exhausted; that is
	// all the line said, and inventing a more specific type would be a guess.
	TypeAPIRequestFailed Type = "api.request_failed"
	// TypeAPIMaxTurns / TypeAPIMaxBudget — a limit the USER set was reached.
	// API class rather than INTERNAL because releasing it takes a human
	// decision, which is exactly what the class means.
	TypeAPIMaxTurns  Type = "api.max_turns"
	TypeAPIMaxBudget Type = "api.max_budget"
	// TypeAPIExecutionError — the SDK aborted execution.
	TypeAPIExecutionError Type = "api.execution_error"
	// TypeAPIRefusal — the model refused.
	TypeAPIRefusal Type = "api.refusal"
	// TypeAPITurnFailed — an is_error turn end whose stop reason is not in
	// the known family. Still API class: an unrecognized abnormal conclusion
	// is a conclusion, and resolving it toward the safe answer rather than
	// the convenient one is the same rule the SSM applies.
	TypeAPITurnFailed Type = "api.turn_failed"
)

// Assistant maps a typed SDK AssistantMessageError to its failure type. It is
// the promotion of what used to be progress.assistantErrorName — a DISPLAY
// string built for one footer row — into the vocabulary every surface shares.
//
// ok=false only for UNSPECIFIED: the enum being unset is the absence of a
// classification, not a classification of "unknown" (which is its own member).
func Assistant(e datav1.AssistantMessageError) (Type, bool) {
	switch e {
	case datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_AUTHENTICATION_FAILED:
		return TypeAPIAuthenticationFailed, true
	case datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_BILLING_ERROR:
		return TypeAPIBillingError, true
	case datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_RATE_LIMIT:
		return TypeAPIRateLimit, true
	case datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_INVALID_REQUEST:
		return TypeAPIInvalidRequest, true
	case datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_SERVER_ERROR:
		return TypeAPIServerError, true
	case datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_UNKNOWN:
		return TypeAPIUnknown, true
	case datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_OAUTH_ORG_NOT_ALLOWED:
		return TypeAPIOAuthOrgNotAllowed, true
	case datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_OVERLOADED:
		return TypeAPIOverloaded, true
	case datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_MODEL_NOT_FOUND:
		return TypeAPIModelNotFound, true
	case datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_MAX_OUTPUT_TOKENS:
		return TypeAPIMaxOutputTokens, true
	default:
		return "", false
	}
}

// prose is the human sentence for EVERY failure type — ONE table, so a type
// added without a sentence is a test failure rather than an empty card.
//
// Deliberately not one table per family: the families overlap (a 401 reaches
// it from an ApiErrorLine and `authentication_failed` reaches it from a
// TurnEnded, and both must read the same), and splitting them is exactly how
// one failure comes to have two different sentences depending on which door
// it arrived through.
var prose = map[Type]string{
	// INTERNAL — agent-repl's own machinery.
	TypeShimNotConnected:           "the agent process is not connected",
	TypeShimRejected:               "the agent process rejected the request",
	TypeShimAckTimeout:             "the agent process did not respond in time",
	TypeShimVersionMismatch:        "the agent process speaks a different protocol version",
	TypeShimSeqRegression:          "the agent process's event stream went backwards",
	TypeShimDegraded:               "no traffic from the agent process",
	TypeShimStoreWriteRejected:     "the agent process could not write to the store",
	TypeUnexpectedQueryTermination: "the agent SDK query ended unexpectedly",
	TypeShimNotSpawned:             "the agent process was never started for this session",
	TypeShimHandshakeIncomplete:    "the agent process connected but never finished wiring up",
	TypeShimUnhealthy:              "the agent process reported itself unhealthy",
	TypeSessionNotEstablished:      "the session did not finish connecting in time",
	TypeSessionNotLive:             "this is no longer the workspace's live session",
	TypeSessionDeleted:             "the session was deleted",
	TypeSessionSuperseded:          "a new Claude session was started for this workspace, so this session was stopped — a workspace and each resumed transcript keep exactly one live session at a time",
	TypeSessionReconnectSuperseded: "this view's session identity is stale — the live session's connection changed, which could mean a new Claude session took over this workspace, a new agent process was spawned for it, or the daemon reconnected to it after a restart; to resync, reload this webview from Emacs with SPC o l (agent-repl-frontend-reload-webview), or run M-x agent-repl-refresh-webviews to remount every workspace view",
	TypeSessionShimDied:            "the agent process exited",
	TypeSessionStartFailed:         "the session could not be started",
	TypeSessionResumeFailed:        "the Claude conversation could not be resumed",
	TypeSessionEndedUnclassified:   "the session ended",
	TypeHistoryRepullInFlight:      "a history re-pull is already running",
	TypeHistoryReplayTruncated:     "the history re-pull ended before it reached the live window",
	TypeInterruptUndelivered:       "the stop could not be delivered",
	TypeQueueEntrySessionUnwired:   "the queued prompt's session is not attached to this daemon, so it cannot be run yet",
	TypeQueueEntryKeepAliveHeld:    "the queued prompt is waiting for a cache keep-alive response and cannot be forced ahead of it",
	TypeSessionHibernated:          "the session is hibernated; choose how to revive it before sending prompts",
	TypeKeepAliveWindowUnclosed:    "a cache keep-alive window could not be closed, so new conversation is being withheld until it is repaired",
	TypeKeepAliveWindowInverted:    "a cache keep-alive window ended before it began, so the daemon's own keep-alive turn may appear in the conversation",
	TypeClientLogIdentityStale:     "a browser log record named a session this workspace no longer runs, so it was not recorded",
	TypeInternalUnclassified:       "the command failed",

	// API — the SDK or the vendor refusing or concluding the work.
	TypeAPIAuthenticationFailed: "authentication failed",
	TypeAPIBillingError:         "a billing problem stopped the request",
	TypeAPIRateLimit:            "rate limited",
	TypeAPIInvalidRequest:       "the request was rejected as invalid",
	TypeAPIServerError:          "the API returned a server error",
	TypeAPIUnknown:              "the API failed for an unknown reason",
	TypeAPIOAuthOrgNotAllowed:   "this organization is not allowed to use the API",
	TypeAPIOverloaded:           "the API is overloaded",
	TypeAPIModelNotFound:        "the requested model does not exist",
	TypeAPIMaxOutputTokens:      "the response hit the output-token ceiling",
	TypeAPINetworkDown:          "the network never reached the API",
	TypeAPIRequestFailed:        "the API request failed",
	TypeAPIMaxTurns:             "the turn hit its maximum-turns limit",
	TypeAPIMaxBudget:            "the turn hit its budget limit",
	TypeAPIExecutionError:       "the turn aborted during execution",
	TypeAPIRefusal:              "the model refused the request",
	TypeAPITurnFailed:           "the turn ended abnormally",
}

// stopReasonTypes maps a TurnEnded stop reason to its failure type. The keys
// are exactly ssm's vendor-blocking family; the SSM decides WHETHER a turn end
// blocks and this decides what to CALL it, so the two can be read against each
// other rather than drifting.
var stopReasonTypes = map[string]Type{
	"error_max_turns":        TypeAPIMaxTurns,
	"error_max_budget":       TypeAPIMaxBudget,
	"error_during_execution": TypeAPIExecutionError,
	"refusal":                TypeAPIRefusal,
	"authentication_failed":  TypeAPIAuthenticationFailed,
	"billing_error":          TypeAPIBillingError,
	"invalid_request":        TypeAPIInvalidRequest,
	"server_error":           TypeAPIServerError,
}

// The daemon's sentinel errors. They live HERE, in the classifier, rather
// than in the packages that return them, for one structural reason: a
// classifier that imported those packages would close a cycle
// (sessioncontroller -> frontend -> errclass -> sessioncontroller). Owning the anchors is
// also the honest arrangement — a sentinel exists precisely to be
// classified, and shimclient/sessioncontroller re-export these under their historic
// names so every existing errors.Is call site keeps matching the same value.
//
// Their Error() text is unchanged from where they were declared, because it
// is what lands in source_detail.
var (
	ErrShimNotConnected    = errors.New("shimclient: no live shim connection")
	ErrShimNack            = errors.New("shimclient: request nacked")
	ErrShimAckTimeout      = errors.New("shimclient: control request timed out")
	ErrShimVersionMismatch = errors.New("shimclient: protocol version mismatch")
	ErrShimSeqRegression   = errors.New("shimclient: sequence regression")
	ErrNotLiveSession      = errors.New("session-controller: not the live session for this workspace")
	// ErrSessionSuperseded identifies a command whose authoritative workspace
	// identity changed after the client rendered its snapshot.  It is distinct
	// from ErrNotLiveSession: the command did name a once-valid session, but a
	// newer controller generation now owns the replay eligibility decision.
	ErrSessionSuperseded = errors.New("session-controller: command superseded by the current workspace generation")
	// The establishment ladder's anchors. They are DISJOINT by construction:
	// exactly one may appear in any one error chain, because each names the
	// deepest hop that hop's failure reached, and a nack that matched two
	// would have no single deepest link to report.
	ErrNoLiveSessionController = errors.New("session-controller: the workspace has no live session controller")
	ErrShimNotReady            = errors.New("session-controller: the shim connection never completed its handshake")
	ErrShimUnhealthy           = errors.New("server: the shim reported itself unhealthy")
	ErrSessionNotEstablished   = errors.New("server: the session did not become established within the deadline")
	ErrRepullInFlight          = errors.New("session-controller: a history re-pull is already in flight for this workspace")
	ErrRepullTruncated         = errors.New("session-controller: history re-pull truncated before reaching the retained window")
	// ErrInterruptUndelivered is the one interrupt outcome that is a failure.
	// It is a sentinel rather than a special case at the ack site so that an
	// undeliverable stop reaches a human through the SAME door as every other
	// command failure — outcome to error to Command to card — instead of
	// getting a second classification path of its own.
	ErrInterruptUndelivered = errors.New("session-controller: the interrupt could not be delivered")
	// ErrQueueEntrySessionUnwired anchors the queue surface's one REFUSAL: a
	// force aimed at a prompt that lives in the materialized parking ledger,
	// whose session has not wired to this daemon (or has wired and not yet
	// adopted the ledger's entries). A force is a delivery and a delivery needs
	// a shim, so the command cannot be honored — and it is a sentinel rather
	// than a bare fmt.Errorf so the refusal reaches a human as a NAMED failure
	// instead of falling through to internal.unclassified, which would log the
	// same refusal a second time under a name that says nothing.
	ErrQueueEntrySessionUnwired = errors.New("session-controller: the queued prompt's session is not wired to this daemon")
	// ErrQueueEntryKeepAliveHeld anchors the keep-alive hold's force refusal.
	// It is a sentinel for the same reason the one above is: an expected,
	// ordinary refusal that reached a human as internal.unclassified would be
	// logged twice under a name that says nothing about it.
	ErrQueueEntryKeepAliveHeld = errors.New("session-controller: the queued prompt is held behind an in-flight cache keep-alive turn")
	// ErrSessionHibernated anchors the revival gate's refusal, so a client can
	// render the revival choice from a NAMED failure rather than parsing text.
	ErrSessionHibernated = errors.New("session-controller: the session is hibernated")
	// ErrClientLogIdentityStale anchors the client-log persistence boundary's
	// session-attribution refusal. The rejecting site wraps it with the exact
	// got/want identities, so nothing the old bare fmt.Errorf reported is lost;
	// what the sentinel adds is a NAME, because a webview that outlived its
	// session produces one of these per forwarded record and every one of them
	// used to be logged as an unclassified daemon fault.
	ErrClientLogIdentityStale = errors.New("server: client log source session identity disagrees with the daemon registry")
)

// sentinelTypes is the ladder, as data rather than as a switch, so the
// per-sentinel tests can iterate it and a newly added sentinel that nobody
// classified is visible as a missing row.
var sentinelTypes = []struct {
	err error
	typ Type
}{
	{ErrShimNotConnected, TypeShimNotConnected},
	{ErrShimNack, TypeShimRejected},
	{ErrShimAckTimeout, TypeShimAckTimeout},
	{ErrShimVersionMismatch, TypeShimVersionMismatch},
	{ErrShimSeqRegression, TypeShimSeqRegression},
	{ErrNotLiveSession, TypeSessionNotLive},
	{ErrSessionSuperseded, TypeSessionReconnectSuperseded},
	{ErrNoLiveSessionController, TypeShimNotSpawned},
	{ErrShimNotReady, TypeShimHandshakeIncomplete},
	{ErrShimUnhealthy, TypeShimUnhealthy},
	{ErrSessionNotEstablished, TypeSessionNotEstablished},
	{ErrRepullInFlight, TypeHistoryRepullInFlight},
	{ErrRepullTruncated, TypeHistoryReplayTruncated},
	{ErrInterruptUndelivered, TypeInterruptUndelivered},
	{ErrQueueEntrySessionUnwired, TypeQueueEntrySessionUnwired},
	{ErrQueueEntryKeepAliveHeld, TypeQueueEntryKeepAliveHeld},
	{ErrSessionHibernated, TypeSessionHibernated},
	{ErrClientLogIdentityStale, TypeClientLogIdentityStale},
}

// Sentinel is the errors.Is ladder over the daemon's sentinel errors. It
// reports the type an error classifies as, and ok=false when nothing matched.
//
// These sentinels all predate this package and were, until now, never
// inspected: every one of them reached a human as its own Error() text. The
// ladder is what turns them from log material into a vocabulary.
func Sentinel(err error) (Type, bool) {
	if err == nil {
		return "", false
	}
	for _, s := range sentinelTypes {
		if errors.Is(err, s.err) {
			return s.typ, true
		}
	}
	return "", false
}

// SessionResumeFailureDetailer exposes the evidence held by the owner that
// refused a create or automatic restore. The command classifier owns the
// common failure vocabulary; the owner supplies the identity it alone knows.
type SessionResumeFailureDetailer interface {
	SessionResumeFailureDetail() *frontendv1.SessionResumeFailure
}

// Command classifies an error returned by a frontend command handler — the
// single funnel through which every handler failure used to become raw
// CommandAck.error text.
//
// An unmatched error is NOT dropped and NOT silently passed through: it
// becomes internal.unclassified carrying the raw text, and logf fires. The
// fallthrough being loud is the whole reason it is safe to have one.
func Command(logf dlog.Logf, err error) *frontendv1.SystemFailureItem {
	if err == nil {
		return nil
	}
	var resume SessionResumeFailureDetailer
	if errors.As(err, &resume) {
		return &frontendv1.SystemFailureItem{
			ErrorClass:   frontendv1.ErrorClass_ERROR_CLASS_INTERNAL,
			ErrorType:    string(TypeSessionResumeFailed),
			Message:      prose[TypeSessionResumeFailed],
			SourceDetail: err.Error(),
			StructuredDetail: &frontendv1.SystemFailureItem_SessionResume{
				SessionResume: resume.SessionResumeFailureDetail(),
			},
		}
	}
	if t, ok := Sentinel(err); ok {
		return &frontendv1.SystemFailureItem{
			ErrorClass:   frontendv1.ErrorClass_ERROR_CLASS_INTERNAL,
			ErrorType:    string(t),
			Message:      prose[t],
			SourceDetail: err.Error(),
		}
	}
	if logf != nil {
		logf("errclass: unclassified command error — no sentinel matched, falling through to %s: %v",
			TypeInternalUnclassified, err)
	}
	return &frontendv1.SystemFailureItem{
		ErrorClass:   frontendv1.ErrorClass_ERROR_CLASS_INTERNAL,
		ErrorType:    string(TypeInternalUnclassified),
		Message:      prose[TypeInternalUnclassified],
		SourceDetail: err.Error(),
	}
}

// InterruptError converts an interrupt's ACK outcome into the daemon's error
// channel: nil when the stop succeeded, ErrInterruptUndelivered when it did
// not.
//
// Only FAILED is a failure. ALREADY_COMPLETE is quiet SUCCESS — the user
// asked for the turn to be over and it already is — and reporting it as an
// error is exactly the mislabeling the outcome enum was added to stop: a
// daemon watching for an `aborted` result could not tell a stop that FAILED
// from a turn that had ALREADY ENDED, and painted the second as the first.
// Deriving it downstream is what made it ambiguous; this reads the shim's own
// verdict instead.
func InterruptError(outcome corev1.InterruptOutcome) error {
	if outcome != corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED {
		return nil
	}
	return ErrInterruptUndelivered
}

// statusTypes maps an HTTP status to its failure type. ApiErrorLine carries
// NO typed error enum — only a status, a message and is_network_down — so the
// status is the only structured evidence there is, and reading it is what
// keeps this from collapsing every API failure into one undifferentiated
// type. (The typed enum lives on ApiRetry, which Assistant covers.)
var statusTypes = map[int64]Type{
	400: TypeAPIInvalidRequest,
	401: TypeAPIAuthenticationFailed,
	402: TypeAPIBillingError,
	403: TypeAPIOAuthOrgNotAllowed,
	404: TypeAPIModelNotFound,
	422: TypeAPIInvalidRequest,
	429: TypeAPIRateLimit,
	529: TypeAPIOverloaded,
}

// Retrying reports whether an ApiErrorLine is mid-backoff — the SDK will try
// again, so the turn has NOT failed and there is nothing to report yet.
//
// This is the daemon's single answer to that question, and it lives in the
// classifier because it IS a classification. It used to exist as one rule in
// the progress resolver and a DIFFERENT rule in the webapp, plus a third for
// "fatal" on top, so the same bytes meant three things depending on who read
// them. Both processes hold the fact; only the daemon holds the cause.
func Retrying(ae *datav1.ApiErrorLine) bool {
	max := ae.GetMaxRetries()
	return max > 0 && ae.GetRetryAttempt() < max
}

// APIError classifies a TERMINAL ApiErrorLine — one whose retries are
// exhausted. A line that is still mid-backoff is not a failure to report: the
// turn is still in flight and the retrying window covers it, which is why the
// retrying/terminal split stays with the caller that holds the attempt counts.
//
// itemUUID is the conversation item the line arrived as, carried on the
// failure so a footer row can address the card.
func APIError(ae *datav1.ApiErrorLine, itemUUID string) *frontendv1.SystemFailureItem {
	t := apiErrorType(ae)
	msg := ae.GetError().GetMessage()
	if msg == "" {
		msg = prose[t]
	}
	if n := ae.GetMaxRetries(); n > 0 {
		msg = fmt.Sprintf("%s (after %d attempts)", msg, n)
	}
	return &frontendv1.SystemFailureItem{
		ErrorClass:   frontendv1.ErrorClass_ERROR_CLASS_API,
		ErrorType:    string(t),
		Message:      msg,
		SourceDetail: apiErrorDetail(ae),
		ItemUuid:     itemUUID,
	}
}

// apiErrorType names an ApiErrorLine from the evidence it carries: an exact
// status first, then the 5xx family, then a link that never reached the
// vendor, and finally the honest generic.
func apiErrorType(ae *datav1.ApiErrorLine) Type {
	d := ae.GetError()
	if t, ok := statusTypes[d.GetStatus()]; ok {
		return t
	}
	if s := d.GetStatus(); s >= 500 && s < 600 {
		return TypeAPIServerError
	}
	if d.GetIsNetworkDown() {
		return TypeAPINetworkDown
	}
	return TypeAPIRequestFailed
}

// apiErrorDetail is the structured account of an API failure, kept verbatim
// beside the prose rather than replacing it.
func apiErrorDetail(ae *datav1.ApiErrorLine) string {
	d := ae.GetError()
	parts := make([]string, 0, 4)
	if s := d.GetStatus(); s != 0 {
		parts = append(parts, fmt.Sprintf("status=%d", s))
	}
	if id := d.GetRequestId(); id != "" {
		parts = append(parts, "request_id="+id)
	}
	if d.GetIsNetworkDown() {
		parts = append(parts, "network_down")
	}
	if src := ae.GetSource(); src != "" {
		parts = append(parts, "source="+src)
	}
	return strings.Join(parts, " ")
}

// TurnEnd classifies an ERRORED turn conclusion. It returns nil for a turn
// that concluded normally, so the caller never has to ask twice.
//
// ssm.VendorBlockingTurnEnd is the authority on whether a conclusion blocks;
// this only names the block it found. Keeping the predicate there and the
// naming here means a stop reason cannot come to block without also having a
// name, or gain a name without blocking.
func TurnEnd(te *corev1.TurnEnded) *frontendv1.SystemFailureItem {
	reason := te.GetStopReason()
	if !ssm.VendorBlockingTurnEnd(reason, te.GetIsError()) {
		return nil
	}
	t, named := stopReasonTypes[reason]
	if !named {
		t = TypeAPITurnFailed
	}
	detail := ""
	if reason != "" {
		detail = "stop_reason=" + reason
	}
	return &frontendv1.SystemFailureItem{
		ErrorClass:   frontendv1.ErrorClass_ERROR_CLASS_API,
		ErrorType:    string(t),
		Message:      prose[t],
		SourceDetail: detail,
	}
}

// The persisted death-reason literals. They live beside their classification
// so a producer cannot write a reason the classifier does not know — which is
// exactly what happened to "shim_died", documented in the registry and
// present in a test fixture while no code path ever wrote it.
const (
	DeathReasonDeleted    = "delete session"
	DeathReasonSuperseded = "superseded"
	DeathReasonShimDied   = "shim_died"
)

// deathReasonTypes maps the persisted registry death reasons to their types.
// A registry carried over from an earlier build may hold anything outside this
// set, which is what the loud default is for.
var deathReasonTypes = map[string]Type{
	DeathReasonDeleted:    TypeSessionDeleted,
	DeathReasonSuperseded: TypeSessionSuperseded,
	DeathReasonShimDied:   TypeSessionShimDied,
}

// Death classifies a persisted registry death reason. It returns nil for the
// empty string: a session with no recorded reason is not a session that died
// mysteriously, it is one that has not died.
//
// A string outside the known set becomes session.ended_unclassified carrying
// the raw value, and logs — the backfillState precedent of a closed switch
// with a loud default, never a silent pass-through.
//
// resolvedAtMs is the durable instant the death STOPPED BEING TRUE (unix
// millis), zero while it still is. It rides straight onto the item because the
// item itself is derived fresh from the registry on every push: a death card
// that could not carry its own resolution would reopen on every push and every
// boot for the rest of the record's retention, which is exactly what a
// superseded session did. WINDOW-shaped deaths are the only ones a writer
// stamps; an EVENT-shaped one (a delete) passes zero forever.
//
// The item also carries a stable item_uuid keyed on the session, so a resolved
// re-push SETTLES the card the open one opened instead of landing beside it as
// a second card.
func Death(logf dlog.Logf, sessionID, reason string, resolvedAtMs int64) *frontendv1.SystemFailureItem {
	if reason == "" {
		return nil
	}
	t, known := deathReasonTypes[reason]
	if !known {
		t = TypeSessionEndedUnclassified
		if logf != nil {
			logf("errclass: unknown persisted death reason %q — classifying as %s; it predates the failure vocabulary or a producer skipped it",
				reason, t)
		}
	}
	return &frontendv1.SystemFailureItem{
		ErrorClass:   frontendv1.ErrorClass_ERROR_CLASS_INTERNAL,
		ErrorType:    string(t),
		Message:      prose[t],
		SourceDetail: reason,
		ItemUuid:     DeathItemUUID(sessionID),
		ResolvedAtMs: resolvedAtMs,
	}
}

// DeathItemUUID is the stable card identity of a session's death item. One
// session dies once, so the session id IS the identity; a resolved re-push
// therefore replaces the open card rather than joining it.
func DeathItemUUID(sessionID string) string {
	if sessionID == "" {
		return ""
	}
	return "death:" + sessionID
}

// StartFailed classifies a bring-up that never wired and could not be retried
// into one. reason is the deepest account the daemon holds of WHY — the shim's
// own degraded report where there is one, the wait's error where there is not —
// and it rides source_detail verbatim so the card names the failure rather than
// merely announcing one.
func StartFailed(reason string) *frontendv1.SystemFailureItem {
	return &frontendv1.SystemFailureItem{
		ErrorClass:   frontendv1.ErrorClass_ERROR_CLASS_INTERNAL,
		ErrorType:    string(TypeSessionStartFailed),
		Message:      prose[TypeSessionStartFailed],
		SourceDetail: reason,
	}
}

// KeepAliveWindowUnclosed classifies a failure to stamp a keep-alive window's
// end in the durable ledger.
//
// It is a system-failure CARD rather than a log line because of what an open
// window means: it excludes every later conversation item on the workspace,
// forever, from every rendering. The user's next prompt would appear to vanish.
// The card is the only thing that tells them why.
func KeepAliveWindowUnclosed(reason string) *frontendv1.SystemFailureItem {
	return &frontendv1.SystemFailureItem{
		ErrorClass:   frontendv1.ErrorClass_ERROR_CLASS_INTERNAL,
		ErrorType:    string(TypeKeepAliveWindowUnclosed),
		Message:      prose[TypeKeepAliveWindowUnclosed],
		SourceDetail: reason,
	}
}

// KeepAliveWindowInverted classifies a keep-alive window close the ledger
// refused because the end instant precedes the window's own start.
//
// It is a CARD rather than a log line for the reason every other exclusion
// fault is: what the user sees is the daemon's own keep-alive prompt sitting in
// their conversation as though they had typed it, with no other account of
// where it came from. The window itself is bounded — the ledger clamps the end
// to the start rather than writing an interval that covers nothing — so this
// reports a clock disagreement, never a rendering blackout.
func KeepAliveWindowInverted(reason string) *frontendv1.SystemFailureItem {
	return &frontendv1.SystemFailureItem{
		ErrorClass:   frontendv1.ErrorClass_ERROR_CLASS_INTERNAL,
		ErrorType:    string(TypeKeepAliveWindowInverted),
		Message:      prose[TypeKeepAliveWindowInverted],
		SourceDetail: reason,
	}
}

// Degraded classifies a shim-reported DegradedState — the store-write
// rejection path, whose reason is a StoreWriteAck error the shim wrapped.
//
// droppedCount rides source_detail rather than being discarded: how much
// conversation was lost is the single most useful thing about a store outage,
// and the passthrough translation threw it away.
func Degraded(component, reason string, droppedCount int64) *frontendv1.SystemFailureItem {
	parts := make([]string, 0, 3)
	if component != "" {
		parts = append(parts, "component="+component)
	}
	if reason != "" {
		parts = append(parts, reason)
	}
	if droppedCount > 0 {
		parts = append(parts, fmt.Sprintf("dropped=%d", droppedCount))
	}
	return &frontendv1.SystemFailureItem{
		ErrorClass:   frontendv1.ErrorClass_ERROR_CLASS_INTERNAL,
		ErrorType:    string(TypeShimStoreWriteRejected),
		Message:      prose[TypeShimStoreWriteRejected],
		SourceDetail: strings.Join(parts, " "),
	}
}

// UnexpectedQueryTermination classifies the reliable DegradedState emitted
// after the standing SDK iterator ends without an intentional shim shutdown.
func UnexpectedQueryTermination(component, reason string) *frontendv1.SystemFailureItem {
	detail := strings.TrimSpace(strings.Join([]string{"component=" + component, "reason=" + reason}, " "))
	return &frontendv1.SystemFailureItem{
		ErrorClass:   frontendv1.ErrorClass_ERROR_CLASS_INTERNAL,
		ErrorType:    string(TypeUnexpectedQueryTermination),
		Message:      prose[TypeUnexpectedQueryTermination],
		SourceDetail: detail,
	}
}

// ConnectionDegraded classifies the daemon's OWN observation that the
// missed-heartbeat window elapsed with no shim traffic. WINDOW-shaped: the
// caller re-sends it under the same uuid with resolved_at_ms set when traffic
// resumes, which is what makes the card settle instead of standing as a
// permanent alarm about something that ended.
func ConnectionDegraded(reason string) *frontendv1.SystemFailureItem {
	return &frontendv1.SystemFailureItem{
		ErrorClass:   frontendv1.ErrorClass_ERROR_CLASS_INTERNAL,
		ErrorType:    string(TypeShimDegraded),
		Message:      prose[TypeShimDegraded],
		SourceDetail: reason,
	}
}

// IsDaemonType reports whether a type is one the daemon may emit — i.e. that
// it does not carry the frontend-reserved prefix. It backs the namespace
// assertion that makes a partition violation a test failure rather than a
// review miss.
func IsDaemonType(t string) bool {
	return !strings.HasPrefix(t, ClientPrefix)
}

// AllTypes is every type this package can mint. It exists so the exhaustiveness
// and namespace tests can iterate the vocabulary instead of restating it.
func AllTypes() []Type {
	return []Type{
		TypeShimNotConnected,
		TypeShimRejected,
		TypeShimAckTimeout,
		TypeShimVersionMismatch,
		TypeShimSeqRegression,
		TypeShimDegraded,
		TypeShimStoreWriteRejected,
		TypeUnexpectedQueryTermination,
		TypeShimNotSpawned,
		TypeShimHandshakeIncomplete,
		TypeShimUnhealthy,
		TypeSessionNotEstablished,
		TypeSessionNotLive,
		TypeSessionDeleted,
		TypeSessionSuperseded,
		TypeSessionReconnectSuperseded,
		TypeSessionShimDied,
		TypeSessionStartFailed,
		TypeSessionResumeFailed,
		TypeSessionEndedUnclassified,
		TypeHistoryRepullInFlight,
		TypeHistoryReplayTruncated,
		TypeInterruptUndelivered,
		TypeQueueEntrySessionUnwired,
		TypeQueueEntryKeepAliveHeld,
		TypeSessionHibernated,
		TypeKeepAliveWindowUnclosed,
		TypeKeepAliveWindowInverted,
		TypeClientLogIdentityStale,
		TypeInternalUnclassified,
		TypeAPIAuthenticationFailed,
		TypeAPIBillingError,
		TypeAPIRateLimit,
		TypeAPIInvalidRequest,
		TypeAPIServerError,
		TypeAPIUnknown,
		TypeAPIOAuthOrgNotAllowed,
		TypeAPIOverloaded,
		TypeAPIModelNotFound,
		TypeAPIMaxOutputTokens,
		TypeAPINetworkDown,
		TypeAPIRequestFailed,
		TypeAPIMaxTurns,
		TypeAPIMaxBudget,
		TypeAPIExecutionError,
		TypeAPIRefusal,
		TypeAPITurnFailed,
	}
}
