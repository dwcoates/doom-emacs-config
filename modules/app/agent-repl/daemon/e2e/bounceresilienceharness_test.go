// THE BOUNCE-RESILIENCE HARNESS: the shared vocabulary of the suite that pins
// what must survive a DAEMON BOUNCE (the daemon dies and is replaced over one
// durable state directory while its shims keep running) and a SHIM BOUNCE (the
// daemon deliberately replaces a shim process under a conversation the user is
// keeping).
//
// WHAT IS BORROWED AND WHAT IS ADDED. The bounce itself is
// shutdownscheduleharness_test.go's `shutdownWorld` — the one place in this
// package where a daemon can be torn down and a successor stood up over the
// same state database, shim socket and running shim-store, with the REAL shim
// processes surviving in between. Everything here is the vocabulary those
// tests need on top of it: submitting prompts, asking and answering a real
// permission, reattaching a scoped frontend to the successor, and reading back
// the facts a reattach is supposed to re-establish.
//
// THE 5s FRAME BUDGET, AND WHY NOTHING HERE CAN HANG. Every wait in this suite
// goes through `awaitAll` (interrupt_e2e_test.go) or through a helper below
// that bounds itself the same way. The gaps this suite covers are gaps whose
// PRESENT-DAY behaviour is an indefinite wait — a permission that is never
// re-asked, a replay that never terminates, a turn that never settles — so a
// test written against them must fail on the ABSENCE OF EVIDENCE at a
// deadline, never sit on a channel until the go test timeout kills the whole
// package and takes every other result with it.
package e2e

import (
	"fmt"
	"runtime"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// --- world / boot options this suite adds -----------------------------------

// worldTuning is what a bounce-resilience world may bend about the SHIM BUNDLE
// every daemon in it spawns. It is deliberately only the build identity: a
// world that changed anything else would stop being the production shape the
// durability tests rest on.
type worldTuning struct{ shimBuildSHA string }

type worldOption func(*worldTuning)

// withWorldShimBuild bakes `sha` into the bundle this world builds from source,
// so a shim that survives a bounce can REPORT an identity for the successor to
// judge. Pair it with withBootShimBuild on the successor to arrange a shim that
// outlived a deploy.
func withWorldShimBuild(sha string) worldOption {
	return func(o *worldTuning) { o.shimBuildSHA = sha }
}

// withBootShimBuild tells ONE boot of the daemon what it should believe the
// current bundle to be. A boot whose value differs from the world's baked
// identity is the daemon that came up on the other side of a deploy.
func withBootShimBuild(sha string) bootOption {
	return func(o *bootTuning) { o.shimBuildSHA = sha }
}

// --- commands ---------------------------------------------------------------

// The ordinary user-sent prompt is asyncspecharness_test.go's `submitPrompt`,
// reused rather than re-declared.

// submitPromptInMode writes a SubmitPromptCmd carrying a prompt-scoped
// permission-mode override.
//
// A NON-SWITCHABLE MODE IS A REAL, DETERMINISTIC DEGRADATION. The shim applies
// the override to the live query and, when the mode cannot be set on a running
// session, reports a DegradedState on the `claude-shim-permission-mode`
// component (uds-session.ts submitPrompt → reportDegraded). That is the ONLY
// shim-owned degradation this harness can provoke on demand, and it is the one
// the degraded-across-a-bounce tests use, because it is genuinely the shim's
// own report rather than an event injected into the store on its behalf.
func submitPromptInMode(t *testing.T, conn *websocket.Conn, requestID, text, mode string) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(
		`{"requestId":%q,"submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT","permissionMode":%q}}`,
		requestID, text, mode))
}

// answerPermission writes a PermissionAnswerCmd for one parked question.
func answerPermission(t *testing.T, conn *websocket.Conn, workspace, requestID, permissionID string, allow bool) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(
		`{"requestId":%q,"workspace":%q,"permissionAnswer":{"permissionRequestId":%q,"allow":%v}}`,
		requestID, workspace, permissionID, allow))
}

// sendRestartSession writes a RestartSessionCmd — the USER-COMMANDED hard
// restart (FrontendCommand arm 23), which is the immediate half of the
// gated-stops contract.
func sendRestartSession(t *testing.T, conn *websocket.Conn, workspace, requestID string) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"workspace":%q,"restartSession":{}}`, requestID, workspace))
}

// sendResync writes a ResyncCmd for the whole history under a fence.
func sendResync(t *testing.T, conn *websocket.Conn, requestID, fence string) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"resync":{"fromSeq":0,"fence":%q}}`, requestID, fence))
}

// --- observation ------------------------------------------------------------

// pendingPermissionIn returns the id of the first PENDING permission item a
// frame carries for workspace, or "".
func pendingPermissionIn(frame *frontendv1.FrontendFrame, workspace string) string {
	for _, item := range deltaItems(frame, workspace) {
		if isPendingPermission(item) {
			return item.GetUuid()
		}
	}
	return ""
}

// degradedCardIn returns the failure card a frame carries for workspace whose
// evidence names component, or nil.
//
// The match is on the RENDERED CARD's text rather than on a component field:
// FailureCardFromDegradedState (frontend/translate.go) classifies the shim's
// DegradedState into the failure vocabulary, and the component survives that
// classification inside the card's own evidence. Matching the classified card
// is what makes this an assertion about what the USER is shown.
func degradedCardIn(frame *frontendv1.FrontendFrame, workspace, component string) *frontendv1.ConversationItem {
	for _, item := range deltaItems(frame, workspace) {
		card := item.GetFailureCard()
		if card == nil {
			continue
		}
		if strings.Contains(protoText(card), component) {
			return item
		}
	}
	return nil
}

// snapshotDegradedCard reports whether a connect StateSnapshot's workspace
// carries an open fault naming component. A reattaching frontend must learn a
// standing degradation from its RESYNC and not only from a live edge it may
// have missed, so this reads the snapshot's own account.
func snapshotDegradedCard(snap *frontendv1.StateSnapshot, workspace, component string) bool {
	for _, state := range snap.GetWorkspaces() {
		if state.GetWorkspace() != workspace {
			continue
		}
		for _, fault := range state.GetActiveFaults() {
			if strings.Contains(protoText(fault), component) {
				return true
			}
		}
	}
	return false
}

// protoText renders a proto message for substring matching and for failure
// messages. It is only ever used to look for a component name a producer put
// into a human-readable field, never to parse structure.
func protoText(msg interface{ String() string }) string {
	if msg == nil {
		return ""
	}
	return msg.String()
}

// itemKeys is the identity set of a conversation slice: every item's uuid, in
// arrival order. A replay's completeness is judged against the live stream's
// keys, so a hole is a MISSING KEY rather than a smaller number.
func itemKeys(items []*frontendv1.ConversationItem) []string {
	out := make([]string, 0, len(items))
	for _, item := range items {
		out = append(out, item.GetUuid())
	}
	return out
}

// missingKeys returns the members of want that never appear in got.
func missingKeys(want, got []string) []string {
	present := make(map[string]bool, len(got))
	for _, key := range got {
		present[key] = true
	}
	var out []string
	for _, key := range want {
		if !present[key] {
			out = append(out, key)
		}
	}
	return out
}

// --- waiting ----------------------------------------------------------------

// awaitPendingPermission reads conn until a PENDING permission item for
// workspace arrives, and returns its id. Bounded by the suite's frame budget:
// the gaps this suite covers turn "the question was never re-asked" into an
// indefinite wait, and that must surface as a named failure here.
func awaitPendingPermission(t *testing.T, conn *websocket.Conn, workspace, what string) string {
	t.Helper()
	var id string
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		what: func(frame *frontendv1.FrontendFrame) bool {
			if got := pendingPermissionIn(frame, workspace); got != "" {
				id = got
				return true
			}
			return false
		},
	})
	return id
}

// awaitReattachedPendingPermission is awaitPendingPermission for a socket dialled
// against a successor whose reattach has NOT been waited for, which is what
// `reattached` returns.
//
// THE TWO FACTS ARE AWAITED TOGETHER, and that is the whole point. A permission
// the shim re-asks on reattach can only be pushed once the handshake completes,
// so awaiting it alone spends the frame budget on the BRING-UP and leaves
// nothing for the push it is actually asserting — a test that fails on machine
// load rather than on the contract, which is exactly the residual failure mode
// frameTimeout's own comment names and prescribes the combined await for.
//
// Awaiting them separately does not work either, for the reason awaitAllSeeded
// exists: the two frames have no ordering between them, so whichever await ran
// first would read past the other's frame and discard it. One await over both
// makes the arrival order irrelevant instead of merely unlikely to matter.
func awaitReattachedPendingPermission(t *testing.T, conn *websocket.Conn, workspace, what string) string {
	t.Helper()
	var id string
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the successor painting " + workspace + " OPERATIONAL, which is the reattach handshake having completed": func(frame *frontendv1.FrontendFrame) bool {
			return workspaceStateFor(frame, workspace).GetConnectivity() == frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_OPERATIONAL
		},
		what: func(frame *frontendv1.FrontendFrame) bool {
			if got := pendingPermissionIn(frame, workspace); got != "" {
				id = got
				return true
			}
			return false
		},
	})
	return id
}

// awaitCount spins until count() reaches want, bounded by the suite's frame
// budget.
//
// Spinning on a counter is a RENDEZVOUS with the goroutine that moves it —
// there is no channel to wait on for "a shim was exec'd" — but an unbounded
// spin on a fact that never becomes true is a hang, and a hang takes the whole
// package's results down with it. The bound is what turns "this never
// happened" into a named failure.
func awaitCount(t *testing.T, count func() int, want int, what string) {
	t.Helper()
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		if got := count(); got >= want {
			return
		}
		runtime.Gosched()
	}
	t.Fatalf("count reached %d, want at least %d, before the deadline: %s", count(), want, what)
}

// awaitEcho reads conn until the fake's reply to prompt arrives. The fake
// echoes the prompt text back (`echo: <prompt> [mode=...]`), so a turn's own
// answer is how that turn is identified.
func awaitEcho(t *testing.T, conn *websocket.Conn, workspace, prompt, what string) {
	t.Helper()
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		what: func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, workspace) {
				if strings.Contains(assistantText(item), echoOf(prompt)) {
					return true
				}
			}
			return false
		},
	})
}

// awaitSettledTurn reads conn until the SSM resolves workspace to DONE under a
// turn_ended cause — the daemon's own statement that a turn finished cleanly,
// as distinct from the bare enum any producer can push.
func awaitSettledTurn(t *testing.T, conn *websocket.Conn, workspace string, reject func(*frontendv1.FrontendFrame) string, what string) {
	t.Helper()
	awaitAll(t, conn, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		what: func(frame *frontendv1.FrontendFrame) bool {
			return ssmResolved(frame, workspace, frontendv1.RenderState_RENDER_STATE_DONE, "turn_ended")
		},
	})
}

// rejectRestartInterrupted fails the moment workspace is painted INTERRUPTED.
//
// It is the negative half of the completed-during-the-gap contract, enforced
// over the same read loop rather than by waiting and then looking: a turn the
// shim genuinely finished must never be CUT by the reconciliation that meets
// the shim again, and "it settled DONE eventually" is satisfied by a workspace
// that was painted red-interrupted first and corrected afterwards.
func rejectRestartInterrupted(workspace, why string) func(*frontendv1.FrontendFrame) string {
	return func(frame *frontendv1.FrontendFrame) string {
		state := workspaceStateFor(frame, workspace)
		if state.GetState() != frontendv1.RenderState_RENDER_STATE_INTERRUPTED {
			return ""
		}
		return fmt.Sprintf("workspace %s was painted RENDER_STATE_INTERRUPTED (cause_kind=%q): %s",
			workspace, state.GetCauseKind(), why)
	}
}

// --- the edge of a bounce ----------------------------------------------------
//
// A test whose ACT is "this work is in flight and THEN the daemon dies" has to
// establish the first half before it performs the second. `writeCmd` cannot do
// that on its own — it returns when the websocket write completes, which is
// before the daemon has read the frame — and the daemon-side ack that would
// settle it is exactly what these tests arrange to lose. So the rendezvous is
// with the SHIM, on a line it writes synchronously as it takes the work up,
// through the world's stderr tap.

// shimSawInterrupt is the shim's own record that a daemon interrupt request
// reached it (uds-session.ts, logged synchronously at the top of the interrupt
// handler, before the verdict is computed and before anything is written back).
//
// Waiting for it establishes DELIVERY without establishing the answer: the
// verdict is still racing the bounce, which is the condition under test.
// Without it the bounce frequently wins the earlier race instead, the daemon
// nacks the interrupt as "no live session for workspace", and the shim's
// interrupt handler never runs at all — a session left parked on a permission
// nobody cancelled, which is not the scenario any of these tests describe.
const shimSawInterrupt = "processing daemon interrupt request"

// shimTookTheTurn is the fake's record that a submitted prompt reached it and
// it has chosen the turn to run (fake-query.ts, logged before the turn emits
// anything). It is how "a turn is under way" is established before a bounce.
const shimTookTheTurn = "selected fake turn branch"

// --- reattach ---------------------------------------------------------------

// reattached brings a scoped frontend up against the SUCCESSOR daemon for a
// workspace whose shim outlived the bounce, and returns the session id and the
// connection.
//
// THE ORDER IS LOAD-BEARING and is the one shutdownscheduledurability_e2e
// established: dial the scoped socket FIRST, then drive the boot sweep's
// re-check pass, then wait on its consequences. The re-check is what claims a
// redialled shim, so a subscriber attached after it can miss the very delivery
// it triggered. The re-check is fired on the OBSERVED redial
// (sweepRecheckWhenParked), never on test phase order.
// The WorkspaceState it returns is the one the scoped connect snapshot carried,
// which is the immutable authority tuple (the fence above all) a real frontend
// captures before dispatching a resync.
func reattached(t *testing.T, b *shutdownBoot, workspace string) (string, *websocket.Conn, *frontendv1.WorkspaceState) {
	t.Helper()
	snap := connectSnapshot(t, b)
	sessionID := snapshotSessionID(t, snap, workspace)
	conn := b.harness().dial(t, sessionID)
	first := readFrame(t, conn)
	if first.GetSnapshot() == nil {
		t.Fatalf("first scoped frame after the bounce = %T, want a StateSnapshot", first.GetFrame())
	}
	state := workspaceStateInSnapshot(t, first, workspace)
	b.sweepRecheckWhenParked(t, sessionID)
	return sessionID, conn, state
}

// reattachedSnapshot is `reattached` for a test whose evidence is the CONNECT
// SNAPSHOT taken after the successor has claimed the surviving shim: it drives
// the re-check first and then dials, because what it asserts is what a client
// joining a settled successor is told, not an edge.
// SETTLED IS WAITED FOR, NEVER ASSUMED. Firing the re-check only ENQUEUES the
// sweep; the reattach it starts — handshake, re-announce, ShimReady — completes
// on the daemon's own goroutines afterwards. A snapshot taken on the next line
// is a snapshot of a successor mid-bring-up, and every fact the reattach is
// supposed to re-establish would read as absent for the same reason: it has not
// happened YET. So this waits for the workspace to reach OPERATIONAL first,
// which the daemon reaches from the ShimReady hook (sessioncontroller
// noteConnectivity, cause "shim_ready") and therefore only after everything the
// shim sent ahead of that ack has been ingested.
//
// The watch is attached BEFORE the re-check is fired, because the edge it waits
// on is published by the very pass it triggers.
func reattachedSnapshot(t *testing.T, b *shutdownBoot, workspace string) (string, *frontendv1.StateSnapshot) {
	t.Helper()
	sessionID := snapshotSessionID(t, connectSnapshot(t, b), workspace)
	watch := b.dialFrontend(t)
	if first := readFrame(t, watch); first.GetSnapshot() == nil {
		t.Fatalf("first /frontend frame on the settle watch = %T, want a StateSnapshot", first.GetFrame())
	}
	b.sweepRecheckWhenParked(t, sessionID)
	awaitAll(t, watch, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the successor painting " + workspace + " OPERATIONAL, which is the reattach handshake having completed": func(frame *frontendv1.FrontendFrame) bool {
			return workspaceStateFor(frame, workspace).GetConnectivity() == frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_OPERATIONAL
		},
	})
	return sessionID, connectSnapshot(t, b)
}

// --- durable evidence -------------------------------------------------------

// storeTurnEndedFor reports whether the store holds a durable TurnEnded for
// turnID under the vendor conversation — the record the handshake
// reconciliation is supposed to judge a returning shim against.
//
// It reads the store the daemon reads, through the same replay path the e2e
// suite already uses (replayStoreEvents), so "the turn ended durably" is a fact
// about the shared durable record rather than about anything the test kept in
// memory.
func storeTurnEndedFor(t *testing.T, vendorSessionID, turnID string) bool {
	t.Helper()
	for _, ev := range replayStoreEvents(t, vendorSessionID) {
		ended := ev.GetTurnEnded()
		if ended == nil {
			continue
		}
		if turnID == "" || ended.GetTurnId() == turnID {
			return true
		}
	}
	return false
}

// awaitStoreTurnEndedFor waits, bounded by the suite's frame budget, for the
// store to hold a durable TurnEnded under the vendor conversation.
//
// WHY A WAIT AND NOT A READ. The shim's turn tail is several store round-trips
// (tool result, closing message, result, TurnEnded) and it is running while the
// daemon that started it is being torn down — nothing sequences the test's read
// after the last of those writes. A single read therefore samples a record that
// is still being written and reports "the shim never finished" for a shim that
// finishes a millisecond later, which is a false report about the ARRANGEMENT
// rather than about the contract. Each poll is one store subscribe-and-drain
// with its own read deadline, so the loop paces itself on the store's answers
// and never sleeps.
func awaitStoreTurnEndedFor(t *testing.T, vendorSessionID, turnID string) bool {
	t.Helper()
	deadline := time.Now().Add(frameTimeout)
	for {
		if storeTurnEndedFor(t, vendorSessionID, turnID) {
			return true
		}
		if !time.Now().Before(deadline) {
			return false
		}
	}
}

// storeTurnIDs returns every turn id the store has a start for, in seq order.
func storeTurnIDs(t *testing.T, vendorSessionID string) []string {
	t.Helper()
	var out []string
	for _, ev := range replayStoreEvents(t, vendorSessionID) {
		if started := ev.GetTurnStarted(); started != nil {
			out = append(out, started.GetTurnId())
		}
	}
	return out
}

// countStoreTurnEnds counts the durable TurnEnded records for one conversation.
// A double-fired interrupt shows up here as a second end for one start.
func countStoreTurnEnds(t *testing.T, vendorSessionID string) int {
	t.Helper()
	n := 0
	for _, ev := range replayStoreEvents(t, vendorSessionID) {
		if ev.GetTurnEnded() != nil {
			n++
		}
	}
	return n
}

// storeEventCount reports how many events one conversation has in the store. It
// is how "the shim kept streaming while unattached" is read: the number grows
// across the gap without any daemon having been there to see it.
func storeEventCount(t *testing.T, vendorSessionID string) int {
	t.Helper()
	return len(replayStoreEvents(t, vendorSessionID))
}
