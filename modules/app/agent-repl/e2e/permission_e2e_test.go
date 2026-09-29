// permission_e2e_test.go — Permissions area (SPEC.md section C, "Permissions",
// entries #11-17). Drives the real shim's permission gate end to end:
// SubmitPrompt -> the shim's canUseTool callback -> AnswerPermission /
// SetPermissionMode(-by-vendor) / UpdateHeldPrompt, against the daemon's
// Connect API. Contract: docs/overhaul/shim.md section "The permission gate",
// docs/overhaul/daemon.md section "Queue, holds, leases — contract facts",
// proto/src/conversation/v1/permission.proto, proto/src/frontend/v1/feed.proto
// and daemon_hold.proto. Every assertion here is on a WatchFeed / WatchTopbar
// / WatchDaemonHolds frame — never on shim or store internals (SPEC.md
// section B, "Frontends: none").
//
// GROUNDING NOTE — golden name vs. fake-SDK scenario name are NOT the same
// string. Cross-checked against
// agent-shim/claude/shim/testdata/captures/MANIFEST.md's per-scenario
// meta.json files and agent-shim/claude/shim/src/fake/scenarios/permissions.ts
// (none of these six goldens carry an UNGROUNDED/INVENTED or DECLARED-ONLY
// mark in the manifest):
//   - permission-allow-once         -> "!perm-allow-once"
//   - permission-allow-standing     -> "!perm-allow-standing"
//   - permission-denied-by-user     -> "!perm-deny-user"
//   - permission-denied-by-policy   -> "!perm-deny-policy"
//   - permission-mode-changed       -> "!perm-allow-standing-mode" (the only
//     permission scenario whose ask offers a `setMode` suggestion; confirmed
//     by reading permissions.ts's PERM_ALLOW_STANDING_MODE body, not guessed
//     from the golden's name).
//   - permission-undecidable-parked -> "!perm-hold", NOT "!perm-undecidable".
//     The "perm-undecidable" scenario (permissions.ts) denies immediately and
//     concludes the turn — it never parks. The golden's own capture record
//     (testdata/captures/permission-undecidable-parked/meta.json) expects
//     "can_use_tool request never answered ... interrupted with a pending
//     gate", which is exactly permissions.ts's PERM_HOLD scenario ("a gated
//     Bash ask, and then a turn that PARKS however the ask resolves ... the
//     only terminal it can reach is an interrupt's").
//   - held-turn-gate (#16) has no fake-SDK scenario of its own to drive by
//     name — it is a DAEMON queue mechanism (daemon.md "Queue, holds, leases
//     — contract facts"), not a vendor-shaped golden. This test drives it
//     with lifecycle.ts's "!hold" scenario (a turn that stays open until
//     interrupted) as the first, occupying turn, so a second submission on
//     the same agent is provably HELD rather than merely fast.
package e2e

import (
	"context"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// ---------------------------------------------------------------------------
// Helpers local to this file. World/SubmitPrompt/AwaitTurnEnded etc. come
// from world_test.go; OpenWorkspace/OpenFeed/AnswerPermission/Interrupt/
// UpdateHeldPrompt have no existing wrapper there, so this file calls the
// daemon's Connect API for them directly, exactly as world_test.go's own
// helpers do for the rpcs it already wraps.
// ---------------------------------------------------------------------------

// pmNewPermissionWorld builds one World, registers a fresh (scripted fake-git)
// repository as a workspace, and opens it — the precondition every
// permission test shares. Real git is out of scope for this suite (project
// lead ruling, 2026-09-02): every external dependency here is mocked — the
// scripted fake `git` the harness installs by default, and the fake SDK
// behind the real shim's `--fake` flag.
func pmNewPermissionWorld(t *testing.T) (*World, *workspacev1.WorkspaceRef) {
	t.Helper()
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	pmOpenWorkspace(t, w, ws)
	return w, ws
}

// pmOpenWorkspace sends OpenWorkspace, which is what spawns the real shim
// session a permission test needs before it can SubmitPrompt.
func pmOpenWorkspace(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) {
	t.Helper()
	resp, err := w.Client().OpenWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenWorkspace: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenWorkspace = %v, want a success", resp.Msg)
	}
}

// pmOpenFeedWatch opens the workspace's root feed and answers both the current
// page and a live tail from exactly where the page ends — the same pattern
// world_test.go's AwaitTurnEnded uses, generalized so this file's own
// predicates are not limited to "turn ended".
func pmOpenFeedWatch(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) (*frontendv1.FeedPageSuccess, *harness.Stream[*frontendv1.FeedRow]) {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}
	return success.GetPage().GetSuccess(), w.WatchFeedOn(w.Client(), success.GetWatch())
}

// pmAwaitFeedRow opens the feed and answers the first row (already on the
// current page, or arriving on the tail) satisfying pred. Used whenever a
// permission test needs a row whose CURRENT state is stable until the test
// itself changes it (an open ask stays open until answered; a settled row
// stays settled) — see pmWatchFeedFromNow for the case where the sequence of
// pushes, not merely the current value, is the fact under test.
func pmAwaitFeedRow(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, what string, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	t.Helper()
	page, stream := pmOpenFeedWatch(t, w, ws)
	defer stream.Close()
	for _, row := range page.GetRows() {
		if pred(row) {
			return row
		}
	}
	return harness.AwaitView(t, w.Ctx(), stream, what, pred)
}

// pmWatchFeedFromNow opens a fresh feed watch and answers only the live
// stream, discarding the current page. A feed row is upserted by id — a
// watch started AFTER the fact only ever sees an id's final value — so a
// test that must prove something never happened along the way (an ask was
// never open; a turn never ended early) has to be watching before the event
// that might produce it, not merely check the outcome afterward.
func pmWatchFeedFromNow(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) *harness.Stream[*frontendv1.FeedRow] {
	t.Helper()
	_, stream := pmOpenFeedWatch(t, w, ws)
	return stream
}

// pmExpectNoTurnEndedPush asserts that no FeedTurnEnded row for turn arrives on
// stream within probe. A negative assertion, so it necessarily waits out a
// bound (harness.ProbeWindow, the suite's existing convention for this)
// rather than synchronizing on an event — mirrors harness.ExpectNoPush, but
// filtered to one turn, since unrelated pushes (a running-tool progress beat)
// are expected and must not trip it.
func pmExpectNoTurnEndedPush(t *testing.T, ctx context.Context, stream *harness.Stream[*frontendv1.FeedRow], turn *conversationv1.TurnId, probe time.Duration, what string) {
	t.Helper()
	deadline, cancel := context.WithTimeout(ctx, probe)
	defer cancel()
	for {
		select {
		case row, ok := <-stream.C:
			if !ok {
				return
			}
			if endsTurn(turn)(row) {
				t.Fatalf("%s: got a turn-ended push %v, want none", what, row.GetTurnEnded())
			}
		case <-deadline.Done():
			return
		}
	}
}

// ---------------------------------------------------------------------------
// #11 PermissionAllowOnce, #12 PermissionAllowStanding,
// #13 PermissionDeniedByUser
// ---------------------------------------------------------------------------

// TestPermissionAskAnsweredArms drives the three permission scenarios whose
// gate reaches an OPEN ask (shim.md "The permission gate": "AgentPermission
// .start from the gate callback"), answers it with the arm under test, and
// asserts: the permission card settles with the matching verdict, the gated
// Bash tool call settles under the SAME turn, and — DENY-AND-CONTINUE
// (daemon.md "Queue, holds, leases — contract facts", ruled 2026-08-29:
// "declining a permission denies that tool and the agent may route around
// it; the turn does NOT end") — every arm reaches the manifest's
// success.completed terminal, deny included.
func TestPermissionAskAnsweredArms(t *testing.T) {
	t.Parallel()
	cases := []struct {
		name         string
		prompt       string
		buildRequest func(ws *workspacev1.WorkspaceRef, permission *frontendv1.FeedId) *agentreplv1.AnswerPermissionRequest
		wantToolRan  bool
	}{
		{
			name:   "AllowOnce",
			prompt: "!perm-allow-once",
			buildRequest: func(ws *workspacev1.WorkspaceRef, id *frontendv1.FeedId) *agentreplv1.AnswerPermissionRequest {
				return &agentreplv1.AnswerPermissionRequest{
					Workspace:  ws,
					Permission: id,
					Answer:     &agentreplv1.AnswerPermissionRequest_AllowOnce{AllowOnce: &agentreplv1.AnswerPermissionAllowOnce{}},
				}
			},
			wantToolRan: true,
		},
		{
			name:   "AllowStanding",
			prompt: "!perm-allow-standing",
			buildRequest: func(ws *workspacev1.WorkspaceRef, id *frontendv1.FeedId) *agentreplv1.AnswerPermissionRequest {
				return &agentreplv1.AnswerPermissionRequest{
					Workspace:  ws,
					Permission: id,
					Answer:     &agentreplv1.AnswerPermissionRequest_AllowStanding{AllowStanding: &agentreplv1.AnswerPermissionAllowStanding{}},
				}
			},
			wantToolRan: true,
		},
		{
			// SPEC.md's own entry #12 also mentions "SessionUpdate's
			// authoritative permission-mode restatement (set_mode)" for
			// allow-standing. Reading permissions.ts's PERM_ALLOW_STANDING
			// body (support.ts's default askPermission suggestion is
			// add-rules only, never setMode) shows THIS scenario's ask never
			// offers a mode change, so this subtest asserts only the
			// standing echo token. The mode-restatement fact is real and
			// grounded, but by the DIFFERENT scenario #17
			// (PermissionModeChangedMidSession) drives —
			// "!perm-allow-standing-mode" — not this one.
			name:   "DeniedByUser",
			prompt: "!perm-deny-user",
			buildRequest: func(ws *workspacev1.WorkspaceRef, id *frontendv1.FeedId) *agentreplv1.AnswerPermissionRequest {
				return &agentreplv1.AnswerPermissionRequest{
					Workspace:  ws,
					Permission: id,
					Answer:     &agentreplv1.AnswerPermissionRequest_Deny{Deny: &agentreplv1.AnswerPermissionDeny{}},
				}
			},
			wantToolRan: false,
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			w, ws := pmNewPermissionWorld(t)
			turn := SubmitPrompt(t, w, ws, tc.prompt)
			askRow := pmAwaitFeedRow(t, w, ws, "the open permission ask", func(r *frontendv1.FeedRow) bool {
				return r.GetTurn().GetValue() == turn.GetValue() && r.GetPermission().GetOpen() != nil
			})
			// support.ts: "askPermission always offers one" (a standing
			// suggestion) unless a scenario explicitly asks for none — none
			// of these three do.
			if askRow.GetPermission().GetStandingOffered() == nil {
				t.Fatalf("permission ask standing_offered = unset, want present")
			}

			// Act
			resp, err := w.Client().AnswerPermission(w.Ctx(), connect.NewRequest(tc.buildRequest(ws, askRow.GetId())))
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("AnswerPermission(%s) = %v, %v, want a success", tc.name, resp, err)
			}

			// Assert: the card settles with the matching verdict.
			answeredRow := pmAwaitFeedRow(t, w, ws, "the answered permission card", func(r *frontendv1.FeedRow) bool {
				return r.GetId().GetValue() == askRow.GetId().GetValue() && r.GetPermission().GetAnswered() != nil
			})
			answer := answeredRow.GetPermission().GetAnswered()
			switch tc.name {
			case "AllowOnce":
				if answer.GetAllowedOnce() == nil {
					t.Fatalf("answered permission = %v, want allowed_once", answer)
				}
			case "AllowStanding":
				if answer.GetAllowedStanding() == nil {
					t.Fatalf("answered permission = %v, want allowed_standing", answer)
				}
			case "DeniedByUser":
				if answer.GetDeniedByUser() == nil {
					t.Fatalf("answered permission = %v, want denied_by_user", answer)
				}
			}

			// Assert: the gated Bash tool call settles under the SAME turn.
			toolRow := pmAwaitFeedRow(t, w, ws, "the gated Bash tool call's settled state", func(r *frontendv1.FeedRow) bool {
				call := r.GetActivity().GetSimpleToolCall()
				return r.GetTurn().GetValue() == turn.GetValue() && call != nil && (call.GetReturned() != nil || call.GetDenied() != nil)
			})
			call := toolRow.GetActivity().GetSimpleToolCall()
			if tc.wantToolRan {
				if call.GetReturned() == nil || call.GetReturned().GetSucceeded() == nil {
					t.Fatalf("gated Bash outcome = %v, want returned.succeeded", call)
				}
			} else {
				// shim.md "The permission gate", project-lead ruling
				// 2026-09-01 (final): "a denied tool has NO result; its unit
				// settles failure with content UNSET ... and is drawn denied
				// via its permission unit" — the feed's own arm for this is
				// FeedToolCallDenied, never returned.failed.
				if call.GetDenied() == nil {
					t.Fatalf("gated Bash outcome = %v, want denied (ruling 2026-09-01: no result, drawn denied)", call)
				}
			}

			// Assert: DENY-AND-CONTINUE — every arm here reaches the
			// manifest's success.completed terminal.
			endedRow := AwaitTurnEnded(t, w, ws, turn)
			if endedRow.GetTurnEnded().GetConcluded() == nil {
				t.Fatalf("turn ended = %v, want concluded (success.completed)", endedRow.GetTurnEnded())
			}
		})
	}
}

// ---------------------------------------------------------------------------
// #14 PermissionDeniedByPolicy
// ---------------------------------------------------------------------------

// TestPermissionDeniedByPolicy drives "!perm-deny-policy", one of the "two
// denials that never reach the callback" (shim.md "The permission gate"):
// permission.proto's AgentPermissionDeniedByPolicy is documented as "the
// only denial that never had an open ask: a consumer sees `start` and this
// in one frame, or this alone". This test watches the feed from BEFORE the
// prompt is submitted so it can prove the open state was never observed —
// not merely absent from the final snapshot (see pmWatchFeedFromNow).
func TestPermissionDeniedByPolicy(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := pmNewPermissionWorld(t)
	stream := pmWatchFeedFromNow(t, w, ws)
	defer stream.Close()

	// Act
	turn := SubmitPrompt(t, w, ws, "!perm-deny-policy")

	// Assert
	var sawOpen, sawDeniedByPolicy, sawToolDenied bool
	for !(sawDeniedByPolicy && sawToolDenied) {
		row := harness.AwaitNext(t, w.Ctx(), stream, "a permission/tool push for this turn")
		if row.GetTurn().GetValue() != turn.GetValue() {
			continue
		}
		if p := row.GetPermission(); p != nil {
			if p.GetOpen() != nil {
				sawOpen = true
			}
			if p.GetAnswered().GetDeniedByPolicy() != nil {
				sawDeniedByPolicy = true
			}
		}
		if call := row.GetActivity().GetSimpleToolCall(); call.GetDenied() != nil {
			sawToolDenied = true
		}
	}
	if sawOpen {
		t.Fatalf("a policy denial showed an open ask; want no open ask ever (permission.proto: policy is " +
			"'the only denial that never had an open ask')")
	}

	endedRow := AwaitTurnEnded(t, w, ws, turn)
	if endedRow.GetTurnEnded().GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want concluded (success.completed)", endedRow.GetTurnEnded())
	}
}

// ---------------------------------------------------------------------------
// #15 PermissionUndecidableParked
// ---------------------------------------------------------------------------

// TestPermissionUndecidableParked drives "!perm-hold" (see the file header's
// grounding note for why this scenario, not "!perm-undecidable", produces
// the permission-undecidable-parked golden's shape): the ask opens and PARKS
// — the turn reaches no terminal while it stands — until an interrupt lands,
// at which point the turn ends interrupted. daemon.md's queue/holds section
// and shim.md's gate section both describe this as the parked-prompt shape;
// the golden's own meta.json records the same expectation verbatim
// ("can_use_tool request never answered ... interrupted with a pending
// gate").
func TestPermissionUndecidableParked(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := pmNewPermissionWorld(t)
	turn := SubmitPrompt(t, w, ws, "!perm-hold")
	askRow := pmAwaitFeedRow(t, w, ws, "the parked permission ask", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == turn.GetValue() && r.GetPermission().GetOpen() != nil
	})
	if askRow.GetPermission().GetOpen() == nil {
		t.Fatalf("permission ask = %v, want open", askRow.GetPermission())
	}

	// Assert: no terminal arrives while the ask stands.
	probeStream := pmWatchFeedFromNow(t, w, ws)
	pmExpectNoTurnEndedPush(t, w.Ctx(), probeStream, turn, harness.ProbeWindow, "the turn ending while its ask is still parked")
	probeStream.Close()

	// Act: release the park.
	resp, err := w.Client().Interrupt(w.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: ws,
		Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("Interrupt = %v, %v, want a success", resp, err)
	}

	// Assert: the turn now ends interrupted.
	endedRow := AwaitTurnEnded(t, w, ws, turn)
	if endedRow.GetTurnEnded().GetInterrupted() == nil {
		t.Fatalf("turn ended = %v, want interrupted", endedRow.GetTurnEnded())
	}
}

// ---------------------------------------------------------------------------
// #16 HeldTurnGate
// ---------------------------------------------------------------------------

// TestHeldTurnGate drives daemon.md's "Queue, holds, leases — contract
// facts": "One turn in flight PER AGENT, structurally... A prompt submitted
// while a turn runs is HELD daemon-side". Turn one is lifecycle.ts's "!hold"
// scenario (parks until interrupted), so a second submission on the SAME
// agent is provably held rather than merely fast. WatchDaemonHolds shows the
// HeldPrompt; UpdateHeldPrompt{release} delivers it, per daemon_hold.proto's
// "Deliver NOW — override the hold (interrupting the running turn when that
// is what delivery takes)".
func TestHeldTurnGate(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := pmNewPermissionWorld(t)
	turn1 := SubmitPrompt(t, w, ws, "!hold")
	holds := w.WatchHolds(ws)
	defer holds.Close()

	// Act
	turn2 := SubmitPrompt(t, w, ws, "a second prompt, held behind the running turn")

	// Assert: the tray shows turn2 held.
	tray := harness.AwaitView(t, w.Ctx(), holds, "the tray to show turn2 held", func(v *frontendv1.DaemonHoldTray) bool {
		for _, item := range v.GetItems() {
			if item.GetPrompt().GetTurn().GetValue() == turn2.GetValue() {
				return true
			}
		}
		return false
	})
	var held *frontendv1.HeldPrompt
	for _, item := range tray.GetItems() {
		if p := item.GetPrompt(); p.GetTurn().GetValue() == turn2.GetValue() {
			held = p
		}
	}
	if held == nil {
		t.Fatalf("held prompt for turn2 vanished between predicate and read")
	}

	// Act: force delivery now.
	resp, err := w.Client().UpdateHeldPrompt(w.Ctx(), connect.NewRequest(&agentreplv1.UpdateHeldPromptRequest{
		Workspace: ws,
		Turn:      turn2,
		Action:    &agentreplv1.UpdateHeldPromptRequest_Release{Release: &agentreplv1.UpdateHeldPromptRelease{}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateHeldPrompt{release} = %v, %v, want a success", resp, err)
	}

	// Assert: the tray empties turn2's hold, and turn2 actually runs to a
	// terminal.
	harness.AwaitView(t, w.Ctx(), holds, "the tray to empty turn2's hold", func(v *frontendv1.DaemonHoldTray) bool {
		for _, item := range v.GetItems() {
			if item.GetPrompt().GetTurn().GetValue() == turn2.GetValue() {
				return false
			}
		}
		return true
	})
	endedRow2 := AwaitTurnEnded(t, w, ws, turn2)
	if endedRow2.GetTurnEnded() == nil {
		t.Fatalf("turn2 ended = %v, want a terminal", endedRow2)
	}

	// The release forced turn1 out of its park (it was the running turn
	// occupying the agent); drain its own terminal too, so nothing is left
	// in flight when this test's World tears down.
	AwaitTurnEnded(t, w, ws, turn1)
}

// ---------------------------------------------------------------------------
// #17 PermissionModeChangedMidSession
// ---------------------------------------------------------------------------

// TestPermissionModeChangedMidSession drives "!perm-allow-standing-mode",
// whose ask offers a standing carrying `setMode: acceptEdits` (permissions.ts
// PERM_ALLOW_STANDING_MODE — "the ONE grounded producer of a mode-changing
// grant"). Granting standing (AnswerPermission{allow_standing}, no
// SetPermissionMode call of the client's own) moves the session's permission
// mode; shim.md: "a standing grant's set_mode can change the session's
// permission mode, restated authoritatively on WatchSession" — surfaced here
// on the topbar's permission-mode picker (frontend/v1/topbar.proto).
func TestPermissionModeChangedMidSession(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := pmNewPermissionWorld(t)
	topbar := w.WatchTopbar(ws)
	defer topbar.Close()
	initial := harness.AwaitView(t, w.Ctx(), topbar, "the initial permission-mode picker", func(v *frontendv1.TopbarView) bool {
		return v.GetPermissionModePicker().GetCurrent().GetMode() != ""
	})
	beforeMode := initial.GetPermissionModePicker().GetCurrent().GetMode()

	turn := SubmitPrompt(t, w, ws, "!perm-allow-standing-mode")
	askRow := pmAwaitFeedRow(t, w, ws, "the mode-offering permission ask", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == turn.GetValue() && r.GetPermission().GetOpen() != nil
	})
	if askRow.GetPermission().GetStandingOffered() == nil {
		t.Fatalf("mode-offering permission ask standing_offered = unset, want present")
	}

	// Act: allow with standing. No SetPermissionMode call is made — the mode
	// change, if any, comes solely from the vendor's offered standing.
	resp, err := w.Client().AnswerPermission(w.Ctx(), connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
		Workspace:  ws,
		Permission: askRow.GetId(),
		Answer:     &agentreplv1.AnswerPermissionRequest_AllowStanding{AllowStanding: &agentreplv1.AnswerPermissionAllowStanding{}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("AnswerPermission{allow_standing} = %v, %v, want a success", resp, err)
	}

	// Assert: the topbar's mode picker updates to acceptEdits without a
	// client-initiated SetPermissionMode call.
	// THE DAEMON'S OWN VOCABULARY IS SNAKE_CASE. The picker's mode string is
	// composed by daemon/internal/resolve/topbar/resolver.go:558-559
	// (permissionModeName: AgentPermissionMode_AcceptEdits -> "accept_edits"),
	// not the vendor's camelCase `acceptEdits`, so the old expectation could
	// never match.
	updated := harness.AwaitView(t, w.Ctx(), topbar, "the mode picker to reflect the vendor-offered accept_edits mode", func(v *frontendv1.TopbarView) bool {
		return v.GetPermissionModePicker().GetCurrent().GetMode() == "accept_edits"
	})
	if got := updated.GetPermissionModePicker().GetCurrent().GetMode(); got == beforeMode {
		t.Fatalf("permission mode did not change from %q", beforeMode)
	}

	AwaitTurnEnded(t, w, ws, turn)
}

// ---------------------------------------------------------------------------
// Coverage extension — `!perm-no-standing`: an ask that offers NO standing.
// ---------------------------------------------------------------------------

// TestPermissionAskOffersNoStanding drives "!perm-no-standing"
// (permissions.ts's PERM_NO_STANDING, which calls askPermission with
// `{ suggestions: [] }` — "the shape the vendor sends when no standing rule
// could be written for the call. The ask can only ever produce a
// once-allow").
//
// The contract fact under test is feed.proto's FeedPermission.standing_offered
// (field 5): "PRESENT iff the vendor offered a standing form — presence is
// what makes the 'always allow' button drawable." So an ask with no
// suggestions must draw the card with standing_offered UNSET, and that
// absence is the whole assertion: it is the exact negative of
// TestPermissionAskAnsweredArms's positive check, which requires the field
// PRESENT for the three scenarios whose asks do offer one.
//
// The once-allow is then answered for real, so the test also pins that a
// no-standing ask still reaches the ordinary allowed_once verdict and runs
// its gated call — an unoffered standing narrows the buttons, it does not
// break the card.
func TestPermissionAskOffersNoStanding(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := pmNewPermissionWorld(t)

	// Act
	turn := SubmitPrompt(t, w, ws, "!perm-no-standing")
	askRow := pmAwaitFeedRow(t, w, ws, "the open permission ask", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == turn.GetValue() && r.GetPermission().GetOpen() != nil
	})

	// Assert: the card offers NO standing.
	if askRow.GetPermission().GetStandingOffered() != nil {
		t.Fatalf("permission ask standing_offered = present, want UNSET: the scenario's ask carries no "+
			"suggestions, and feed.proto declares standing_offered \"PRESENT iff the vendor offered a standing "+
			"form\" (row %v)", askRow)
	}

	// Act: the only answer such an ask can produce is a once-allow.
	resp, err := w.Client().AnswerPermission(w.Ctx(), connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
		Workspace:  ws,
		Permission: askRow.GetId(),
		Answer:     &agentreplv1.AnswerPermissionRequest_AllowOnce{AllowOnce: &agentreplv1.AnswerPermissionAllowOnce{}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("AnswerPermission(AllowOnce) = %v, %v, want a success", resp, err)
	}

	// Assert: it settles allowed_once, still with no standing offered.
	answeredRow := pmAwaitFeedRow(t, w, ws, "the answered permission card", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == askRow.GetId().GetValue() && r.GetPermission().GetAnswered() != nil
	})
	if answeredRow.GetPermission().GetAnswered().GetAllowedOnce() == nil {
		t.Fatalf("answered permission = %v, want allowed_once", answeredRow.GetPermission().GetAnswered())
	}
	if answeredRow.GetPermission().GetStandingOffered() != nil {
		t.Errorf("answered card standing_offered = present, want UNSET: answering must not synthesize an offer the ask never made (row %v)", answeredRow)
	}

	// Assert: the gated `git log -1` ran and the turn concluded.
	toolRow := pmAwaitFeedRow(t, w, ws, "the gated Bash tool call's settled state", func(r *frontendv1.FeedRow) bool {
		call := r.GetActivity().GetSimpleToolCall()
		return r.GetTurn().GetValue() == turn.GetValue() && call != nil && (call.GetReturned() != nil || call.GetDenied() != nil)
	})
	if call := toolRow.GetActivity().GetSimpleToolCall(); call.GetReturned().GetSucceeded() == nil {
		t.Fatalf("gated Bash outcome = %v, want returned.succeeded", call)
	}
	if ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded(); ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want concluded", ended)
	}
}

// ---------------------------------------------------------------------------
// Coverage extension — `!perm-undecidable`: denied for want of a decider.
// ---------------------------------------------------------------------------

// TestPermissionDeniedForWantOfDecider drives "!perm-undecidable"
// (permissions.ts's PERM_UNDECIDABLE) for real, rather than merely naming it
// in a comment as TestPermissionDeniedByPolicy does.
//
// # THE DISCRIMINATOR, ARM BY ARM
//
// permission.proto declares THREE denial arms and gives `undecidable` its own
// (AgentPermissionDenied.undecidable, field 3): "NOBODY REFUSED ... Drawn as a
// user refusal it accuses the user of something they did not do; drawn as
// policy it implies a rule that does not exist." The shim honors that at the
// conversation layer — src/convert/permission.ts maps
// `decision_reason_type: "classifier"` (its UNDECIDED_DECIDER constant) onto
// the undecidable arm, and every other discriminator onto `policy`.
//
// The frontend contract now honors it too: feed.proto's
// FeedPermissionAnswered.denied_undecidable (landing 10) is the undecidable
// denial's own arm, so daemon/internal/resolve/feed/permission.go's
// decisionArm no longer folds it onto denied_by_policy. This test asserts that
// arm and pins the composed wording it carries ("denied for want of a
// decider"), which permission.go spells only here and never for a real policy
// denial.
func TestPermissionDeniedForWantOfDecider(t *testing.T) {
	t.Parallel()
	// Arrange: watch from BEFORE the prompt — like a policy denial, this one
	// never has an open ask, and a watch started afterward could only ever
	// see the row's final value (see pmWatchFeedFromNow).
	w, ws := pmNewPermissionWorld(t)
	stream := pmWatchFeedFromNow(t, w, ws)
	defer stream.Close()

	// Act
	turn := SubmitPrompt(t, w, ws, "!perm-undecidable")

	// Assert
	var sawOpen, sawToolDenied bool
	var answered *frontendv1.FeedPermissionAnswered
	for answered == nil || !sawToolDenied {
		row := harness.AwaitNext(t, w.Ctx(), stream, "a permission/tool push for this turn")
		if row.GetTurn().GetValue() != turn.GetValue() {
			continue
		}
		if p := row.GetPermission(); p != nil {
			if p.GetOpen() != nil {
				sawOpen = true
			}
			if a := p.GetAnswered(); a != nil {
				answered = a
			}
		}
		if call := row.GetActivity().GetSimpleToolCall(); call.GetDenied() != nil {
			sawToolDenied = true
		}
	}
	if sawOpen {
		t.Errorf("an undecidable denial showed an open ask; want none ever — the classifier denied without " +
			"reaching the callback, exactly as a policy denial does")
	}
	if answered.GetDeniedUndecidable() == nil {
		t.Fatalf("answered permission = %v, want denied_undecidable: frontend/v1/feed.proto's "+
			"FeedPermissionAnswered carries the conversation layer's AgentPermissionDenied.undecidable "+
			"as its own arm (landing 10), never folded onto denied_by_policy", answered)
	}
	if got := answered.GetDeniedUndecidable().GetText(); !strings.Contains(got, "denied for want of a decider") {
		t.Errorf("denied_undecidable text = %q, want the want-of-a-decider wording %q", got,
			"denied for want of a decider")
	}
	// The vendor's own account rides along as the detail clause, which is
	// what makes the composed line say WHAT could not decide.
	if got := answered.GetDeniedUndecidable().GetText(); !strings.Contains(got, "could not reach a verdict") {
		t.Errorf("denied_undecidable text = %q, want the scenario's own detail (%q) appended as the reason clause",
			got, "could not reach a verdict")
	}

	if ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded(); ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want concluded (a denial is an answer, and the agent routes around it)", ended)
	}
}
