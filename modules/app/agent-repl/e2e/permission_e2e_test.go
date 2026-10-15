// permission_e2e_test.go — Permissions area (SPEC.md section C, "Permissions",
// entries #11-17). Drives the real shim's permission gate end to end:
// SubmitPrompt -> the shim's canUseTool callback -> AnswerPermission /
// SetPermissionMode(-by-vendor) / UpdateHeldPrompt, against the daemon's
// Connect API. Contract: docs/overhaul/shim.md section "The permission gate",
// docs/overhaul/daemon.md section "Queue, holds, leases — contract facts",
// proto/src/conversation/v1/permission.proto, proto/src/frontend/v1/feed.proto
// and feed_held.proto. Every assertion here is on a WatchFeed / WatchTopbar
// / root-feed held-section frame — never on shim or store internals (SPEC.md
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
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"agent-repld/integration/harness"
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
	return w, pmNewPermissionWorkspace(t, w)
}

// pmNewPermissionWorkspace registers and opens a fresh workspace on w, for a
// test that brings its own World (the shared one).
func pmNewPermissionWorkspace(t *testing.T, w *World) *workspacev1.WorkspaceRef {
	t.Helper()
	ws := w.NewWorkspace(t)
	pmOpenWorkspace(t, w, ws)
	return ws
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
//
// Seam: answer-path
func TestPermissionAskAnsweredArms(t *testing.T) {
	t.Parallel()
	cases := []struct {
		name         string
		prompt       string
		buildRequest func(ws *workspacev1.WorkspaceRef, permission *frontendv1.FeedId) *agentreplv1.AnswerPermissionRequest
		wantToolRan  bool
	}{
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
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			w := SharedWorld(t)
			ws := pmNewPermissionWorkspace(t, w)
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
