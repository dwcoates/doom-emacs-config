//go:build playtest

package e2e

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// OWNER 5 of PLAYTEST-PLAN.md's partition: B14-B16 -- failed, hibernated,
// and merging/parked. Artifacts land under `playtest/05-tab-arms-lifecycle/`,
// one sub-directory per playbook.
//
// Every identifier this file introduces is prefixed `pt05` so it cannot
// collide with another owner's helper of the same shape.

// pt05Dir is this owner's artifact directory, and pt05Book names one
// playbook's sub-directory under it.
const pt05Dir = "05-tab-arms-lifecycle"

func pt05Book(name string) string { return pt05Dir + "/" + name }

// pt05GatedPrompt is the prompt this owner's gated turns submit. The fake SDK
// holds a turn whose FULL submitted text matches the gate text until the gate
// file exists (`agent-shim/claude/shim/src/fake/index.ts`), so a playbook that
// must photograph a RUNNING tab is synchronized on the work rather than
// racing a turn that would otherwise finish in microseconds. It carries no
// `!` prefix, so it falls through to the default prose scenario and concludes
// ordinarily once the gate opens.
const pt05GatedPrompt = "hold this turn open for owner 5's playtest"

// pt05ProsePrompt is the fake SDK's plain streamed-prose scenario: it
// concludes on its own and never parks.
const pt05ProsePrompt = "!prose-streamed"

// pt05DaemonBound bounds one cross-check against the daemon Emacs launched.
//
// It is `emacsVerbBound`'s own value reused by reference rather than a twin:
// the two facts waited on here — a host view carrying a park, a roster row
// filed under recently_merged — are each ONE daemon push behind an act the
// Emacs side has already observed (the sweep's stand-down, a merge landing),
// exactly the shape that bound was measured for.
const pt05DaemonBound = emacsVerbBound

// pt05MergeLandBound bounds the wait for a merge to LAND once its test gate
// opens: the gate script exits, the orchestrator stamps the landing, republishes
// the roster, and the child's tab is torn down in Emacs — one gate exit and
// two pushes. `HibernationChainTimeout` is the layer's budget for a chain of
// that many real-process edges, and it is reused by reference rather than
// restated.
const pt05MergeLandBound = HibernationChainTimeout

// pt05IdleCutoffMS is the compressed idle cutoff plan B.15's world sets, the
// same figure `hibernation_e2e_test.go` uses (hibernationIdleCutoffMS): the
// daemon caps its sweep cadence at the cutoff, so a compressed cutoff also
// compresses how often the sweep looks. It travels as environment because the
// daemon in this layer is spawned by Emacs (`drain.IdleCutoffEnv`).
const pt05IdleCutoffMS = hibernationIdleCutoffMS

// pt05IdleCutoffEnv is `daemon/internal/drain/controller.go`'s IdleCutoffEnv,
// spelled here because that package sits under the daemon module and this
// layer states it as environment only.
const pt05IdleCutoffEnv = "AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS"

// ---------------------------------------------------------------------------
// B.14 — failed
// ---------------------------------------------------------------------------

// TestPlaytestTabArmFailed is plan B.14: a turn that fails at the vendor, the
// arm the tab paints for it, and that arm PERSISTING until the next submit
// clears it.
//
// `!fail-execution` ends the turn on the vendor's own execution error, which
// the roster resolves to `vendor_blocked`. ON THE TAB BAR THAT IS BLUE, and
// deliberately so: `agent-repl-status-tab-bar-color-overrides` declares that
// purple can carry only one meaning on a surface this small (the in-flight
// merge arms take it), so `:vendor-blocked` joins the blue band of "every way
// the route to a working session is compromised". The sidebar dot paints the
// same arm purple. The picture's subject is that the tab is NOT red (a turn
// running) and NOT green (a turn that settled cleanly).
func TestPlaytestTabArmFailed(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	gatePath := filepath.Join(box.Scratch(), "b14-turn-gate")
	s := newPlaytestScenario(t, pt05Book("14-arm-failed"),
		"Plan B.14. A turn that fails at the vendor, the arm the tab paints for it, and the next "+
			"submit clearing that arm.",
		WithEmacsEnv(turnGatePathEnv, gatePath),
		WithEmacsEnv(turnGateTextEnv, pt05GatedPrompt))
	p := s.Book

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	p.note("one repository registered with its panel open",
		"the panel's webview is live and the webapp drew its footer")

	s.submit(t, "!fail-execution")
	// `:vendor-blocked` IS THE ANSWER, asserted by name rather than as "any
	// settled arm": a wait satisfied by `:done` would photograph a tab that
	// says the turn went fine.
	s.awaitArm(t, name, "the tab's arm to reach vendor-blocked", playtestVendorBlockedArm)
	s.captureArm(t, "arm-failed", name,
		"the fake SDK's `!fail-execution` scenario ended the turn on an execution error",
		playtestVendorBlockedArm,
		"The bracket must NOT be RED (that would say the turn is still running) and must NOT be "+
			"GREEN (that would say it settled cleanly). The webapp's feed carries the prompt bubble "+
			"and a failure for the turn.")

	// PERSISTENCE. The arm is re-read after the panel has had every chance to
	// move on: nothing else is submitted, so a tab that has drifted off
	// `:vendor-blocked` here drifted on its own, which is the defect the plan
	// line "persists until next submit" exists to catch.
	s.awaitArm(t, name, "the vendor-blocked arm to still stand with nothing submitted", playtestVendorBlockedArm)
	p.note("nothing submitted; the arm re-read",
		"`agent-repl-roster-status-for-ws` still reads :vendor-blocked")

	// THE NEXT SUBMIT CLEARS IT. The turn is GATED so the clearing is
	// photographed at `:thinking` rather than raced: an ungated prose turn
	// settles in microseconds and the picture would be of `:done`, which
	// shows the arm was cleared but not the running tab that replaced it.
	s.submit(t, pt05GatedPrompt)
	s.awaitArm(t, name, "the tab's arm to reach thinking on the next submit", ":thinking")
	s.captureArm(t, "arm-cleared-by-next-submit", name,
		"a plain prompt submitted with composer RET, and held in flight by the fake's turn gate",
		":thinking",
		"The BLUE of the failure is GONE: the next submit cleared it and the bracket is painted for "+
			"a running turn. The webapp's footer says a turn is running.")

	if err := os.WriteFile(gatePath, nil, 0o644); err != nil {
		t.Fatalf("open the fake's turn gate at %s: %v", gatePath, err)
	}
	settled := s.awaitArm(t, name, "the tab's arm to settle once the gate opens", emGHISettledArms...)
	s.captureArm(t, "arm-settled-after-clear", name,
		"the gate opened, and the fake's prose answer concluded the turn", settled,
		"The turn is over and the failure is history: nothing on the tab says vendor-blocked.")
}

// ---------------------------------------------------------------------------
// B.15 — hibernated, then revived
// ---------------------------------------------------------------------------

// TestPlaytestTabArmHibernated is plan B.15: a session stood down by the idle
// sweep, and the tab through the stand-down and the revival.
//
// THERE IS NO HIBERNATED ARM, AND THAT IS THE MODULE'S DECISION. The roster's
// status vocabulary carries no `hibernated` (status.el: "THERE IS NO TEAL ...
// both left with hibernation"), and the daemon's resolver says why
// (`daemon/internal/resolve/sidebar/status.go`): "A PARKED SESSION IS IDLE,
// NOT BROKEN. The idle sweep stands the shim down deliberately and a prompt
// brings it straight back, so the row keeps an IDLE arm ... drawing `severed`
// or `dead` would report a fault where there is none." So what this playbook
// photographs is a tab that does NOT change when its shim goes away, and the
// hibernation itself is asserted where it is visible: on the daemon's own
// host view, whose park shape is "live, shim detached"
// (`shimDetached`, hibernation_e2e_test.go).
//
// The cutoff is compressed to pt05IdleCutoffMS, so the session parks within
// the sweep's next look after every settled turn — including the mount-spawned
// one before anything is submitted, which is why the first assertion is made
// only AFTER a turn has run on the session this playbook is about.
func TestPlaytestTabArmHibernated(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, pt05Book("15-arm-hibernated"),
		"Plan B.15. A session stood down by the compressed idle cutoff, and the tab through the "+
			"stand-down and the revival a submit performs.",
		WithEmacsEnv(pt05IdleCutoffEnv, fmt.Sprint(pt05IdleCutoffMS)))
	p := s.Book

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	ref := s.pt05Ref(t, name)

	s.submit(t, pt05ProsePrompt)
	settled := s.awaitArm(t, name, "the first turn to settle", emGHISettledArms...)
	p.note("one prose turn submitted with composer RET and concluded",
		fmt.Sprintf("the arm settled on %s", settled))

	// THE HIBERNATION, ON THE DAEMON'S OWN VIEW. The host view is the one
	// surface where a park is representable: the session still exists (never
	// severed, never dead) and its shim is detached.
	s.pt05AwaitHost(t, ref, "the daemon's host view to report the session parked with its shim detached", shimDetached)
	hibernated := s.awaitArm(t, name, "the tab's arm while the session is parked", emGHISettledArms...)
	s.captureArm(t, "arm-hibernated", name,
		"the idle sweep hibernated the settled session, which the daemon's host view reports as live with the shim detached",
		hibernated,
		"NOTHING ON THE TAB SAYS THE SHIM IS GONE, and that is the product's decision: a park is "+
			"idle, not broken, so the bracket must NOT be BLUE (which would say something on this "+
			"machine broke) and there is no teal.")

	// THE REVIVAL IS A SUBMIT. A prompt after a park is an implicit revive
	// (daemon.md's SPAWN ON MOUNT / "implicit revive on prompt"), and the
	// composer's ordinary RET is that prompt. The host view proves the shim
	// came back; the roster proves the turn ran on it.
	s.submit(t, pt05ProsePrompt)
	s.pt05AwaitHost(t, ref, "the daemon's host view to report the shim attached again",
		func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
			live := r.GetHost().GetExisting().GetLive()
			return live != nil && live.GetShimAttached()
		})
	revived := s.awaitArm(t, name, "the revived turn to settle", emGHISettledArms...)
	// THE PARK'S OWN ROW, ASSERTED BEFORE IT IS PHOTOGRAPHED. The daemon
	// compacts before it stands a shim down (`daemon.md`'s Hibernate
	// directive), so the feed across this boundary is not two turns and
	// nothing else: it carries the context cut that park paid, drawn as the
	// compaction separator and its foldable summary. The manifest sentence
	// below says so because the picture shows it, and this is what makes
	// that sentence a claim rather than a description.
	s.awaitInPage(t, "the pre-park compaction's own row in the feed",
		`document.querySelector('[data-fold="compaction-summary"]')`)
	// THE OCCLUSION THIS PICTURE ONCE CAUGHT, now a claim rather than a look:
	// the revived turn's last bubble must sit clear of the progress footer,
	// and the footer settles around this very moment.
	s.awaitTailClearsFooter(t)
	s.captureArm(t, "arm-revived", name,
		"a second prose prompt submitted with composer RET revived the parked session and its turn concluded",
		revived,
		"The tab is painted exactly as it was before the park: the revival paid nothing the user "+
			"can see on the bracket. The webapp's feed carries BOTH turns in order with the PARK'S OWN "+
			"CONTEXT CUT between them -- an orange separator reading `context compacted on request` and "+
			"a foldable summary card -- because the daemon compacts before it stands a shim down. A "+
			"SECOND such cut BELOW the revived turn is right and not a duplicate: this playbook "+
			"compresses the idle cutoff, so the sweep parks the session again the moment that turn "+
			"settles. THE TOPBAR'S PERMISSION MODE STILL READS `default`: the summarizing query runs "+
			"under plan, and a revival that came back in plan mode would be that throwaway leaking into "+
			"the user's session.")

	// THE POSTURE SURVIVED THE PARK. The mode is read from the module's own
	// topbar rather than inferred from the picture, so the sentence above is
	// backed: a revival must restore the session as the user left it, and the
	// compaction's plan-mode summarizer is the one thing that had moved it.
	s.awaitInPage(t, "the topbar's mode button to still name the session's own permission mode",
		`(function () { var el = document.querySelector('.topbar-mode-button[data-mode]');
                        return !!el && el.getAttribute('data-mode') === 'default'; })()`)
}

// ---------------------------------------------------------------------------
// B.16 — merging, done, parked
// ---------------------------------------------------------------------------

// pt05GateScript is the merge TEST GATE this owner installs as
// `AGENT_REPL_TEST_ALL_SCRIPT`: it holds the merge's tests phase — and so the
// roster's `merging` arm — open until a gate file exists, then passes.
//
// WHY NOT THE HARNESS RECORDER. `harness.NewTestAllScript` scripts an exit
// code and an stdout and returns at once, so against the scripted fake git a
// clean merge runs from enqueue to landed in a few milliseconds and no
// capture could ever be taken at `merging`: `captureArm` would re-read the
// arm and refuse, correctly, because the subject had moved. Holding the gate
// is the same synchronization the fake SDK's turn gate provides for a turn,
// applied to the one phase of a merge that runs a subprocess. The loop polls
// the gate path rather than sleeping a fixed interval and hoping: the gate
// opens on the playbook's own schedule, and the poll only decides how soon
// after that the script notices.
const pt05GateScript = `#!/bin/sh
gate="$1"
while [ ! -f "$gate" ]; do
  sleep 0.02
done
printf 'playtest 05: the merge gate opened; tests passed\n'
exit 0
`

// pt05WriteGateScript writes the gate script beside the gate file it waits
// on and answers the script's path. The gate path is baked into a one-line
// wrapper so the orchestrator's `bash <script>` invocation, which passes no
// arguments, still reaches it.
func pt05WriteGateScript(t *testing.T, dir, gatePath string) string {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir %s for the merge gate: %v", dir, err)
	}
	body := filepath.Join(dir, "merge-gate-body.sh")
	if err := os.WriteFile(body, []byte(pt05GateScript), 0o755); err != nil {
		t.Fatalf("write the merge gate body %s: %v", body, err)
	}
	script := filepath.Join(dir, "test-all.sh")
	wrapper := "#!/bin/sh\nexec sh " + pt05ShQuote(body) + " " + pt05ShQuote(gatePath) + "\n"
	if err := os.WriteFile(script, []byte(wrapper), 0o755); err != nil {
		t.Fatalf("write the merge gate wrapper %s: %v", script, err)
	}
	return script
}

// pt05ShQuote single-quotes one argument for /bin/sh.
func pt05ShQuote(s string) string {
	out := "'"
	for _, r := range s {
		if r == '\'' {
			out += `'\''`
			continue
		}
		out += string(r)
	}
	return out + "'"
}

// TestPlaytestTabArmMergingDoneParked is plan B.16: a merge enqueued through
// `SPC TAB M` on the self-repo method, photographed at `merging`, at its
// landing, and — on a second child whose merge hits a scripted conflict — at
// `parked`.
//
// THE SELF-REPO METHOD is `daemon.md`'s split on whether the merge target is
// the daemon's own checkout (`AGENT_REPL_SELF_REPO_DIR`): that is the method
// that runs the merge TEST GATE, so it is the one whose `merging` phase this
// playbook can hold open (see pt05GateScript). A child created with no
// commits lands an empty range, so no rollout classifies and no handover
// fires — the same arrangement mergequeue_e2e_test.go's self-repo rows use.
//
// WHAT "DONE" LOOKS LIKE ON THE TAB BAR. A landed merge closes the workspace
// (`merge/terminal.go`: SetMergedAt, SetClosed, republish), and a row with
// `closed = true` HAS NO TAB (roster.el's membership rule). So the landing's
// picture is a tab bar from which the child's tab is GONE, asserted on both
// sides: Emacs's own tabline names no longer carry it, and the daemon's
// roster files the row closed under recently_merged.
//
// THE PARK is the module's `merge_conflict` arm — the roster has no parked
// arm of its own, and a parked merge "must never fall through to the
// session's status" (`sidebar/status.go`). It takes no color on the tab bar
// and reports itself with a glyph, which is what the picture must show.
func TestPlaytestTabArmMergingDoneParked(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	selfRepo := harness.NewRepoAt(t, filepath.Join(box.Scratch(), "self-repo"))
	gatePath := filepath.Join(box.Scratch(), "b16-merge-gate")
	gateScript := pt05WriteGateScript(t, filepath.Join(box.Scratch(), "b16-merge-gate-bin"), gatePath)
	s := newPlaytestScenario(t, pt05Book("16-arm-merging-done-parked"),
		"Plan B.16. A merge enqueued with SPC TAB M on the daemon's own checkout: the tab at merging, "+
			"the tab bar once the merge landed, and a second child's tab once its merge parked on a "+
			"scripted conflict.",
		WithEmacsEnv("AGENT_REPL_SELF_REPO_DIR", selfRepo.Dir),
		WithEmacsEnv("AGENT_REPL_TEST_ALL_SCRIPT", gateScript))
	p, e := s.Book, s.E
	e.ArtifactPaths = append(e.ArtifactPaths, filepath.Join(selfRepo.Dir, ".claude"))

	main := s.register(t, selfRepo.Dir)
	s.openPanel(t)
	label := emHO40AwaitSectionLabel(t, e, selfRepo.Dir)
	p.note("the daemon's own checkout registered as a workspace, its panel open",
		fmt.Sprintf("the roster carries the checkout's repository section %q", label))

	// THE BINDING IS ASSERTED ONCE, before either merge: `SPC TAB M` is a
	// LOOKUP here because the command reads its target from the current
	// workspace, and this playbook merges a workspace that is NOT the
	// selected one, so the command is invoked with its documented argument.
	if want, got := "agent-repl-merge-workspace", e.LeaderBinding("TAB M"); got != want {
		t.Fatalf("SPC TAB M resolves to %q, want %q", got, want)
	}

	// ---- the merge that lands ----
	landing := s.pt05CreateChild(t, label, "land")
	s.awaitArm(t, landing, "the first child's session to come up idle", emGHISettledArms...)
	p.note("a child workspace created on the checkout through `agent-repl-create-workspace`, with no prompt",
		fmt.Sprintf("%q is in Emacs's registry and its arm settled", landing))

	e.Eval(`(agent-repl-merge-workspace ` + elispString(landing) + `)`)
	// `:merging` BY NAME. The pipeline walks enqueuing -> queued -> merging,
	// and the test gate holds the last of those open, so a wait on it cannot
	// be satisfied by a state that is about to move.
	s.awaitArm(t, landing, "the first child's arm to reach merging, held by the test gate", ":merging")
	s.captureArm(t, "arm-merging", landing,
		"`agent-repl-merge-workspace` enqueued the child's merge; the no-ff merge landed on the fake git and the tests phase is held by this playbook's gate",
		":merging",
		"Two tabs: the checkout's own, and the child's. The child's bracket is PURPLE because the "+
			"tab bar overrides the in-flight merge arms to purple, and it must NOT be red: the "+
			"work in flight is the system's, not the agent's.")

	if err := os.WriteFile(gatePath, nil, 0o644); err != nil {
		t.Fatalf("open the merge test gate at %s: %v", gatePath, err)
	}
	landingRef := s.pt05Ref(t, landing)
	e.AwaitEvalFor(pt05MergeLandBound, "the merged child's tab to be torn down",
		emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return !containsString(decodeStrings(raw), landing) })
	awaitDaemonRoster(t, e.DaemonAddr(), pt05DaemonBound,
		"the daemon to file the merged child closed under recently_merged",
		func(r *frontendv1.WorkspaceRoster) bool {
			return pt05RowClosedIn(r.GetRecentlyMerged().GetRows().GetRows(), landingRef.GetId())
		})
	// WHERE THE USER ENDS UP, ASSERTED. The merged child is the workspace the
	// user was STANDING ON when the roster tore its tab down, so the teardown
	// has to name where they land (`agent-repl--land-after-teardown`) and arm
	// the survivor to show itself. Without that the frame kept whatever
	// persp-mode dropped it in and the main area came up on the fallback
	// buffer -- `*scratch*` under a lone tab, which is what the picture below
	// showed before this was fixed and is indistinguishable from a wedged
	// editor. The selected window's buffer is read back through the panel
	// name's own identity segment (`agent-repl--extract-panel-id`), so the
	// assertion is that the survivor's OWN panel is on the frame rather than
	// merely that some buffer is.
	e.AwaitEvalFor(pt05MergeLandBound, "the landing to put the surviving workspace's own panel in the selected window",
		`(let ((name (buffer-name (window-buffer (selected-window)))))
                   (or (agent-repl--extract-panel-id name) name))`,
		func(raw json.RawMessage) bool {
			var name string
			return json.Unmarshal(raw, &name) == nil && name == main
		})
	p.capture("tab-bar-after-landing",
		"the merge test gate opened, the tests passed, and the merge landed",
		fmt.Sprintf("`agent-repl--ws-tabline-names` no longer carries %q, and the daemon's roster files its row closed "+
			"under recently_merged; the selected window shows a panel of %q, read back through `agent-repl--extract-panel-id`", landing, main),
		fmt.Sprintf("ONE tab only, the checkout's own %q. The merged child %q has NO tab: a landed merge "+
			"closes the workspace and a closed row has no tab. Nothing is painted for a merge any "+
			"more. The main area belongs to %q -- its own panel, NOT `*scratch*` or any other "+
			"fallback buffer: the tab that vanished was the one the user was standing on, so the "+
			"teardown landed them on the survivor and the survivor shows itself.", main, landing, main))

	// ---- the merge that parks ----
	parking := s.pt05CreateChild(t, label, "park")
	s.awaitArm(t, parking, "the second child's session to come up idle", emGHISettledArms...)
	parkingDir := s.pt05Ref(t, parking).GetDir()
	// The scripted conflict makes the NEXT no-ff merge of the child's branch
	// into the checkout's main worktree leave a path conflicted, so the
	// orchestrator opens a conflict-repair turn; the fake SDK's default
	// scenario answers it without touching the index, and the run PARKS.
	selfRepo.ScriptConflict(selfRepo.Dir, filepath.Base(parkingDir), "conflict.txt")
	p.note("a second child created, and a conflict scripted against its branch in the fake git",
		fmt.Sprintf("%q is in Emacs's registry with its arm settled; the fake git will conflict the next no-ff merge of %q", parking, filepath.Base(parkingDir)))

	e.Eval(`(agent-repl-merge-workspace ` + elispString(parking) + `)`)
	s.awaitArm(t, parking, "the second child's arm to reach merge-conflict once the repair turn concludes without a fix", ":merge-conflict")
	s.captureArm(t, "arm-parked", parking,
		"`agent-repl-merge-workspace` enqueued the second child's merge; the scripted conflict parked it after the repair turn concluded",
		":merge-conflict",
		"Two tabs again: the checkout's own and the parked child's. The child's bracket carries NO "+
			"state color — the merge wants the user, so it is not purple — and the glyph beside its "+
			"name is what says the merge is parked on a conflict.")

	// Teardown hygiene, not an assertion: a parked merge holds its lease, and
	// the world's own shutdown must not wait on a resolution nobody gives.
	e.Eval(`(ignore-errors (agent-repl-kill-workspace ` + elispString(parking) + `) t)`)
}

// ---------------------------------------------------------------------------
// This owner's helpers
// ---------------------------------------------------------------------------

// pt05Ref reads WS's daemon-minted WorkspaceRef out of Emacs — the echo token
// the daemon handed it — so a cross-check on the Go client addresses the same
// workspace by the same id.
func (s *playtestScenario) pt05Ref(t *testing.T, ws string) *workspacev1.WorkspaceRef {
	t.Helper()
	pair := s.E.EvalStrings(`(let ((ref (agent-repl-host-ref ` + elispString(ws) + `)))
                                (list (or (plist-get ref :id) "") (or (plist-get ref :dir) "")))`)
	if len(pair) != 2 || pair[0] == "" || pair[1] == "" {
		t.Fatalf("the workspace %q holds no daemon-minted ref in Emacs (got %v), want an id and a dir", ws, pair)
	}
	return &workspacev1.WorkspaceRef{Id: pair[0], Dir: pair[1]}
}

// pt05AwaitHost dials the daemon AT THE ADDRESS EMACS'S LAUNCHER PUBLISHED and
// waits for the workspace's host view to satisfy PRED. It is the host-view
// twin of `awaitDaemonRoster`, and it exists for the same reason: a park is
// representable on the host view and nowhere Emacs paints.
func (s *playtestScenario) pt05AwaitHost(t *testing.T, ref *workspacev1.WorkspaceRef, what string,
	pred func(*agentreplv1.WatchHostWorkspaceResponse) bool) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), pt05DaemonBound)
	defer cancel()
	client := harness.DialAt(t, s.E.DaemonAddr())
	stream, err := client.WatchHostWorkspace(ctx,
		connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{Workspace: ref}))
	if err != nil {
		t.Fatalf("await %s: WatchHostWorkspace on the daemon Emacs launched: %v", what, err)
	}
	defer stream.Close()
	var last *agentreplv1.WatchHostWorkspaceResponse
	for stream.Receive() {
		last = stream.Msg()
		if pred(last) {
			return
		}
	}
	t.Fatalf("await %s: never satisfied within %s; last host view was %v (stream error: %v)",
		what, pt05DaemonBound, last, stream.Err())
}

// pt05CreateChild creates a child workspace on the repository LABEL names,
// through the ORDINARY create command with only its readers stubbed (the way
// the handover scenario's emHO40Create does): the repository picker answers
// LABEL, the initial prompt is left BLANK so the child comes up idle with no
// turn of its own, the name is NAME, and the base ref is left blank. It
// answers the one name Emacs's registry gained.
func (s *playtestScenario) pt05CreateChild(t *testing.T, label, name string) string {
	t.Helper()
	before := s.E.EvalStrings(emGHIWorkspaceNamesForm)
	s.E.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) ` + elispString(label) + `))
                       ((symbol-function 'read-string)
                        (lambda (prompt &rest _)
                          (cond ((string-prefix-p "Name" prompt) ` + elispString(name) + `)
                                (t "")))))
                 (agent-repl-create-workspace)
                 t)`)
	after := decodeStrings(s.E.AwaitEval(fmt.Sprintf("the child workspace %q to appear in Emacs's registry", name),
		emGHIWorkspaceNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == len(before)+1 }))
	child := emHO40AddedName(t, before, after)
	s.E.AwaitTrue("the child workspace to hold a daemon-minted ref",
		`(and (agent-repl-host-ref `+elispString(child)+`) t)`)
	return child
}

// pt05RowClosedIn reports whether ROWS (walked depth-first) carry a row for
// the workspace id that is marked closed.
func pt05RowClosedIn(rows []*frontendv1.RosterRow, id string) bool {
	for _, row := range rows {
		if row.GetWorkspace().GetWorkspace().GetId() == id && row.GetClosed().GetClosed() {
			return true
		}
		if pt05RowClosedIn(row.GetChildren(), id) {
			return true
		}
	}
	return false
}
