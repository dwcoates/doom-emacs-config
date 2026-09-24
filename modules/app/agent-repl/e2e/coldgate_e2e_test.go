// coldgate_e2e_test.go — the COLD-CONTEXT GATE, end to end: raised by a real
// refusal from the real shim, drawn on the real feed and footer, and answered
// through the real AnswerColdGate rpc, once per button.
//
// WHAT ACTUALLY PROVOKES THE GATE (established from the contract, not guessed):
//
//   - docs/overhaul/daemon.md, "Session lifecycle: spawn / attach / end,
//     resume, the cold gate": "Cold gate: a cold context (cache lapsed, or
//     model switching) is REFUSED with its cost named, never silently paid.
//     The daemon reopens naming a remediation — pay | clear | compact{model,
//     scope} — chosen by the user (AnswerColdGate) or daemon policy... The
//     footer only says the session is parked; the feed's gate row is the
//     answering surface."
//   - docs/overhaul/shim.md, service.proto section: `SetSessionModel` is
//     "refused IMMEDIATELY with `SessionCold` when context exceeds the
//     request's threshold". That is the OTHER cold site and is NOT the trigger
//     used here: a topbar model pick deliberately bypasses the gate, so no
//     gate row is raised from it.
//   - The trigger this file drives is the RESUME site, in
//     agent-shim/claude/shim/src/engine/session.ts:1453-1462 — on
//     `StartSession{resume}` the shim reads the transcript's facts
//     (engine/cold.ts's readTranscriptFacts: the LAST ASSISTANT line's
//     `message.usage` for the context size and its `timestamp` for the request
//     instant), judges them (judgeCold: `nowMs - lastRequestAtMs >
//     cacheTtlMs` → "lapsed"), and REFUSES with `SessionCold` when the caller
//     named no remediation. engine/cold.ts's own header says why the refusal
//     rather than a warning: "a warning arrives after the money is spent."
//   - The daemon turns that refusal into the gate:
//     daemon/internal/workspace/sessions.go:698-782 (`raiseColdGate` — the
//     standing FeedColdGate row with the shim's own facts, plus
//     Footer.SetColdGate), and answers it by re-opening with the chosen
//     remediation (daemon/internal/workspace/answers.go:167-214).
//   - proto/src/agentrepl/v1/endpoint_answer_cold_gate.proto: the three
//     buttons (pay | clear | compact{model, scope}), and
//     proto/src/frontend/v1/feed.proto's FeedColdGate{standing, resolved}.
//
// SO THE ARRANGEMENT IS: one real turn that leaves a STALE transcript, a real
// crash-restart cold boot with no surviving shim, and a real mount whose
// RESUME the shim refuses as lapsed. See raiseColdGate's own comment for why
// the park is a cold boot and neither a Kill/Restart verb nor the idle-cutoff
// hibernation.
//
// THE FAKE AFFORDANCE THIS NEEDS ALREADY EXISTS: the `!cold-seed` scenario
// (agent-shim/claude/shim/src/fake/scenarios/session.ts's COLD_SEED) emits an
// ordinary turn whose assistant transcript line is stamped TWO HOURS in the
// past, using the documented AssistantOptions.timestamp lever
// ("FOR SCENARIOS THAT DELIBERATELY LIE ABOUT WHEN, and only about when — the
// cold-context gate reads the LAST ASSISTANT LINE's `timestamp` and `usage`
// together", src/fake/scenario.ts). Nothing was added to the fake for this
// file. Two hours clears the tier the fake's own usage object buys: its
// `cache_creation.ephemeral_1h_input_tokens` is non-zero, so engine/cold.ts
// reads CACHE_TTL_1H_MS (one hour) as the lapse bound.
//
// ALL THREE BUTTONS ARE DRIVEN FOR REAL — nothing here is faked or skipped.
// pay resumes the same conversation, clear mints a new vendor session id
// (session.ts:1667), and compact runs the shim's real throwaway-session
// compaction (engine/compaction.ts) before resuming.
//
// THE STANDING FAILURE IS FIXED (2026-09-04), and the assertions that reported
// it are unchanged — two defects stood between the gate's answer and a working
// session, one in each half:
//
//   - THE DAEMON'S HALF, and the real one. AnswerColdGate called
//     shim.StartSession DIRECTLY: it cleared the gate and drew the resolved row
//     over a session the shim had re-opened perfectly well, while the daemon
//     itself installed no session watcher, recorded no session facts and never
//     republished the host view as live — so the next prompt was refused
//     `no_session` ("daemon.promptqueue.submit: the workspace has no session
//     watcher"). The re-open is a SESSION BRING-UP, so it now goes through the
//     fleet's one start path: Sessions.ResumeCold (daemon/internal/workspace/
//     sessions.go) sends the remediated resume and then runs `sessionUp` — the
//     SAME watcher/facts/host-view step Fleet.Start runs, extracted rather than
//     copied so the two paths cannot drift again.
//   - THIS FILE'S OWN ARRANGEMENT. The hand-rolled successor daemon below
//     inherited none of NewWorld's env, so its shim looked for `shim-lock` at
//     the deploy location under $HOME, found nothing, and its kernel claim died
//     `spawn ENOENT` — which the shim reports as `conversation_owned`. The
//     symptom was an AnswerColdGate refused "another process already owns
//     vendor session ...", and it hit ONLY the remediated re-open because the
//     cold refusal returns before either lock is taken. AGENT_REPL_SHIM_LOCK_BIN
//     is now restated on the successor beside AGENT_REPL_LOCK_DIR.
//
// The shim was checked and is CLEAN on the point the daemon half was suspected
// of: a `SessionCold` refusal takes NO lock to leave behind, because both
// claims are made after the refusal returns (engine/session.ts) — docs/overhaul/
// shim.md's "an inert shim holds neither lock" holds as written.
//
// WHAT `Clear` NEEDED, AND NOW HAS (2026-09-04), WAS A SIDECAR LANDING RATHER
// THAN A CHANGE HERE — the assertions below are unchanged. `clear` is the one
// button that ROTATES the vendor session id (session.ts:1667), and the file
// plane had no way to know that:
//
//   - the shim keeps writing under the conversation's ORIGINAL id, which is R9
//     ("the AgentId is unaffected"), and leaves a link file naming the original
//     at `<state>/shim/<workspace>/vendor-id/<new-id>.json`
//     (engine/identity.ts's SessionIdentity.rotate / vendorLinkPath);
//   - the sidecar CONSUMED THAT FILE NOWHERE AT ALL, so it read the rotated
//     transcript and keyed the same rows under the NEW id — the store rightly
//     refused the batch ("would move the row from book <original> to <new> — an
//     upsert supersedes a row's content, never its identity"), parked the file,
//     and its cursor never advanced, which is the wait this file's last step
//     timed out on.
//
// THE SIDECAR NOW RESOLVES A TRANSCRIPT'S BOOK THROUGH THOSE FILES
// (agent-shim/claude/shim-sidecar/internal/identity, wired at
// cycle.go's mainAgentFor / rekeyRotations). It learns the state root from
// --state-dir ($AGENT_REPL_STATE_DIR, else ~/.claude-emacs — the daemon's own
// precedence), which NewWorld passes it as the world's own root, and it never
// derives a workspace key: it ENUMERATES `<state>/shim/*`, because the only
// thing it holds for a transcript is the vendor's lossy, non-invertible cwd
// slug. A rotated id answers from its link file, an unrotated one answers
// itself, and a transcript no record names keeps the book it always had.
//
// The book is re-resolved on every rescan, so a link that appears after the
// transcript was first seen moves the file's book — and un-parks it when the
// refusal that parked it was that very book move. Nothing is duplicated: a
// record's write and upsert identities are digested from a file position that
// did not move, so the re-read supersedes those rows rather than adding any.
//
// All three buttons now pass end to end.
package e2e

import (
	"context"
	"os/exec"
	"strconv"
	"strings"
	"syscall"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// coldGateChainTimeout bounds every wait in this file. Reused verbatim from
// HandoverChainTimeout rather than a same-value twin, on that constant's own
// stated grounds (world_test.go): each wait here chains TWO real shim process
// lifecycles onto one budget — the seeding session, and the resume the gate's
// answer re-opens — which is exactly the shape that constant is sized for.
const coldGateChainTimeout = HandoverChainTimeout

// coldSeedContextTokens is the context size the gate must report: the context
// the `!cold-seed` scenario states on its assistant line, summed the way
// engine/cold.ts sums it (cache_read + cache_creation + input). Asserting the
// exact figure is what proves the number crossed the whole stack from the
// vendor's transcript line rather than being composed by the daemon.
//
// THE SCENARIO STATES 90,000 BECAUSE THE GATE HAS A FLOOR. The owner ruled on
// 2026-09-13 that a cold read under 70,000 tokens is not gated at all, so the
// mock's ordinary ~25,000-token usage would resume warm and this whole area
// would test nothing.
const coldSeedContextTokens = 90_000

// coldGateWarnings are the daemon warning records this area's arrangement
// legitimately produces: the graceful stand-down and the shim exit it causes, the
// session/link faults a workspace with no producer opens, and the bring-up
// whose resume the shim REFUSES as cold (the subject itself). Declared as
// daemon/integration's own cold-gate tests declare them
// (session_lifecycle_test.go's TestStartSessionResumeCold...).
var coldGateWarnings = []string{
	"daemon.workspace.kill",
	"daemon.workspace.bring_up",
	"daemon.shimclient.exit",
	"daemon.shimclient.kill_session",
	"daemon.shimclient.redial",
	"daemon.sessionwatcher.reopen",
	"daemon.sessionwatcher.link_fault",
	"daemon.sessionwatcher.watch_agent",
	"daemon.sessionwatcher.watch_session",
	"daemon.health.open_fault",
}

// coldGate is one test's raised gate: the world it stands in, its workspace,
// the gate row exactly as the feed served it, and the footer stream that was
// open before the gate was raised.
type coldGate struct {
	w   *World
	ws  *workspacev1.WorkspaceRef
	row *frontendv1.FeedRow
	// configDir is the account root the workspace routes through: the FIRST
	// daemon's, which the successor was launched against.
	configDir string
	footer    *FooterWatch
}

// raiseColdGate arranges a REAL standing cold gate and asserts the two
// surfaces the contract names (the feed's gate row is the answering surface;
// the footer only says the session is parked). It answers the gate row and the
// turn whose delivery the gate is holding.
//
// THE PARK IS A CRASH-RESTART COLD BOOT WITH NO SURVIVING SHIM, and the three
// alternatives were tried and rejected on evidence, not taste:
//
//   - A forced KillWorkspace is UNUSABLE: the real shim's KillSession never
//     answers the daemon's forced call, so the daemon force-stops the process
//     and the rpc itself fails deadline_exceeded ("the forced KillSession did
//     not answer", daemon.workspace.kill).
//   - A graceful RestartWorkspace wedges on the SAME defect: the relaunch
//     engine prelaunches the inert shim, takes the restart hold, finds the
//     workspace free, calls KillSession on the old shim — and that call never
//     answers, so no relaunch resume ever happens. Both are real defects, but
//     neither is this area's to own or to build an arrangement on.
//   - The IDLE-CUTOFF HIBERNATION cannot raise this gate AT ALL, and should
//     not: before standing a shim down the daemon calls Hibernate, the shim
//     COMPACTS (engine/compaction.ts), and the fake's throwaway compaction
//     query writes a FRESH assistant line into the same transcript — so the
//     revival reads a warm conversation. That is daemon.md's "revival never
//     pays a cold context" working, observed end to end while writing this
//     file, not a gap. (It is also the reason KillSession answers there and
//     nowhere else: Hibernate has already closed the vendor query.)
//
// What is left is the cold start, which is the FIRST of the two bring-up paths
// the contract names — "ONE TRANSCRIPT-AWARE SOURCE CLASSIFIER used by BOTH
// bring-up paths — the cold start and the rollout's relaunch resume"
// (daemon.md, RESUME GUARDS). The daemon is SIGKILLed, its shim is reaped, and
// a successor boots against the same state root, lock dir and store socket:
// with no shim to re-adopt, the successor's own bring-up RESUMES the recorded
// conversation, reads the two-hour-old transcript, and is refused
// (daemon/internal/workspace/sessions.go:698-782 raises the gate from that
// refusal).
//
// WHY THE LAPSE, AND ONLY THE LAPSE, IS DRIVEN HERE: the resume site cannot
// reach `model_switch` even in principle. engine/session.ts:1447 sets
// `requestedModel = facts.lastModel ?? ""` for a resume, so judgeCold's
// model-mismatch branch is structurally unreachable from StartSession{resume};
// model_switch belongs to the SetSessionModel site, which is deliberately
// gate-free. Driving it here would have meant faking something.
func raiseColdGate(t *testing.T) *coldGate {
	t.Helper()

	// Arrange: a workspace whose one real turn leaves a two-hour-old
	// transcript behind.
	first := NewWorld(t, WorldOpts{})
	first.ExpectWarnings(coldGateWarnings...)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, first.Daemon, repo.Dir)

	seed := driveScenarioToCompletion(t, first, ws, first.DefaultConfigDir, "cold-seed")
	// THE CRASH WAITS FOR THE SEED TURN'S DURABLE CLOSE. The feed publishes a
	// turn's end before the prompt queue stamps its close, so a kill landing
	// between the two leaves the turn open on disk, and the successor rightly
	// closes it as an orphan at WARN (`daemon.promptqueue.restore_holds`) — a
	// crash inside that window is not this arrangement's subject, and racing
	// it failed TestColdGate/Compact one run in three. The queue's own record
	// of the turn end is written after the close, so it is the edge to wait on.
	first.Daemon.AwaitWorkspaceLogRecord(ws.GetDir(), "the first daemon's durable close of the seed turn", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.turn_ended" &&
			r.Message == "the turn ended; nothing is waiting to be delivered" &&
			r.Context["turn"] == seed.GetValue()
	})
	configDir := first.DefaultConfigDir

	// Act: crash the daemon and kill its shim, so the successor has nothing to
	// re-adopt and must RESUME.
	//
	// BOTH HALVES ARE NECESSARY, and neither harness verb does both safely: a
	// SIGTERM stand-down leaves the shim RUNNING on purpose (it retires its
	// stderr mirror and keeps logging durably), so the successor ADOPTS it and
	// never resumes; and Daemon.ReapStrays, which would kill it, keys on "any
	// process naming this state directory" — which in this suite includes the
	// store and the sidecar, since NewWorld routes their logs under the same
	// state root, and both must outlive the bounce.
	first.Kill()
	coldGateKillShims(t, first.Daemon)
	successor := harness.StartDaemon(t, harness.Opts{
		StateDir:    first.StateDir,
		ShimNode:    requireNode(t),
		ShimMain:    requireShimBundle(t),
		StoreSocket: first.Store.Socket,
		// THE LOCK BINARY IS RESTATED HERE, exactly as the lock DIRECTORY is.
		// NewWorld hands every shim it spawns AGENT_REPL_SHIM_LOCK_BIN (the
		// shim-lock this run built into a temp directory), but this successor
		// is a hand-rolled second daemon and inherits none of that world's
		// env. Without it the successor's shim falls back to the deploy
		// location under $HOME — which the e2e HOME does not have — and its
		// kernel claim dies `spawn ENOENT`, which locks.ts's caller reports as
		// `conversation_owned`: "another process already owns vendor session".
		// The first StartSession never reaches the claim (the cold refusal
		// returns before either lock is taken), so the whole arrangement stands
		// up and only the REMEDIATED re-open collides.
		ExtraEnv: append([]string{
			"AGENT_REPL_LOCK_DIR=" + first.LockDir,
			"AGENT_REPL_SHIM_LOCK_BIN=" + requireLockBinary(t),
		}, buildIdentityEnv()...),
		// THE SUCCESSOR MUST ROUTE THROUGH THE FIRST DAEMON'S ACCOUNT ROOT,
		// or the resume it performs looks at an empty projects tree, the
		// source classifier finds no transcript, and the session comes up
		// FRESH instead of cold (daemon.md's RESUME GUARDS: "a vendor id
		// whose transcript is MISSING comes up FRESH"). harness.StartDaemon
		// mints a fresh config root per daemon and exposes no override for
		// it, so the flag is RESTATED here: ExtraArgs is appended after the
		// harness's own argv, and the daemon parses with Go's flag package,
		// where the last occurrence of a flag wins. (OmitArgs cannot help —
		// harness.StartDaemon applies it AFTER ExtraArgs, so it would strip
		// this restatement along with the harness's own.)
		ExtraArgs: []string{"--default-config-dir", first.DefaultConfigDir},
		// The successor's boot and the resume its bring-up performs are two
		// real process lifecycles on one budget — the shape AdoptionChainTimeout
		// documents (world_test.go), reused verbatim rather than the tighter
		// single-boot DefaultTimeout.
		Timeout: AdoptionChainTimeout,
	})
	successor.ExpectWarnings(append([]string{"daemon.rollout.reconcile"}, coldGateWarnings...)...)
	w := &World{Daemon: successor, Store: first.Store, Sidecar: first.Sidecar}

	// The footer stream is opened BEFORE the mount, so the parked status is
	// queued in order rather than possibly missed.
	footer := w.WatchFooter(ws)

	// Mounting the workspace on the successor IS the implicit revival
	// (daemon.md's SPAWN ON MOUNT), and its resume is what the shim refuses.
	opened, err := w.Client().OpenWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws}))
	if err != nil {
		footer.Close()
		t.Fatalf("OpenWorkspace (cold resume): %v", err)
	}
	if opened.Msg.GetSuccess() == nil {
		footer.Close()
		t.Fatalf("OpenWorkspace (cold resume) = %v, want a success carrying the gate on the feed", opened.Msg)
	}

	// Assert: the feed carries the standing gate, with the SHIM's own facts.
	row := awaitFeedRow(t, w, ws, "the standing cold-gate row", func(r *frontendv1.FeedRow) bool {
		return r.GetColdGate().GetStanding() != nil
	})
	standing := row.GetColdGate().GetStanding()
	if got := standing.GetContextTokens().GetTokens(); got != coldSeedContextTokens {
		t.Errorf("cold gate context_tokens = %d, want %d (the fake's own usage object, summed as engine/cold.ts sums it)", got, coldSeedContextTokens)
	}
	if standing.GetLastRequest().GetAtMs() == 0 {
		t.Error("cold gate last_request.at_ms = 0, want the back-dated request instant the transcript states")
	}
	if standing.GetModel().GetModel().GetName() == "" {
		t.Error("cold gate model.model.name = \"\", want the model the re-read would run on")
	}
	if len(standing.GetCompact().GetModels()) == 0 {
		t.Errorf("cold gate compact menu = %v, want at least one summarizer offered", standing.GetCompact())
	}
	if len(standing.GetCompact().GetScopes()) == 0 {
		t.Errorf("cold gate compact menu = %v, want the offered compaction types", standing.GetCompact())
	}

	// Assert: the footer says the session is parked on the gate, and nothing
	// more (daemon.md: "The footer only says the session is parked").
	awaitColdFooter(t, w, footer, "the footer waiting on the cold gate", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWaiting().GetColdGate() != nil
	})

	return &coldGate{w: w, ws: ws, row: row, configDir: configDir, footer: footer}
}

// coldGateKillShims SIGKILLs this daemon's shim processes and nothing else, and
// does not return until they are GONE. It narrows Daemon.StrayPIDs (every live
// process naming the state directory) to the ones whose argv names the shim
// bundle, so the store and the sidecar — which name the same state root only
// because their logs live under it — are left running.
//
// THE WAIT IS THE POINT, and leaving it out cost a run. `kill` only DELIVERS
// the signal; the kernel closes the dead process's file descriptors, and with
// them releases its workspace flock, on its own schedule afterwards. The
// successor daemon was started the moment this returned, and its boot probes
// that flock: on a loaded box it read the lock still HELD, concluded a shim
// survives, tried to adopt it, got `connection refused` on a socket nobody was
// listening to any more, and FAILED THE WHOLE BOOT — `boot: adopt the
// surviving shim`. The successor never reached its serving record and the test
// timed out waiting for it, with the sweep flagging the adoption errors on top.
// Observed once in eight in-container runs of the package.
//
// A pid that no longer exists is the honest statement that the flock is gone:
// a process closes its descriptors before it becomes a zombie, and a zombie is
// reaped before its pid disappears. So the wait is on the pid, not on a guess
// about how long a SIGKILL takes.
func coldGateKillShims(t *testing.T, d *harness.Daemon) {
	t.Helper()
	killed := []int{}
	for _, pid := range d.StrayPIDs() {
		args, err := exec.Command("ps", "-p", strconv.Itoa(pid), "-o", "args=").Output()
		if err != nil {
			continue
		}
		if !strings.Contains(string(args), "main.js") {
			continue
		}
		if err := syscall.Kill(pid, syscall.SIGKILL); err != nil {
			t.Fatalf("kill the seeding shim (pid %d): %v", pid, err)
		}
		killed = append(killed, pid)
	}
	if len(killed) == 0 {
		t.Fatal("no shim process was found to kill; the successor would adopt the survivor instead of resuming")
	}
	awaitPIDsGone(t, killed, coldGateReapBound)
}

// coldGateReapBound is how long a SIGKILLed shim has to leave the process table.
//
// It reclaims a REAL OS process, which is the one shape in this suite that
// genuinely deserves seconds rather than milliseconds — the same reasoning the
// Emacs suite records for its own 5s fake-daemon exit bound. The work itself is
// the kernel tearing down one node process, and a loaded container is exactly
// the case a bound exists to tolerate. It is never spent on a healthy run: the
// loop ends the moment the pid is gone.
const coldGateReapBound = 5 * time.Second

// awaitPIDsGone blocks until none of pids names a live process, failing loudly
// with whatever is left when the bound expires.
func awaitPIDsGone(t *testing.T, pids []int, bound time.Duration) {
	t.Helper()
	// POLLED, because a pid this process did not fork gives no event to wait
	// on: the shim was the FIRST daemon's child and was reparented when that
	// daemon was killed, so there is no Wait to block in and no descriptor to
	// select on. The cadence matches awaitWorldStraysGone's, which polls the
	// same kernel fact for the same reason.
	deadline := time.Now().Add(bound)
	ticker := time.NewTicker(10 * time.Millisecond)
	defer ticker.Stop()
	for {
		alive := []int{}
		for _, pid := range pids {
			if syscall.Kill(pid, 0) == nil {
				alive = append(alive, pid)
			}
		}
		if len(alive) == 0 {
			return
		}
		if !time.Now().Before(deadline) {
			t.Fatalf("SIGKILLed shim(s) %v were still alive after %s; the successor's boot would probe their workspace lock as HELD, try to adopt a shim that is going away, and fail the boot", alive, bound)
		}
		<-ticker.C
	}
}

// awaitColdFooter bounds a footer-stream wait at this area's own budget.
func awaitColdFooter(t *testing.T, w *World, f *FooterWatch, what string, pred func(*frontendv1.FooterView) bool) *frontendv1.FooterView {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), coldGateChainTimeout)
	defer cancel()
	return harness.AwaitView(t, ctx, f.Stream, what, pred)
}

// answerColdGate answers the standing gate and fails on anything but the
// success arm.
func answerColdGate(t *testing.T, g *coldGate, choice any) {
	t.Helper()
	req := &agentreplv1.AnswerColdGateRequest{Workspace: g.ws, Gate: g.row.GetId()}
	switch c := choice.(type) {
	case *agentreplv1.AnswerColdGatePay:
		req.Choice = &agentreplv1.AnswerColdGateRequest_Pay{Pay: c}
	case *agentreplv1.AnswerColdGateClear:
		req.Choice = &agentreplv1.AnswerColdGateRequest_Clear{Clear: c}
	case *agentreplv1.AnswerColdGateCompact:
		req.Choice = &agentreplv1.AnswerColdGateRequest_Compact{Compact: c}
	default:
		t.Fatalf("answerColdGate: unsupported choice %T", choice)
	}
	resp, err := g.w.Client().AnswerColdGate(g.w.Ctx(), connect.NewRequest(req))
	if err != nil {
		t.Fatalf("AnswerColdGate(%T): %v", choice, err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("AnswerColdGate(%T) = %v, want a success", choice, resp.Msg)
	}
}

// awaitResolvedGate waits for the gate row to be replaced by its resolved
// trace and answers it (feed.proto: "Chosen; drawn as the one-line trace of
// what was done").
func awaitResolvedGate(t *testing.T, g *coldGate) *frontendv1.FeedColdGateResolved {
	t.Helper()
	row := awaitFeedRow(t, g.w, g.ws, "the resolved cold-gate trace", func(r *frontendv1.FeedRow) bool {
		return r.GetColdGate().GetResolved() != nil
	})
	resolved := row.GetColdGate().GetResolved()
	if resolved.GetAtMs() == 0 {
		t.Error("resolved cold gate at_ms = 0, want when the choice landed")
	}
	return resolved
}

// assertSessionProceeds proves the answered gate actually re-opened a working
// session: the footer leaves the gate's waiting status behind, and a real
// prompt afterwards runs to its terminal on the re-opened session.
// SubmitPrompt (via driveScenarioToCompletion) fails the test on any refusal,
// so a clean drive IS the assertion that the remediated resume landed.
func assertSessionProceeds(t *testing.T, g *coldGate) {
	t.Helper()
	awaitColdFooter(t, g.w, g.footer, "the footer leaving the cold gate's waiting status", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWaiting().GetColdGate() == nil
	})

	// The remediated re-open is a SHIM-side act the daemon performs behind the
	// answer, so the workspace's session watcher is wired a moment after
	// AnswerColdGate returns; a prompt submitted into that window is refused
	// no_session. The host view is the wire's own statement that the session
	// is back — live, with its shim attached and the composer open — so it is
	// what this waits on before prompting.
	host := g.w.WatchHost(g.ws)
	defer host.Close()
	ctx, cancel := context.WithTimeout(g.w.Ctx(), coldGateChainTimeout)
	defer cancel()
	harness.AwaitView(t, ctx, host, "the re-opened session reporting live, attached and open",
		func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
			live := r.GetHost().GetExisting().GetLive()
			return live != nil && live.GetShimAttached() && live.GetOpen() != nil
		})

	driveScenarioToCompletion(t, g.w, g.ws, g.configDir, "prose-streamed")
}

// ---------------------------------------------------------------------------
// The three buttons, one subtest each. Each builds its OWN world: a gate is a
// once-per-session event, and answering it retires it, so the three answers
// cannot share one arrangement.
// ---------------------------------------------------------------------------

func TestColdGate(t *testing.T) {
	t.Parallel()
	t.Run("Pay", func(t *testing.T) {
		// Arrange
		g := raiseColdGate(t)
		defer g.footer.Close()

		// Act
		answerColdGate(t, g, &agentreplv1.AnswerColdGatePay{})

		// Assert
		if resolved := awaitResolvedGate(t, g); resolved.GetPay() == nil {
			t.Errorf("resolved cold gate = %v, want the pay trace", resolved)
		}
		assertSessionProceeds(t, g)
	})

	t.Run("Clear", func(t *testing.T) {
		// Arrange
		g := raiseColdGate(t)
		defer g.footer.Close()

		// Act
		answerColdGate(t, g, &agentreplv1.AnswerColdGateClear{})

		// Assert
		if resolved := awaitResolvedGate(t, g); resolved.GetClear() == nil {
			t.Errorf("resolved cold gate = %v, want the clear trace", resolved)
		}
		assertSessionProceeds(t, g)
	})

	t.Run("Compact", func(t *testing.T) {
		// Arrange
		g := raiseColdGate(t)
		defer g.footer.Close()
		// The answer must echo a model and a scope the gate ITSELF served:
		// anything else is refused as unserved_remediation (answers.go's
		// coldRemediation).
		menu := g.row.GetColdGate().GetStanding().GetCompact()
		wantModel := menu.GetModels()[0].GetModel()
		wantScope := menu.GetScopes()[0]

		// Act
		answerColdGate(t, g, &agentreplv1.AnswerColdGateCompact{
			Model: &conversationv1.AgentModel{Name: wantModel.GetName()},
			Scope: wantScope,
		})

		// Assert: the trace echoes the choice back exactly.
		resolved := awaitResolvedGate(t, g)
		compact := resolved.GetCompact()
		if compact == nil {
			t.Fatalf("resolved cold gate = %v, want the compact trace", resolved)
		}
		if got := compact.GetModel().GetModel().GetName(); got != wantModel.GetName() {
			t.Errorf("resolved compact.model = %q, want the served %q", got, wantModel.GetName())
		}
		if compact.GetScope() != wantScope {
			t.Errorf("resolved compact.scope = %v, want the served %v", compact.GetScope(), wantScope)
		}
		assertSessionProceeds(t, g)
	})
}
