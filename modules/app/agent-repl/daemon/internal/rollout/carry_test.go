package rollout

import (
	"context"
	"errors"
	"os"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"google.golang.org/protobuf/encoding/protojson"

	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// previousDaemon is the outgoing daemon writeHandoverManifest names, and so
// the only daemon whose carry a successor under test honors.
const previousDaemon = ids.InstanceID("daemon-outgoing-previous")

// carryFor writes the carry the previous daemon would have sealed for ws.
func carryFor(t *testing.T, h *harness, ws ids.WorkspaceID, carry Carry) {
	t.Helper()
	carry.Workspace = ws
	if carry.Daemon == "" {
		carry.Daemon = previousDaemon
	}
	if err := h.c.writeCarry(carry); err != nil {
		t.Fatalf("writeCarry: %v", err)
	}
}

// exists reports whether path is on disk.
func exists(t *testing.T, path string) bool {
	t.Helper()
	_, err := os.Stat(path)
	if err == nil {
		return true
	}
	if !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("stat %s: %v", path, err)
	}
	return false
}

// hasRecord reports whether a record with this operation, level and message
// was captured.
func hasRecord(h *harness, operation, level, message string) bool {
	for _, rec := range levelRecords(records(h.log, operation), level) {
		if rec.Message == message {
			return true
		}
	}
	return false
}

func TestMidWorkCompatible(t *testing.T) {
	tests := []struct {
		name  string
		build string
		want  bool
	}{
		{name: "a shim that reported no build predates the re-announcement", build: "", want: false},
		{name: "a shim that reported a build is adopted mid-work", build: "some-build", want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			carry := Carry{MidWork: true, ShimBuild: tc.build}

			// Act
			got, why := midWorkCompatible(carry)

			// Assert
			if got != tc.want {
				t.Fatalf("midWorkCompatible = %v (%q), want %v", got, why, tc.want)
			}
			if !got && why == "" {
				t.Fatalf("an incompatible shim was refused with no reason")
			}
		})
	}
}

func TestReadCarry(t *testing.T) {
	tests := []struct {
		name      string
		write     func(t *testing.T, h *harness, ws ids.WorkspaceID)
		wantFound bool
		wantErr   bool
	}{
		{
			name:  "no carry is not a failure",
			write: func(*testing.T, *harness, ids.WorkspaceID) {},
		},
		{
			name: "a written carry reads back",
			write: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
				carryFor(t, h, ws, Carry{MidWork: true, ShimBuild: "b"})
			},
			wantFound: true,
		},
		{
			name: "a carry that is not JSON is an error",
			write: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
				if err := os.MkdirAll(h.c.carryDir(), 0o755); err != nil {
					t.Fatalf("MkdirAll: %v", err)
				}
				if err := writeFile(h.c.carryPath(ws), "{not json"); err != nil {
					t.Fatalf("writeFile: %v", err)
				}
			},
			wantErr: true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := ids.WorkspaceID("0123456789abcdef")
			tc.write(t, h, ws)

			// Act
			carry, found, err := h.c.readCarry(ws)

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("readCarry error = %v, want error %v", err, tc.wantErr)
			}
			if found != tc.wantFound {
				t.Fatalf("readCarry found = %v, want %v", found, tc.wantFound)
			}
			if found && (!carry.MidWork || carry.ShimBuild != "b" || carry.Daemon != previousDaemon) {
				t.Fatalf("carry = %+v, want what was written", carry)
			}
		})
	}
}

func TestACarryWithNowhereToGoIsRefusedAtError(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) { d.IntentManifest = "" })

	// Act
	err := h.c.writeCarry(Carry{Workspace: "0123456789abcdef"})

	// Assert
	if err == nil {
		t.Fatalf("writeCarry succeeded with no manifest path configured")
	}
	if !hasRecord(h, opCarry, "error", "cannot write the handover carry") {
		t.Fatalf("records = %+v, want the refusal at ERROR", records(h.log, opCarry))
	}
}

// ---- the successor's half -------------------------------------------------

func TestAMidWorkAdoptionOfAShimThatReportedNoBuildIsRefusedBeforeTheClaim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	carryFor(t, h, ws, Carry{MidWork: true, ShimBuild: ""})
	arm(t, h, ws, Participants{Host: true})

	// Act
	err := h.c.AdoptHost(context.Background(), ws)

	// Assert
	if !errors.Is(err, ErrMidWorkRefused) || !errors.Is(err, ErrNotYetAdopted) {
		t.Fatalf("AdoptHost = %v, want the mid-work refusal, a not_yet_adopted answer", err)
	}
	if got := h.fleet.Adoptions(); len(got) != 0 {
		t.Fatalf("adoptions = %v, want no shim dialed", got)
	}
	if !exists(t, h.c.refusalPath(ws)) {
		t.Fatalf("no refusal marker was written for the incumbent to take the workspace back on")
	}
}

func TestAMidWorkAdoptionWhoseShimNeverReAnnouncesIsRefusedAndLetGo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	shim := h.fleet.live[ws]
	h.fleet.factsErr[ws] = context.DeadlineExceeded
	carryFor(t, h, ws, Carry{MidWork: true, ShimBuild: "installed-build"})
	arm(t, h, ws, Participants{Host: true})

	// Act
	err := h.c.AdoptHost(context.Background(), ws)

	// Assert
	if !errors.Is(err, ErrMidWorkRefused) {
		t.Fatalf("AdoptHost = %v, want the mid-work refusal", err)
	}
	if !shim.Detached() || len(shim.KillRequests()) != 0 || len(shim.ForceKills()) != 0 {
		t.Fatalf("detached %v, kills %d/%d; want the shim let go, never killed",
			shim.Detached(), len(shim.KillRequests()), len(shim.ForceKills()))
	}
	owner, err := h.db.Serving(context.Background(), ws)
	if err != nil || owner != nil {
		t.Fatalf("serving = (%v, %v), want the row given back for the incumbent", owner, err)
	}
	if indexOf(h.order.Taken(), "drain_intake") >= 0 {
		t.Fatalf("steps = %v, want the handover hold left for the incumbent", h.order.Taken())
	}
}

func TestAMidWorkAdoptionAwaitsTheAdoptedFactsBeforeDrainingTheIntake(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	carryFor(t, h, ws, Carry{MidWork: true, ShimBuild: "installed-build"})

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	taken := h.order.Taken()
	adopt, facts, drain := indexOf(taken, "adopt"), indexOf(taken, "await_facts"), indexOf(taken, "drain_intake")
	if adopt < 0 || facts < adopt || drain < facts {
		t.Fatalf("steps = %v, want adopt, then the facts awaited, then the intake drained", taken)
	}
}

func TestAnAdoptionAtFreenessDoesNotAwaitTheAdoptedFacts(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	carryFor(t, h, ws, Carry{MidWork: false})

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	if got := h.fleet.FactsAwaited(); len(got) != 0 {
		t.Fatalf("facts awaited on %v, want none: nothing was in flight", got)
	}
}

func TestAnAdoptionInstallsTheCarriedQueueMemory(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	queue := bounce.Handoff{
		Acts: []bounce.HandoffAct{{Kind: "compact", Turn: "turn-1"}},
		Cut:  &bounce.HandoffCut{Turn: "turn-0", Command: 2},
	}
	carryFor(t, h, ws, Carry{MidWork: true, ShimBuild: "installed-build", Queue: queue})

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	h.registry.mu.Lock()
	got, installed := h.registry.adoptedHandoffs[ws]
	h.registry.mu.Unlock()
	if !installed || len(got.Acts) != 1 || got.Acts[0].Kind != "compact" || got.Cut == nil || got.Cut.Turn != "turn-0" {
		t.Fatalf("installed handoff = %+v (%v), want the carried queued /compact and running cut", got, installed)
	}
}

func TestAnAdoptionConsumesTheCarry(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	carryFor(t, h, ws, Carry{MidWork: true, ShimBuild: "installed-build"})

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	if exists(t, h.c.carryPath(ws)) {
		t.Fatalf("the carry is still on disk; its removal is what tells the incumbent the adoption landed")
	}
}

func TestAnAdoptionReJudgesTheHeldPromptsTheSealSuperseded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	carryFor(t, h, ws, Carry{MidWork: true, ShimBuild: "installed-build"})

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	h.registry.mu.Lock()
	rejudged := append([]ids.WorkspaceID(nil), h.registry.rejudged...)
	h.registry.mu.Unlock()
	if len(rejudged) != 1 || rejudged[0] != ws {
		t.Fatalf("re-judged = %v, want the adopted workspace", rejudged)
	}
}

func TestAnAdoptionOfAParkedShimRaisesTheCarriedColdGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	raw, err := protojson.Marshal(&conversationv1.SessionCold{ContextTokens: 1234})
	if err != nil {
		t.Fatalf("encode the cold facts: %v", err)
	}
	carryFor(t, h, ws, Carry{ColdGate: raw})

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	h.fleet.mu.Lock()
	cold, parked := h.fleet.parkedAdoptions[ws]
	h.fleet.mu.Unlock()
	if !parked || cold.GetContextTokens() != 1234 {
		t.Fatalf("parked adoption = (%v, %v), want the carried gate raised", cold, parked)
	}
	if got := h.fleet.Adoptions(); len(got) != 0 {
		t.Fatalf("adoptions = %v, want the parked shim held with no watcher, not adopted", got)
	}
}

func TestACarriedForcedRestartRunsOnTheAdoptingDaemon(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.registry.park = true
	carryFor(t, h, ws, Carry{
		MidWork: true, ShimBuild: "installed-build",
		Replacements: []CarriedReplacement{{Reason: string(ReasonRestartVerb), Force: true}},
	})

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	var restarts []registryCall
	for _, call := range h.registry.Requests() {
		if call.WS == ws && call.Req.Reason == string(ReasonRestartVerb) {
			restarts = append(restarts, call)
		}
	}
	if len(restarts) != 1 || !restarts[0].Req.Force || !restarts[0].Req.ReplacesShim {
		t.Fatalf("restart requests = %+v, want the one forced shim replacement asked of this daemon", restarts)
	}
}

func TestAStandingRefusalRefusesAnotherAdoptionAttempt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if err := h.c.writeRefusal(ws, "an earlier attempt refused it"); err != nil {
		t.Fatalf("writeRefusal: %v", err)
	}
	arm(t, h, ws, Participants{Host: true})

	// Act
	err := h.c.AdoptHost(context.Background(), ws)

	// Assert
	if !errors.Is(err, ErrMidWorkRefused) {
		t.Fatalf("AdoptHost = %v, want the refusal to stand until the incumbent takes the workspace back", err)
	}
	if got := h.fleet.Adoptions(); len(got) != 0 {
		t.Fatalf("adoptions = %v, want none", got)
	}
}

func TestAFailedQueueInstallStillDrainsTheIntake(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.registry.adoptHandoffErr = errFake
	carryFor(t, h, ws, Carry{MidWork: true, ShimBuild: "installed-build"})
	arm(t, h, ws, Participants{Host: true})

	// Act
	err := h.c.AdoptHost(context.Background(), ws)

	// Assert
	if !errors.Is(err, errFake) {
		t.Fatalf("AdoptHost = %v, want the install's failure surfaced", err)
	}
	if indexOf(h.order.Taken(), "drain_intake") < 0 {
		t.Fatalf("steps = %v, want the held intake drained all the same", h.order.Taken())
	}
	if !hasRecord(h, opAdopt, "error", "could not install the carried queue memory; the held intake is drained all the same") {
		t.Fatalf("records = %+v, want the failed install at ERROR", records(h.log, opAdopt))
	}
}

func TestACarryFromAnotherDaemonIsRetiredUnread(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	carryFor(t, h, ws, Carry{Daemon: "daemon-long-gone", Queue: bounce.Handoff{Head: "turn-9"}})

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	h.registry.mu.Lock()
	_, installed := h.registry.adoptedHandoffs[ws]
	h.registry.mu.Unlock()
	if installed {
		t.Fatalf("a stale carry's queue memory was installed")
	}
	if exists(t, h.c.carryPath(ws)) {
		t.Fatalf("the stale carry was left on disk")
	}
	if !hasRecord(h, opCarry, "error", "the handover carry was written by another daemon or for another workspace; it is stale and retired unread") {
		t.Fatalf("records = %+v, want the stale carry at ERROR", records(h.log, opCarry))
	}
}

// ---- the incumbent's half -------------------------------------------------

// sealedCarry answers the carry the incumbent wrote for ws.
func sealedCarry(t *testing.T, h *harness, ws ids.WorkspaceID) Carry {
	t.Helper()
	carry, found, err := h.c.readCarry(ws)
	if err != nil || !found {
		t.Fatalf("readCarry = (%v, %v), want the sealed carry", found, err)
	}
	return carry
}

// handOverUntilWindow starts an unforced handover and waits for its first
// transfer to arm the adoption window, which is after the carry is written.
func handOverUntilWindow(t *testing.T, h *harness) {
	t.Helper()
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, adoptionWindow)
}

func TestASealRecordsWhetherTheWorkspaceWasMidWork(t *testing.T) {
	tests := []struct {
		name string
		free bool
		want bool
	}{
		{name: "a free workspace is carried at freeness", free: true, want: false},
		{name: "a busy workspace is carried mid-work", free: false, want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			h.freeness.SetFree(ws, tc.free)

			// Act
			handOverUntilWindow(t, h)

			// Assert
			if got := sealedCarry(t, h, ws).MidWork; got != tc.want {
				t.Fatalf("carry mid_work = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestASealCarriesTheShimBuildTheIncumbentLastHeard(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.c.mu.Lock()
	h.c.reported[ws] = "heard-build"
	h.c.mu.Unlock()

	// Act
	handOverUntilWindow(t, h)

	// Assert
	if got := sealedCarry(t, h, ws).ShimBuild; got != "heard-build" {
		t.Fatalf("carry shim_build = %q, want the build the shim last reported", got)
	}
}

func TestASealCarriesTheQueueMemory(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.registry.sealed[ws] = bounce.Handoff{Acts: []bounce.HandoffAct{{Kind: "compact"}}}

	// Act
	handOverUntilWindow(t, h)

	// Assert
	if got := sealedCarry(t, h, ws).Queue.Acts; len(got) != 1 || got[0].Kind != "compact" {
		t.Fatalf("carried acts = %+v, want the queued /compact", got)
	}
}

func TestASealCarriesTheStandingColdGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.coldGates[ws] = &conversationv1.SessionCold{ContextTokens: 77}

	// Act
	handOverUntilWindow(t, h)

	// Assert
	cold, err := decodeCold(sealedCarry(t, h, ws).ColdGate)
	if err != nil || cold.GetContextTokens() != 77 {
		t.Fatalf("carried cold gate = (%v, %v), want the standing gate's facts", cold, err)
	}
}

func TestASealCarriesAForcedRestartTheMoveOvertook(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.registry.across[ws] = []bounce.Request{{Reason: string(ReasonRestartVerb), Force: true}}

	// Act
	handOverUntilWindow(t, h)

	// Assert
	got := sealedCarry(t, h, ws).Replacements
	if len(got) != 1 || got[0].Reason != string(ReasonRestartVerb) || !got[0].Force {
		t.Fatalf("carried replacements = %+v, want the forced restart", got)
	}
}

func TestASealLeavesAStaleBuildReplacementForTheSuccessorToJudge(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.registry.across[ws] = []bounce.Request{{Reason: string(ReasonBuildStale)}}

	// Act
	handOverUntilWindow(t, h)

	// Assert
	if got := sealedCarry(t, h, ws).Replacements; len(got) != 0 {
		t.Fatalf("carried replacements = %+v, want none: the successor judges every shim against its own build", got)
	}
}

func TestASealThatFailsTakesTheWorkspaceBack(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.registry.sealErr = errFake

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()

	// Assert
	if calls := h.pusher.Calls(); len(calls) != 0 {
		t.Fatalf("pushes = %v, want no transfer notice for a move that could not be sealed", calls)
	}
	owner, err := h.db.Serving(context.Background(), ws)
	if err != nil || owner == nil || *owner != selfInstance {
		t.Fatalf("serving = (%v, %v), want the workspace still served here", owner, err)
	}
	if !hasRecord(h, opTransfer, "error", "could not seal the workspace's queue for its move") {
		t.Fatalf("records = %+v, want the failed seal at ERROR", records(h.log, opTransfer))
	}
}

func TestAnAdoptionThatLandedTellsTheCarriedRestartItWasHandedAcross(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	told := make(chan error, 1)
	h.registry.across[ws] = []bounce.Request{{Reason: string(ReasonRestartVerb), Force: true, Done: func(err error) { told <- err }}}
	handOverUntilWindow(t, h)

	// Act
	h.successorAdopts(t)
	awaitExit(t, h)

	// Assert
	select {
	case err := <-told:
		if !errors.Is(err, bounce.ErrHandedAcross) {
			t.Fatalf("the carried restart was told %v, want handed across", err)
		}
	case <-time.After(10 * time.Second):
		t.Fatalf("the carried restart's requester was never told its outcome")
	}
}

func TestAMoveTakenBackAsksForItsCarriedReplacementAgainHere(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.registry.across[ws] = []bounce.Request{{
		Reason: string(ReasonRestartVerb), Force: true, ReplacesShim: true,
		Run: func(context.Context, ids.WorkspaceID) error { return nil },
	}}
	handOverUntilWindow(t, h)

	// Act: the window expires with no adoption.
	h.clock.Fire(adoptionWindow)
	call := h.registry.awaitRequest(t, func(c registryCall) bool { return c.Req.Reason == string(ReasonRestartVerb) })

	// Assert
	if call.WS != ws || !call.Req.Force {
		t.Fatalf("re-requested %+v, want the forced restart asked of this daemon again", call)
	}
}

func TestAMoveTakenBackPutsTheSealedQueueMemoryBack(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.registry.sealed[ws] = bounce.Handoff{Head: "turn-7"}
	handOverUntilWindow(t, h)

	// Act: the window expires with no adoption.
	h.clock.Fire(adoptionWindow)
	select {
	case <-h.registry.unsealedCh:
	case <-time.After(10 * time.Second):
		t.Fatalf("the sealed queue memory was never put back")
	}

	// Assert
	h.registry.mu.Lock()
	got := h.registry.unsealed[ws]
	h.registry.mu.Unlock()
	if got.Head != "turn-7" {
		t.Fatalf("put back %+v, want the sealed semantic head", got)
	}
}

func TestAMidWorkMoveHasNotLandedUntilTheSuccessorConsumesTheCarry(t *testing.T) {
	tests := []struct {
		name     string
		midWork  bool
		consumed bool
		want     bool
	}{
		{name: "a mid-work carry still on disk has not landed", midWork: true, consumed: false, want: false},
		{name: "a mid-work carry the successor consumed has landed", midWork: true, consumed: true, want: true},
		{name: "a carry written at freeness lands on the row alone", midWork: false, consumed: false, want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			carryFor(t, h, ws, Carry{Daemon: selfInstance, MidWork: tc.midWork})
			if tc.consumed {
				if err := h.c.removeCarry(ws); err != nil {
					t.Fatalf("removeCarry: %v", err)
				}
			}
			if err := h.db.ClaimServing(context.Background(), ws, "daemon-successor"); err != nil {
				t.Fatalf("ClaimServing: %v", err)
			}
			move := &sealedMove{written: true, midWork: tc.midWork}

			// Act
			got, _ := h.c.landed(context.Background(), ws, "", move, dlog.Context{})

			// Assert
			if got != tc.want {
				t.Fatalf("landed = %v, want %v", got, tc.want)
			}
		})
	}
}

// refusedBySuccessor writes the refusal marker a successor writes when it
// refuses a mid-work adoption before its claim.
func refusedBySuccessor(t *testing.T, h *harness, ws ids.WorkspaceID) {
	t.Helper()
	if err := h.c.writeRefusal(ws, "the shim reported no build"); err != nil {
		t.Fatalf("writeRefusal: %v", err)
	}
}

// isFreenessTransfer matches the fallback's transfer request.
func isFreenessTransfer(c registryCall) bool {
	return c.Req.Reason == string(ReasonHandoverTransfer) && c.Req.WaitFor == bounce.GateFreeness
}

func TestARefusedMidWorkAdoptionIsAskedAgainAtFreeness(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	handOverUntilWindow(t, h)

	// Act
	refusedBySuccessor(t, h, ws)
	call := h.registry.awaitRequest(t, isFreenessTransfer)

	// Assert
	if call.WS != ws || call.Req.Force || !call.Req.KeepDraining {
		t.Fatalf("fallback request = %+v, want an unforced kept-draining transfer at freeness", call)
	}
	if !h.registry.Pending(ws) {
		t.Fatalf("the fallback transfer is not registered behind the busy workspace's work")
	}
}

func TestARefusedMidWorkAdoptionIsTakenBackAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	handOverUntilWindow(t, h)

	// Act
	refusedBySuccessor(t, h, ws)
	h.registry.awaitRequest(t, isFreenessTransfer)

	// Assert
	owner, err := h.db.Serving(context.Background(), ws)
	if err != nil || owner == nil || *owner != selfInstance {
		t.Fatalf("serving = (%v, %v), want the refused workspace served here again", owner, err)
	}
	if exists(t, h.c.carryPath(ws)) || exists(t, h.c.refusalPath(ws)) {
		t.Fatalf("the carry or the refusal marker outlived the take-back")
	}
	faults, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws})
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(faults) != 0 {
		t.Fatalf("faults = %+v, want none: a refusal is not an expired window", faults)
	}
}

func TestAForcedHandoverFallsBackToAnUnforcedTransferAtFreeness(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	if _, err := h.c.HandOver(context.Background(), true); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, adoptionWindow)

	// Act
	refusedBySuccessor(t, h, ws)
	call := h.registry.awaitRequest(t, isFreenessTransfer)

	// Assert
	if call.Req.Force {
		t.Fatalf("fallback request = %+v, want it unforced: a forced one runs mid-work and is refused again", call)
	}
}

func TestARefusedWorkspaceIsNamedOnTheHoldoutCadenceWhileItWaitsForFreeness(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	handOverUntilWindow(t, h)
	refusedBySuccessor(t, h, ws)
	h.registry.awaitRequest(t, isFreenessTransfer)

	// Act
	h.clock.awaitArmed(t, holdoutCadence)
	h.clock.Fire(holdoutCadence)
	h.clock.awaitArmed(t, holdoutCadence)

	// Assert
	warns := levelRecords(records(h.log, opHandover), "warn")
	if len(warns) != 1 {
		t.Fatalf("holdout warnings = %+v, want one per cadence", warns)
	}
	if holdouts, _ := warns[0].Context["holdouts"].([]string); len(holdouts) != 1 || holdouts[0] != string(ws) {
		t.Fatalf("holdouts = %v, want the refused workspace named", warns[0].Context["holdouts"])
	}
}

func TestARefusedWorkspaceIsNeverInterruptedWhileItWaitsForFreeness(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	original := h.fleet.live[ws]
	h.freeness.SetFree(ws, false)
	handOverUntilWindow(t, h)
	refusedBySuccessor(t, h, ws)
	h.registry.awaitRequest(t, isFreenessTransfer)

	// Act
	h.clock.awaitArmed(t, holdoutCadence)
	h.clock.Fire(holdoutCadence)
	h.clock.awaitArmed(t, holdoutCadence)

	// Assert
	h.fleet.mu.Lock()
	reattached := h.fleet.adopted[ws]
	h.fleet.mu.Unlock()
	if reattached == nil {
		t.Fatalf("the refused workspace's running shim was not re-attached")
	}
	for _, shim := range []*fakeShim{original, reattached} {
		if len(shim.KillRequests()) != 0 || len(shim.ForceKills()) != 0 {
			t.Fatalf("shim %d was killed; nothing is interrupted to hurry a workspace to freeness", shim.PID())
		}
	}
}

func TestARefusedWorkspaceMovesOnceItFallsFree(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	handOverUntilWindow(t, h)
	refusedBySuccessor(t, h, ws)
	h.registry.awaitRequest(t, isFreenessTransfer)

	// Act: it falls free; the successor adopts the second transfer headlessly.
	h.registry.free(ws)
	h.clock.awaitArmed(t, adoptionWindow)
	lease, held, err := h.db.Lease(context.Background(), ws)
	if err != nil || !held {
		t.Fatalf("Lease = (%v, %v), want the second transfer's hold", held, err)
	}
	if err := h.db.ClaimServing(context.Background(), ws, "daemon-successor"); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	if err := h.db.ReleaseLease(context.Background(), lease.ID); err != nil {
		t.Fatalf("ReleaseLease: %v", err)
	}
	awaitExit(t, h)

	// Assert
	if pushes := h.pusher.Calls(); len(pushes) != 2 {
		t.Fatalf("pushes = %+v, want the mid-work transfer and the one at freeness", pushes)
	}
	if carry := sealedCarry(t, h, ws); carry.MidWork {
		t.Fatalf("the transfer at freeness was carried mid-work")
	}
}

func TestATakeBackLearnsTheReAttachedShimsFactsBeforeAskingForTheTransferAtFreeness(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	h.registry.order = h.order
	handOverUntilWindow(t, h)

	// Act
	refusedBySuccessor(t, h, ws)
	h.registry.awaitRequest(t, isFreenessTransfer)

	// Assert
	taken := h.order.Taken()
	facts := indexOf(taken, "await_facts")
	request := indexOf(taken, "request:"+string(ReasonHandoverTransfer)+":freeness")
	if facts < 0 || request < facts {
		t.Fatalf("steps = %v, want the re-attached shim's facts awaited before freeness is judged", taken)
	}
}

func TestATakeBackWhoseReAttachedShimNeverReAnnouncesIsNotTransferredAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	h.fleet.factsErr[ws] = context.DeadlineExceeded
	handOverUntilWindow(t, h)

	// Act
	refusedBySuccessor(t, h, ws)
	awaitRecord(t, h, opAdoption, "the refused workspace was taken back, but not cleanly; it is not transferred again")

	// Assert
	for _, call := range h.registry.Requests() {
		if isFreenessTransfer(call) {
			t.Fatalf("requests = %+v, want no transfer judged against a shim whose turn this daemon cannot see", h.registry.Requests())
		}
	}
	if !hasRecord(h, opTransfer, "error", "the re-attached shim never re-announced its session facts; this daemon cannot see its turn in flight, and the workspace is drawn degraded until it does") {
		t.Fatalf("records = %+v, want the missing facts at ERROR", records(h.log, opTransfer))
	}
}

func TestATakeBackWhoseReAttachedShimNeverReAnnouncesIsDrawnDegraded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	h.fleet.factsErr[ws] = context.DeadlineExceeded
	handOverUntilWindow(t, h)

	// Act
	refusedBySuccessor(t, h, ws)
	awaitRecord(t, h, opAdoption, "the refused workspace was taken back, but not cleanly; it is not transferred again")

	// Assert: every status surface is told, and the workspace stays usable.
	got := h.Unreported()
	if len(got) != 1 || got[0].ws != ws || !got[0].unreported {
		t.Fatalf("StateUnreported calls = %+v, want one stating %s unreported", got, ws)
	}
}

func TestATakeBackWhoseReAttachedShimReAnnouncesIsNotDrawnDegraded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	handOverUntilWindow(t, h)

	// Act
	refusedBySuccessor(t, h, ws)
	h.registry.awaitRequest(t, isFreenessTransfer)

	// Assert
	if got := h.Unreported(); len(got) != 0 {
		t.Fatalf("StateUnreported calls = %+v, want none for a shim that re-announced", got)
	}
}
