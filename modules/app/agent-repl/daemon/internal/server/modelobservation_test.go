package server

import (
	"strings"
	"testing"

	"claude-repld/internal/registry"
)

// --- RegistryRegistrar.SessionModelObserved: the ORDER of the two writers ----
//
// rec.Model has two writers — the shim's confirmation of a deliberate model
// change, and the SystemInit the SDK re-announces on every submit — and nothing
// used to order them. The interleaving that cost a user their selection: a
// confirmation for Opus lands, then the SystemInit belonging to the submit that
// was ALREADY IN FLIGHT (announcing the Sonnet it began under) is processed and
// rewrites the record. The picker repaints to the older value and the next
// respawn pins it.

// modelObsHarness is a registrar over a real registry with one live record,
// plus the log and view-push it writes through.
type modelObsHarness struct {
	t      *testing.T
	reg    *registry.Registry
	r      *RegistryRegistrar
	logged []string
	pushed []string
}

func newModelObsHarness(t *testing.T, model string) *modelObsHarness {
	t.Helper()
	h := &modelObsHarness{t: t, reg: openTestRegistry(t)}
	if err := h.reg.Put(registry.Record{SessionID: "s1", CWD: "/w", Model: model}); err != nil {
		t.Fatalf("put: %v", err)
	}
	h.r = &RegistryRegistrar{
		Reg:      h.reg,
		Logf:     func(f string, a ...any) { h.logged = append(h.logged, f) },
		PushView: func(id string) { h.pushed = append(h.pushed, id) },
	}
	return h
}

// model is the record's current model.
func (h *modelObsHarness) model() string {
	h.t.Helper()
	rec, ok := h.reg.Get("s1")
	if !ok {
		h.t.Fatal("the record vanished")
	}
	return rec.Model
}

// loggedContaining reports whether the canonical log carries a line with
// needle in it.
func (h *modelObsHarness) loggedContaining(needle string) bool {
	for _, line := range h.logged {
		if strings.Contains(line, needle) {
			return true
		}
	}
	return false
}

// obs is a token in the harness's single generation.
func obs(genOrdinal, seq uint64) registry.ModelObservation {
	return registry.ModelObservation{Generation: "cg_test", GenOrdinal: genOrdinal, StreamSeq: seq}
}

// --- the guarantee ----------------------------------------------------------

func TestAStrictlyNewerModelObservationIsAccepted(t *testing.T) {
	// Arrange.
	h := newModelObsHarness(t, "sonnet")

	// Act.
	h.r.SessionModelObserved("s1", "opus", obs(1, 7))

	// Assert.
	if got := h.model(); got != "opus" {
		t.Fatalf("record model = %q, want the newer observation applied", got)
	}
}

func TestAnAcceptedModelObservationRepushesTheSessionView(t *testing.T) {
	// Arrange — without the push the topbar picker waits for whatever
	// unrelated event next happens to push a view.
	h := newModelObsHarness(t, "sonnet")

	// Act.
	h.r.SessionModelObserved("s1", "opus", obs(1, 7))

	// Assert.
	if len(h.pushed) != 1 || h.pushed[0] != "s1" {
		t.Fatalf("pushed = %v, want [s1]", h.pushed)
	}
}

func TestAModelObservationFromANewerGenerationSupersedesRegardlessOfSeq(t *testing.T) {
	// Arrange — a respawn restarts the file plane's seq space, so a fresh
	// controller's FIRST init carries a low seq. Ordering on seq alone would
	// refuse the only report the new session has made about itself.
	h := newModelObsHarness(t, "sonnet")
	h.r.SessionModelObserved("s1", "opus", obs(1, 900))

	// Act.
	h.r.SessionModelObserved("s1", "haiku", obs(2, 1))

	// Assert.
	if got := h.model(); got != "haiku" {
		t.Fatalf("record model = %q, want the newer generation's report applied", got)
	}
}

// --- the violation ----------------------------------------------------------

// F2's exact interleaving: the confirmation lands, then the SystemInit from the
// submit that was already in flight is processed. It must not win.
func TestAStaleSystemInitCannotOverwriteANewerConfirmedSelection(t *testing.T) {
	// Arrange — the user's Opus selection is confirmed as of stream seq 12.
	h := newModelObsHarness(t, "sonnet")
	h.r.SessionModelObserved("s1", "opus", obs(1, 12))

	// Act — the in-flight submit's init, riding seq 9, announces the model
	// that submit began under.
	h.r.SessionModelObserved("s1", "sonnet", obs(1, 9))

	// Assert.
	if got := h.model(); got != "opus" {
		t.Fatalf("record model = %q, want the confirmed selection preserved", got)
	}
}

func TestARefusedModelObservationIsRecordedInTheCanonicalLog(t *testing.T) {
	// Arrange.
	h := newModelObsHarness(t, "sonnet")
	h.r.SessionModelObserved("s1", "opus", obs(1, 12))
	h.logged = nil

	// Act.
	h.r.SessionModelObserved("s1", "sonnet", obs(1, 9))

	// Assert — the shared log alone must explain why the record disagrees with
	// the report, so it carries the refusal and both tokens.
	if !h.loggedContaining("model observation REFUSED") {
		t.Fatalf("log = %v, want the refusal recorded", h.logged)
	}
	if !h.loggedContaining("not_newer_than_accepted") {
		t.Fatalf("log = %v, want the reason recorded", h.logged)
	}
}

func TestARefusedModelObservationPushesNoSessionView(t *testing.T) {
	// Arrange — a push would repaint the picker to the value that was refused.
	h := newModelObsHarness(t, "sonnet")
	h.r.SessionModelObserved("s1", "opus", obs(1, 12))
	h.pushed = nil

	// Act.
	h.r.SessionModelObserved("s1", "sonnet", obs(1, 9))

	// Assert.
	if len(h.pushed) != 0 {
		t.Fatalf("pushed = %v, want nothing pushed for a refused observation", h.pushed)
	}
}

func TestAnEqualModelObservationTokenIsNotNewerAndIsRefused(t *testing.T) {
	// Arrange — a re-delivered event carries the seq it originally carried, so
	// equal is a REPLAY rather than a fresh report.
	h := newModelObsHarness(t, "sonnet")
	h.r.SessionModelObserved("s1", "opus", obs(1, 12))

	// Act.
	h.r.SessionModelObserved("s1", "sonnet", obs(1, 12))

	// Assert.
	if got := h.model(); got != "opus" {
		t.Fatalf("record model = %q, want the replayed observation refused", got)
	}
}

func TestAnUntokenedModelObservationIsRefusedLoudly(t *testing.T) {
	// Arrange — a report that orders against nothing would restore precisely
	// the last-writer-wins behavior the token exists to remove.
	h := newModelObsHarness(t, "sonnet")

	// Act.
	h.r.SessionModelObserved("s1", "opus", registry.ModelObservation{})

	// Assert.
	if got := h.model(); got != "sonnet" {
		t.Fatalf("record model = %q, want the untokened observation refused", got)
	}
	if !h.loggedContaining("untokened_observation") {
		t.Fatalf("log = %v, want the untokened refusal recorded", h.logged)
	}
}

func TestARefusedModelObservationDoesNotLowerTheAcceptedMark(t *testing.T) {
	// Arrange — the accepted mark stands at seq 20, and a stale report at 15
	// is refused. A refusal that still moved the mark down to 15 would let
	// everything between 15 and 20 back in.
	h := newModelObsHarness(t, "sonnet")
	h.r.SessionModelObserved("s1", "opus", obs(1, 20))
	h.r.SessionModelObserved("s1", "haiku", obs(1, 15))

	// Act — a second stale report, still below the standing mark.
	h.r.SessionModelObserved("s1", "fable", obs(1, 18))

	// Assert.
	if got := h.model(); got != "opus" {
		t.Fatalf("record model = %q, want the mark held at the accepted observation", got)
	}
}
