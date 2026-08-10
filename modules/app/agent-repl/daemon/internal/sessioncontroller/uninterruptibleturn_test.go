package sessioncontroller

import (
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
)

// runCut brings the harness to "a context cut is the running turn": the cut is
// submitted while the session is idle, so it takes the ordinary forward path
// and is recorded as d.runningText, and the boundary then marks the turn live.
func runCut(t *testing.T, h *queueHarness, text string) {
	t.Helper()
	if err := h.submit(text); err != nil {
		t.Fatalf("submitting %q: %v", text, err)
	}
	h.turn(true)
}

func TestQueuedBehindACutIsStampedUninterruptible(t *testing.T) {
	for _, tc := range []struct {
		name string
		cut  string
		want frontendv1.SessionCommand
	}{
		{"bare compact", "/compact", frontendv1.SessionCommand_SESSION_COMMAND_COMPACT},
		{"scoped compact", "/compact summarize the responses", frontendv1.SessionCommand_SESSION_COMMAND_COMPACT},
		{"clear", "/clear", frontendv1.SessionCommand_SESSION_COMMAND_CLEAR},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — a classifier that would answer, so the assertion is
			// about the stamp being taken instead of one being absent.
			cls := &fakeClassifier{res: ClassifyResult{Classification: VerdictInterject}}
			h := newQueueHarness(t, cls)
			runCut(t, h, tc.cut)

			// Act.
			if err := h.submit("later"); err != nil {
				t.Fatalf("submit: %v", err)
			}

			// Assert.
			es := h.entries()
			if len(es) != 1 {
				t.Fatalf("entries = %d, want 1", len(es))
			}
			if es[0].classification != VerdictUninterruptibleTurn {
				t.Fatalf("classification = %s, want uninterruptible-turn", es[0].classification)
			}
			if es[0].uninterruptibleCommand != tc.want {
				t.Fatalf("command = %s, want %s", es[0].uninterruptibleCommand, tc.want)
			}
		})
	}
}

func TestQueuedBehindAnOrdinaryTurnIsClassified(t *testing.T) {
	// Arrange — the cut stamp must not swallow the ordinary case.
	cls := &fakeClassifier{release: make(chan struct{})}
	h := newQueueHarness(t, cls)
	runCut(t, h, "do the work")

	// Act.
	_ = h.submit("later")

	// Assert.
	if es := h.entries(); len(es) != 1 || es[0].classification != VerdictPending {
		t.Fatalf("entries = %+v, want one PENDING", es)
	}
	close(cls.release)
}

func TestQueuedBehindAnUnknownTurnIsClassified(t *testing.T) {
	// Arrange — a turn that predates this daemon carries no text, and an
	// unknown turn is NOT a cut.
	cls := &fakeClassifier{release: make(chan struct{})}
	h := newQueueHarness(t, cls)
	h.turn(true)

	// Act.
	_ = h.submit("later")

	// Assert.
	if es := h.entries(); len(es) != 1 || es[0].classification != VerdictPending {
		t.Fatalf("entries = %+v, want one PENDING", es)
	}
	close(cls.release)
}

func TestClassifierNeverRunsBehindACut(t *testing.T) {
	// Arrange — the whole point: no model call is spent on a prompt whose
	// verdict could only be wrong.
	cls := &fakeClassifier{res: ClassifyResult{Classification: VerdictInterject}}
	h := newQueueHarness(t, cls)
	runCut(t, h, "/compact")

	// Act.
	_ = h.submit("later")
	waitFor(t, "the queue view push for the stamped entry", func() bool {
		v := h.push.lastQueue()
		return v != nil && len(v.GetEntries()) == 1
	})

	// Assert.
	if reqs := cls.requests(); len(reqs) != 0 {
		t.Fatalf("classifier was asked %d times, want 0: %+v", len(reqs), reqs)
	}
}

func TestUninterruptibleEntryIsNeverInterjected(t *testing.T) {
	// Arrange.
	cls := &fakeClassifier{res: ClassifyResult{Classification: VerdictInterject}}
	h := newQueueHarness(t, cls)
	runCut(t, h, "/compact")

	// Act.
	_ = h.submit("later")
	waitFor(t, "the queue view push for the stamped entry", func() bool {
		v := h.push.lastQueue()
		return v != nil && len(v.GetEntries()) == 1
	})

	// Assert — the cut's turn is untouched.
	if n := h.client.interruptCount(); n != 0 {
		t.Fatalf("interrupts = %d, want 0: a context cut is never interrupted for a queued prompt", n)
	}
}

func TestUninterruptibleEntryProjectsItsArm(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	runCut(t, h, "/clear")
	_ = h.submit("later")

	// Act.
	v := h.queueView()

	// Assert.
	if len(v.GetEntries()) != 1 {
		t.Fatalf("entries = %d, want 1", len(v.GetEntries()))
	}
	arm := v.GetEntries()[0].GetUninterruptibleTurn()
	if arm == nil {
		t.Fatalf("classification arm = %T, want the uninterruptible-turn arm", v.GetEntries()[0].GetClassification())
	}
	if arm.GetCommand() != frontendv1.SessionCommand_SESSION_COMMAND_CLEAR {
		t.Fatalf("command = %s, want CLEAR", arm.GetCommand())
	}
}

func TestUninterruptibleEntryIsDeliveredWhenTheCutEnds(t *testing.T) {
	// Arrange — DELAYED, NEVER DROPPED: the prompt runs the moment the cut's
	// turn ends, with no interrupt and no force.
	h := newQueueHarness(t, nil)
	runCut(t, h, "/compact")
	_ = h.submit("later")

	// Act.
	h.turn(false)

	// Assert.
	waitFor(t, "the queued prompt to be delivered", func() bool {
		texts := h.client.promptTexts()
		return len(texts) == 2 && texts[1] == "later"
	})
}

func TestForceIsRefusedBehindACut(t *testing.T) {
	// Arrange — a force's MECHANISM is an interrupt, so it is refused rather
	// than honored into the one interrupt that must not happen.
	h := newQueueHarness(t, nil)
	runCut(t, h, "/compact")
	_ = h.submit("later")

	// Act.
	err := h.m.ForceQueueEntry("ws", h.entries()[0].id)

	// Assert.
	if !errors.Is(err, errclass.ErrQueueEntryUninterruptibleTurn) {
		t.Fatalf("force error = %v, want the uninterruptible-turn sentinel", err)
	}
	if n := h.client.interruptCount(); n != 0 {
		t.Fatalf("interrupts = %d, want 0", n)
	}
	if len(h.entries()) != 1 {
		t.Fatal("a refused force must leave the entry queued")
	}
}

func TestRefusedForceClassifiesAsItsOwnFailure(t *testing.T) {
	// Arrange — an ordinary, expected refusal must reach a human NAMED rather
	// than as internal.unclassified.
	h := newQueueHarness(t, nil)
	runCut(t, h, "/compact")
	_ = h.submit("later")

	// Act.
	typ, ok := errclass.Sentinel(h.m.ForceQueueEntry("ws", h.entries()[0].id))

	// Assert.
	if !ok || typ != errclass.TypeQueueEntryUninterruptibleTurn {
		t.Fatalf("classified as (%s, %v), want queue.entry_uninterruptible_turn", typ, ok)
	}
}

func TestInterjectIsRefusedWhenACutIsRunning(t *testing.T) {
	// Arrange — the funnel guard: whatever asks for an interject, a cut is not
	// interrupted for a queued prompt.
	h := newQueueHarness(t, nil)
	runCut(t, h, "/compact")
	_ = h.submit("later")
	entryID := h.entries()[0].id

	// Act.
	h.m.beginInterject(h.controller(), entryID, "test")

	// Assert.
	if n := h.client.interruptCount(); n != 0 {
		t.Fatalf("interrupts = %d, want 0", n)
	}
	es := h.entries()
	if len(es) != 1 || es[0].interjecting {
		t.Fatalf("entries = %+v, want one entry that is NOT interjecting", es)
	}
}

func TestVerdictIsDiscardedWhenACutStartedWhileClassifying(t *testing.T) {
	// Arrange — the race the submit-time check cannot cover: the entry is
	// queued behind an ordinary turn, and a compaction becomes the running turn
	// before the classifier answers INTERJECT.
	cls := &fakeClassifier{
		res:     ClassifyResult{Classification: VerdictInterject, Rationale: "go now"},
		release: make(chan struct{}),
	}
	h := newQueueHarness(t, cls)
	runCut(t, h, "do the work")
	_ = h.submit("later")
	waitFor(t, "the classifier to be asked", func() bool { return len(cls.requests()) == 1 })

	// Act — the ordinary turn ends and a compaction takes its place, then the
	// verdict about the turn that is now gone lands.
	d := h.controller()
	h.m.mu.Lock()
	d.runningText = "/compact"
	h.m.mu.Unlock()
	close(cls.release)

	// Assert.
	waitFor(t, "the verdict to be discarded", func() bool {
		es := h.entries()
		return len(es) == 1 && es[0].classification == VerdictUninterruptibleTurn
	})
	if n := h.client.interruptCount(); n != 0 {
		t.Fatalf("interrupts = %d, want 0: the compaction must not be interrupted for a stale verdict", n)
	}
}

func TestUninterruptibleRationaleNamesTheCut(t *testing.T) {
	// Arrange / Act.
	got := uninterruptibleRationale(frontendv1.SessionCommand_SESSION_COMMAND_COMPACT)

	// Assert.
	if !strings.Contains(got, "/compact") {
		t.Fatalf("rationale = %q, want it to name /compact", got)
	}
}

func TestSessionCommandLiteralReadsTheSchema(t *testing.T) {
	// Arrange / Act / Assert.
	if got := sessionCommandLiteral(frontendv1.SessionCommand_SESSION_COMMAND_CLEAR); got != "/clear" {
		t.Fatalf("literal = %q, want /clear", got)
	}
}

func TestSessionCommandLiteralNamesAnUnspecifiedCommand(t *testing.T) {
	// Arrange / Act — a value with no spec renders its NAME, so an impossible
	// command is visible in the line instead of vanishing into an empty string.
	got := sessionCommandLiteral(frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED)

	// Assert.
	if got != frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED.String() {
		t.Fatalf("literal = %q, want the enum name", got)
	}
}

func TestStampUninterruptibleSetsTheWholeFact(t *testing.T) {
	// Arrange — the verdict, the command and the prose are ONE fact, and the
	// helper exists so no site can set two of the three.
	e := &queueEntry{classification: VerdictPending}

	// Act.
	stampUninterruptible(e, frontendv1.SessionCommand_SESSION_COMMAND_COMPACT)

	// Assert.
	if e.classification != VerdictUninterruptibleTurn {
		t.Fatalf("classification = %s, want uninterruptible-turn", e.classification)
	}
	if e.uninterruptibleCommand != frontendv1.SessionCommand_SESSION_COMMAND_COMPACT {
		t.Fatalf("command = %s, want COMPACT", e.uninterruptibleCommand)
	}
	if e.rationale != uninterruptibleRationale(frontendv1.SessionCommand_SESSION_COMMAND_COMPACT) {
		t.Fatalf("rationale = %q, want the stated one", e.rationale)
	}
}

func TestEveryStampingSiteProducesTheSameShape(t *testing.T) {
	// Arrange — the three routes to the verdict must be indistinguishable in
	// the entry they leave behind, or a hand-rolled site would pass with an
	// entry whose arm and whose prose disagree.
	want := &queueEntry{}
	stampUninterruptible(want, frontendv1.SessionCommand_SESSION_COMMAND_COMPACT)

	for _, tc := range []struct {
		name string
		act  func(t *testing.T) queueEntry
	}{
		{
			name: "submit behind a running cut",
			act: func(t *testing.T) queueEntry {
				h := newQueueHarness(t, nil)
				runCut(t, h, "/compact")
				_ = h.submit("later")
				return h.entries()[0]
			},
		},
		{
			name: "a verdict discarded because a cut started",
			act: func(t *testing.T) queueEntry {
				cls := &fakeClassifier{
					res:     ClassifyResult{Classification: VerdictInterject, Rationale: "go now"},
					release: make(chan struct{}),
				}
				h := newQueueHarness(t, cls)
				runCut(t, h, "do the work")
				_ = h.submit("later")
				waitFor(t, "the classifier to be asked", func() bool { return len(cls.requests()) == 1 })
				d := h.controller()
				h.m.mu.Lock()
				d.runningText = "/compact"
				h.m.mu.Unlock()
				close(cls.release)
				waitFor(t, "the verdict to be discarded", func() bool {
					es := h.entries()
					return len(es) == 1 && es[0].classification == VerdictUninterruptibleTurn
				})
				return h.entries()[0]
			},
		},
		{
			name: "an interject refused at the funnel",
			act: func(t *testing.T) queueEntry {
				cls := &fakeClassifier{release: make(chan struct{})}
				h := newQueueHarness(t, cls)
				runCut(t, h, "do the work")
				_ = h.submit("later")
				d := h.controller()
				h.m.mu.Lock()
				d.runningText = "/compact"
				h.m.mu.Unlock()
				h.m.beginInterject(d, h.entries()[0].id, "test")
				close(cls.release)
				return h.entries()[0]
			},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := tc.act(t)

			// Assert.
			if got.classification != want.classification ||
				got.uninterruptibleCommand != want.uninterruptibleCommand ||
				got.rationale != want.rationale {
				t.Fatalf("stamp = (%s, %s, %q), want (%s, %s, %q)",
					got.classification, got.uninterruptibleCommand, got.rationale,
					want.classification, want.uninterruptibleCommand, want.rationale)
			}
		})
	}
}
