package sessioncontroller

import (
	"errors"
	"strings"
	"testing"
)

// THE BARE `/model` is the one model change the daemon can neither perform nor
// observe: the CLI opens its own picker and the user chooses inside it. The
// daemon reads the answer back at the command's own turn boundary — which is a
// rendezvous with the instant the answer exists, not a delay long enough to
// probably work.

// awaitModelReadbacks blocks until every in-flight live-model read-back has
// finished.
//
// The read-back runs on its own goroutine, and this is the production
// WaitGroup that a shutdown waits on — so the test rendezvous is the real
// one rather than a polled approximation of it. Nothing here sleeps.
func (h *queueHarness) awaitModelReadbacks() {
	h.m.modelReadbacks.Wait()
}

// failureItemUUIDs are the uuids of every system-failure item pushed.
func (h *queueHarness) failureItemUUIDs() []string {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []string
	for _, cd := range h.push.convo {
		for _, it := range cd.GetItems() {
			if it.GetFailureCard() != nil {
				out = append(out, it.GetUuid())
			}
		}
	}
	return out
}

// hasFailureItem reports whether a failure card was pushed under uuid.
func (h *queueHarness) hasFailureItem(uuid string) bool {
	for _, got := range h.failureItemUUIDs() {
		if got == uuid {
			return true
		}
	}
	return false
}

// --- the guarantee ----------------------------------------------------------

func TestTheBareModelCommandReadsTheLiveModelBackWhenItsTurnEnds(t *testing.T) {
	// Arrange — without the read the daemon learns the new model only when
	// some later submit re-announces a SystemInit, so the picker names the old
	// one until then.
	h := newQueueHarness(t, nil)
	h.client.mu.Lock()
	h.client.queriedModel = "claude-opus-5"
	h.client.mu.Unlock()

	// Act — the command runs, then its turn ends.
	if err := h.submitAs("r1", "/model"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	h.m.onTurnEvent(h.controller(), false, "r1", turnOutcome{})
	h.awaitModelReadbacks()

	// Assert.
	if got := h.client.modelQueryCount(); got != 1 {
		t.Fatalf("live-model read-backs = %d, want exactly 1", got)
	}
}

func TestTheReadBackModelIsPersisted(t *testing.T) {
	// Arrange — the read exists to write the record, so the next respawn is
	// pinned to what the session IS.
	h := newQueueHarness(t, nil)
	h.client.mu.Lock()
	h.client.queriedModel = "claude-opus-5"
	h.client.mu.Unlock()

	// Act.
	if err := h.submitAs("r1", "/model"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	h.m.onTurnEvent(h.controller(), false, "r1", turnOutcome{})
	h.awaitModelReadbacks()

	// Assert.
	if got := h.observedModels(); len(got) != 1 || got[0] != "claude-opus-5" {
		t.Fatalf("observed models = %q, want the read-back selection persisted", got)
	}
}

// --- the cases that must NOT read back --------------------------------------

func TestANamedModelCommandDoesNotReadTheModelBack(t *testing.T) {
	// Arrange — `/model opus` is PERFORMED, so the shim already confirmed the
	// selection. Asking again would be a second authority on the same fact.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "/model opus"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	h.m.onTurnEvent(h.controller(), false, "r1", turnOutcome{})

	// Assert.
	if got := h.client.modelQueryCount(); got != 0 {
		t.Fatalf("live-model read-backs = %d, want none for a performed model change", got)
	}
}

func TestAnOrdinaryPromptDoesNotReadTheModelBack(t *testing.T) {
	// Arrange — every turn ends, and a read-back on each would ask the shim on
	// every prompt in the session.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	h.m.onTurnEvent(h.controller(), false, "r1", turnOutcome{})

	// Assert.
	if got := h.client.modelQueryCount(); got != 0 {
		t.Fatalf("live-model read-backs = %d, want none for an ordinary prompt", got)
	}
}

func TestAReObservedTurnEndReadsTheModelBackOnlyOnce(t *testing.T) {
	// Arrange — a turn's end can be observed more than once (a durable
	// boundary behind a re-delivered event). A read per observation would ask
	// the shim repeatedly and rewrite the record from each answer.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "/model"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	h.m.onTurnEvent(h.controller(), false, "r1", turnOutcome{})
	h.awaitModelReadbacks()
	h.m.onTurnEvent(h.controller(), false, "r1", turnOutcome{})
	h.awaitModelReadbacks()

	// Assert.
	if got := h.client.modelQueryCount(); got != 1 {
		t.Fatalf("live-model read-backs = %d, want exactly 1 for a re-observed end", got)
	}
}

// --- the violation ----------------------------------------------------------

// A shim that cannot answer must SURFACE, never leave the picker naming a
// model nobody verified.
func TestAShimThatCannotAnswerTheReadBackSurfacesAFailure(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.client.mu.Lock()
	h.client.queryModelErr = errors.New("the SDK query does not exist yet")
	h.client.mu.Unlock()

	// Act.
	if err := h.submitAs("r1", "/model"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	h.m.onTurnEvent(h.controller(), false, "r1", turnOutcome{})
	h.awaitModelReadbacks()

	// Assert.
	if !h.hasFailureItem("model-readback-r1") {
		t.Fatal("an unanswerable read-back was swallowed; the picker would keep naming a model nobody verified")
	}
}

func TestAFailedReadBackLeavesTheRecordAlone(t *testing.T) {
	// Arrange — a failed read must not write a guess, and must not blank what
	// the record already holds.
	h := newQueueHarness(t, nil)
	h.client.mu.Lock()
	h.client.queryModelErr = errors.New("the SDK query does not exist yet")
	h.client.mu.Unlock()

	// Act.
	if err := h.submitAs("r1", "/model"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	h.m.onTurnEvent(h.controller(), false, "r1", turnOutcome{})
	h.awaitModelReadbacks()

	// Assert.
	if got := h.observedModels(); len(got) != 0 {
		t.Fatalf("observed models = %q, want the record untouched by a failed read", got)
	}
}

func TestAFailedReadBackIsRecordedInTheCanonicalLog(t *testing.T) {
	// Arrange — the shared log alone must explain why the picker and the
	// session may disagree.
	log := &logCapture{}
	h := newQueueHarnessWithPusher(t, nil, nil, log.logf)
	h.client.mu.Lock()
	h.client.queryModelErr = errors.New("the SDK query does not exist yet")
	h.client.mu.Unlock()

	// Act.
	if err := h.submitAs("r1", "/model"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	h.m.onTurnEvent(h.controller(), false, "r1", turnOutcome{})
	h.awaitModelReadbacks()

	// Assert.
	if !log.contains("live model read-back FAILED") {
		t.Fatal("the failed read-back was not recorded in the canonical log")
	}
}

// --- the model transition is diagnosable from the shared log alone ----------
//
// The `/model` invocation used to log the command's NAME and nothing else:
// workspace, session, request id. An operator reading it after the picker and
// the session disagreed could not tell what the session was switched to, or
// that a switch was the reason.

// matching returns the captured lines holding needle. The sink itself is
// rotation_test.go's logCapture, reused rather than duplicated.
func logLinesMatching(l *logCapture, needle string) []string {
	l.mu.Lock()
	defer l.mu.Unlock()
	var out []string
	for _, line := range l.lines {
		if strings.Contains(line, needle) {
			out = append(out, line)
		}
	}
	return out
}

func TestTheModelCommandLogNamesTheResolvedArgument(t *testing.T) {
	// Arrange.
	log := &logCapture{}
	h := newQueueHarnessWithPusher(t, nil, nil, log.logf)

	// Act.
	if err := h.submitAs("r1", "/model opus"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	lines := logLinesMatching(log, "session command SESSION_COMMAND_MODEL invoked")
	if len(lines) != 1 || !strings.Contains(lines[0], `resolved_model="opus"`) {
		t.Fatalf("invocation log = %v, want it to name the resolved argument", lines)
	}
}

func TestTheModelCommandLogNamesTheShimConfirmedSelection(t *testing.T) {
	// Arrange — the requested name and the confirmed selection deliberately
	// differ: the requested value alone cannot say whether the change took.
	log := &logCapture{}
	h := newQueueHarnessWithPusher(t, nil, nil, log.logf)
	h.client.mu.Lock()
	h.client.setModelSelected = "claude-opus-5"
	h.client.mu.Unlock()

	// Act.
	if err := h.submitAs("r1", "/model opus"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	lines := logLinesMatching(log, "session command SESSION_COMMAND_MODEL invoked")
	if len(lines) != 1 || !strings.Contains(lines[0], `shim_selected="claude-opus-5"`) {
		t.Fatalf("invocation log = %v, want it to name the shim-confirmed selection", lines)
	}
}

func TestTheModelCommandLogNamesTheRecordsPriorValue(t *testing.T) {
	// Arrange — the confirmed value without the one it replaced is a reading
	// rather than a transition, and explains nothing about a disagreeing
	// picker. The session is already running Sonnet.
	log := &logCapture{}
	h := newQueueHarnessWithPusher(t, nil, nil, log.logf)
	h.controller().consumer.Consume(systemInitEvent(t, 1, "claude-sonnet-5"))

	// Act.
	if err := h.submitAs("r1", "/model opus"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	lines := logLinesMatching(log, "session command SESSION_COMMAND_MODEL invoked")
	if len(lines) != 1 || !strings.Contains(lines[0], `record_previous="claude-sonnet-5"`) {
		t.Fatalf("invocation log = %v, want it to name the value the record held before", lines)
	}
}

func TestAnOrdinarySessionCommandLogCarriesNoModelOutcome(t *testing.T) {
	// Arrange — a command that resolves to nothing must not pad its line with
	// empty model fields that read as a model change that did not happen.
	log := &logCapture{}
	h := newQueueHarnessWithPusher(t, nil, nil, log.logf)

	// Act.
	if err := h.submitAs("r1", "/cost"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	lines := logLinesMatching(log, "session command SESSION_COMMAND_COST invoked")
	if len(lines) != 1 || strings.Contains(lines[0], "resolved_model") {
		t.Fatalf("invocation log = %v, want no model outcome for a command that resolves none", lines)
	}
}
