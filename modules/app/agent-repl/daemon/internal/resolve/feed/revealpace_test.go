package feed

import (
	"context"
	"errors"
	"reflect"
	"slices"
	"strings"
	"testing"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/revealpace"
	"agent-repld/internal/wsm"
)

// fakePacing records what the resolver measures and persists, and answers
// set bounds per key and per kind.
type fakePacing struct {
	observed   []observedSpeed
	persisted  []revealpace.Key
	bounds     map[revealpace.Key]revealpace.Bounds
	kindBounds map[wsm.RevealKind]revealpace.Bounds
	persistOK  error
}

type observedSpeed struct {
	key            revealpace.Key
	charsPerSecond float64
}

func (f *fakePacing) Observe(key revealpace.Key, charsPerSecond float64) {
	f.observed = append(f.observed, observedSpeed{key: key, charsPerSecond: charsPerSecond})
}

func (f *fakePacing) Bounds(key revealpace.Key) revealpace.Bounds {
	return f.bounds[key]
}

func (f *fakePacing) KindBounds(kind wsm.RevealKind) revealpace.Bounds {
	return f.kindBounds[kind]
}

func (f *fakePacing) Persist(_ context.Context, key revealpace.Key) error {
	if f.persistOK != nil {
		return f.persistOK
	}
	f.persisted = append(f.persisted, key)
	return nil
}

const pacedModel = "claude-opus-5"

var (
	proseKey    = revealpace.Key{Model: pacedModel, Kind: wsm.RevealKindProse}
	thinkingKey = revealpace.Key{Model: pacedModel, Kind: wsm.RevealKindThinking}
)

// The bounds every fake pacer answers, distinct per source so a test can tell
// which record a pace was read from.
var (
	proseBounds        = revealpace.Bounds{MinCharsPerSecond: 49, MaxCharsPerSecond: 90}
	thinkingBounds     = revealpace.Bounds{MinCharsPerSecond: 45, MaxCharsPerSecond: 55}
	proseKindBounds    = revealpace.Bounds{MinCharsPerSecond: 30, MaxCharsPerSecond: 70}
	thinkingKindBounds = revealpace.Bounds{MinCharsPerSecond: 20, MaxCharsPerSecond: 40}
)

func newFakePacing() *fakePacing {
	return &fakePacing{
		bounds: map[revealpace.Key]revealpace.Bounds{proseKey: proseBounds, thinkingKey: thinkingBounds},
		kindBounds: map[wsm.RevealKind]revealpace.Bounds{
			wsm.RevealKindProse: proseKindBounds, wsm.RevealKindThinking: thinkingKindBounds,
		},
	}
}

// paceOf answers the wire pace bounds b are served as.
func paceOf(b revealpace.Bounds) *frontendv1.FeedResponseRevealPace {
	return &frontendv1.FeedResponseRevealPace{
		MinCharsPerSecond: b.MinCharsPerSecond,
		MaxCharsPerSecond: b.MaxCharsPerSecond,
		BankLimitMs:       1500,
	}
}

// chars answers n copies of "x".
func chars(n int) string { return strings.Repeat("x", n) }

// newPacedHarness is a harness whose resolver measures into a fake pacer and
// whose session runs pacedModel.
func newPacedHarness(t *testing.T) (*harness, *fakePacing) {
	t.Helper()
	h := newHarness(t)
	pacing := newFakePacing()
	h.resolver.deps.Pacing = pacing
	h.resolver.OnSessionStarted(testWorkspace, &conversationv1.SessionStarted{EffectiveModel: &conversationv1.AgentModel{Name: pacedModel}})
	return h, pacing
}

// fragmentAt delivers one prose fragment of UNIT at the clock reading atMs.
func (h *harness) fragmentAt(unit, text string, atMs int64) {
	h.t.Helper()
	h.nowMs = atMs
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame(unit, &conversationv1.AgentResponseUpdate{NewMarkdown: text}, nil), nil, nil)
}

// settleProse delivers UNIT's settled whole.
func (h *harness) settleProse(unit, markdown string) {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame(unit, &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: markdown},
		}, nil), nil, nil)
}

// onlyResponse answers the one prose bubble on the root feed, beside the
// prompt row every paced test delivers.
func (h *harness) onlyResponse() *frontendv1.FeedResponse {
	h.t.Helper()
	rows := h.responseRows()
	if len(rows) != 1 {
		h.t.Fatalf("response rows = %d, want exactly 1", len(rows))
	}
	return rows[0].GetActivity().GetResponse()
}

func TestALiveProseBlockMeasuresItsArrivalSpeed(t *testing.T) {
	// Arrange: 10 chars at 1.000s (left out: they arrived at the start), 60
	// more over the next second.
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", chars(10), 1_000)
	h.fragmentAt("unit-1", chars(30), 1_400)
	h.nowMs = 2_000

	// Act
	h.settleProse("unit-1", chars(70))

	// Assert
	want := []observedSpeed{{key: proseKey, charsPerSecond: 60}}
	if !reflect.DeepEqual(pacing.observed, want) {
		t.Fatalf("observed = %+v, want %+v", pacing.observed, want)
	}
	if !reflect.DeepEqual(pacing.persisted, []revealpace.Key{proseKey}) {
		t.Fatalf("persisted = %v, want [%v]", pacing.persisted, proseKey)
	}
}

func TestALiveThinkingBlockMeasuresUnderTheThinkingKind(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.nowMs = 1_000
	h.send(thinkingResultFrame("think-1", thinkingTextDelta("a")))
	h.nowMs = 3_000

	// Act
	h.send(thinkingResultFrame("think-1", thinkingTextSuccess("a"+chars(100))))

	// Assert
	want := []observedSpeed{{key: thinkingKey, charsPerSecond: 50}}
	if !reflect.DeepEqual(pacing.observed, want) {
		t.Fatalf("observed = %+v, want %+v", pacing.observed, want)
	}
}

func TestABlockSpeedCountsCharactersNotBytes(t *testing.T) {
	// Arrange: 50 two-byte characters after the first fragment, over 1s.
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)
	h.nowMs = 2_000

	// Act
	h.settleProse("unit-1", "a"+strings.Repeat("é", 50))

	// Assert
	want := []observedSpeed{{key: proseKey, charsPerSecond: 50}}
	if !reflect.DeepEqual(pacing.observed, want) {
		t.Fatalf("observed = %+v, want %+v", pacing.observed, want)
	}
}

func TestABlockTooSmallToMeasureIsLeftOut(t *testing.T) {
	cases := []struct {
		name      string
		settledAt int64
		markdown  string
	}{
		{name: "too few characters", settledAt: 2_000, markdown: "a" + chars(minMeasuredChars-1)},
		{name: "too short a time", settledAt: 1_000 + minMeasuredDuration.Milliseconds() - 1, markdown: "a" + chars(100)},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h, pacing := newPacedHarness(t)
			h.deliverPrompt("turn-1", "hello")
			h.fragmentAt("unit-1", "a", 1_000)
			h.nowMs = tc.settledAt

			// Act
			h.settleProse("unit-1", tc.markdown)

			// Assert
			if len(pacing.observed) != 0 || len(pacing.persisted) != 0 {
				t.Fatalf("observed %v, persisted %v, want neither for a block too small to measure", pacing.observed, pacing.persisted)
			}
			if !h.hasRecordWith("debug", "daemon.feed.reveal_block_unmeasured", "kind", string(wsm.RevealKindProse)) {
				t.Fatal("no DEBUG record of the unmeasured block")
			}
		})
	}
}

func TestABlockThatNeverStreamedIsNotMeasured(t *testing.T) {
	// Arrange: the settled whole lands with no fragment before it.
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.nowMs = 5_000

	// Act
	h.settleProse("unit-1", chars(200))

	// Assert
	if len(pacing.observed) != 0 {
		t.Fatalf("observed = %+v, want nothing for a block that never streamed", pacing.observed)
	}
}

func TestARestartedBlockMeasuresFromItsNewFirstFragment(t *testing.T) {
	// Arrange: a block streams, restarts, and streams again.
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, nil), nil, nil)
	h.fragmentAt("unit-1", "b", 9_000)
	h.nowMs = 10_000

	// Act
	h.settleProse("unit-1", "b"+chars(80))

	// Assert
	want := []observedSpeed{{key: proseKey, charsPerSecond: 80}}
	if !reflect.DeepEqual(pacing.observed, want) {
		t.Fatalf("observed = %+v, want %+v measured from the restart", pacing.observed, want)
	}
}

func TestABlockCutShortIsNotMeasured(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", chars(10), 1_000)
	h.nowMs = 3_000

	// Act
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseFailure{
			Prose: &conversationv1.AgentResponseProse{Markdown: chars(200)},
		}, nil), nil, nil)

	// Assert
	if len(pacing.observed) != 0 || len(pacing.persisted) != 0 {
		t.Fatalf("observed %v, persisted %v, want neither for a block the turn's death cut short", pacing.observed, pacing.persisted)
	}
}

func TestARedeliveredSettleMeasuresNothingMore(t *testing.T) {
	// Arrange: the stream plane settles, then the file plane restates it.
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)
	h.nowMs = 2_000
	h.settleProse("unit-1", "a"+chars(100))

	// Act
	h.nowMs = 2_500
	h.settleProse("unit-1", "a"+chars(100))

	// Assert
	if len(pacing.observed) != 1 || len(pacing.persisted) != 1 {
		t.Fatalf("observed %v, persisted %v, want one of each for one block", pacing.observed, pacing.persisted)
	}
}

func TestAFailedPersistIsRaisedOnTheTopbar(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	pacing.persistOK = errors.New("disk full")
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)
	h.nowMs = 2_000

	// Act
	h.settleProse("unit-1", "a"+chars(100))

	// Assert
	if !slices.Contains(h.warnings.keys(), "reveal_pacing_unsaved") {
		t.Fatalf("raised warnings = %v, want reveal_pacing_unsaved", h.warnings.keys())
	}
	if h.onlyResponse().GetSuccess() == nil {
		t.Fatal("the settled bubble was not drawn after a failed persist")
	}
}

func TestAnArrivingBubbleCarriesItsModelsPace(t *testing.T) {
	// Arrange
	h, _ := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.fragmentAt("unit-1", "a", 1_000)

	// Assert
	if got := h.onlyResponse().GetUpdate().GetRevealPace(); !proto.Equal(got, paceOf(proseBounds)) {
		t.Fatalf("reveal pace = %v, want %v", got, paceOf(proseBounds))
	}
}

func TestASettledBubbleCarriesItsModelsPace(t *testing.T) {
	// Arrange
	h, _ := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)

	// Act
	h.settleProse("unit-1", "ab")

	// Assert
	if got := h.onlyResponse().GetSuccess().GetRevealPace(); !proto.Equal(got, paceOf(proseBounds)) {
		t.Fatalf("reveal pace = %v, want %v", got, paceOf(proseBounds))
	}
}

func TestAThinkingBubbleCarriesTheThinkingPace(t *testing.T) {
	// Arrange
	h, _ := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.send(thinkingResultFrame("think-1", thinkingTextDelta("a")))

	// Assert
	rows := h.thinkingRows()
	if len(rows) != 1 || !proto.Equal(rows[0].GetUpdate().GetRevealPace(), paceOf(thinkingBounds)) {
		t.Fatalf("thinking rows = %v, want one carrying %v", rows, paceOf(thinkingBounds))
	}
}

func TestASubagentsBubbleCarriesItsKindsPaceAndIsNotMeasured(t *testing.T) {
	// Arrange: a subagent whose model the daemon does not know.
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.spawnSubagent("spawn-1", subAgent(), "Explore", "look")
	h.nowMs = 1_000
	h.resolver.OnActivity(testWorkspace, subAgent(),
		responseFrame("sub-unit", &conversationv1.AgentResponseUpdate{NewMarkdown: "a"}, nil), nil, nil)
	h.nowMs = 3_000

	// Act
	h.resolver.OnActivity(testWorkspace, subAgent(),
		responseFrame("sub-unit", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "a" + chars(100)},
		}, nil), nil, nil)

	// Assert
	if len(pacing.observed) != 0 {
		t.Fatalf("observed = %+v, want a subagent's block left out", pacing.observed)
	}
	var got *frontendv1.FeedResponseRevealPace
	for _, row := range h.rows(subFeed()) {
		if r := row.GetActivity().GetResponse(); r != nil {
			got = r.GetSuccess().GetRevealPace()
		}
	}
	if !proto.Equal(got, paceOf(proseKindBounds)) {
		t.Fatalf("subagent reveal pace = %v, want the kind-wide %v", got, paceOf(proseKindBounds))
	}
}

func TestNoModelMeansTheKindsPaceAndNoMeasurement(t *testing.T) {
	// Arrange: a session whose model was never stated.
	h := newHarness(t)
	pacing := newFakePacing()
	h.resolver.deps.Pacing = pacing
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)
	h.nowMs = 3_000

	// Act
	h.settleProse("unit-1", "a"+chars(100))

	// Assert
	if len(pacing.observed) != 0 {
		t.Fatalf("observed = %+v, want nothing measured without a model", pacing.observed)
	}
	if got := h.onlyResponse().GetSuccess().GetRevealPace(); !proto.Equal(got, paceOf(proseKindBounds)) {
		t.Fatalf("reveal pace = %v, want the kind-wide %v", got, paceOf(proseKindBounds))
	}
}

func TestAReplayedBlockIsNotMeasuredAndCarriesNoPace(t *testing.T) {
	// Arrange: the workspace is drawing a history page.
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.resolver.state(testWorkspace).plane = planeHistory
	h.fragmentAt("unit-1", "a", 1_000)
	h.nowMs = 3_000

	// Act
	h.settleProse("unit-1", "a"+chars(100))

	// Assert
	if len(pacing.observed) != 0 {
		t.Fatalf("observed = %+v, want a replay left out", pacing.observed)
	}
	if pace := h.onlyResponse().GetSuccess().RevealPace; pace != nil {
		t.Fatalf("reveal pace = %v, want none on a replayed draw", pace)
	}
}

func TestNoPacingRecordMeansNoPace(t *testing.T) {
	// Arrange: a resolver with no pacing record wired.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.fragmentAt("unit-1", "a", 1_000)

	// Assert
	if pace := h.onlyResponse().GetUpdate().RevealPace; pace != nil {
		t.Fatalf("reveal pace = %v, want none without a pacing record", pace)
	}
}

// thinkingTextSuccess is a reasoning block's settled whole.
func thinkingTextSuccess(text string) *conversationv1.AgentThinkingSuccess {
	return &conversationv1.AgentThinkingSuccess{
		Reasoning: &conversationv1.AgentThinkingSuccess_Text{
			Text: &conversationv1.AgentThinkingText{Text: text},
		},
	}
}
