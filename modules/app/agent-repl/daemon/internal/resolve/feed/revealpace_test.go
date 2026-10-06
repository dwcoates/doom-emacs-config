package feed

import (
	"context"
	"errors"
	"reflect"
	"slices"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/revealpace"
	"claude-repld/internal/wsm"
)

// fakePacing records what the resolver measures and persists, and answers a
// set expected gap.
type fakePacing struct {
	observed  []observedGap
	persisted []revealpace.Key
	gaps      map[revealpace.Key]uint32
	persistOK error
}

type observedGap struct {
	key revealpace.Key
	ms  int64
}

func (f *fakePacing) Observe(key revealpace.Key, gap time.Duration) {
	f.observed = append(f.observed, observedGap{key: key, ms: gap.Milliseconds()})
}

func (f *fakePacing) ExpectedGap(key revealpace.Key) (uint32, bool) {
	gap, ok := f.gaps[key]
	return gap, ok
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

// newPacedHarness is a harness whose resolver measures into a fake pacer and
// whose session runs pacedModel.
func newPacedHarness(t *testing.T) (*harness, *fakePacing) {
	t.Helper()
	h := newHarness(t)
	pacing := &fakePacing{gaps: map[revealpace.Key]uint32{}}
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

func gapsOf(observed []observedGap) []int64 {
	out := make([]int64, 0, len(observed))
	for _, o := range observed {
		out = append(out, o.ms)
	}
	return out
}

func TestALiveProseBlockMeasuresTheGapsBetweenItsFragments(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.fragmentAt("unit-1", "a", 1_000)
	h.fragmentAt("unit-1", "b", 1_040)
	h.fragmentAt("unit-1", "c", 1_100)

	// Assert
	want := []observedGap{{key: proseKey, ms: 40}, {key: proseKey, ms: 60}}
	if !reflect.DeepEqual(pacing.observed, want) {
		t.Fatalf("observed = %+v, want %+v", pacing.observed, want)
	}
}

func TestABlocksFirstFragmentOnlyStartsTheClock(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.fragmentAt("unit-1", "a", 5_000)

	// Assert
	if len(pacing.observed) != 0 {
		t.Fatalf("observed = %+v, want no sample from a block's first fragment", pacing.observed)
	}
}

func TestAnEmptyFragmentIsNotASample(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)

	// Act
	h.fragmentAt("unit-1", "", 1_030)
	h.fragmentAt("unit-1", "b", 1_050)

	// Assert
	if got := gapsOf(pacing.observed); !slices.Equal(got, []int64{50}) {
		t.Fatalf("gaps = %v, want [50] measured from the last text-bearing fragment", got)
	}
}

func TestANewBlocksFirstFragmentIsNotMeasuredFromThePreviousBlock(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)
	h.settleProse("unit-1", "a")

	// Act
	h.fragmentAt("unit-2", "b", 3_000)

	// Assert
	if len(pacing.observed) != 0 {
		t.Fatalf("observed = %+v, want the gap across blocks left out", pacing.observed)
	}
}

func TestALiveThinkingBlockMeasuresUnderTheThinkingKind(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.nowMs = 1_000
	h.send(thinkingResultFrame("think-1", thinkingTextDelta("a")))

	// Act
	h.nowMs = 1_025
	h.send(thinkingResultFrame("think-1", thinkingTextDelta("b")))

	// Assert
	want := []observedGap{{key: thinkingKey, ms: 25}}
	if !reflect.DeepEqual(pacing.observed, want) {
		t.Fatalf("observed = %+v, want %+v", pacing.observed, want)
	}
}

func TestAnArrivingBubbleCarriesTheExpectedGapOnceTheWindowIsFull(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	pacing.gaps[proseKey] = 45
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.fragmentAt("unit-1", "a", 1_000)

	// Assert
	if got := h.onlyResponse().GetUpdate().GetRevealWindow().GetExpectedGapMs(); got != 45 {
		t.Fatalf("reveal window = %d, want 45", got)
	}
}

func TestAnArrivingBubbleCarriesNoWindowWhileTheWindowIsFilling(t *testing.T) {
	// Arrange
	h, _ := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.fragmentAt("unit-1", "a", 1_000)

	// Assert
	if window := h.onlyResponse().GetUpdate().RevealWindow; window != nil {
		t.Fatalf("reveal window = %v, want none before the window is full", window)
	}
}

func TestASettledBubbleCarriesTheExpectedGap(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	pacing.gaps[proseKey] = 45
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)

	// Act
	h.settleProse("unit-1", "ab")

	// Assert
	if got := h.onlyResponse().GetSuccess().GetRevealWindow().GetExpectedGapMs(); got != 45 {
		t.Fatalf("reveal window = %d, want 45", got)
	}
}

func TestAThinkingBubbleReadsTheThinkingWindow(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	pacing.gaps[proseKey] = 45
	pacing.gaps[thinkingKey] = 90
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.send(thinkingResultFrame("think-1", thinkingTextDelta("a")))

	// Assert
	rows := h.thinkingRows()
	if len(rows) != 1 || rows[0].GetUpdate().GetRevealWindow().GetExpectedGapMs() != 90 {
		t.Fatalf("thinking rows = %v, want one carrying the 90ms thinking window", rows)
	}
}

func TestASettledBlockPersistsTheWindowItAddedTo(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)
	h.fragmentAt("unit-1", "b", 1_040)

	// Act
	h.settleProse("unit-1", "ab")

	// Assert
	if !reflect.DeepEqual(pacing.persisted, []revealpace.Key{proseKey}) {
		t.Fatalf("persisted = %v, want [%v]", pacing.persisted, proseKey)
	}
}

func TestARedeliveredSettlePersistsNothingMore(t *testing.T) {
	// Arrange: the stream plane settles, then the file plane restates it.
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)
	h.fragmentAt("unit-1", "b", 1_040)
	h.settleProse("unit-1", "ab")

	// Act
	h.settleProse("unit-1", "ab")

	// Assert
	if len(pacing.persisted) != 1 {
		t.Fatalf("persisted = %v, want one write for one block", pacing.persisted)
	}
}

func TestASettledBlockThatMeasuredNothingPersistsNothing(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)

	// Act
	h.settleProse("unit-1", "a")

	// Assert
	if len(pacing.persisted) != 0 {
		t.Fatalf("persisted = %v, want nothing for a block that added no gap", pacing.persisted)
	}
}

func TestABlockCutShortPersistsTheWindowItAddedTo(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)
	h.fragmentAt("unit-1", "b", 1_040)

	// Act
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseFailure{
			Prose: &conversationv1.AgentResponseProse{Markdown: "ab"},
		}, nil), nil, nil)

	// Assert
	if !reflect.DeepEqual(pacing.persisted, []revealpace.Key{proseKey}) {
		t.Fatalf("persisted = %v, want [%v]", pacing.persisted, proseKey)
	}
}

func TestAFailedPersistIsRaisedOnTheTopbar(t *testing.T) {
	// Arrange
	h, pacing := newPacedHarness(t)
	pacing.persistOK = errors.New("disk full")
	h.deliverPrompt("turn-1", "hello")
	h.fragmentAt("unit-1", "a", 1_000)
	h.fragmentAt("unit-1", "b", 1_040)

	// Act
	h.settleProse("unit-1", "ab")

	// Assert
	if !slices.Contains(h.warnings.keys(), "reveal_pacing_unsaved") {
		t.Fatalf("raised warnings = %v, want reveal_pacing_unsaved", h.warnings.keys())
	}
	if h.onlyResponse().GetSuccess() == nil {
		t.Fatal("the settled bubble was not drawn after a failed persist")
	}
}

func TestNoModelMeansNoMeasurementAndNoWindow(t *testing.T) {
	// Arrange: a session whose model was never stated.
	h := newHarness(t)
	pacing := &fakePacing{gaps: map[revealpace.Key]uint32{{Kind: wsm.RevealKindProse}: 45}}
	h.resolver.deps.Pacing = pacing
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.fragmentAt("unit-1", "a", 1_000)
	h.fragmentAt("unit-1", "b", 1_040)

	// Assert
	if len(pacing.observed) != 0 || h.onlyResponse().GetUpdate().RevealWindow != nil {
		t.Fatalf("observed %v and window %v, want neither without a model", pacing.observed, h.onlyResponse().GetUpdate().RevealWindow)
	}
}

func TestAReplayedBlockIsNotMeasured(t *testing.T) {
	// Arrange: the workspace is drawing a history page.
	h, pacing := newPacedHarness(t)
	pacing.gaps[proseKey] = 45
	h.deliverPrompt("turn-1", "hello")
	h.resolver.state(testWorkspace).plane = planeHistory

	// Act
	h.fragmentAt("unit-1", "a", 1_000)
	h.fragmentAt("unit-1", "b", 1_040)

	// Assert
	if len(pacing.observed) != 0 {
		t.Fatalf("observed = %+v, want a replay's burst left out", pacing.observed)
	}
	if window := h.onlyResponse().GetUpdate().RevealWindow; window != nil {
		t.Fatalf("reveal window = %v, want none on a replayed draw", window)
	}
}

func TestASubagentsBlockIsNotMeasured(t *testing.T) {
	// Arrange: a subagent whose model the daemon does not know.
	h, pacing := newPacedHarness(t)
	h.deliverPrompt("turn-1", "hello")
	sub := &conversationv1.AgentId{Value: "agent-sub"}
	h.spawnSubagent("spawn-1", sub, "Explore", "look")

	// Act
	for i, at := range []int64{1_000, 1_040} {
		h.nowMs = at
		h.resolver.OnActivity(testWorkspace, sub,
			responseFrame("sub-unit", &conversationv1.AgentResponseUpdate{NewMarkdown: string(rune('a' + i))}, nil), nil, nil)
	}

	// Assert
	if len(pacing.observed) != 0 {
		t.Fatalf("observed = %+v, want a subagent's gaps left out", pacing.observed)
	}
}
