package runtimemetrics

import (
	"strings"
	"testing"

	"agent-repld/internal/pageheld"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

func TestAClosedVocabResolvesEveryValueOnce(t *testing.T) {
	// Arrange
	made := map[string]int{}

	// Act
	vocab := newClosedVocab("m", []string{"a", "b"}, func(v string) string { made[v]++; return "child-" + v })

	// Assert
	if vocab.of("a") != "child-a" || vocab.of("b") != "child-b" || made["a"] != 1 || made["b"] != 1 {
		t.Fatalf("children %v, made %v; want each value resolved once", vocab.children, made)
	}
}

func TestAClosedVocabPanicsOnAValueItDoesNotHoldNamingItsMetric(t *testing.T) {
	// Arrange
	vocab := newClosedVocab("agentrepl_x_total", []string{"a"}, func(v string) string { return v })
	defer func() {
		// Assert
		got, _ := recover().(string)
		if !strings.Contains(got, "agentrepl_x_total") || !strings.Contains(got, "z") {
			t.Fatalf("panic = %q, want one naming the metric and the value", got)
		}
	}()

	// Act
	vocab.of("z")
}

// TestEveryClosedVocabularySeriesSharesTheHelper pins that each series whose
// label is a closed vocabulary resolves its children through closedVocab, so
// every one serves each value from 0 and panics alike on an undeclared one.
func TestEveryClosedVocabularySeriesSharesTheHelper(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	sizes := map[string][2]int{
		PromptsSubmittedName:  {len(s.prompts.byOrigin.children), len(s.prompts.byOrigin.children)},
		WorkspacesVisibleName: {len(s.visibility.byReason.children), len(wsm.VisibilityReasonsAll)},
		ResultsReadName:       {len(s.visibility.byReader.children), len(sidebar.ResultReadersAll)},
		PageLoadsName:         {len(s.pages.byLoad.children), len(pageheld.LoadReasons)},
		PageUnloadsName:       {len(s.pages.byUnload.children), len(pageheld.UnloadReasons)},
	}
	metrics := map[string]string{
		PromptsSubmittedName:  s.prompts.byOrigin.metric,
		WorkspacesVisibleName: s.visibility.byReason.metric,
		ResultsReadName:       s.visibility.byReader.metric,
		PageLoadsName:         s.pages.byLoad.metric,
		PageUnloadsName:       s.pages.byUnload.metric,
	}

	// Assert
	for name, size := range sizes {
		if size[0] == 0 || size[0] != size[1] || metrics[name] != name {
			t.Errorf("%s: %d children of %d values, metric %q; want every value resolved under its own name", name, size[0], size[1], metrics[name])
		}
	}
}
