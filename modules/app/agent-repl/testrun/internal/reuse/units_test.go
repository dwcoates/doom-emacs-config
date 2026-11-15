package reuse

import (
	"testing"

	"agentrepl/testrun/reusewhy"
)

func TestATallyNamesEveryReasonInTheVocabularyTheDaemonReads(t *testing.T) {
	// Assert: the daemon reads a gate's tally back through reusewhy, so the
	// runner's tally speaks it word for word and in its order.
	if len(whyOrder) != len(reusewhy.All) {
		t.Fatalf("the tally names %d reasons, reusewhy %d", len(whyOrder), len(reusewhy.All))
	}
	for i, kind := range whyOrder {
		if whyTally[kind] != reusewhy.All[i] {
			t.Errorf("the tally's reason %d is %q, reusewhy's %q", i, whyTally[kind], reusewhy.All[i])
		}
	}
}

func TestANeverRecordedUnitsReasonIsTheTallysWord(t *testing.T) {
	// Act
	got := Why{Kind: WhyNeverRecorded}.String()

	// Assert
	if got != reusewhy.NeverRecorded {
		t.Fatalf("Why.String = %q, want %q", got, reusewhy.NeverRecorded)
	}
}
