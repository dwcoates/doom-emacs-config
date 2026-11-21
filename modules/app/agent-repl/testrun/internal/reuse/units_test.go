package reuse

import (
	"slices"
	"strings"
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

// TestTheRerunLogNamesTheDifferingVariable pins that a suite whose units run
// because the environment differs from their records says, once per change,
// which variable differs, its old and new values, and how many units it
// reruns.
func TestTheRerunLogNamesTheDifferingVariable(t *testing.T) {
	// Arrange
	goflags := EnvChange{Name: "GOFLAGS", Old: "-p=1", OldSet: true, OldKnown: true, New: "-race", NewSet: true}
	lang := EnvChange{Name: "LANG", Old: "en_US.UTF-8", OldSet: true, OldKnown: true}
	why := Why{Kind: WhyInputsChanged, Changed: []string{"@env GOFLAGS", "@env LANG"}, Env: []EnvChange{goflags, lang}}
	reused := Record{}
	v := SuiteVerdict{SuiteUnits: SuiteUnits{Suite: lockSuite}, Units: []UnitVerdict{
		{Unit: Unit{Suite: "lock", Name: "a"}, Keyed: true, Key: Key{Digest: strings.Repeat("1", 64)}, Why: why},
		{Unit: Unit{Suite: "lock", Name: "b"}, Keyed: true, Key: Key{Digest: strings.Repeat("2", 64)}, Why: Why{Kind: WhyInputsChanged, Changed: []string{"@env GOFLAGS"}, Env: []EnvChange{goflags}}},
		{Unit: Unit{Suite: "lock", Name: "c"}, Keyed: true, Key: Key{Digest: strings.Repeat("3", 64)}, Reused: &reused},
	}}
	log := &logLines{}

	// Act
	Report(log, v)

	// Assert
	name := lockSuite.Name
	for _, want := range []string{
		name + `: the environment differs from 2 units' records: GOFLAGS was "-p=1", is now "-race"`,
		name + `: the environment differs from 1 units' records: LANG was "en_US.UTF-8", is now unset`,
	} {
		if !slices.Contains(log.info, want) {
			t.Errorf("info = %q, want %q", log.info, want)
		}
	}
}

func TestAnUnchangedEnvironmentLogsNoEnvironmentLine(t *testing.T) {
	// Arrange
	v := SuiteVerdict{SuiteUnits: SuiteUnits{Suite: lockSuite}, Units: []UnitVerdict{
		{Unit: Unit{Suite: "lock", Name: "a"}, Keyed: true, Key: Key{Digest: strings.Repeat("1", 64)}, Why: Why{Kind: WhyInputsChanged, Changed: []string{"x.go"}}},
	}}
	log := &logLines{}

	// Act
	Report(log, v)

	// Assert
	for _, line := range log.info {
		if strings.Contains(line, "the environment differs") {
			t.Fatalf("info = %q, want no environment line", log.info)
		}
	}
}
