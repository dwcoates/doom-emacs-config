package reuse

import (
	"os"
	"slices"
	"strings"
	"testing"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/reusewhy"
	"agentrepl/testrun/roster"
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

// installedWeb is webFixture with its packages installed as its lock names
// them, so a run of its units records its passes (staleInstall).
func installedWeb(t *testing.T, f fixture) roster.Suite {
	t.Helper()
	s := webFixture(t, f)
	write(t, f.d.Repo, fixtureModule+"/web/package-lock.json", `{"packages": {}}`)
	write(t, f.d.Repo, fixtureModule+"/web/node_modules/.package-lock.json", `{"packages": {}}`)
	return s
}

// batchOf is the unit named name of v.
func batchOf(t *testing.T, v SuiteVerdict, name string) UnitVerdict {
	t.Helper()
	for _, u := range v.Units {
		if u.Name == name {
			return u
		}
	}
	t.Fatalf("no unit %s in %+v", name, v.Units)
	return UnitVerdict{}
}

func TestABatchRunsOnlyTheMembersWhoseOwnInputsChanged(t *testing.T) {
	// Arrange: both files passed together, then b's source changed.
	f := newFixture(t)
	s := installedWeb(t, f)
	f.record(t, Reusing, run.Passed, f.decide(t, Reusing, s, webBatchUnits()))
	write(t, f.d.Repo, fixtureModule+"/web/src/b.ts", "export const b = 2")

	// Act
	v := f.decide(t, Reusing, s, webBatchUnits())
	both := batchOf(t, v, "both")
	kept := SkipReused(v, webBatchUnits())

	// Assert
	if both.Reused != nil || !slices.Equal(both.RunMembers(), []string{"test/b.test.ts"}) {
		t.Fatalf("both runs %v (reused %v), want only test/b.test.ts", both.RunMembers(), both.Reused != nil)
	}
	if both.Why.Kind != WhyInputsChanged || !slices.Contains(both.Why.Changed, fixtureModule+"/web/src/b.ts") {
		t.Fatalf("why = %+v, want src/b.ts changed", both.Why)
	}
	var items []string
	for _, sp := range kept.Splits {
		if sp.Group == "web/both" {
			items = sp.Items
		}
	}
	if !slices.Equal(items, []string{"test/b.test.ts"}) {
		t.Fatalf("the batch's split runs %v, want only test/b.test.ts", items)
	}
	profile, err := os.ReadFile(both.Profile)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(profile), "web/src/b.ts") || strings.Contains(string(profile), "web/src/a.ts") {
		t.Fatalf("the batch's profile is not confined to the inputs of the member it runs:\n%s", profile)
	}
}

func TestMovingAPassedFileToAnotherBatchRerunsNothing(t *testing.T) {
	// Arrange: both files passed in one batch.
	f := newFixture(t)
	s := installedWeb(t, f)
	f.record(t, Reusing, run.Passed, f.decide(t, Reusing, s, webBatchUnits()))

	// Act: the same files, each in a batch of its own.
	v := f.decide(t, Reusing, s, webUnits())

	// Assert
	for _, name := range []string{"test/a.test.ts", "test/b.test.ts"} {
		if batchOf(t, v, name).Reused == nil {
			t.Errorf("%s reran after moving batch", name)
		}
	}
}

func TestAFailedBatchRecordsEachMemberItRanAsFailed(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := installedWeb(t, f)
	f.record(t, Reusing, run.Failed, f.decide(t, Reusing, s, webBatchUnits()))

	// Act
	both := batchOf(t, f.decide(t, Reusing, s, webBatchUnits()), "both")

	// Assert: every member runs again, as having failed before.
	if len(both.RunMembers()) != 2 {
		t.Fatalf("both runs %v, want both members", both.RunMembers())
	}
	for _, m := range both.Members {
		if m.Why.Kind != WhyFailedBefore {
			t.Fatalf("%s's reason = %+v, want failed before", m.File, m.Why)
		}
	}
}

func TestAWhollyReusedBatchDoesNotRun(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := installedWeb(t, f)
	f.record(t, Reusing, run.Passed, f.decide(t, Reusing, s, webBatchUnits()))

	// Act
	v := f.decide(t, Reusing, s, webBatchUnits())
	kept := SkipReused(v, webBatchUnits())

	// Assert
	if batchOf(t, v, "both").Reused == nil {
		t.Fatal("the batch was not reused with every member passed")
	}
	for _, sp := range kept.Splits {
		if sp.Group == "web/both" {
			t.Fatalf("the reused batch still runs: %+v", sp.Items)
		}
	}
}

func TestDecideBatchRefusesAMemberWithNoInputSet(t *testing.T) {
	// Arrange
	f := newFixture(t)
	v := UnitVerdict{Unit: Unit{Suite: "web", Name: "both", Members: []string{"test/a.test.ts"}, MemberSets: map[string]*InputSet{}}}

	// Act
	err := decideBatch(f.d, &v, Reusing, nil)

	// Assert: refused before anything is decided.
	if err == nil || !strings.Contains(err.Error(), "has no input set for its member test/a.test.ts") {
		t.Fatalf("decideBatch = %v, want the missing set named", err)
	}
	if len(v.Members) != 0 || v.Profile != "" {
		t.Fatalf("verdict = %+v, want nothing decided", v)
	}
}

func TestReportSaysHowManyOfABatchsFilesRun(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := installedWeb(t, f)
	f.record(t, Reusing, run.Passed, f.decide(t, Reusing, s, webBatchUnits()))
	write(t, f.d.Repo, fixtureModule+"/web/src/b.ts", "export const b = 2")
	log := &logLines{}

	// Act
	Report(log, f.decide(t, Reusing, s, webBatchUnits()))

	// Assert
	if !slices.ContainsFunc(log.info, func(l string) bool { return strings.Contains(l, "unit both runs 1 of its 2 files") }) {
		t.Fatalf("report = %q, want the batch's run count", log.info)
	}
}
