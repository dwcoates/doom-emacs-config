package plan

import (
	"reflect"
	"sort"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/internal/history"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
)

// estimates is a history of estimates alone, with no peaks: every unit is
// bounded as one never measured.
type estimates map[string]float64

func (e estimates) Get(key string) (float64, bool) {
	v, ok := e[key]
	return v, ok
}

func (e estimates) Peak(string) (float64, bool) { return 0, false }

func (e estimates) Longest(prefix string) (float64, bool) {
	longest, known := 0.0, false
	for k, v := range e {
		if strings.HasPrefix(k, prefix) {
			longest, known = max(longest, v), true
		}
	}
	return longest, known
}

// peaked is a history whose keys also have peaks.
type peaked struct {
	estimates
	peaks map[string]float64
}

func (e peaked) Peak(key string) (float64, bool) {
	v, ok := e.peaks[key]
	return v, ok
}

func (e peaked) Longest(prefix string) (float64, bool) {
	longest, known := e.estimates.Longest(prefix)
	for k, v := range e.peaks {
		if strings.HasPrefix(k, prefix) {
			longest, known = max(longest, v), true
		}
	}
	return longest, known
}

// fromSamples is the history one run's measurements leave behind.
func fromSamples(m map[string]history.Sample) peaked {
	e := peaked{estimates: estimates{}, peaks: map[string]float64{}}
	for k, v := range m {
		e.estimates[k], e.peaks[k] = v.Seconds, v.Peak
	}
	return e
}

// secs is a whole number of seconds as a duration.
func secs(v float64) time.Duration { return time.Duration(v * float64(time.Second)) }

func atomic(id, suite string) run.Spec {
	s := run.Spec{Argv: []string{id}}
	s.ID, s.Suite = id, suite
	return s
}

func split(group string, items ...string) suites.Split {
	return suites.Split{
		Group: group, Suite: group, Items: items,
		Chunk: func(id string, items []string) run.Spec {
			s := run.Spec{Argv: append([]string{"chunk"}, items...)}
			s.ID, s.Suite = id, group
			return s
		},
	}
}

func specByID(p Planned) map[string]run.Spec {
	m := map[string]run.Spec{}
	for _, s := range p.Specs {
		m[s.ID] = s
	}
	return m
}

func TestBuildEstimatesAtomicUnits(t *testing.T) {
	tests := []struct {
		name string
		est  estimates
		want map[string]float64
	}{
		{"measured units take their measurement", estimates{UnitKey("a"): 3, UnitKey("b"): 7}, map[string]float64{"a": 3, "b": 7}},
		{"an unmeasured unit is as long as the longest measured one", estimates{UnitKey("a"): 3}, map[string]float64{"a": 3, "b": 3}},
		{"nothing measured is UnknownUnit", estimates{}, map[string]float64{"a": UnknownUnit, "b": UnknownUnit}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			p, err := Build(tt.est, []suites.Units{{Atomic: []run.Spec{atomic("a", "s"), atomic("b", "s")}}}, 4, false)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			got := map[string]float64{}
			for id, s := range specByID(p) {
				got[id] = s.Est
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("estimates = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestBuildChunksASplitAndBuildsEachChunksSpec(t *testing.T) {
	// Arrange: four 2s items, a 0.1s overhead, two slots.
	est := estimates{OverheadKey("ert"): 0.1}
	for _, it := range []string{"a", "b", "c", "d"} {
		est[ItemKey("ert", it)] = 2
	}

	// Act
	p, err := Build(est, []suites.Units{{Splits: []suites.Split{split("ert", "a", "b", "c", "d")}}}, 2, false)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if p.Chunks["ert"] != 2 {
		t.Fatalf("chunks = %v, want ert=2", p.Chunks)
	}
	var all []string
	for _, s := range p.Specs {
		if s.Est != 4.1 {
			t.Errorf("%s estimate = %v, want 4.1", s.ID, s.Est)
		}
		all = append(all, s.Argv[1:]...)
	}
	sort.Strings(all)
	if want := []string{"a", "b", "c", "d"}; !reflect.DeepEqual(all, want) {
		t.Fatalf("items across chunks = %v, want each exactly once", all)
	}
}

func TestBuildGivesEachChunkItsSplitsWidth(t *testing.T) {
	// Arrange
	sp := split("it", "a", "b")
	sp.Slots = 2

	// Act
	p, err := Build(estimates{}, []suites.Units{{Splits: []suites.Split{sp}}}, 4, false)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, s := range p.Specs {
		if s.Width() != 2 {
			t.Fatalf("%s is %d wide, want the split's 2", s.ID, s.Width())
		}
	}
}

func TestBuildCapsAProvenProcessOverhead(t *testing.T) {
	// Arrange: history attributed a large queueing gap to startup, but this
	// split's provider knows its already-built process starts within one second.
	sp := split("go", "a", "b", "c", "d")
	sp.OverheadCap = 1
	est := estimates{OverheadKey("go"): 100}
	for _, item := range sp.Items {
		est[ItemKey("go", item)] = 10
	}

	// Act
	p, err := Build(est, []suites.Units{{Splits: []suites.Split{sp}}}, 4, false)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got := p.Chunks["go"]; got != 4 {
		t.Fatalf("chunks = %d, want 4 after the one-second overhead cap", got)
	}
	for _, spec := range p.Specs {
		if spec.Est != 11 {
			t.Fatalf("%s estimate = %v, want one 10s item + 1s startup", spec.ID, spec.Est)
		}
	}
}

func TestBuildEstimatesAnUnmeasuredItemAtItsGroupsMean(t *testing.T) {
	// Arrange: two measured items average 3s; "new" is unmeasured; one slot
	// keeps everything in one chunk so its estimate is the sum.
	est := estimates{ItemKey("g", "a"): 2, ItemKey("g", "b"): 4, OverheadKey("g"): 0}

	// Act
	p, err := Build(est, []suites.Units{{Splits: []suites.Split{split("g", "a", "b", "new")}}}, 1, false)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got := p.Specs[0].Est; got != 9 {
		t.Fatalf("chunk estimate = %v, want 2+4+3", got)
	}
}

func TestBuildEstimatesAnUnmeasuredGroupAtItsSuitesMean(t *testing.T) {
	// Arrange: package "old" of suite "s" is measured at 0.5s a test; package
	// "new" of the same suite was never run.
	est := estimates{ItemKey("s:old", "a"): 0.5, OverheadKey("s:new"): 0}
	old, fresh := split("s:old", "a"), split("s:new", "x", "y")
	old.Suite, fresh.Suite = "s", "s"

	// Act
	p, err := Build(est, []suites.Units{{Splits: []suites.Split{old, fresh}}}, 1, false)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got := specByID(p)["s:new#00"].Est; got != 1.0 {
		t.Fatalf("new package estimate = %v, want 2 x the suite's 0.5s", got)
	}
}

func TestBuildWithNothingMeasuredUsesTheUnknownItemAndOverhead(t *testing.T) {
	// Act
	p, err := Build(estimates{}, []suites.Units{{Splits: []suites.Split{split("g", "a", "b")}}}, 1, false)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got, want := p.Specs[0].Est, 2*UnknownItem+UnknownOverhead; got != want {
		t.Fatalf("chunk estimate = %v, want %v", got, want)
	}
}

func TestChunkOf(t *testing.T) {
	tests := []struct {
		id        string
		wantGroup string
		wantIdx   int
		wantOK    bool
	}{
		{"ert#03", "ert", 3, true},
		{"daemon:internal/x", "", 0, false},
		{"weird#x", "", 0, false},
	}
	for _, tt := range tests {
		t.Run(tt.id, func(t *testing.T) {
			// Act
			g, i, ok := chunkOf(tt.id)

			// Assert
			if g != tt.wantGroup || i != tt.wantIdx || ok != tt.wantOK {
				t.Fatalf("chunkOf = %q, %d, %v", g, i, ok)
			}
		})
	}
}

func result(id string, outcome run.Outcome, wall float64, items map[string]float64) run.Result {
	s := run.Spec{}
	s.ID = id
	start := time.Unix(0, 0)
	return run.Result{Spec: s, Outcome: outcome, Start: start, End: start.Add(time.Duration(wall * float64(time.Second))), Items: items}
}

func TestMeasurements(t *testing.T) {
	// Arrange
	results := []run.Result{
		result("daemon:x", run.Passed, 5, nil),
		result("ert#00", run.Passed, 10, map[string]float64{"a.el": 3, "b.el": 5}),
		result("ert#01", run.Passed, 6, map[string]float64{"c.el": 2}),
		result("broken", run.Failed, 99, nil),
		result("ert#02", run.Failed, 99, map[string]float64{"d.el": 1}),
	}

	// Act
	got := Measurements(results)

	// Assert
	want := map[string]history.Sample{
		UnitKey("daemon:x"):    {Seconds: 5, Peak: 5},
		ItemKey("ert", "a.el"): {Seconds: 3, Peak: 3},
		ItemKey("ert", "b.el"): {Seconds: 5, Peak: 5},
		ItemKey("ert", "c.el"): {Seconds: 2, Peak: 2},
		// (10-8 + 6-2) / 2 chunks, and the larger of the two
		OverheadKey("ert"): {Seconds: 3, Peak: 4},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Measurements = %v, want %v", got, want)
	}
}

func TestMeasurementsNeverRecordANegativeOverhead(t *testing.T) {
	// Arrange: the items claim more than the chunk's wall (clock skew).
	results := []run.Result{result("g#00", run.Passed, 1, map[string]float64{"x": 2})}

	// Act
	got := Measurements(results)

	// Assert
	if got[OverheadKey("g")] != (history.Sample{}) {
		t.Fatalf("overhead = %+v, want 0", got[OverheadKey("g")])
	}
}

func TestMeasurementsAndEstimatesShareTheirKeys(t *testing.T) {
	// Arrange: what one run measures must be exactly what the next estimates.
	results := []run.Result{
		result("u", run.Passed, 7, nil),
		result("g#00", run.Passed, 5, map[string]float64{"i": 4}),
	}
	est := fromSamples(Measurements(results))

	// Act
	p, err := Build(est, []suites.Units{{Atomic: []run.Spec{atomic("u", "s")}, Splits: []suites.Split{split("g", "i")}}}, 4, false)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	got := specByID(p)
	if got["u"].Est != 7 || got["g#00"].Est != 5 {
		t.Fatalf("estimates = u:%v g#00:%v, want the measurements back", got["u"].Est, got["g#00"].Est)
	}
	if got["u"].Expected != secs(7) || got["g#00"].Expected != secs(5) {
		t.Fatalf("expected = u:%v g#00:%v, want the measured peaks back", got["u"].Expected, got["g#00"].Expected)
	}
}

func TestBuildBoundsEveryUnitAtBoundFactorTimesItsExpectedTime(t *testing.T) {
	// Arrange: a's peak is measured; b's is not; the chunk's overhead and items are.
	est := peaked{
		estimates: estimates{UnitKey("a"): 3, UnitKey("b"): 2, OverheadKey("g"): 1, ItemKey("g", "i"): 2, ItemKey("g", "j"): 3},
		peaks:     map[string]float64{UnitKey("a"): 5, OverheadKey("g"): 2, ItemKey("g", "i"): 4, ItemKey("g", "j"): 6},
	}
	tests := []struct {
		name         string
		id           string
		wantExpected time.Duration
	}{
		{"a measured unit is expected to take its peak", "a", secs(5)},
		{"an unmeasured unit is expected to take the longest unit's time", "b", secs(5)},
		{"a chunk is expected to take its overhead peak plus its items' peaks", "g#00", secs(2 + 4 + 6)},
	}
	units := []suites.Units{{Atomic: []run.Spec{atomic("a", "s"), atomic("b", "s")}, Splits: []suites.Split{split("g", "i", "j")}}}

	// Act
	p, err := Build(est, units, 1, false)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	got := specByID(p)
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			s := got[tt.id]
			if s.Expected != tt.wantExpected || s.Bound != run.BoundFactor*tt.wantExpected {
				t.Fatalf("%s: expected %v, bound %v; want %v, %v", tt.id, s.Expected, s.Bound, tt.wantExpected, run.BoundFactor*tt.wantExpected)
			}
		})
	}
}

func TestBuildBoundsAChunkWithAnUnmeasuredItemAsUnmeasured(t *testing.T) {
	// Arrange: the chunk's new item has no peak; the longest unit takes 40s.
	est := peaked{
		estimates: estimates{UnitKey("long"): 40, OverheadKey("g"): 1, ItemKey("g", "i"): 2},
		peaks:     map[string]float64{OverheadKey("g"): 2, ItemKey("g", "i"): 4},
	}

	// Act
	p, err := Build(est, []suites.Units{{Splits: []suites.Split{split("g", "i", "new")}}}, 1, false)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if s := specByID(p)["g#00"]; s.Expected != secs(40) {
		t.Fatalf("expected = %v, want the longest unit's 40s", s.Expected)
	}
}

func TestBuildBoundsAnUnmeasuredUnitByItsOwnEstimateWhenLonger(t *testing.T) {
	// Arrange: the chunk's estimate (overhead and items) passes every unit's.
	est := estimates{UnitKey("short"): 1, OverheadKey("g"): 5, ItemKey("g", "i"): 30}

	// Act
	p, err := Build(est, []suites.Units{{Splits: []suites.Split{split("g", "i")}}}, 1, false)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if s := specByID(p)["g#00"]; s.Expected != secs(35) {
		t.Fatalf("expected = %v, want its own 35s estimate", s.Expected)
	}
}

func TestBuildBoundsOnAHostThatMeasuredNothingByUnknownUnit(t *testing.T) {
	// Act
	p, err := Build(estimates{}, []suites.Units{{Atomic: []run.Spec{atomic("a", "s")}}}, 1, false)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if s := specByID(p)["a"]; s.Expected != secs(UnknownUnit) {
		t.Fatalf("expected = %v, want UnknownUnit", s.Expected)
	}
}

func TestBuildRemeasureBoundsAMeasuredUnitAsUnmeasured(t *testing.T) {
	// Arrange: a's peak is 1s; the longest unit takes 40s.
	est := peaked{estimates: estimates{UnitKey("a"): 1, UnitKey("long"): 40}, peaks: map[string]float64{UnitKey("a"): 1}}

	// Act
	p, err := Build(est, []suites.Units{{Atomic: []run.Spec{atomic("a", "s")}}}, 1, true)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if s := specByID(p)["a"]; s.Expected != secs(40) {
		t.Fatalf("expected = %v, want the longest unit's 40s, not a's own peak", s.Expected)
	}
}
