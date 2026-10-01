package plan

import (
	"reflect"
	"sort"
	"testing"
	"time"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
)

type estimates map[string]float64

func (e estimates) Get(key string) (float64, bool) {
	v, ok := e[key]
	return v, ok
}

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
			p, err := Build(tt.est, []suites.Units{{Atomic: []run.Spec{atomic("a", "s"), atomic("b", "s")}}}, 4)

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
	p, err := Build(est, []suites.Units{{Splits: []suites.Split{split("ert", "a", "b", "c", "d")}}}, 2)

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

func TestBuildEstimatesAnUnmeasuredItemAtItsGroupsMean(t *testing.T) {
	// Arrange: two measured items average 3s; "new" is unmeasured; one slot
	// keeps everything in one chunk so its estimate is the sum.
	est := estimates{ItemKey("g", "a"): 2, ItemKey("g", "b"): 4, OverheadKey("g"): 0}

	// Act
	p, err := Build(est, []suites.Units{{Splits: []suites.Split{split("g", "a", "b", "new")}}}, 1)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got := p.Specs[0].Est; got != 9 {
		t.Fatalf("chunk estimate = %v, want 2+4+3", got)
	}
}

func TestBuildWithNothingMeasuredUsesTheUnknownItemAndOverhead(t *testing.T) {
	// Act
	p, err := Build(estimates{}, []suites.Units{{Splits: []suites.Split{split("g", "a", "b")}}}, 1)

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
	want := map[string]float64{
		UnitKey("daemon:x"):    5,
		ItemKey("ert", "a.el"): 3,
		ItemKey("ert", "b.el"): 5,
		ItemKey("ert", "c.el"): 2,
		// (10-8 + 6-2) / 2 chunks
		OverheadKey("ert"): 3,
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
	if got[OverheadKey("g")] != 0 {
		t.Fatalf("overhead = %v, want 0", got[OverheadKey("g")])
	}
}

func TestMeasurementsAndEstimatesShareTheirKeys(t *testing.T) {
	// Arrange: what one run measures must be exactly what the next estimates.
	results := []run.Result{
		result("u", run.Passed, 7, nil),
		result("g#00", run.Passed, 5, map[string]float64{"i": 4}),
	}
	est := estimates(Measurements(results))

	// Act
	p, err := Build(est, []suites.Units{{Atomic: []run.Spec{atomic("u", "s")}, Splits: []suites.Split{split("g", "i")}}}, 4)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	got := specByID(p)
	if got["u"].Est != 7 || got["g#00"].Est != 5 {
		t.Fatalf("estimates = u:%v g#00:%v, want the measurements back", got["u"].Est, got["g#00"].Est)
	}
}
