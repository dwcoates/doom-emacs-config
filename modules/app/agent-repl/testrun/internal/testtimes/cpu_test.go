package testtimes

import (
	"math"
	"strings"
	"testing"
	"time"
)

// at is T0 plus S seconds.
func at(s float64) time.Time {
	return time.Unix(1_800_000_000, 0).Add(time.Duration(s * float64(time.Second)))
}

func TestAttributeCPU(t *testing.T) {
	cases := []struct {
		name   string
		tests  []Test
		slices []CPUSlice
		unit   float64
		end    float64
		want   []float64
	}{
		{
			name:   "tests one after another each take the slice they ran in",
			tests:  []Test{{Name: "a", Start: at(0), Seconds: 1}, {Name: "b", Start: at(1), Seconds: 1}},
			slices: []CPUSlice{{From: at(0), To: at(1), CPU: 0.25}, {From: at(1), To: at(2), CPU: 0.75}},
			unit:   1, end: 2,
			want: []float64{0.25, 0.75},
		},
		{
			name:   "tests side by side share a slice evenly",
			tests:  []Test{{Name: "a", Start: at(0), Seconds: 1}, {Name: "b", Start: at(0), Seconds: 1}},
			slices: []CPUSlice{{From: at(0), To: at(1), CPU: 3}},
			unit:   3, end: 1,
			want: []float64{1.5, 1.5},
		},
		{
			name:   "a slice is split by how long each test ran in it",
			tests:  []Test{{Name: "a", Start: at(0), Seconds: 0.25}, {Name: "b", Start: at(0.25), Seconds: 0.75}},
			slices: []CPUSlice{{From: at(0), To: at(1), CPU: 2}},
			unit:   2, end: 1,
			want: []float64{0.5, 1.5},
		},
		{
			name:   "a slice no test ran in is nobody's",
			tests:  []Test{{Name: "a", Start: at(1), Seconds: 1}},
			slices: []CPUSlice{{From: at(0), To: at(1), CPU: 4}, {From: at(1), To: at(2), CPU: 1}},
			unit:   5, end: 2,
			want: []float64{1},
		},
		{
			name:   "what the unit used beyond the slices is one more slice to its end",
			tests:  []Test{{Name: "a", Start: at(0), Seconds: 1}, {Name: "b", Start: at(1), Seconds: 1}},
			slices: []CPUSlice{{From: at(0), To: at(1), CPU: 1}},
			unit:   3, end: 2,
			want: []float64{1, 2},
		},
		{
			name:  "tests that say no start split the unit's CPU by their seconds",
			tests: []Test{{Name: "a", Seconds: 1}, {Name: "b", Seconds: 3}},
			unit:  8, end: 4,
			want: []float64{2, 6},
		},
		{
			name:  "tests that say no start and took no time split it evenly",
			tests: []Test{{Name: "a"}, {Name: "b"}},
			unit:  1, end: 1,
			want: []float64{0.5, 0.5},
		},
		{
			name:   "a test that never ran gets none beside timed ones",
			tests:  []Test{{Name: "a", Start: at(0), Seconds: 1}, {Name: "skipped"}},
			slices: []CPUSlice{{From: at(0), To: at(1), CPU: 2}},
			unit:   2, end: 1,
			want: []float64{2, 0},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			tests := append([]Test(nil), tc.tests...)

			// Act
			err := AttributeCPU(tests, tc.slices, tc.unit, at(0), at(tc.end))

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			for i, w := range tc.want {
				if !tests[i].HasCPU || math.Abs(tests[i].CPU-w) > 1e-9 {
					t.Fatalf("test %s: CPU %v (has %v); want %v", tests[i].Name, tests[i].CPU, tests[i].HasCPU, w)
				}
			}
		})
	}
}

func TestAttributeCPURefusesTestsThatSayAStartForSomeOnly(t *testing.T) {
	// Arrange
	tests := []Test{{Name: "a", Start: at(0), Seconds: 1}, {Name: "b", Seconds: 1}}

	// Act
	err := AttributeCPU(tests, nil, 2, at(0), at(2))

	// Assert: refused, and no test was given anything.
	if err == nil || !strings.Contains(err.Error(), "1 of the unit's tests say when they started and 1 that took time do not") {
		t.Fatalf("AttributeCPU = %v; want the broken contract named", err)
	}
	for _, tt := range tests {
		if tt.HasCPU || tt.CPU != 0 {
			t.Fatalf("test %s was given CPU %v after a refusal", tt.Name, tt.CPU)
		}
	}
}

func TestAttributeCPUOfNoTestsIsNothing(t *testing.T) {
	if err := AttributeCPU(nil, []CPUSlice{{From: at(0), To: at(1), CPU: 1}}, 1, at(0), at(1)); err != nil {
		t.Fatalf("AttributeCPU(no tests) = %v; want nil", err)
	}
}

// A SLICE'S PROCESSES ARE SPLIT AS ITS CPU IS: by how long each test ran in
// it, or by seconds when no test says its start; the remainder slice carries
// CPU alone.
func TestAttributeCPUGivesEachTestItsShareOfTheProcessesStarted(t *testing.T) {
	cases := []struct {
		name   string
		tests  []Test
		slices []CPUSlice
		unit   float64
		end    float64
		want   []float64
	}{
		{
			name:   "tests one after another each take the processes of the slice they ran in",
			tests:  []Test{{Name: "a", Start: at(0), Seconds: 1}, {Name: "b", Start: at(1), Seconds: 1}},
			slices: []CPUSlice{{From: at(0), To: at(1), CPU: 1, Started: 3}, {From: at(1), To: at(2), CPU: 1, Started: 1}},
			unit:   2, end: 2,
			want: []float64{3, 1},
		},
		{
			name:   "tests side by side share a slice's processes evenly",
			tests:  []Test{{Name: "a", Start: at(0), Seconds: 1}, {Name: "b", Start: at(0), Seconds: 1}},
			slices: []CPUSlice{{From: at(0), To: at(1), Started: 3}},
			unit:   0, end: 1,
			want: []float64{1.5, 1.5},
		},
		{
			name:   "processes started where no test ran are nobody's",
			tests:  []Test{{Name: "a", Start: at(1), Seconds: 1}},
			slices: []CPUSlice{{From: at(0), To: at(1), Started: 4}, {From: at(1), To: at(2), Started: 1}},
			unit:   0, end: 2,
			want: []float64{1},
		},
		{
			name:   "the unit's CPU beyond the slices starts no process",
			tests:  []Test{{Name: "a", Start: at(0), Seconds: 2}},
			slices: []CPUSlice{{From: at(0), To: at(1), CPU: 1, Started: 2}},
			unit:   5, end: 2,
			want: []float64{2},
		},
		{
			name:   "tests that say no start split the processes by their seconds",
			tests:  []Test{{Name: "a", Seconds: 1}, {Name: "b", Seconds: 3}},
			slices: []CPUSlice{{From: at(0), To: at(4), CPU: 1, Started: 8}},
			unit:   1, end: 4,
			want: []float64{2, 6},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			tests := append([]Test(nil), tc.tests...)

			// Act
			err := AttributeCPU(tests, tc.slices, tc.unit, at(0), at(tc.end))

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			for i, w := range tc.want {
				if !tests[i].HasCPU || math.Abs(tests[i].Processes-w) > 1e-9 {
					t.Fatalf("test %s: processes %v (has %v); want %v", tests[i].Name, tests[i].Processes, tests[i].HasCPU, w)
				}
			}
		})
	}
}

func TestAttributeCPUGivesEachTestItsShareOfEachProgram(t *testing.T) {
	cases := []struct {
		name   string
		tests  []Test
		slices []CPUSlice
		unit   float64
		end    float64
		want   []map[string]float64
	}{
		{
			name:  "timed tests split each slice's programs by how long each ran in it",
			tests: []Test{{Name: "a", Start: at(0), Seconds: 1}, {Name: "b", Start: at(1), Seconds: 1}},
			slices: []CPUSlice{
				{From: at(0), To: at(1), CPU: 2, Programs: map[string]float64{ProgramDaemon: 1.5, ProgramTestBinary: 0.5}},
				{From: at(1), To: at(2), CPU: 1, Programs: map[string]float64{ProgramFakeShim: 1}},
			},
			unit: 3, end: 2,
			want: []map[string]float64{{ProgramDaemon: 1.5, ProgramTestBinary: 0.5}, {ProgramFakeShim: 1}},
		},
		{
			name:  "what the wait accounting holds beyond the slices is unsampled",
			tests: []Test{{Name: "a", Start: at(0), Seconds: 2}},
			slices: []CPUSlice{
				{From: at(0), To: at(1), CPU: 1, Programs: map[string]float64{ProgramDaemon: 1}},
			},
			unit: 1.5, end: 2,
			want: []map[string]float64{{ProgramDaemon: 1, ProgramUnsampled: 0.5}},
		},
		{
			name:  "untimed tests split the programs by each one's seconds",
			tests: []Test{{Name: "a", Seconds: 3}, {Name: "b", Seconds: 1}},
			slices: []CPUSlice{
				{From: at(0), To: at(1), CPU: 4, Programs: map[string]float64{ProgramShell: 4}},
			},
			unit: 4, end: 1,
			want: []map[string]float64{{ProgramShell: 3}, {ProgramShell: 1}},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := AttributeCPU(tc.tests, tc.slices, tc.unit, at(0), at(tc.end))

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			for i, w := range tc.want {
				got := tc.tests[i].Programs
				if len(got) != len(w) {
					t.Fatalf("test %s programs = %v; want %v", tc.tests[i].Name, got, w)
				}
				for prog, cpu := range w {
					if math.Abs(got[prog]-cpu) > 1e-9 {
						t.Fatalf("test %s programs = %v; want %v", tc.tests[i].Name, got, w)
					}
				}
			}
		})
	}
}

func TestUnitProgramsSumsEverySliceAndTheUnsampledRest(t *testing.T) {
	cases := []struct {
		name   string
		slices []CPUSlice
		unit   float64
		want   map[string]float64
	}{
		{
			name: "the slices and the rest",
			slices: []CPUSlice{
				{From: at(0), To: at(1), CPU: 1, Programs: map[string]float64{ProgramDaemon: 0.75, ProgramGit: 0.25}},
				{From: at(1), To: at(2), CPU: 1, Programs: map[string]float64{ProgramDaemon: 1}},
			},
			unit: 2.5,
			want: map[string]float64{ProgramDaemon: 1.75, ProgramGit: 0.25, ProgramUnsampled: 0.5},
		},
		{
			name: "no read is all unsampled",
			unit: 0.5,
			want: map[string]float64{ProgramUnsampled: 0.5},
		},
		{
			name: "slices that hold all the unit's CPU leave nothing unsampled",
			slices: []CPUSlice{
				{From: at(0), To: at(1), CPU: 2, Programs: map[string]float64{ProgramNode: 2}},
			},
			unit: 1.5,
			want: map[string]float64{ProgramNode: 2},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := UnitPrograms(tc.slices, tc.unit, at(0), at(3))

			// Assert
			if len(got) != len(tc.want) {
				t.Fatalf("UnitPrograms = %v; want %v", got, tc.want)
			}
			for prog, cpu := range tc.want {
				if math.Abs(got[prog]-cpu) > 1e-9 {
					t.Fatalf("UnitPrograms = %v; want %v", got, tc.want)
				}
			}
		})
	}
}
