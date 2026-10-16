package profilingtest

import (
	"reflect"
	"testing"
)

func TestSharedShapeProblems(t *testing.T) {
	shared := "profiling.Arm(\"store\", l, f, g, h)\nprofiling.Describe(\"store\", e)"
	tests := []struct {
		name   string
		source string
		want   []string
	}{
		{name: "the shared shape", source: shared, want: nil},
		{name: "no Arm", source: `profiling.Describe("store", e)`, want: []string{`does not call profiling.Arm("store"`}},
		{name: "another runtime's name", source: "profiling.Arm(\"daemon\", l)\nprofiling.Describe(\"store\", e)", want: []string{`does not call profiling.Arm("store"`}},
		{name: "its own Profiler", source: shared + "\nprofiling.New(opts)", want: []string{"hand-rolls profiling.New( instead of Arm and Describe"}},
		{name: "its own watcher", source: shared + "\nprofiling.Watch(p, done)", want: []string{"hand-rolls profiling.Watch( instead of Arm and Describe"}},
		{name: "its own wording", source: shared + "\ncase profiling.Started:", want: []string{"hand-rolls case profiling. instead of Arm and Describe"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := SharedShapeProblems(tt.source, "store")

			// Assert
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("SharedShapeProblems = %q, want %q", got, tt.want)
			}
		})
	}
}
