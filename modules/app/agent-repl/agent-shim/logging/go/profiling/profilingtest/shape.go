// Package profilingtest holds what every Go runtime's profiling tests share.
package profilingtest

import "strings"

// SharedShapeProblems answers how SOURCE, a runtime's profiling.go, departs
// from the shape every Go runtime shares: it arms through profiling.Arm and
// words its events through profiling.Describe, both under RUNTIME's name, and
// never builds its own Profiler, runs its own watcher or switches on event
// kinds itself. None is the shared shape.
func SharedShapeProblems(source, runtime string) []string {
	var problems []string
	for _, want := range []string{`profiling.Arm("` + runtime + `"`, `profiling.Describe("` + runtime + `"`} {
		if !strings.Contains(source, want) {
			problems = append(problems, "does not call "+want)
		}
	}
	for _, banned := range []string{"profiling.New(", "profiling.Watch(", "case profiling."} {
		if strings.Contains(source, banned) {
			problems = append(problems, "hand-rolls "+banned+" instead of Arm and Describe")
		}
	}
	return problems
}
