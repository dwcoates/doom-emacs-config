// Package scenario runs an e2e scenario: one top-level test whose named steps
// run in order.
//
// The gate runs every e2e test side by side under the host's CPU cap, and a
// test that passes alone fails there waiting on a CPU inside its bound. So the
// worlds are few: scenarios run in parallel with each other, and the steps of
// one run one at a time, each over the world it builds or takes, torn down
// before the next step starts (e2e/scenarios_test.go lists them).
//
// A STEP IS A FUNCTION NAMED testXxx, and its case is named Xxx: the former
// top-level TestXxx is the step Scenario/Xxx, so the name a red run prints is
// the name the source declares, and nothing hand-spelled can drift from it.
// A step reports pass or fail on its own; a failed step ends only itself and
// the steps after it still run. The merge queue reruns a failed step by its
// path (daemon/internal/merge/failingtests.go).
package scenario

import (
	"fmt"
	"reflect"
	"runtime"
	"strings"
	"testing"
	"unicode"
)

// Runner is the part of *testing.T a scenario drives.
type Runner interface {
	Helper()
	Parallel()
	Run(name string, f func(*testing.T)) bool
}

// Run runs steps as r's named cases, in order, beside the other scenarios.
func Run(r Runner, steps []func(*testing.T)) {
	r.Helper()
	if len(steps) == 0 {
		panic("e2e: a scenario with no steps runs nothing")
	}
	r.Parallel()
	for _, step := range steps {
		r.Run(StepName(step), step)
	}
}

// StepName is the case name of STEP, its function's name without the "test"
// prefix. A step that is not a package-level function named testXxx is a bug
// in the scenario's list, and panics.
func StepName(step func(*testing.T)) string {
	full := runtime.FuncForPC(reflect.ValueOf(step).Pointer()).Name()
	name := full[strings.LastIndex(full, ".")+1:]
	rest, ok := strings.CutPrefix(name, StepPrefix)
	if !ok || rest == "" || !unicode.IsUpper(rune(rest[0])) {
		panic(fmt.Sprintf("e2e: scenario step %s is not a function named %sXxx", full, StepPrefix))
	}
	return rest
}

// StepPrefix starts every step function's name.
const StepPrefix = "test"
