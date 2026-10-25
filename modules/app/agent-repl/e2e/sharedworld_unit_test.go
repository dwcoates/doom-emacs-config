package e2e

import (
	"os"
	"strings"
	"testing"

	"agent-repld/integration/harness"
)

func TestASharedWorldWhoseBuildFailedFailsEveryTestThatAsks(t *testing.T) {
	// Arrange
	var s sharedWorld
	build := func() *World { panic(harness.SuiteFatal{Message: "no store binary"}) }
	first, second := harness.NewRecordingTB(t, "TestFirst"), harness.NewRecordingTB(t, "TestSecond")

	// Act
	harness.Within(func() { s.viewFor(first, build) })
	harness.Within(func() { s.viewFor(second, build) })

	// Assert
	for _, r := range []*harness.RecordingTB{first, second} {
		if !strings.Contains(r.Failures(), "the shared World did not build: no store binary") {
			t.Errorf("%s failures = %q, want the build's failure", r.Name(), r.Failures())
		}
	}
}

func TestASharedWorldWhoseBuildLackedAPreconditionSkipsEveryTestThatAsks(t *testing.T) {
	// Arrange
	var s sharedWorld
	build := func() *World { panic(harness.SuiteSkip{Reason: "no node on PATH"}) }
	r := harness.NewRecordingTB(t, "TestFirst")

	// Act
	harness.Within(func() { s.viewFor(r, build) })

	// Assert
	if r.Failures() != "SKIP: e2e: the shared World did not build: no node on PATH" {
		t.Fatalf("failures = %q, want the build's skip", r.Failures())
	}
}

func TestASharedWorldIsBuiltOnceForEveryTestThatAsks(t *testing.T) {
	// Arrange
	var s sharedWorld
	builds := 0
	build := func() *World { builds++; panic(harness.SuiteFatal{Message: "stop after counting"}) }

	// Act
	for i := 0; i < 3; i++ {
		harness.Within(func() { s.viewFor(harness.NewRecordingTB(t, "TestAsker"), build) })
	}

	// Assert
	if builds != 1 {
		t.Fatalf("builds = %d, want 1", builds)
	}
}

func TestStoppingASharedStoreIsRefused(t *testing.T) {
	// Arrange
	s := &Store{t: t, shared: true}
	var got any

	// Act
	func() {
		defer func() { got = recover() }()
		s.Stop()
	}()

	// Assert
	if msg, _ := got.(string); !strings.Contains(msg, "Stop on the shared World's store") {
		t.Fatalf("recovered %v, want the shared store's Stop refused", got)
	}
}

func TestStoppingASharedSidecarIsRefused(t *testing.T) {
	// Arrange
	s := &Sidecar{t: t, shared: true}
	var got any

	// Act
	func() {
		defer func() { got = recover() }()
		s.Stop()
	}()

	// Assert
	if msg, _ := got.(string); !strings.Contains(msg, "Stop on the shared World's sidecar") {
		t.Fatalf("recovered %v, want the shared sidecar's Stop refused", got)
	}
}

func TestRestartingASharedServiceIsRefused(t *testing.T) {
	tests := []struct {
		name string
		act  func(r *harness.RecordingTB)
		want string
	}{
		{"the store", func(r *harness.RecordingTB) { (&Store{t: r, shared: true}).StartSameDB(r) }, "StartSameDB on the shared World's store"},
		{"the sidecar", func(r *harness.RecordingTB) { (&Sidecar{t: r, shared: true}).Restart(r) }, "Restart on the shared World's sidecar"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := harness.NewRecordingTB(t, "TestRestarter")

			// Act
			completed := harness.Within(func() { tt.act(r) })

			// Assert
			if completed || !strings.Contains(r.Failures(), tt.want) {
				t.Fatalf("completed=%v failures=%q, want %q", completed, r.Failures(), tt.want)
			}
		})
	}
}

// TestEveryWorldChecksItsServicesThroughOneHelper pins the extraction: an
// owned World, the shared World's views and an EmacsWorld all check that
// their services outlived the test through requireServicesOutlivedTheTest.
func TestEveryWorldChecksItsServicesThroughOneHelper(t *testing.T) {
	// Arrange
	sites := map[string]string{
		"world_test.go":       "t.Cleanup(func() { requireServicesOutlivedTheTest(t, store, sidecar, zettel) })",
		"sharedworld_test.go": "t.Cleanup(func() { requireServicesOutlivedTheTest(t, s.world.Store, s.world.Sidecar, s.world.Zettel) })",
		"emacs_e2e_test.go":   "t.Cleanup(func() { requireServicesOutlivedTheTest(t, store, sidecar, zettel) })",
	}

	for file, call := range sites {
		// Act
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}

		// Assert
		if !strings.Contains(string(body), call) {
			t.Errorf("%s does not check its services through requireServicesOutlivedTheTest", file)
		}
		// The helper itself is the one place that spells the check.
		want := map[string]int{"world_test.go": 1, "sharedworld_test.go": 0, "emacs_e2e_test.go": 0}[file]
		if got := strings.Count(string(body), "the store exited before test cleanup"); got != want {
			t.Errorf("%s spells the service check %d times, want %d: only the helper spells it", file, got, want)
		}
	}
}
