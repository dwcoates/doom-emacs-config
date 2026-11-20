package stack

import (
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

func TestAWorldGroupIsOneWorldPerName(t *testing.T) {
	// Arrange
	g := &worldGroups{worlds: map[string]*sharedWorld{}, opts: map[string]WorldOpts{}}
	opts := WorldOpts{DaemonOpts: harness.Opts{KeepaliveInterval: 2}}

	// Act
	first, err1 := g.group("keepalive", opts)
	again, err2 := g.group("keepalive", opts)
	other, err3 := g.group("zettel", WorldOpts{Zettel: true})

	// Assert
	if err1 != nil || err2 != nil || err3 != nil {
		t.Fatalf("group = %v, %v, %v; want no refusal", err1, err2, err3)
	}
	if first != again {
		t.Fatal("asking for one group twice answered two Worlds; want the same one")
	}
	if first == other {
		t.Fatal("two groups answered the same World; want one per name")
	}
}

func TestAWorldGroupRefusesAskingUnderOtherOptions(t *testing.T) {
	// Arrange
	g := &worldGroups{worlds: map[string]*sharedWorld{}, opts: map[string]WorldOpts{}}
	if _, err := g.group("keepalive", WorldOpts{DaemonOpts: harness.Opts{KeepaliveInterval: 2}}); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := g.group("keepalive", WorldOpts{DaemonOpts: harness.Opts{KeepaliveInterval: 3}})

	// Assert: refused, and the group keeps the options it was built with.
	if err == nil || !strings.Contains(err.Error(), `the group World "keepalive" was built with`) {
		t.Fatalf("group = %v; want the mismatched options refused", err)
	}
	if got := g.opts["keepalive"].DaemonOpts.KeepaliveInterval; got != 2 {
		t.Fatalf("the group's options became %v; want the first ask's kept", got)
	}
}
