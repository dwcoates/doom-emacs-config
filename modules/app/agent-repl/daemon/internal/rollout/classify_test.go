package rollout

import (
	"reflect"
	"testing"
)

func TestClassifyMapsEachPrefixOntoItsSubsystem(t *testing.T) {
	// Arrange
	cases := []struct {
		name string
		path string
		want Subsystem
	}{
		{"the daemon", ModuleRoot + "daemon/internal/rollout/api.go", SubsystemDaemon},
		{"the shim", ModuleRoot + "agent-shim/claude/shim/src/main.ts", SubsystemShim},
		{"the webapp", ModuleRoot + "webapp/src/App.tsx", SubsystemWebapp},
		{"the module's lisp directory", ModuleRoot + "lisp/services.el", SubsystemElisp},
		{"a top-level module elisp file", ModuleRoot + "config.el", SubsystemElisp},
		{"the store", ModuleRoot + "agent-shim/shim-store/main.go", SubsystemStore},
		{"the sidecar", ModuleRoot + "agent-shim/claude/shim-sidecar/main.go", SubsystemSidecar},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Classify([]string{tc.path})

			// Assert
			if len(got) != 1 || got[0] != tc.want {
				t.Fatalf("Classify(%q) = %v, want [%s]", tc.path, got, tc.want)
			}
		})
	}
}

func TestAProtoChangeClassifiesAsEveryConsumerOfTheBindings(t *testing.T) {
	// Arrange
	paths := []string{ModuleRoot + "proto/src/agentrepl/v1/endpoint_watch_daemon.proto"}

	// Act
	got := Classify(paths)

	// Assert
	want := []Subsystem{SubsystemDaemon, SubsystemProto, SubsystemShim, SubsystemWebapp}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Classify = %v, want %v", got, want)
	}
}

func TestTheSidecarPrefixIsNotSwallowedByTheShimPrefix(t *testing.T) {
	// Arrange
	paths := []string{ModuleRoot + "agent-shim/claude/shim-sidecar/cursor.go"}

	// Act
	got := Classify(paths)

	// Assert
	if len(got) != 1 || got[0] != SubsystemSidecar {
		t.Fatalf("Classify = %v, want [sidecar]: shim/ and shim-sidecar/ share a head", got)
	}
}

func TestClassifyIgnoresAPathOutsideTheModule(t *testing.T) {
	// Arrange
	paths := []string{"init.el", "modules/lang/personal-cc/config.el"}

	// Act
	got := Classify(paths)

	// Assert
	if len(got) != 0 {
		t.Fatalf("Classify = %v, want nothing for paths outside the module", got)
	}
}

func TestClassifyDeduplicatesRepeatedSubsystems(t *testing.T) {
	// Arrange
	paths := []string{
		ModuleRoot + "daemon/internal/a.go",
		ModuleRoot + "daemon/internal/b.go",
	}

	// Act
	got := Classify(paths)

	// Assert
	if len(got) != 1 || got[0] != SubsystemDaemon {
		t.Fatalf("Classify = %v, want the daemon named once", got)
	}
}

func TestUnhandledNamesTheStoreAndTheSidecar(t *testing.T) {
	// Arrange
	subsystems := []Subsystem{SubsystemDaemon, SubsystemSidecar, SubsystemStore}

	// Act
	got := Unhandled(subsystems)

	// Assert
	want := []Subsystem{SubsystemSidecar, SubsystemStore}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Unhandled = %v, want %v", got, want)
	}
}

func TestUnhandledNamesNothingForAHandledClassification(t *testing.T) {
	// Arrange
	subsystems := []Subsystem{SubsystemDaemon, SubsystemWebapp, SubsystemElisp}

	// Act
	got := Unhandled(subsystems)

	// Assert
	if len(got) != 0 {
		t.Fatalf("Unhandled = %v, want nothing", got)
	}
}
