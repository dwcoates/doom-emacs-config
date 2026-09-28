package health

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/wsm"
)

func TestDeployFailureReadsBackWhatItRecorded(t *testing.T) {
	tests := []struct {
		name    string
		failure DeployFailure
	}{
		{
			name:    "a failed build",
			failure: DeployFailure{Step: DeployStepBuild, BuildStep: "proto", Detail: "protoc: 1 error", Log: "/s/build.log"},
		},
		{
			name:    "a failed install",
			failure: DeployFailure{Step: DeployStepInstall, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_WEBAPP, Detail: "rename: busy"},
		},
		{
			name:    "a failed service restart",
			failure: DeployFailure{Step: DeployStepRestartServices, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE, Detail: "socket never came"},
		},
		{
			name:    "a failed install that was rolled back",
			failure: DeployFailure{Step: DeployStepInstall, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON, Detail: "EACCES", Rollback: RollbackRestored},
		},
		{
			name:    "a failed rollback",
			failure: DeployFailure{Step: DeployStepRollback, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_SIDECAR, Detail: "kickstart refused"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			fault := wsm.Fault{Kind: KindDeployFailed, Evidence: tt.failure.Evidence()}

			// Act.
			got := DeployFailureOf(fault)

			// Assert.
			if got != tt.failure {
				t.Fatalf("DeployFailureOf = %+v, want %+v", got, tt.failure)
			}
		})
	}
}

func TestDeployFailedDetailNamesTheStepAndTheLastLine(t *testing.T) {
	tests := []struct {
		name    string
		failure DeployFailure
		want    string
	}{
		{
			name:    "a build names its step and the last line of its output",
			failure: DeployFailure{Step: DeployStepBuild, BuildStep: "webapp", Detail: "vite\nerror TS2322: nope\n\n"},
			want:    "build webapp: error TS2322: nope",
		},
		{
			name:    "an install names the component in words",
			failure: DeployFailure{Step: DeployStepInstall, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_SIDECAR, Detail: "permission denied"},
			want:    "install sidecar: permission denied",
		},
		{
			name:    "a service restart spells its step with a space",
			failure: DeployFailure{Step: DeployStepRestartServices, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE, Detail: "exit 5"},
			want:    "restart services store: exit 5",
		},
		{
			name:    "an install that was rolled back says so",
			failure: DeployFailure{Step: DeployStepInstall, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON, Detail: "EACCES", Rollback: RollbackRestored},
			want:    "install daemon, rolled back: EACCES",
		},
		{
			name:    "a service restart whose rollback failed says so",
			failure: DeployFailure{Step: DeployStepRestartServices, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE, Detail: "exit 5", Rollback: RollbackIncomplete},
			want:    "restart services store, rollback failed: exit 5",
		},
		{
			name:    "a failed rollback names the component it could not restore",
			failure: DeployFailure{Step: DeployStepRollback, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON, Detail: "restore daemon: EROFS"},
			want:    "rollback daemon: restore daemon: EROFS",
		},
		{
			name:    "a build whose one step is the build itself says it once",
			failure: DeployFailure{Step: DeployStepBuild, BuildStep: "build", Detail: "refused"},
			want:    "build: refused",
		},
		{
			name:    "a failure with no account is its step alone",
			failure: DeployFailure{Step: DeployStepBuild, BuildStep: "daemon"},
			want:    "build daemon",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			fault := wsm.Fault{Kind: KindDeployFailed, Detail: "prose", Evidence: tt.failure.Evidence()}

			// Act.
			got := DeployFailedDetail(fault)

			// Assert.
			if got != tt.want {
				t.Fatalf("DeployFailedDetail = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestDeployFailedDetailOfARecordWithNoEvidenceIsItsProse(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{Kind: KindDeployFailed, Detail: "the deploy failed"}

	// Act.
	got := DeployFailedDetail(fault)

	// Assert.
	if got != "the deploy failed" {
		t.Fatalf("DeployFailedDetail = %q, want the record's own prose", got)
	}
}

func TestRollbackClauseSaysWhatBecameOfTheInstall(t *testing.T) {
	tests := []struct {
		name     string
		rollback string
		want     string
	}{
		{"nothing was installed", RollbackNone, "nothing was installed"},
		{"the rollback restored the previous build", RollbackRestored, "it was rolled back to the previous build"},
		{"the rollback did not restore it", RollbackIncomplete, "its rollback did NOT restore the previous build"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the rollback state is the input.

			// Act.
			got := RollbackClause(tt.rollback)

			// Assert.
			if got != tt.want {
				t.Fatalf("RollbackClause(%q) = %q, want %q", tt.rollback, got, tt.want)
			}
		})
	}
}

func TestDeployFailedOverlaySaysWhatFailed(t *testing.T) {
	tests := []struct {
		name    string
		failure DeployFailure
		want    DeployOverlay
	}{
		{
			name:    "a build names its build step and its archived log",
			failure: DeployFailure{Step: DeployStepBuild, BuildStep: "webapp", Detail: "tsc\nerror TS2322", Log: "/s/build.log"},
			want:    DeployOverlay{Step: "build", Component: "webapp", Rollback: "nothing was installed", Detail: "tsc\nerror TS2322", Log: "/s/build.log"},
		},
		{
			name:    "a restart names the component and the rollback in words",
			failure: DeployFailure{Step: DeployStepRestartServices, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_SIDECAR, Detail: "exit 5", Rollback: RollbackRestored},
			want:    DeployOverlay{Step: "restart services", Component: "sidecar", Rollback: "it was rolled back to the previous build", Detail: "exit 5"},
		},
		{
			name:    "a rollback's own fault says the rollback itself failed",
			failure: DeployFailure{Step: DeployStepRollback, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON, Detail: "EROFS"},
			want:    DeployOverlay{Step: "rollback", Component: "daemon", Rollback: "the rollback itself failed; the previous build was not restored", Detail: "EROFS"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			fault := wsm.Fault{Kind: KindDeployFailed, Evidence: tt.failure.Evidence()}

			// Act.
			got, ok := DeployFailedOverlay(fault)

			// Assert.
			if !ok || got != tt.want {
				t.Fatalf("DeployFailedOverlay = (%+v, %v), want (%+v, true)", got, ok, tt.want)
			}
		})
	}
}

func TestDeployFailedOverlayOfARecordWithNoStepIsNone(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{Kind: KindDeployFailed, Detail: "an older daemon's prose"}

	// Act.
	_, ok := DeployFailedOverlay(fault)

	// Assert.
	if ok {
		t.Fatal("DeployFailedOverlay answered an overlay for a record naming no step")
	}
}
