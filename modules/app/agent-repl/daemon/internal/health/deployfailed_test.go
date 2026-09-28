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
