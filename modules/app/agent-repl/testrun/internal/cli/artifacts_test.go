package cli

import (
	"reflect"
	"testing"
)

func TestRunArtifactsEnv(t *testing.T) {
	tests := []struct {
		name   string
		runLog string
		set    map[string]string
		want   map[string]string
	}{
		{name: "a run with no log leaves the harness defaults", runLog: "", want: nil},
		{
			name:   "a logged run puts both beside its log",
			runLog: "/tmp/agent-repl-test-runs/r1/test-all.log",
			want: map[string]string{
				IntegrationArtifactsEnv: "/tmp/agent-repl-test-runs/r1/artifacts/integration",
				E2EArtifactsEnv:         "/tmp/agent-repl-test-runs/r1/artifacts/e2e",
			},
		},
		{
			name:   "a directory the caller named is kept",
			runLog: "/tmp/agent-repl-test-runs/r1/test-all.log",
			set:    map[string]string{E2EArtifactsEnv: "/elsewhere"},
			want:   map[string]string{IntegrationArtifactsEnv: "/tmp/agent-repl-test-runs/r1/artifacts/integration"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := RunArtifactsEnv(tt.runLog, func(k string) string { return tt.set[k] })

			// Assert.
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("RunArtifactsEnv = %v, want %v", got, tt.want)
			}
		})
	}
}
