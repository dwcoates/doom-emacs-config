package harness

import (
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/deployconfig"
)

func TestAFixtureRepositoryDeploysItsHooksAlone(t *testing.T) {
	// Act
	repo := NewRepo(t)

	// Assert
	cfg, err := deployconfig.OnDisk{}.Load(repo.Dir)
	if err != nil {
		t.Fatalf("Load = %v, want the fixture's config", err)
	}
	if cfg.GetPreDeploy().GetLocalMerge() != nil || cfg.GetDeploy() != nil || len(cfg.GetPostDeploy()) != 0 {
		t.Fatalf("config = %v, want hooks alone", cfg)
	}
}

func TestAgentReplDeployStatesAgentReplsPipeline(t *testing.T) {
	// Arrange
	repo := NewRepo(t)

	// Act
	repo.WriteDeployConfig(AgentReplDeploy)

	// Assert
	cfg, err := deployconfig.OnDisk{}.Load(repo.Dir)
	if err != nil {
		t.Fatalf("Load = %v, want agent-repl's config", err)
	}
	if cfg.GetPreDeploy().GetLocalMerge().GetVersionTag() == nil || cfg.GetDeploy().GetRunsFor().GetMain() == nil ||
		cfg.GetDeploy().GetHook().GetScript().GetPath() != DeployLandingScript {
		t.Fatalf("config = %v, want a tagged local merge and the deploy script for main", cfg)
	}
}

func TestTheFrontDoorIsReadOffTheRealDeployScript(t *testing.T) {
	// Arrange
	script, err := os.ReadFile(filepath.Join(RepoRoot(t), DeployLandingScript))
	if err != nil {
		t.Fatalf("read the deploy script: %v", err)
	}

	// Act
	door := frontDoorOf(t, string(script))

	// Assert
	if door != "modules/app/agent-repl/skills/agent-repl-workspaces/agent-repld" {
		t.Fatalf("front door = %q, want agent-repl's own", door)
	}
}

func TestTheDeployScriptCallsTheDaemonRunningItsDeploy(t *testing.T) {
	tests := []struct {
		name string
		env  []string
		want string
	}{
		{name: "a hold's release names nothing", want: "runtime roll-out-landing"},
		{name: "a deploy names its ledger and its daemon",
			env:  []string{"AGENT_REPL_DEPLOY_LEDGER=ledger-1", "AGENT_REPL_DEPLOY_DAEMON=127.0.0.1:4242"},
			want: "runtime roll-out-landing -deploy ledger-1 -daemon 127.0.0.1:4242"},
		{name: "a deploy run by a daemon that names no address", env: []string{"AGENT_REPL_DEPLOY_LEDGER=ledger-1"},
			want: "runtime roll-out-landing -deploy ledger-1"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the real script in a repository whose front door
			// prints what it was asked.
			root := t.TempDir()
			script, err := os.ReadFile(filepath.Join(RepoRoot(t), DeployLandingScript))
			if err != nil {
				t.Fatalf("read the deploy script: %v", err)
			}
			writeExecutable(t, filepath.Join(root, DeployLandingScript), string(script))
			writeExecutable(t, filepath.Join(root, frontDoorOf(t, string(script))), "#!/bin/sh\necho \"$@\"\n")
			cmd := exec.Command("bash", filepath.Join(root, DeployLandingScript))
			cmd.Dir = root
			cmd.Env = append([]string{"PATH=" + os.Getenv("PATH")}, tt.env...)

			// Act
			out, err := cmd.Output()

			// Assert
			if err != nil || strings.TrimSpace(string(out)) != tt.want {
				t.Fatalf("the script asked %q (%v), want %q", out, err, tt.want)
			}
		})
	}
}

func TestRemoveDeployConfigLeavesNoDeploy(t *testing.T) {
	// Arrange
	repo := NewRepo(t)

	// Act
	repo.RemoveDeployConfig()

	// Assert
	var missing *deployconfig.MissingError
	if _, err := (deployconfig.OnDisk{}).Load(repo.Dir); !errors.As(err, &missing) {
		t.Fatalf("Load = %v, want the config missing", err)
	}
}
