package harness

import (
	"errors"
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

func TestTheDeployLandingScriptCallsTheDaemonWithItsDeploy(t *testing.T) {
	// Act
	body := deployLandingScript("/built/agent-repld")

	// Assert
	for _, want := range []string{"'/built/agent-repld' runtime roll-out-landing -deploy \"$AGENT_REPL_DEPLOY_LEDGER\"", "'/built/agent-repld' runtime roll-out-landing\n"} {
		if !strings.Contains(body, want) {
			t.Errorf("the deploy script reads %q, want %q", body, want)
		}
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
