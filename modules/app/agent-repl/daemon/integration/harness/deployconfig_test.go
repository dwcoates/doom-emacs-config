package harness

import (
	"errors"
	"os"
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

func TestDeploysAsAgentReplStatesAgentReplsPipeline(t *testing.T) {
	// Arrange
	repo := NewRepo(t)

	// Act
	repo.DeploysAsAgentRepl()

	// Assert
	cfg, err := deployconfig.OnDisk{}.Load(repo.Dir)
	if err != nil {
		t.Fatalf("Load = %v, want agent-repl's config", err)
	}
	if cfg.GetPreDeploy().GetLocalMerge().GetVersionTag() == nil || cfg.GetDeploy().GetRunsFor().GetMain() == nil ||
		cfg.GetDeploy().GetHook().GetScript().GetPath() != DeployLandingScript {
		t.Fatalf("config = %v, want a tagged local merge and the deploy script for main", cfg)
	}
	script := filepath.Join(repo.Dir, DeployLandingScript)
	info, err := os.Stat(script)
	if err != nil || info.Mode()&0o111 == 0 {
		t.Fatalf("the deploy script %s = %v, %v; want an executable", script, info, err)
	}
	body, err := os.ReadFile(script)
	if err != nil || !strings.Contains(string(body), DaemonBinary(t)) || !strings.Contains(string(body), "runtime roll-out-landing -deploy \"$AGENT_REPL_DEPLOY_LEDGER\"") {
		t.Fatalf("the deploy script reads %q, want the daemon's roll-out-landing with the ledger", body)
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
