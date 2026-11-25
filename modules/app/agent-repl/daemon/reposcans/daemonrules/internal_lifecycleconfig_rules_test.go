package daemonrules

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalLifecycleconfigDir is the package these rules hold, under the daemon module's root.
const internalLifecycleconfigDir = "internal/lifecycleconfig"

// TestNoProductionCodeHandBuildsAHook pins that every production site builds a
// hook through PromptHook or ScriptHook, so one spelling of a hook stands.
func TestNoProductionCodeHandBuildsAHook(t *testing.T) {
	sourcescan.InPackage(t, internalLifecycleconfigDir)
	// Arrange
	daemon, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	var byHand []string

	// Act
	err = filepath.WalkDir(daemon, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") ||
			strings.HasSuffix(path, ".pb.go") || filepath.Dir(path) == filepath.Join(daemon, "internal", "lifecycleconfig") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(raw), "repoconfigv1.Hook_Prompt{") || strings.Contains(string(raw), "repoconfigv1.Hook_Script{") {
			byHand = append(byHand, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(byHand) != 0 {
		t.Fatalf("hooks built by hand in %v, want PromptHook and ScriptHook", byHand)
	}
}

func TestEveryScriptIsCheckedByCheckScript(t *testing.T) {
	sourcescan.InPackage(t, internalLifecycleconfigDir)
	// Arrange: a stage's script hook and a queue_drained script are held to
	// one set of rules, so the timeout rule is spelled once.
	raw, err := os.ReadFile("lifecycleconfig.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	spelled := strings.Count(string(raw), "GetTimeoutSeconds() < 1")

	// Assert
	if spelled != 1 {
		t.Fatalf("the script timeout rule is spelled %d times in lifecycleconfig.go, want once, in checkScript", spelled)
	}
}

func TestNoProductionCodeReadsTheTestScriptByHand(t *testing.T) {
	sourcescan.InPackage(t, internalLifecycleconfigDir)
	// Arrange
	daemon, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	var byHand []string

	// Act
	err = filepath.WalkDir(daemon, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") ||
			strings.HasSuffix(path, ".pb.go") || filepath.Dir(path) == filepath.Join(daemon, "internal", "lifecycleconfig") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(raw), "GetAgentReplSuites()") {
			byHand = append(byHand, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(byHand) != 0 {
		t.Fatalf("the test script read by hand in %v, want lifecycleconfig.TestScript", byHand)
	}
}

// TestNoProductionCodeRunsAScriptHookByItsOwnPath pins that every runner of a
// script hook builds its command with ScriptArgv, so a built-in default's
// absolute path and a config's relative one resolve one way.
func TestNoProductionCodeRunsAScriptHookByItsOwnPath(t *testing.T) {
	sourcescan.InPackage(t, internalLifecycleconfigDir)
	// Arrange
	daemon, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	var byHand []string

	// Act
	err = filepath.WalkDir(daemon, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || strings.HasSuffix(path, ".pb.go") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(raw), "filepath.Join(repoDir, script.GetPath())") || strings.Contains(string(raw), "script.GetPath())}") {
			byHand = append(byHand, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(byHand) != 0 {
		t.Fatalf("a script hook's command built by hand in %v, want ScriptArgv", byHand)
	}
}

// TestEveryWorkspaceScriptHookEnvironmentIsHookEnvs pins that no runner of a
// workspace's script hook spells the stage variable itself.
func TestEveryWorkspaceScriptHookEnvironmentIsHookEnvs(t *testing.T) {
	sourcescan.InPackage(t, internalLifecycleconfigDir)
	// Arrange
	daemon, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	var byHand []string

	// Act
	err = filepath.WalkDir(daemon, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || filepath.Base(filepath.Dir(path)) == "lifecycleconfig" {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(raw), "EnvCallingWorkspace + \"=\"") || strings.Contains(string(raw), "EnvSlackThread+\"=\"") {
			byHand = append(byHand, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(byHand) != 0 {
		t.Fatalf("a script hook's environment built by hand in %v, want HookEnv", byHand)
	}
}
