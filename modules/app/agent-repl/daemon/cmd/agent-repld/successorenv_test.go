package main

import (
	"os"
	"path/filepath"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/userenv"
)

// checkoutWithManifest is an agent-repl module root (paths.Checkout) whose
// requirements manifest declares the hotspot setting.
func checkoutWithManifest(t *testing.T) string {
	t.Helper()
	checkout := t.TempDir()
	body := `{"requirements": [{"id": "h", "kind": "env", "name": "AGENT_REPL_PERSISTENT_WIFI_HOTSPOT"}]}`
	if err := os.WriteFile(filepath.Join(checkout, "requirements.json"), []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
	return checkout
}

func TestSuccessorEnvironRecordsEachSettingItChanges(t *testing.T) {
	// Arrange
	envFile := filepath.Join(t.TempDir(), "env")
	if err := os.WriteFile(envFile, []byte(`("AGENT_REPL_PERSISTENT_WIFI_HOTSPOT=phone")`), 0o644); err != nil {
		t.Fatal(err)
	}
	t.Setenv(userenv.EnvFile, envFile)
	t.Setenv("AGENT_REPL_PERSISTENT_WIFI_HOTSPOT", "")
	os.Unsetenv("AGENT_REPL_PERSISTENT_WIFI_HOTSPOT")
	log := dlog.NewTestLogger()

	// Act
	env, err := successorEnviron(checkoutWithManifest(t), log)()

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	found := false
	for _, kv := range env {
		found = found || kv == "AGENT_REPL_PERSISTENT_WIFI_HOTSPOT=phone"
	}
	if !found {
		t.Fatal("the spawned environment lacks the env file's hotspot")
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: opSuccessorEnv})
	if !ok || rec.Context["name"] != "AGENT_REPL_PERSISTENT_WIFI_HOTSPOT" || rec.Context["now_set"] != true {
		t.Fatalf("record = %+v (found %v), want the hotspot's change recorded", rec, ok)
	}
}

func TestSuccessorEnvironRecordsAnUnreadableEnvFileAtError(t *testing.T) {
	// Arrange
	gone := filepath.Join(t.TempDir(), "gone")
	t.Setenv(userenv.EnvFile, gone)
	log := dlog.NewTestLogger()

	// Act
	env, err := successorEnviron(checkoutWithManifest(t), log)()

	// Assert
	if err == nil || env != nil {
		t.Fatalf("successorEnviron = %v, %v; want the failure and no environment", env, err)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opSuccessorEnv})
	if !ok || rec.Context["env_file"] != gone || rec.Context["cause"] == "" {
		t.Fatalf("record = %+v (found %v), want the failure at ERROR naming the env file and its cause", rec, ok)
	}
}

func TestSuccessorEnvironReadsTheManifestOfTheRealModuleRoot(t *testing.T) {
	// Arrange: this package sits at <module root>/daemon/cmd/agent-repld, and
	// the module root is what paths.Checkout names.
	moduleRoot, err := filepath.Abs(filepath.Join("..", "..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	t.Setenv(userenv.EnvFile, filepath.Join(t.TempDir(), "env"))
	if err := os.WriteFile(os.Getenv(userenv.EnvFile), []byte(`()`), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err = successorEnviron(moduleRoot, dlog.NewTestLogger())()

	// Assert
	if err != nil {
		t.Fatalf("successorEnviron over the real module root = %v, want the manifest read", err)
	}
}
