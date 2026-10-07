package harness

import (
	"os"
	"regexp"
	"strings"
	"testing"

	"claude-repld/internal/chessboard"
)

func TestHostNeutralCheckoutEnvClearsTheEngineDir(t *testing.T) {
	if !envHas(HostNeutralCheckoutEnv(), chessboard.EngineDirEnv+"=") {
		t.Fatalf("HostNeutralCheckoutEnv() = %v, want %s stated empty", HostNeutralCheckoutEnv(), chessboard.EngineDirEnv)
	}
}

func TestHostNeutralCheckoutEnvClearsTheMultiRepoRoot(t *testing.T) {
	if !envHas(HostNeutralCheckoutEnv(), chessboard.MultiRepoRootEnv+"=") {
		t.Fatalf("HostNeutralCheckoutEnv() = %v, want %s stated empty", HostNeutralCheckoutEnv(), chessboard.MultiRepoRootEnv)
	}
}

func TestStartDaemonAppliesHostNeutralCheckoutEnvAfterTheInheritedEnvironment(t *testing.T) {
	// The one call site must state the cleared checkout after os.Environ() and
	// before ExtraEnv, so it beats the host's value and a test can still set one.
	src, err := os.ReadFile("daemon.go")
	if err != nil {
		t.Fatal(err)
	}
	body := string(src)
	inherited := strings.Index(body, "env := append(os.Environ(),")
	neutral := regexp.MustCompile(`env = append\(env, HostNeutralCheckoutEnv\(\)\.\.\.\)`).FindStringIndex(body)
	extra := strings.Index(body, "opts.ExtraEnv...")
	if inherited < 0 || neutral == nil || extra < 0 || !(inherited < neutral[0] && neutral[0] < extra) {
		t.Fatalf("StartDaemon must append HostNeutralCheckoutEnv() between os.Environ() and opts.ExtraEnv (offsets %d, %v, %d)", inherited, neutral, extra)
	}
}
