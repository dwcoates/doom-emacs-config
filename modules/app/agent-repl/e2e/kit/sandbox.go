package kit

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

// SandboxScriptRel is the sandbox entry point, relative to the repo root.
const SandboxScriptRel = "modules/app/agent-repl/e2e/sandbox/bin/e2e-sandbox.sh"

// RunHostPreflight runs the sandbox preflight under a deadline and returns the
// skip reason, or "" when the sandbox image is usable.
//
// It is a free function rather than an inlined closure so a test can drive it
// without racing the package-level sync.Once that guards the real probe.
func RunHostPreflight(script string, bound time.Duration) string {
	if _, err := os.Stat(script); err != nil {
		return fmt.Sprintf("%s not found: %v", SandboxScriptRel, err)
	}

	// wall-clock: the preflight asks an EXTERNAL engine (Docker) to answer,
	// and a wedged engine is not this binary's processes being starved; the
	// bound is the engine's, in wall time.
	ctx, cancel := context.WithTimeout(context.Background(), bound)
	defer cancel()
	return HostPreflightUntil(ctx, script, bound)
}

// HostPreflightUntil is RunHostPreflight with its bound as a context: the
// preflight is reported as not answering the moment ctx is done, and bound
// only names the wait in that report. A test ends the wait itself, at a
// point it has observed, rather than racing a wall-clock bound.
func HostPreflightUntil(ctx context.Context, script string, bound time.Duration) string {
	out, err := exec.CommandContext(ctx, script, "preflight").CombinedOutput()
	partial := strings.TrimRight(string(out), "\n")

	if ctx.Err() != nil {
		// The bound fired. Say so, and carry whatever partial output there
		// was: a preflight that got as far as naming its runtime tells the
		// reader which engine is wedged.
		reason := fmt.Sprintf(
			"the sandbox preflight (%s preflight) did not answer within %s, so the sandbox is "+
				"presumed unusable and this layer never falls back to an unsandboxed run. "+
				"The container runtime's engine is likely wedged (Docker Desktop backend alive "+
				"but the socket unresponsive): restart Docker Desktop, then re-run.",
			SandboxScriptRel, bound)
		if partial != "" {
			return reason + "\nPartial preflight output before the bound expired:\n" + partial
		}
		return reason + "\nThe preflight produced no output before the bound expired."
	}

	if err != nil {
		// Quoted VERBATIM: preflight's message is actionable (start
		// Docker, build the image) and paraphrasing it would throw away
		// the only instructions the reader needs.
		return fmt.Sprintf(
			"the sandbox is not usable, and this layer never falls back to an unsandboxed run.\n%s",
			partial)
	}
	return ""
}

// SandboxRunCommand spells the module-aware invocation a host-side skip hands
// back. The module root has no go.mod; the sandbox must enter e2e before Go
// can discover e2e/go.mod.
func SandboxRunCommand(testPattern string) string {
	return fmt.Sprintf("%s run --dir e2e go test ./ -run %s -v", SandboxScriptRel, testPattern)
}
