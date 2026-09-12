//go:build realtest

package realtest

import (
	"strings"
	"testing"
)

// The launcher's unit tests. They assert the argv rather than run `open`,
// because a realtest launch would start the owner's editor and this is exactly
// the layer that must never do that outside bin/realtest.sh.

func TestOpenBackgroundArgsLaunchHiddenAndInTheBackground(t *testing.T) {
	// Arrange/Act: -g alone still activated Emacs on run 3's cold launch, so
	// the launch must also pass -j to launch the app hidden (row 29).
	args := openBackgroundArgs()

	// Assert.
	joined := strings.Join(args, " ")
	if args[0] != "-gj" {
		t.Errorf("the first flag is %q, want -gj so the launch is both background and hidden", args[0])
	}
	if !strings.Contains(joined, "-a "+emacsBundle) {
		t.Errorf("the argv %q does not name the Emacs bundle to open", joined)
	}
}

func TestOpenBackgroundArgsStateTheVendorGuardOnTheCommandLine(t *testing.T) {
	// Arrange/Act: `open` hands the app to launchd, which drops this process's
	// environment, so the guard must be stated with --env or it never reaches
	// Emacs.
	args := openBackgroundArgs()

	// Assert.
	foundEnv := false
	for i, arg := range args {
		if arg == "--env" && i+1 < len(args) && args[i+1] == vendorGuardEnv+"=1" {
			foundEnv = true
		}
	}
	if !foundEnv {
		t.Errorf("the argv %v does not state %s=1 with --env", args, vendorGuardEnv)
	}
}

func TestOpenBackgroundArgsStateTheGuardAsTheONLYSubstitution(t *testing.T) {
	// Arrange/Act: the vendor guard is the single variable a realtest states,
	// and it now IMPLIES fake shims -- a guarded daemon spawns every shim with
	// `--fake` rather than refusing the spawn
	// (daemon/internal/shimclient/supervisor.go, `fakeMode`). A second knob
	// stated here would be a second contract for one rule, and the pair could
	// then disagree: an Emacs carrying the fake hook but not the guard would
	// look guarded to this harness while the vendor was one prompt away.
	args := openBackgroundArgs()

	// Assert.
	var stated []string
	for i, arg := range args {
		if arg == "--env" && i+1 < len(args) {
			stated = append(stated, args[i+1])
		}
	}
	if len(stated) != 1 || stated[0] != vendorGuardEnv+"=1" {
		t.Errorf("the argv states the environment %v, want exactly [%s=1]: the guard implies fake shims, "+
			"so no second variable belongs on the launch", stated, vendorGuardEnv)
	}
}

func TestMethodOpenBackgroundNamesTheHiddenLaunch(t *testing.T) {
	// Arrange/Act/Assert: the method string is what the manifest's focus line
	// reports, so it must name the launch the run actually performed.
	if !strings.Contains(string(MethodOpenBackground), "-gj") {
		t.Errorf("the method is %q, want it to name the -gj launch", MethodOpenBackground)
	}
}
