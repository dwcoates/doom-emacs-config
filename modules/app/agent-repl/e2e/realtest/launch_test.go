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

func TestMethodOpenBackgroundNamesTheHiddenLaunch(t *testing.T) {
	// Arrange/Act/Assert: the method string is what the manifest's focus line
	// reports, so it must name the launch the run actually performed.
	if !strings.Contains(string(MethodOpenBackground), "-gj") {
		t.Errorf("the method is %q, want it to name the -gj launch", MethodOpenBackground)
	}
}
