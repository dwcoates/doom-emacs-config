package envc

import "fmt"

// RequireTestRun answers nil on a daemon running under EnvForbidVendorCalls,
// and the one boot refusal of a test-run seam otherwise.
//
// EVERY TEST-RUN SEAM IS HONORED ONLY UNDER THE VENDOR GUARD: it is what
// every test run sets and what no daemon serving a person runs under, so a
// seam a live daemon inherited is refused at boot rather than honored.
// SETTING is the seam as the environment spells it (a name, or name=value),
// and CONSEQUENCE, when not empty, says what a live daemon does instead.
func (c Contracts) RequireTestRun(setting, consequence string) error {
	if c.forbidVendorCalls {
		return nil
	}
	if consequence != "" {
		consequence = "; " + consequence
	}
	return fmt.Errorf("%s is a test-run seam and is honored only with %s set%s", setting, EnvForbidVendorCalls, consequence)
}
