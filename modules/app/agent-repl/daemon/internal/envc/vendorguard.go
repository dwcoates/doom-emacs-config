package envc

import "fmt"

// VendorGuard answers, for one call site, whether a vendor invocation is
// permitted. Every exec of the vendor binary — the classifier's headless run,
// the login pty, a merge brief's agent — asks before spawning. See
// ARCHITECTURE.md "No real vendor calls anywhere".
type VendorGuard struct {
	forbidden bool
}

// NewVendorGuard builds the guard from the resolved contracts.
func NewVendorGuard(c Contracts) VendorGuard {
	return VendorGuard{forbidden: c.ForbidVendorCalls()}
}

// Check returns nil when site may invoke the vendor, and a ForbiddenError
// naming the site when AGENT_REPL_FORBID_VENDOR_CALLS is set. The caller
// surfaces the error; it is never defaulted away.
func (g VendorGuard) Check(site string) error {
	if !g.forbidden {
		return nil
	}
	return &ForbiddenError{Site: site}
}

// ForbiddenError is the refusal a guarded site returns. It names the site so
// the log record and the surfaced failure identify which call was refused.
type ForbiddenError struct {
	// Site is the call site's stable name, e.g. "classifier" or "login".
	Site string
}

func (e *ForbiddenError) Error() string {
	return fmt.Sprintf("vendor calls are forbidden (%s=1): %s", EnvForbidVendorCalls, e.Site)
}
