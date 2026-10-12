// Package standdown answers whether a failed call was cut short by the end of
// the lifetime it ran under: the daemon standing down is an orderly exit, and
// a call it ended is recorded as that, never as a failure.
package standdown

import "context"

// CutShort reports whether err is a call the stand-down cut short: the call
// failed under a ctx (the caller's lifetime) that has ended.
func CutShort(ctx context.Context, err error) bool {
	return err != nil && ctx.Err() != nil
}
