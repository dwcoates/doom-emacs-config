// Package notimpl carries the daemon's single not-implemented sentinel.
//
// The foundation lands every seam as a compilable stub; each stub returns
// Err so a caller wired against a not-yet-implemented leaf fails loudly and
// identifiably rather than observing a zero value. Leaves delete their stub
// bodies as they land; nothing in the finished daemon returns Err.
package notimpl

import "errors"

// Err is the one sentinel every unlanded seam returns. Callers test it with
// errors.Is.
var Err = errors.New("not implemented")
