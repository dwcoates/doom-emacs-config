//go:build !darwin

package persistentwifi

import (
	"context"
	"fmt"
	"runtime"
)

// SystemLid refuses: this platform has no lid notification here, which the
// standing states as an unwatched lid.
func SystemLid() LidWatcher { return otherLid{} }

type otherLid struct{}

func (otherLid) Watch(context.Context, func(Lid)) error {
	return fmt.Errorf("persistentwifi: no lid notification for %s", runtime.GOOS)
}
