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

// SystemPowerSource refuses: this platform has no power source notification
// here, which the controller holds as an unwatched power source.
func SystemPowerSource() PowerSourceWatcher { return otherPowerSource{} }

type otherPowerSource struct{}

func (otherPowerSource) Watch(context.Context, func(PowerSource)) error {
	return fmt.Errorf("persistentwifi: no power source notification for %s", runtime.GOOS)
}
