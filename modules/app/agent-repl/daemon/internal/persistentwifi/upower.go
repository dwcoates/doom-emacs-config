package persistentwifi

import (
	"context"
	"errors"
	"fmt"

	"github.com/godbus/dbus/v5"
)

// ON LINUX THE POWER SOURCE IS HEARD FROM UPower (powersource_linux.go): its
// OnBattery property says whether the machine draws battery power, and UPower
// says when it changes with PropertiesChanged on the system bus, so nothing
// polls. The signal's reading is here, free of a build tag, so it is proven on
// every platform. Untested on a real Linux laptop (AGENTS.md).

// UPower's daemon on the system bus.
const (
	upowerName      = "org.freedesktop.UPower"
	upowerPath      = dbus.ObjectPath("/org/freedesktop/UPower")
	upowerInterface = "org.freedesktop.UPower"
	upowerOnBattery = "OnBattery"
)

// onBatteryChange reads a UPower PropertiesChanged signal: the OnBattery it
// carries and true, or false when sig changes no OnBattery (or names it
// invalidated, which carries no value: the property is read again).
func onBatteryChange(sig *dbus.Signal) (onBattery, invalidated, ok bool) {
	if sig == nil || sig.Path != upowerPath || sig.Name != propsInterface+"."+propsChanged || len(sig.Body) < 3 {
		return false, false, false
	}
	if iface, isString := sig.Body[0].(string); !isString || iface != upowerInterface {
		return false, false, false
	}
	if changed, isMap := sig.Body[1].(map[string]dbus.Variant); isMap {
		if v, has := changed[upowerOnBattery]; has {
			b, isBool := v.Value().(bool)
			return b, false, isBool
		}
	}
	if names, isList := sig.Body[2].([]string); isList {
		for _, name := range names {
			if name == upowerOnBattery {
				return false, true, true
			}
		}
	}
	return false, false, false
}

// upowerBus is what the watch needs of the system bus: OnBattery's value, and
// the signals it subscribed to.
type upowerBus interface {
	OnBattery() (bool, error)
	Signals() <-chan *dbus.Signal
}

// watchUPower hands heard the power source read after subscribing, then every
// change, until ctx ends (nil) or the bus's signals end (an error).
func watchUPower(ctx context.Context, bus upowerBus, heard func(PowerSource)) error {
	read := func() error {
		onBattery, err := bus.OnBattery()
		if err != nil {
			return fmt.Errorf("persistentwifi: read UPower's OnBattery: %w", err)
		}
		heard(powerSourceOf(onBattery))
		return nil
	}
	if err := read(); err != nil {
		return err
	}
	signals := bus.Signals()
	for {
		select {
		case <-ctx.Done():
			return nil
		case sig, open := <-signals:
			if !open {
				if ctx.Err() != nil {
					return nil
				}
				return errors.New("persistentwifi: the system bus connection hearing UPower closed")
			}
			onBattery, invalidated, ok := onBatteryChange(sig)
			switch {
			case !ok:
			case invalidated:
				if err := read(); err != nil {
					return err
				}
			default:
				heard(powerSourceOf(onBattery))
			}
		}
	}
}
