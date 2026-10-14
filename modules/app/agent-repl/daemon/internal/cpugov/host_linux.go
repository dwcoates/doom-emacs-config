package cpugov

// Power reads the source the host runs on from /sys/class/power_supply.
func (*Host) Power() (Power, error) {
	return powerFromSysfs("/sys/class/power_supply")
}
