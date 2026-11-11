package hostpower

// Host is the real PowerSource: the source this machine runs on, read the
// platform's own way (power_darwin.go, host_linux.go, host_other.go).
type Host struct{}
