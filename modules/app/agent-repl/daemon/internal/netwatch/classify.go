package netwatch

import (
	"net"
	"net/netip"
	"strconv"
)

// routable reports whether an address an interface gained or lost can carry
// traffic off this machine: not loopback, not link-local, not unspecified. A
// link-local address comes and goes with interfaces that reach no network
// (AWDL, the low-latency WLAN link), so its events are passed over.
func routable(addr netip.Addr) bool {
	addr = addr.Unmap()
	return addr.IsValid() && !addr.IsUnspecified() && !addr.IsLoopback() &&
		!addr.IsLinkLocalUnicast() && !addr.IsLinkLocalMulticast() && !addr.IsMulticast()
}

// interfaceName names an interface by its index for the record: its name
// while it exists, its index once it is gone.
func interfaceName(index int) string {
	if iface, err := net.InterfaceByIndex(index); err == nil {
		return iface.Name
	}
	return strconv.Itoa(index)
}
