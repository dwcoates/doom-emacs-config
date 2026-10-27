package netwatch

import (
	"encoding/binary"
	"fmt"
	"net/netip"
	"os"
	"syscall"

	"golang.org/x/net/route"
	"golang.org/x/sys/unix"
)

// routeHeaderLen is the length, type and version prefix every routing message
// starts with: a two-byte length, a version byte, a type byte.
const routeHeaderLen = 4

// OpenSystem opens the kernel's routing socket, which the kernel writes a
// message to at every change of an interface's addresses or of a route.
func OpenSystem() (Source, error) {
	fd, err := unix.Socket(unix.AF_ROUTE, unix.SOCK_RAW, unix.AF_UNSPEC)
	if err != nil {
		return nil, fmt.Errorf("open the routing socket: %w", err)
	}
	unix.CloseOnExec(fd)
	// NON-BLOCKING, so the runtime's poller owns the reads and Close wakes a
	// read that is blocked.
	if err := unix.SetNonblock(fd, true); err != nil {
		_ = unix.Close(fd)
		return nil, fmt.Errorf("make the routing socket non-blocking: %w", err)
	}
	return &routeSource{file: os.NewFile(uintptr(fd), "route"), buf: make([]byte, 1<<16)}, nil
}

type routeSource struct {
	file *os.File
	buf  []byte
}

func (s *routeSource) Next() (Change, error) {
	for {
		n, err := s.file.Read(s.buf)
		if err != nil {
			return Change{}, fmt.Errorf("read the routing socket: %w", err)
		}
		change, ok, err := classifyRouteMessages(s.buf[:n])
		if err != nil {
			return Change{}, err
		}
		if ok {
			return change, nil
		}
	}
}

func (s *routeSource) Close() error { return s.file.Close() }

// classifyRouteMessages reads one read's worth of routing messages and
// answers the first that can bring the network up or take it down. Only the
// address and route messages are parsed: every other kind is passed over by
// its header alone.
func classifyRouteMessages(b []byte) (Change, bool, error) {
	for len(b) >= routeHeaderLen {
		l := int(binary.NativeEndian.Uint16(b[:2]))
		if l < routeHeaderLen || l > len(b) {
			return Change{}, false, fmt.Errorf("a routing message claims %d bytes of the %d read", l, len(b))
		}
		msg := b[:l]
		b = b[l:]
		switch int(msg[3]) {
		case unix.RTM_NEWADDR, unix.RTM_DELADDR, unix.RTM_ADD, unix.RTM_DELETE, unix.RTM_CHANGE:
		default:
			continue
		}
		parsed, err := route.ParseRIB(route.RIBTypeRoute, msg)
		if err != nil {
			return Change{}, false, fmt.Errorf("parse a routing message of type %d: %w", msg[3], err)
		}
		for _, m := range parsed {
			if change, ok := classifyRouteMessage(m); ok {
				return change, true, nil
			}
		}
	}
	return Change{}, false, nil
}

// classifyRouteMessage answers whether one parsed routing message can bring
// the network up or take it down: a routable address gained or lost, or a
// default route added, removed or changed.
func classifyRouteMessage(m route.Message) (Change, bool) {
	switch m := m.(type) {
	case *route.InterfaceAddrMessage:
		addr, ok := addrAt(m.Addrs, syscall.RTAX_IFA)
		if !ok || !routable(addr) {
			return Change{}, false
		}
		kind := "address_added"
		if m.Type == unix.RTM_DELADDR {
			kind = "address_removed"
		}
		return Change{Kind: kind, Interface: interfaceName(m.Index), Detail: addr.String()}, true
	case *route.RouteMessage:
		if !isDefaultRoute(m) {
			return Change{}, false
		}
		var kind string
		switch m.Type {
		case unix.RTM_ADD:
			kind = "default_route_added"
		case unix.RTM_DELETE:
			kind = "default_route_removed"
		case unix.RTM_CHANGE:
			kind = "default_route_changed"
		default:
			return Change{}, false
		}
		change := Change{Kind: kind, Interface: interfaceName(m.Index)}
		if gateway, ok := addrAt(m.Addrs, syscall.RTAX_GATEWAY); ok {
			change.Detail = gateway.String()
		}
		return change, true
	default:
		return Change{}, false
	}
}

// isDefaultRoute reports whether a route message is for a default route: an
// unspecified destination under an absent or all-zero mask.
func isDefaultRoute(m *route.RouteMessage) bool {
	dst, ok := addrAt(m.Addrs, syscall.RTAX_DST)
	if !ok || !dst.IsUnspecified() {
		return false
	}
	mask, ok := addrAt(m.Addrs, syscall.RTAX_NETMASK)
	return !ok || mask.IsUnspecified()
}

// addrAt answers the IP address at one slot of a message's addresses.
func addrAt(addrs []route.Addr, slot int) (netip.Addr, bool) {
	if slot >= len(addrs) {
		return netip.Addr{}, false
	}
	switch a := addrs[slot].(type) {
	case *route.Inet4Addr:
		return netip.AddrFrom4(a.IP), true
	case *route.Inet6Addr:
		return netip.AddrFrom16(a.IP), true
	default:
		return netip.Addr{}, false
	}
}
