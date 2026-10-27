package netwatch

import (
	"encoding/binary"
	"syscall"
	"testing"

	"golang.org/x/net/route"
	"golang.org/x/sys/unix"
)

// routeAddrs lays addresses out at their RTAX slots.
func routeAddrs(slots map[int]route.Addr) []route.Addr {
	addrs := make([]route.Addr, syscall.RTAX_MAX)
	for slot, a := range slots {
		addrs[slot] = a
	}
	return addrs
}

func TestClassifyRouteMessage(t *testing.T) {
	v4 := func(a, b, c, d byte) *route.Inet4Addr { return &route.Inet4Addr{IP: [4]byte{a, b, c, d}} }
	tests := []struct {
		name     string
		msg      route.Message
		wantOK   bool
		wantKind string
		detail   string
	}{
		{
			name:   "a routable address gained",
			msg:    &route.InterfaceAddrMessage{Type: unix.RTM_NEWADDR, Index: 1 << 30, Addrs: routeAddrs(map[int]route.Addr{syscall.RTAX_IFA: v4(192, 168, 1, 5)})},
			wantOK: true, wantKind: "address_added", detail: "192.168.1.5",
		},
		{
			name:   "a routable address lost",
			msg:    &route.InterfaceAddrMessage{Type: unix.RTM_DELADDR, Index: 1 << 30, Addrs: routeAddrs(map[int]route.Addr{syscall.RTAX_IFA: v4(10, 0, 0, 2)})},
			wantOK: true, wantKind: "address_removed", detail: "10.0.0.2",
		},
		{
			name: "a link-local address gained is passed over",
			msg: &route.InterfaceAddrMessage{Type: unix.RTM_NEWADDR, Addrs: routeAddrs(map[int]route.Addr{
				syscall.RTAX_IFA: &route.Inet6Addr{IP: [16]byte{0xfe, 0x80, 15: 1}},
			})},
		},
		{
			name: "an address message with no address is passed over",
			msg:  &route.InterfaceAddrMessage{Type: unix.RTM_NEWADDR},
		},
		{
			name: "a default route added",
			msg: &route.RouteMessage{Type: unix.RTM_ADD, Index: 1 << 30, Addrs: routeAddrs(map[int]route.Addr{
				syscall.RTAX_DST: v4(0, 0, 0, 0), syscall.RTAX_GATEWAY: v4(192, 168, 1, 1), syscall.RTAX_NETMASK: v4(0, 0, 0, 0),
			})},
			wantOK: true, wantKind: "default_route_added", detail: "192.168.1.1",
		},
		{
			name: "a default route with no mask removed",
			msg: &route.RouteMessage{Type: unix.RTM_DELETE, Index: 1 << 30, Addrs: routeAddrs(map[int]route.Addr{
				syscall.RTAX_DST: v4(0, 0, 0, 0),
			})},
			wantOK: true, wantKind: "default_route_removed",
		},
		{
			name: "an IPv6 default route changed",
			msg: &route.RouteMessage{Type: unix.RTM_CHANGE, Index: 1 << 30, Addrs: routeAddrs(map[int]route.Addr{
				syscall.RTAX_DST: &route.Inet6Addr{},
			})},
			wantOK: true, wantKind: "default_route_changed",
		},
		{
			name: "a host route cloned for one destination is passed over",
			msg: &route.RouteMessage{Type: unix.RTM_ADD, Addrs: routeAddrs(map[int]route.Addr{
				syscall.RTAX_DST: v4(140, 82, 112, 3),
			})},
		},
		{
			name: "a zero destination under a nonzero mask is passed over",
			msg: &route.RouteMessage{Type: unix.RTM_ADD, Addrs: routeAddrs(map[int]route.Addr{
				syscall.RTAX_DST: v4(0, 0, 0, 0), syscall.RTAX_NETMASK: v4(255, 0, 0, 0),
			})},
		},
		{
			name: "a lookup of the default route is passed over",
			msg: &route.RouteMessage{Type: unix.RTM_GET, Addrs: routeAddrs(map[int]route.Addr{
				syscall.RTAX_DST: v4(0, 0, 0, 0),
			})},
		},
		{
			name: "an interface's flags changing is passed over",
			msg:  &route.InterfaceMessage{Type: unix.RTM_IFINFO, Index: 4},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			change, ok := classifyRouteMessage(tt.msg)

			// Assert.
			if ok != tt.wantOK {
				t.Fatalf("classifyRouteMessage ok = %v, want %v (change %+v)", ok, tt.wantOK, change)
			}
			if !ok {
				return
			}
			if change.Kind != tt.wantKind || change.Detail != tt.detail || change.Interface != "1073741824" {
				t.Fatalf("change = %+v, want kind %q detail %q on interface 1073741824", change, tt.wantKind, tt.detail)
			}
		})
	}
}

// header is a routing message's four-byte prefix with no body.
func header(length uint16, typ byte) []byte {
	b := make([]byte, length)
	binary.NativeEndian.PutUint16(b[:2], length)
	b[2] = 5 // RTM_VERSION
	b[3] = typ
	return b
}

func TestClassifyRouteMessagesPassesOverOtherKindsByHeader(t *testing.T) {
	// Arrange: a miss and an interface-info message, neither parsed.
	b := append(header(8, unix.RTM_MISS), header(8, unix.RTM_IFINFO)...)

	// Act.
	_, ok, err := classifyRouteMessages(b)

	// Assert.
	if err != nil || ok {
		t.Fatalf("classifyRouteMessages = ok %v, err %v; want neither", ok, err)
	}
}

func TestClassifyRouteMessagesRefusesALengthPastTheRead(t *testing.T) {
	// Arrange.
	b := header(8, unix.RTM_ADD)
	binary.NativeEndian.PutUint16(b[:2], 64)

	// Act.
	_, _, err := classifyRouteMessages(b)

	// Assert.
	if err == nil {
		t.Fatal("classifyRouteMessages accepted a message longer than the read")
	}
}

func TestOpenSystemOpensAndClosesTheRoutingSocket(t *testing.T) {
	// Arrange.
	src, err := OpenSystem()
	if err != nil {
		t.Fatalf("OpenSystem() = %v", err)
	}
	done := make(chan error, 1)
	go func() {
		_, err := src.Next()
		done <- err
	}()

	// Act.
	closeErr := src.Close()

	// Assert.
	if closeErr != nil {
		t.Fatalf("Close() = %v", closeErr)
	}
	if err := <-done; err == nil {
		t.Fatal("Next() after Close answered no error")
	}
}
