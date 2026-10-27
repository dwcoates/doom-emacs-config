package netwatch

import (
	"encoding/binary"
	"syscall"
	"testing"

	"golang.org/x/sys/unix"
)

// rtattr lays one route attribute out, padded to its alignment.
func rtattr(typ uint16, value []byte) []byte {
	l := unix.SizeofRtAttr + len(value)
	b := make([]byte, (l+unix.RTA_ALIGNTO-1)&^(unix.RTA_ALIGNTO-1))
	binary.NativeEndian.PutUint16(b[:2], uint16(l))
	binary.NativeEndian.PutUint16(b[2:4], typ)
	copy(b[unix.SizeofRtAttr:], value)
	return b
}

func ifaddr(typ uint16, scope byte, addr []byte) syscall.NetlinkMessage {
	data := make([]byte, unix.SizeofIfAddrmsg)
	data[0] = unix.AF_INET
	data[3] = scope
	binary.NativeEndian.PutUint32(data[4:8], 1<<30)
	data = append(data, rtattr(unix.IFA_ADDRESS, addr)...)
	return syscall.NetlinkMessage{Header: syscall.NlMsghdr{Type: typ}, Data: data}
}

func rtroute(typ uint16, dstLen, table, kind byte, gateway []byte) syscall.NetlinkMessage {
	data := make([]byte, unix.SizeofRtMsg)
	data[0] = unix.AF_INET
	data[1] = dstLen
	data[4] = table
	data[7] = kind
	if gateway != nil {
		data = append(data, rtattr(unix.RTA_GATEWAY, gateway)...)
	}
	return syscall.NetlinkMessage{Header: syscall.NlMsghdr{Type: typ}, Data: data}
}

func TestClassifyNetlinkMessage(t *testing.T) {
	tests := []struct {
		name     string
		msg      syscall.NetlinkMessage
		wantOK   bool
		wantKind string
		detail   string
	}{
		{name: "a universe address gained", msg: ifaddr(unix.RTM_NEWADDR, unix.RT_SCOPE_UNIVERSE, []byte{192, 168, 1, 5}),
			wantOK: true, wantKind: "address_added", detail: "192.168.1.5"},
		{name: "a universe address lost", msg: ifaddr(unix.RTM_DELADDR, unix.RT_SCOPE_UNIVERSE, []byte{10, 0, 0, 2}),
			wantOK: true, wantKind: "address_removed", detail: "10.0.0.2"},
		{name: "a link-scope address is passed over", msg: ifaddr(unix.RTM_NEWADDR, unix.RT_SCOPE_LINK, []byte{169, 254, 1, 1})},
		{name: "a host-scope address is passed over", msg: ifaddr(unix.RTM_NEWADDR, unix.RT_SCOPE_HOST, []byte{127, 0, 0, 1})},
		{name: "a main-table default route added", msg: rtroute(unix.RTM_NEWROUTE, 0, unix.RT_TABLE_MAIN, unix.RTN_UNICAST, []byte{192, 168, 1, 1}),
			wantOK: true, wantKind: "default_route_added", detail: "192.168.1.1"},
		{name: "a main-table default route removed", msg: rtroute(unix.RTM_DELROUTE, 0, unix.RT_TABLE_MAIN, unix.RTN_UNICAST, nil),
			wantOK: true, wantKind: "default_route_removed"},
		{name: "a subnet route is passed over", msg: rtroute(unix.RTM_NEWROUTE, 24, unix.RT_TABLE_MAIN, unix.RTN_UNICAST, nil)},
		{name: "a local-table route is passed over", msg: rtroute(unix.RTM_NEWROUTE, 0, unix.RT_TABLE_LOCAL, unix.RTN_UNICAST, nil)},
		{name: "a link message is passed over", msg: syscall.NetlinkMessage{Header: syscall.NlMsghdr{Type: unix.RTM_NEWLINK}}},
		{name: "a truncated address message is passed over", msg: syscall.NetlinkMessage{Header: syscall.NlMsghdr{Type: unix.RTM_NEWADDR}, Data: []byte{1}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			change, ok := classifyNetlinkMessage(tt.msg)

			// Assert.
			if ok != tt.wantOK {
				t.Fatalf("classifyNetlinkMessage ok = %v, want %v (change %+v)", ok, tt.wantOK, change)
			}
			if ok && (change.Kind != tt.wantKind || change.Detail != tt.detail) {
				t.Fatalf("change = %+v, want kind %q detail %q", change, tt.wantKind, tt.detail)
			}
		})
	}
}
