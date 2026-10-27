package netwatch

import (
	"encoding/binary"
	"fmt"
	"net/netip"
	"os"
	"syscall"

	"golang.org/x/sys/unix"
)

// OpenSystem opens an rtnetlink socket subscribed to address and route
// changes, which the kernel writes a message to at every one.
func OpenSystem() (Source, error) {
	fd, err := unix.Socket(unix.AF_NETLINK, unix.SOCK_RAW|unix.SOCK_CLOEXEC|unix.SOCK_NONBLOCK, unix.NETLINK_ROUTE)
	if err != nil {
		return nil, fmt.Errorf("open the rtnetlink socket: %w", err)
	}
	groups := uint32(unix.RTMGRP_IPV4_IFADDR | unix.RTMGRP_IPV6_IFADDR | unix.RTMGRP_IPV4_ROUTE | unix.RTMGRP_IPV6_ROUTE)
	if err := unix.Bind(fd, &unix.SockaddrNetlink{Family: unix.AF_NETLINK, Groups: groups}); err != nil {
		_ = unix.Close(fd)
		return nil, fmt.Errorf("subscribe the rtnetlink socket to address and route changes: %w", err)
	}
	return &netlinkSource{file: os.NewFile(uintptr(fd), "rtnetlink"), buf: make([]byte, 1<<16)}, nil
}

type netlinkSource struct {
	file *os.File
	buf  []byte
}

func (s *netlinkSource) Next() (Change, error) {
	for {
		n, err := s.file.Read(s.buf)
		if err != nil {
			return Change{}, fmt.Errorf("read the rtnetlink socket: %w", err)
		}
		msgs, err := syscall.ParseNetlinkMessage(s.buf[:n])
		if err != nil {
			return Change{}, fmt.Errorf("parse an rtnetlink message: %w", err)
		}
		for _, m := range msgs {
			if change, ok := classifyNetlinkMessage(m); ok {
				return change, nil
			}
		}
	}
}

func (s *netlinkSource) Close() error { return s.file.Close() }

// classifyNetlinkMessage answers whether one rtnetlink message can bring the
// network up or take it down: a universe-scope address gained or lost, or a
// main-table default unicast route added or removed.
func classifyNetlinkMessage(m syscall.NetlinkMessage) (Change, bool) {
	switch m.Header.Type {
	case unix.RTM_NEWADDR, unix.RTM_DELADDR:
		if len(m.Data) < unix.SizeofIfAddrmsg {
			return Change{}, false
		}
		scope := m.Data[3]
		index := int(binary.NativeEndian.Uint32(m.Data[4:8]))
		if scope != unix.RT_SCOPE_UNIVERSE {
			return Change{}, false
		}
		kind := "address_added"
		if m.Header.Type == unix.RTM_DELADDR {
			kind = "address_removed"
		}
		change := Change{Kind: kind, Interface: interfaceName(index)}
		if addr, ok := netlinkAddr(m.Data[unix.SizeofIfAddrmsg:], unix.IFA_ADDRESS); ok {
			if !routable(addr) {
				return Change{}, false
			}
			change.Detail = addr.String()
		}
		return change, true
	case unix.RTM_NEWROUTE, unix.RTM_DELROUTE:
		if len(m.Data) < unix.SizeofRtMsg {
			return Change{}, false
		}
		dstLen, table, typ := m.Data[1], m.Data[4], m.Data[7]
		if dstLen != 0 || table != unix.RT_TABLE_MAIN || typ != unix.RTN_UNICAST {
			return Change{}, false
		}
		kind := "default_route_added"
		if m.Header.Type == unix.RTM_DELROUTE {
			kind = "default_route_removed"
		}
		change := Change{Kind: kind}
		if gateway, ok := netlinkAddr(m.Data[unix.SizeofRtMsg:], unix.RTA_GATEWAY); ok {
			change.Detail = gateway.String()
		}
		return change, true
	default:
		return Change{}, false
	}
}

// netlinkAddr answers the IP address of one attribute type among a message's
// route attributes.
func netlinkAddr(b []byte, want uint16) (netip.Addr, bool) {
	for len(b) >= unix.SizeofRtAttr {
		l := int(binary.NativeEndian.Uint16(b[:2]))
		typ := binary.NativeEndian.Uint16(b[2:4])
		if l < unix.SizeofRtAttr || l > len(b) {
			return netip.Addr{}, false
		}
		if typ == want {
			addr, ok := netip.AddrFromSlice(b[unix.SizeofRtAttr:l])
			return addr, ok
		}
		b = b[(l+unix.RTA_ALIGNTO-1)&^(unix.RTA_ALIGNTO-1):]
	}
	return netip.Addr{}, false
}
