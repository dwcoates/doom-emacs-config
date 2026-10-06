package vendortraffic

import (
	"encoding/binary"
	"fmt"
)

// THE WIRE IS THE KERNEL'S NETWORK STATISTICS CONTROL (xnu bsd/net/ntstat.h),
// the interface nettop itself reads. Only the handful of fields this package
// needs are spelled here, every one of them at a fixed offset of a fixed
// header, so nothing reads a per-OS-version descriptor layout:
//
//   - every message opens with nstat_msg_hdr {u64 context; u32 type;
//     u16 length; u16 flags} (16 bytes, little-endian), and the kernel BATCHES
//     several messages into one datagram, each sized by its own length;
//   - a source's counters lead its update: srcref at 16, event flags at 24,
//     then nstat_counts {rxpackets, rxbytes, txpackets, txbytes, ...}, so the
//     bytes received are at 40 and the bytes sent at 56;
//   - the update a source sends as it CLOSES carries the CLOSING header flag
//     and that socket's final counts, which is what makes the measurement
//     lossless: a socket opened and closed between two polls is still counted
//     exactly once.
//
// ATTRIBUTION IS THE KERNEL'S, never a parse: each subscription filters by pid
// (SPECIFIC_USER_BY_PID), so every update one control socket receives belongs
// to the one process it was opened for, and no descriptor is ever read.

// Message types (nstat_msg_type).
const (
	msgSuccess    uint32 = 0
	msgError      uint32 = 1
	msgAddAllSrcs uint32 = 1002
	msgGetUpdate  uint32 = 1007
	msgSrcAdded   uint32 = 10001
	msgSrcRemoved uint32 = 10002
	msgSrcUpdate  uint32 = 10006
)

// Providers (nstat_provider_id_t): the kernel's own TCP and UDP sockets. The
// vendor CLI speaks through BSD sockets, so these two carry its traffic.
const (
	providerTCPKernel uint32 = 2
	providerUDPKernel uint32 = 4
)

// subscribedProviders are the providers every subscription adds.
var subscribedProviders = []uint32{providerTCPKernel, providerUDPKernel}

// Filter bits (nstat_msg_add_all_srcs.filter).
const (
	// filterAcceptNonLocal keeps only sockets whose peer is off this machine:
	// the vendor's traffic, never a loopback hop. Measured: a process moving
	// 1,000,000 bytes over loopback beside a 259,166-byte HTTPS fetch reports
	// exactly the fetch under this bit.
	filterAcceptNonLocal uint64 = 0x1000
	// filterSuppressSrcAdded drops the ADDED notice for every source on the
	// machine; the subscription needs only updates and removals.
	filterSuppressSrcAdded uint64 = 0x100000
	// filterSpecificUserByPID restricts reporting to target_pid's sockets.
	filterSpecificUserByPID uint64 = 1 << 32
)

// subscriptionFilter is the one filter every subscription carries.
const subscriptionFilter = filterSpecificUserByPID | filterSuppressSrcAdded | filterAcceptNonLocal

// headerFlagClosing marks a source's last update: its socket closed and the
// counts are final.
const headerFlagClosing uint16 = 0x4

// srcRefAll asks GET_UPDATE for every source the subscription holds.
const srcRefAll = ^uint64(0)

// Sizes of the fixed shapes.
const (
	headerSize      = 16
	addAllSrcsSize  = 56
	getUpdateSize   = 24
	srcRemovedSize  = 24
	srcUpdateMinLen = 64
	errorMinLen     = 20
)

// encodeHeader writes nstat_msg_hdr into b.
func encodeHeader(b []byte, ctx uint64, typ uint32, length uint16) {
	binary.LittleEndian.PutUint64(b[0:], ctx)
	binary.LittleEndian.PutUint32(b[8:], typ)
	binary.LittleEndian.PutUint16(b[12:], length)
	binary.LittleEndian.PutUint16(b[14:], 0)
}

// encodeAddAllSrcs is nstat_msg_add_all_srcs: subscribe to every source of
// provider that belongs to pid, under the one subscription filter.
func encodeAddAllSrcs(ctx uint64, provider uint32, pid int32) []byte {
	b := make([]byte, addAllSrcsSize)
	encodeHeader(b, ctx, msgAddAllSrcs, addAllSrcsSize)
	binary.LittleEndian.PutUint64(b[16:], subscriptionFilter)
	binary.LittleEndian.PutUint64(b[24:], 0) // events
	binary.LittleEndian.PutUint32(b[32:], provider)
	binary.LittleEndian.PutUint32(b[36:], uint32(pid))
	// target_uuid (b[40:56]) stays zero: the pid names the process.
	return b
}

// encodeGetUpdate is nstat_msg_query_src_req for every source. The kernel
// answers with one update per live source and then a SUCCESS carrying ctx,
// which is how a caller knows the whole answer has arrived.
func encodeGetUpdate(ctx uint64) []byte {
	b := make([]byte, getUpdateSize)
	encodeHeader(b, ctx, msgGetUpdate, getUpdateSize)
	binary.LittleEndian.PutUint64(b[16:], srcRefAll)
	return b
}

// eventKind is what one decoded message says.
type eventKind int

const (
	// eventIgnored is a message this package has no use for (an ADDED notice
	// the filter did not suppress, a description).
	eventIgnored eventKind = iota
	// eventSuccess acknowledges the request whose context it carries.
	eventSuccess
	// eventFailure is the kernel refusing the request whose context it carries.
	eventFailure
	// eventUpdate is one source's cumulative counts.
	eventUpdate
	// eventRemoved is a source leaving the subscription.
	eventRemoved
)

// event is one decoded message.
type event struct {
	kind eventKind
	// ctx is the request context a SUCCESS or ERROR answers.
	ctx uint64
	// errno is the kernel's error for an eventFailure.
	errno uint32
	// srcRef names the source an update or removal is about.
	srcRef uint64
	// closing marks a source's final update.
	closing bool
	// counts are the source's cumulative bytes for an update.
	counts Counts
}

// decodeDatagram splits one datagram into its messages. A message whose
// length does not fit what remains, or is too short for its own type, is a
// malformed datagram: the whole datagram is refused rather than read past.
func decodeDatagram(b []byte) ([]event, error) {
	var events []event
	for off := 0; off < len(b); {
		if len(b)-off < headerSize {
			return nil, fmt.Errorf("vendortraffic: %d trailing bytes at offset %d are shorter than a message header", len(b)-off, off)
		}
		ctx := binary.LittleEndian.Uint64(b[off:])
		typ := binary.LittleEndian.Uint32(b[off+8:])
		length := int(binary.LittleEndian.Uint16(b[off+12:]))
		flags := binary.LittleEndian.Uint16(b[off+14:])
		if length < headerSize || off+length > len(b) {
			return nil, fmt.Errorf("vendortraffic: message type %d at offset %d declares length %d in a %d-byte datagram", typ, off, length, len(b))
		}
		msg := b[off : off+length]
		ev, err := decodeMessage(ctx, typ, flags, msg)
		if err != nil {
			return nil, err
		}
		events = append(events, ev)
		off += length
	}
	return events, nil
}

// decodeMessage reads one message of a known length.
func decodeMessage(ctx uint64, typ uint32, flags uint16, msg []byte) (event, error) {
	switch typ {
	case msgSuccess:
		return event{kind: eventSuccess, ctx: ctx}, nil
	case msgError:
		if len(msg) < errorMinLen {
			return event{}, fmt.Errorf("vendortraffic: an error message of %d bytes is shorter than %d", len(msg), errorMinLen)
		}
		return event{kind: eventFailure, ctx: ctx, errno: binary.LittleEndian.Uint32(msg[16:])}, nil
	case msgSrcUpdate:
		if len(msg) < srcUpdateMinLen {
			return event{}, fmt.Errorf("vendortraffic: a source update of %d bytes is shorter than %d", len(msg), srcUpdateMinLen)
		}
		return event{
			kind:    eventUpdate,
			srcRef:  binary.LittleEndian.Uint64(msg[16:]),
			closing: flags&headerFlagClosing != 0,
			counts: Counts{
				Received: binary.LittleEndian.Uint64(msg[40:]),
				Sent:     binary.LittleEndian.Uint64(msg[56:]),
			},
		}, nil
	case msgSrcRemoved:
		if len(msg) < srcRemovedSize {
			return event{}, fmt.Errorf("vendortraffic: a source removal of %d bytes is shorter than %d", len(msg), srcRemovedSize)
		}
		return event{kind: eventRemoved, srcRef: binary.LittleEndian.Uint64(msg[16:])}, nil
	default:
		return event{kind: eventIgnored}, nil
	}
}
