package persistentwifi

import (
	"context"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"strconv"
	"strings"
	"sync"
)

// LINUX HEARS THE LID FROM THE KERNEL'S LID SWITCH (lid_linux.go): the input
// device whose switch bits name SW_LID reports every change as an EV_SW event,
// so nothing polls. Its parsing is here, free of a build tag, so it is proven
// on every platform.

// The input event codes the lid speaks (linux/input-event-codes.h).
const (
	evSW  = 0x05
	swLid = 0x00
)

// inputEventSize is struct input_event on a 64-bit kernel: a struct timeval
// (16 bytes), then the type and code (two uint16) and the value (int32).
const inputEventSize = 24

// lidDeviceIn finds the lid switch's event device in the kernel's input device
// list (/proc/bus/input/devices): the block whose switch bitmap ("B: SW=", in
// hex) has SW_LID's bit set, answered as its handler's name ("event3").
// Empty when the machine has no lid switch.
func lidDeviceIn(devices string) string {
	for block := range strings.SplitSeq(devices, "\n\n") {
		var handler string
		lid := false
		for line := range strings.SplitSeq(block, "\n") {
			switch {
			case strings.HasPrefix(line, "H: Handlers="):
				for _, h := range strings.Fields(strings.TrimPrefix(line, "H: Handlers=")) {
					if strings.HasPrefix(h, "event") {
						handler = h
					}
				}
			case strings.HasPrefix(line, "B: SW="):
				words := strings.Fields(strings.TrimPrefix(line, "B: SW="))
				if len(words) == 0 {
					continue
				}
				// The bitmap's words run most significant first; SW_LID is bit 0
				// of the last.
				if bits, err := strconv.ParseUint(words[len(words)-1], 16, 64); err == nil && bits&(1<<swLid) != 0 {
					lid = true
				}
			}
		}
		if lid && handler != "" {
			return handler
		}
	}
	return ""
}

// lidEvent decodes one input event, answering the lid it reports and whether
// it is a lid event at all.
func lidEvent(raw []byte) (Lid, bool) {
	typ := binary.LittleEndian.Uint16(raw[16:18])
	code := binary.LittleEndian.Uint16(raw[18:20])
	value := int32(binary.LittleEndian.Uint32(raw[20:24]))
	if typ != evSW || code != swLid {
		return 0, false
	}
	if value != 0 {
		return LidClosed, true
	}
	return LidOpen, true
}

// lidFromSwitches reads SW_LID's bit from EVIOCGSW's switch state bytes.
func lidFromSwitches(state []byte) Lid {
	if len(state) > 0 && state[0]&(1<<swLid) != 0 {
		return LidClosed
	}
	return LidOpen
}

// watchLidEvents hands heard the lid read at subscription (initial), then
// every lid event read from events, until ctx ends, which closes events to
// wake the read. It answers nil once ctx ends, and an error when events fails
// or ends while ctx is live.
func watchLidEvents(ctx context.Context, events io.ReadCloser, initial Lid, heard func(Lid)) error {
	stopped := make(chan struct{})
	var closer sync.WaitGroup
	closer.Add(1)
	// inflight: joined: the function that starts it waits for it.
	go func() {
		defer closer.Done()
		select {
		case <-ctx.Done():
		case <-stopped:
		}
		_ = events.Close()
	}()
	defer closer.Wait()
	defer close(stopped)
	heard(initial)
	raw := make([]byte, inputEventSize)
	for {
		if _, err := io.ReadFull(events, raw); err != nil {
			if ctx.Err() != nil {
				return nil
			}
			if errors.Is(err, io.EOF) {
				return errors.New("the lid switch's event device ended")
			}
			return fmt.Errorf("read the lid switch's events: %w", err)
		}
		if l, ok := lidEvent(raw); ok {
			heard(l)
		}
	}
}
