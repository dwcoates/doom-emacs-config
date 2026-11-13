package kit

import (
	"strconv"
	"sync/atomic"
)

// XvfbScreen is the virtual screen Xvfb serves. A webview is laid out in
// real pixels, so the geometry is a real input to what the panel renders:
// this is a conventional desktop size, large enough that the panel's own
// window split is not degenerate.
const XvfbScreen = "1280x1024x24"

// THE HARNESS OWNS THE DISPLAY NUMBERS, AND NEVER REUSES ONE.
//
// `-displayfd` alone had each server SEARCH the container's display namespace
// (`/tmp/.X11-unix' and `/tmp/.X<n>-lock') for a free number, and a killed
// server leaves its socket and lock files behind. On the 2026-10-22 20:21Z
// gate run a start met a number some earlier server's state still claimed:
// "_XSERVTransMakeAllCOTSServerListeners: server already running", and no
// display was ever reported. Every server in the container is this test
// process's (one sandbox unit, one `go test`), so handing each start the next
// number of a counter that only climbs makes a collision impossible: no number
// is asked for twice, by anyone. `-displayfd` stays as the readiness edge.
const FirstXvfbDisplay = 10

var LastXvfbDisplay atomic.Int64

// NextXvfbDisplay is the next display number of this process, never one
// handed out before.
func NextXvfbDisplay() int {
	return FirstXvfbDisplay + int(LastXvfbDisplay.Add(1)) - 1
}

// XvfbArgv is the command line of one Xvfb on DISPLAY, its framebuffer under
// FBDIR. `-displayfd 1` reports the number once the server is listening, which
// is the start's readiness edge; `-nolisten tcp` keeps it to the container's
// own sockets.
func XvfbArgv(display int, fbDir string) []string {
	return []string{"Xvfb", ":" + strconv.Itoa(display), "-displayfd", "1",
		"-screen", "0", XvfbScreen, "-nolisten", "tcp", "-fbdir", fbDir}
}
