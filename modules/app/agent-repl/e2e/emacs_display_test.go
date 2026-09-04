package e2e

import (
	"context"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

// THE GUI DISPLAY.
//
// The agent-repl panel IS an `xwidget-webkit` webview, and a webview cannot
// be created on a tty frame: `make-xwidget` signals "GTK has not been
// initialized" there, which is exactly where the proof-of-life test stopped
// for as long as this layer ran `emacs -nw`. A webview needs a GRAPHICAL
// frame, a graphical frame needs a display, and `--network none` rules out
// a remote one -- so the display is started inside the container, per test,
// by this file.
//
// The image already carries `Xvfb` and `xauth` for this (see
// sandbox/README.md, "Giving a scenario a GUI frame"); what was missing was
// the harness side, which is what this is.
//
// NOTHING HERE SLEEPS. Xvfb's own `-displayfd` is the synchronization
// primitive: the server writes the display number it settled on to that
// descriptor ONLY once it is listening, so reading a number back is both
// "which display" and "it is up" in one edge. That also removes the race a
// hand-picked `:99` would have had between two concurrent worlds, since the
// server -- not the test -- chooses the free number.

// xvfbScreen is the virtual screen Xvfb serves. A webview is laid out in
// real pixels, so the geometry is a real input to what the panel renders:
// this is a conventional desktop size, large enough that the panel's own
// window split is not degenerate.
const xvfbScreen = "1280x1024x24"

// xvfbReadyBound is how long Xvfb may take to bind a display and report it.
//
// PROVISIONAL, and marked so per the module AGENTS.md rule that bounds are
// measured rather than guessed. The failure it exists to catch is a server
// that cannot start at all -- no /tmp/.X11-unix, no free display -- not a
// slow one.
const xvfbReadyBound = 5 * time.Second

// xdisplay is one Xvfb server and the display it bound.
type xdisplay struct {
	// Display is the value to put in DISPLAY, e.g. ":1".
	Display string
	// LogPath is Xvfb's own stderr, preserved in failure artifacts: a GUI
	// frame that never appears is usually explained there and nowhere else.
	LogPath string

	t    *testing.T
	proc sandboxProc
}

// startXvfb brings one Xvfb up under dir and returns once it is listening.
//
// Teardown is registered here, so a caller that starts Emacs afterwards gets
// the ordering it needs for free: t.Cleanup unwinds LIFO, so Emacs is torn
// down BEFORE the display it is drawing on goes away.
func startXvfb(t *testing.T, box sandbox, dir string) *xdisplay {
	t.Helper()

	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatalf("prepare the display directory %s: %v", dir, err)
	}
	logPath := filepath.Join(dir, "xvfb.log")
	numPath := filepath.Join(dir, "xvfb.display")

	// Xvfb writes the display number to `-displayfd` and everything else to
	// stderr, so the two are separate files rather than one that has to be
	// parsed apart.
	numFile, err := os.Create(numPath)
	if err != nil {
		t.Fatalf("create the Xvfb display file %s: %v", numPath, err)
	}
	defer func() {
		if err := numFile.Close(); err != nil {
			t.Errorf("close the Xvfb display file %s: %v", numPath, err)
		}
	}()
	logFile, err := os.Create(logPath)
	if err != nil {
		t.Fatalf("create the Xvfb log %s: %v", logPath, err)
	}

	ctx, cancel := context.WithCancel(context.Background())
	x := &xdisplay{LogPath: logPath, t: t}
	// `-displayfd 1` makes the SERVER pick a free display and report it, so
	// no two worlds can collide over a hard-coded number. `-nolisten tcp`
	// keeps it to the container's own abstract/unix sockets.
	proc, err := box.StartProcess(ctx, numFile, logFile,
		"Xvfb", "-displayfd", "1", "-screen", "0", xvfbScreen, "-nolisten", "tcp")
	if err != nil {
		cancel()
		if closeErr := logFile.Close(); closeErr != nil {
			t.Errorf("close the Xvfb log %s: %v", logPath, closeErr)
		}
		t.Fatalf("start Xvfb in the sandbox: %v", err)
	}
	x.proc = proc

	t.Cleanup(func() {
		x.proc.Kill()
		cancel()
		if err := logFile.Close(); err != nil {
			t.Errorf("close the Xvfb log %s: %v", logPath, err)
		}
	})
	// Registered AFTER the kill above, so LIFO runs it FIRST: it observes an
	// X server that died on its own, before this teardown killed it on
	// purpose. A display that vanished mid-test explains every downstream
	// GUI failure and must not be swallowed.
	t.Cleanup(func() {
		if x.proc.Exited() {
			t.Errorf("the Xvfb display %s exited before test cleanup:\n%s",
				x.Display, x.tailLog())
		}
	})

	x.Display = x.awaitDisplay(numPath)
	return x
}

// awaitDisplay reads the display number Xvfb reports, or fails loudly.
func (x *xdisplay) awaitDisplay(numPath string) string {
	x.t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), xvfbReadyBound)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if x.proc.Exited() {
			x.t.Fatalf("Xvfb exited before it reported a display:\n%s", x.tailLog())
		}
		body, err := os.ReadFile(numPath)
		if err != nil {
			x.t.Fatalf("read the Xvfb display file %s: %v", numPath, err)
		}
		// The number is terminated by a newline, and only then is it
		// complete: a partial write must not be read as a display.
		if line, _, ok := strings.Cut(string(body), "\n"); ok {
			n, convErr := strconv.Atoi(strings.TrimSpace(line))
			if convErr != nil {
				x.t.Fatalf("Xvfb reported %q, which is not a display number: %v\n%s",
					line, convErr, x.tailLog())
			}
			return ":" + strconv.Itoa(n)
		}
		select {
		case <-ctx.Done():
			x.t.Fatalf("Xvfb did not report a display within %s:\n%s", xvfbReadyBound, x.tailLog())
			return ""
		case <-ticker.C:
		}
	}
}

// tailLog is Xvfb's own output, for a failure message.
func (x *xdisplay) tailLog() string {
	body, err := os.ReadFile(x.LogPath)
	if err != nil {
		return "(Xvfb log unreadable: " + err.Error() + ")"
	}
	return string(tailBytes(body, artifactTailBytes))
}

// Env is what a process must carry to draw on this display.
//
// GDK_BACKEND is pinned to x11 because the image's Emacs is a `--with-pgtk`
// build: pure GTK prefers Wayland when it can, and there is no Wayland
// compositor here, only Xvfb.
func (x *xdisplay) Env() []string {
	return []string{
		"DISPLAY=" + x.Display,
		"GDK_BACKEND=x11",
	}
}
