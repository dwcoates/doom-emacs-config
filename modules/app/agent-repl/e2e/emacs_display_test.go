package e2e

import (
	"context"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	"claude-repld/integration/harness"
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
// MEASURED: across ten healthy starts the slowest was 121ms and the rest sat
// at 21-62ms. The slow one is always the FIRST in a fresh container, which
// pays for creating /tmp/.X11-unix; the multiple here is therefore taken
// against that first start rather than against the steady state. The failure
// it exists to catch is a server that cannot start at all -- no free display,
// no socket directory -- not a slow one.
const xvfbReadyBound = 1 * time.Second

// xvfbStartMu serializes Xvfb STARTS -- not their lives.
//
// The display number is a namespace shared by every server in the container
// (`/tmp/.X11-unix' and `/tmp/.X<n>-lock'), and `-displayfd' has each server
// SEARCH that namespace for a free number. Two searches running at once are
// a check-then-act over shared state, and the harness owns the concurrency
// that creates it, so the harness holds the lock: a start is admitted only
// once the previous server has reported the number it settled on, after
// which the two run side by side with nothing in common.
//
// MEASURED, and this is the defect it fixes. Boots were blocking FOREVER --
// not slowly: raised to 20s, a stalled boot still never finished -- with
// Emacs in `select', no pty output, and the boot breadcrumb file absent
// entirely, so it had not reached init.el and was still connecting to its
// display. It happened only under the layer's own parallelism, at 1-to-5 of
// every 8 boots. With the searches serialized: zero in 32.
var xvfbStartMu sync.Mutex

// xvfbFramebufferFile is what Xvfb calls screen 0's framebuffer under
// `-fbdir`. The name is the server's, not this layer's: one file per screen,
// `Xvfb_screen<n>`.
const xvfbFramebufferFile = "Xvfb_screen0"

// xdisplay is one Xvfb server and the display it bound.
type xdisplay struct {
	// Display is the value to put in DISPLAY, e.g. ":1".
	Display string
	// LogPath is Xvfb's own stderr, preserved in failure artifacts: a GUI
	// frame that never appears is usually explained there and nowhere else.
	LogPath string
	// FramebufferPath is screen 0's framebuffer, on disk and LIVE: Xvfb
	// mmaps it under `-fbdir`, so reading it is reading what is on the
	// screen at that instant. It is the only way anything in this image can
	// see what is on the screen at all.
	FramebufferPath string

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
	xvfbStartMu.Lock()
	defer xvfbStartMu.Unlock()

	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatalf("prepare the display directory %s: %v", dir, err)
	}
	logPath := filepath.Join(dir, "xvfb.log")
	numPath := filepath.Join(dir, "xvfb.display")
	// THE FRAMEBUFFER IS ON DISK, ALWAYS, FOR EVERY SCENARIO ALIKE.
	//
	// `-fbdir` makes Xvfb mmap screen 0's framebuffer to a file instead of
	// anonymous memory. It is the ONLY way anything in this image can see
	// what is on the screen: the image carries no xwd, no ImageMagick, no
	// scrot -- and Emacs's own `x-export-frames` is not a substitute,
	// MEASURED: it re-renders the frame through Emacs's redisplay, so the
	// panel's `xwidget-webkit` webview comes out as an empty white
	// rectangle, which is precisely the surface a screenshot is wanted for.
	//
	// One code path rather than two: the cost is 5 MiB of the container's
	// own tmpfs per display (1280x1024x32 plus a 3232-byte XWD header),
	// against a measured whole-run peak of 1.22 GiB at two concurrent
	// displays, and the same pages the server would have held anonymously.
	fbDir := filepath.Join(dir, "fb")
	if err := os.MkdirAll(fbDir, 0o700); err != nil {
		t.Fatalf("prepare the Xvfb framebuffer directory %s: %v", fbDir, err)
	}

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
	x := &xdisplay{
		LogPath:         logPath,
		FramebufferPath: filepath.Join(fbDir, xvfbFramebufferFile),
		t:               t,
	}
	// `-displayfd 1` makes the SERVER pick a free display and report it, so
	// no two worlds can collide over a hard-coded number. `-nolisten tcp`
	// keeps it to the container's own abstract/unix sockets.
	proc, err := box.StartProcess(ctx, numFile, logFile,
		"Xvfb", "-displayfd", "1", "-screen", "0", xvfbScreen, "-nolisten", "tcp",
		"-fbdir", fbDir)
	if err != nil {
		cancel()
		if closeErr := logFile.Close(); closeErr != nil {
			t.Errorf("close the Xvfb log %s: %v", logPath, closeErr)
		}
		t.Fatalf("start Xvfb in the sandbox: %v", err)
	}
	x.proc = proc
	// THE DISPLAY IS THE SCENARIO'S OWN, NOT A DAEMON STRAY. Its `-fbdir` is
	// under the Emacs root, which is exactly the key `Emacs.findStrays` reaps
	// on, and the display outlives the Emacs teardown by design (its cleanup
	// is registered after, so LIFO runs it later). Undeclared, it was a
	// process the daemon-exit wait could never see leave, so that wait spent
	// its whole bound on every scenario.
	harness.SpareFromStrayReaping(t, proc.Pid())

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
