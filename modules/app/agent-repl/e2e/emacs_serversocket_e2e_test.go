package e2e

// The Emacs client layer — the server socket the layer's own transport rides.
//
// EVERY scenario in this layer talks to Emacs over one unix socket, and every
// assertion below it is worth exactly what that socket's isolation is worth.
// The socket path itself is per-scenario (`Emacs.ServerSocket', under the
// scratch root `os.MkdirTemp' mints), but `server-name' is only half of what
// decides where a server lands: `server-socket-dir' decides it for every
// server started under the DEFAULT name, and its stock value comes from
// XDG_RUNTIME_DIR or TMPDIR -- both empty in this container, leaving
// /tmp/emacs<uid>, which every concurrently running scenario shares.
//
// That mattered, and it is why this file exists. Doom's own
// `lisp/doom-editor.el' starts a server on a graphical frame
// (`(use-package! server :when (display-graphic-p) ... :config (unless
// (server-running-p) (server-start)))'), and it does so from a
// `with-eval-after-load' -- so it fired inside the boot hook's own `(require
// 'server)', BEFORE the hook had set `server-name'. Concurrent boots then
// raced for /tmp/emacs<uid>/server between that `server-running-p' and its
// `bind', and the losers died with "Cannot bind server socket: Address
// already in use" for a path in no scenario's scratch root at all -- which is
// why the boot hook's occupant pre-flight and the Go side's root listing both
// reported nothing. Measured 2026-09-05: 2 of 45 scenarios on a quiet box.
//
// `sandbox/doom/init.el' now pins BOTH variables before anything can load
// `server'. The name is asserted by the boot hook itself, on every scenario;
// the directory has no other witness, so it is asserted here.

import (
	"path/filepath"
	"strings"
	"testing"
)

// TestEmacsConfinesTheServerSocketDirectoryToItsOwnScratchRoot pins the one
// half of the socket's isolation that no other assertion in the layer covers:
// a server started under the default name must still land inside this
// scenario's own root, so it cannot collide with a concurrent scenario's.
func TestEmacsConfinesTheServerSocketDirectoryToItsOwnScratchRoot(t *testing.T) {
	t.Parallel()

	// Arrange.
	box := requireSandbox(t)
	w := NewEmacsWorld(t, box)
	e := w.Emacs

	// Act.
	dir := e.EvalString(`server-socket-dir`)

	// Assert.
	root := e.Root + string(filepath.Separator)
	if !strings.HasPrefix(dir, root) {
		t.Fatalf("`server-socket-dir' is %q, outside this scenario's scratch root %s: "+
			"a default-named `server-start' -- Doom's own on a graphical frame, magit's "+
			"with-editor, an interactive M-x -- would bind a path every scenario in this "+
			"container shares, and concurrent boots would race for it", dir, e.Root)
	}
}
