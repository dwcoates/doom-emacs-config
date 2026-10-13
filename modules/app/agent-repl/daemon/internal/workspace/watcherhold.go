package workspace

import (
	"context"
	"fmt"
	"io"
	"net"
	"path/filepath"
	"sync/atomic"

	"agent-repld/internal/dlog"
	"agent-repld/internal/envc"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/shimclient"
)

// EnvTestHoldFirstWatcherStart names a unix socket a TEST RUN's daemon dials
// before it starts its FIRST session watcher, and holds that start until the
// test closes the connection.
//
// WHY. sessionUp marks a session started before its watcher opens, and a
// prompt submitted in between was refused `no_session` (integration
// TestAPromptSentRightAfterABootOverADeadShimIsDelivered, 2026-10-12). That
// window is a few microseconds wide, so a test racing it hits it only under
// load. The hold pins it open: the test's Accept is its proof the daemon is
// inside the window, and its Close is the release. Nothing is timed.
const EnvTestHoldFirstWatcherStart = "AGENT_REPL_TEST_HOLD_FIRST_WATCHER_START"

// opWatcherHold is the hold's operation in the daemon's log.
const opWatcherHold = "daemon.workspace.watcher_hold"

// HoldFirstWatcherStartFromEnv reads EnvTestHoldFirstWatcherStart at boot and
// answers START, or START held behind the test's socket on its first call.
//
// IT IS HONORED ONLY UNDER THE VENDOR GUARD (envc.EnvForbidVendorCalls), as
// every test-run seam is: a live daemon handed it REFUSES TO BOOT rather than
// park its first bring-up on a socket nobody serves. A relative path is
// refused too, since the daemon's working directory is not the test's.
func HoldFirstWatcherStartFromEnv(contracts envc.Contracts, getenv func(string) string, start WatcherStarter) (WatcherStarter, error) {
	socket := getenv(EnvTestHoldFirstWatcherStart)
	if socket == "" {
		return start, nil
	}
	if err := contracts.RequireTestRun(EnvTestHoldFirstWatcherStart, "a live daemon never holds a watcher's start"); err != nil {
		return nil, fmt.Errorf("workspace: %w", err)
	}
	if !filepath.IsAbs(socket) {
		return nil, fmt.Errorf("workspace: %s=%q is not an absolute socket path", EnvTestHoldFirstWatcherStart, socket)
	}
	return holdFirstWatcherStart(socket, dialHold, start), nil
}

// holdDialer reaches the test's hold. It is a function so a hold that ends
// in a read error, which a real unix socket closed by its peer never
// produces, is exercised.
type holdDialer func(ctx context.Context, socket string) (net.Conn, error)

// dialHold is the production holdDialer: a unix-socket connect.
func dialHold(ctx context.Context, socket string) (net.Conn, error) {
	var dialer net.Dialer
	return dialer.DialContext(ctx, "unix", socket)
}

// holdFirstWatcherStart is START with its first call held on SOCKET.
func holdFirstWatcherStart(socket string, dial holdDialer, start WatcherStarter) WatcherStarter {
	var spent atomic.Bool
	return func(ctx context.Context, ws ids.WorkspaceID, client shimclient.Client, session sessionwatcher.Session, sinks sessionwatcher.Sinks, log dlog.Logger) (sessionwatcher.Watcher, error) {
		if spent.CompareAndSwap(false, true) {
			if err := awaitWatcherRelease(ctx, socket, dial, ws, log); err != nil {
				return nil, err
			}
		}
		return start(ctx, ws, client, session, sinks, log)
	}
}

// awaitWatcherRelease dials the test's hold and reads until the test closes
// it. A hold that cannot be reached, or that ends in anything but the test's
// close, fails the watcher's start: the test asked for the hold, and a start
// that went ahead without it would exercise nothing.
func awaitWatcherRelease(ctx context.Context, socket string, dial holdDialer, ws ids.WorkspaceID, log dlog.Logger) error {
	log = log.With(dlog.Context{"workspace_id": string(ws), "socket": socket})
	conn, err := dial(ctx, socket)
	if err != nil {
		log.Error(opWatcherHold, "could not reach the test's hold on the session watcher's start", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("workspace: reach the watcher-start hold %q: %w", socket, err)
	}
	defer conn.Close()
	log.Info(opWatcherHold, "the session watcher's start is held until the test releases it", nil)
	if _, err := io.Copy(io.Discard, conn); err != nil {
		log.Error(opWatcherHold, "the test's hold on the session watcher's start ended without its release", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("workspace: await the watcher-start hold %q: %w", socket, err)
	}
	log.Info(opWatcherHold, "the test released the session watcher's start", nil)
	return nil
}
