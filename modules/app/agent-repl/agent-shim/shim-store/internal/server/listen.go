package server

import (
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"time"

	"agentrepl/shim-store/internal/logging"
)

// EnvSocket is the environment variable the store's socket path defaults from.
// An explicit --socket flag beats it; it exists so a test harness can point
// every participant at a private store without editing any command line.
const EnvSocket = "AGENT_REPL_STORE_SOCKET"

// occupancyDialTimeout bounds the exclusivity probe. Connecting to a unix
// socket is a kernel-local operation: a live listener accepts at once and an
// abandoned path refuses at once, so this is a guard against a pathological
// filesystem, never a wait anything normal pays.
const occupancyDialTimeout = 2 * time.Second

// Listen binds the store's unix domain socket.
//
// A LEFTOVER SOCKET IS RECLAIMED; A LIVE ONE IS NEVER STOLEN. A store that died
// holding the path leaves a stale socket which would otherwise make the service
// permanently unstartable — but unlinking whatever happens to sit at an
// operator-supplied path is how a service deletes somebody's file, so the mode
// is proved before the path is removed, and the path is DIALLED before it is
// unlinked. A successful dial means a live store owns it: unlinking then
// re-binding would leave the first store serving a socket no client can reach
// any more while this one silently took its clients, so the boot is refused
// instead. The socket is the store's singleton token, and the kernel is what
// arbitrates it.
func Listen(path string, log *logging.Logger) (net.Listener, error) {
	if log == nil {
		panic("shim-store server: nil logger")
	}
	absolute, err := filepath.Abs(path)
	if err != nil {
		log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: path}, "resolving the socket path failed: %v", err)
		return nil, fmt.Errorf("shim-store server: resolve socket path %q: %w", path, err)
	}
	if err := os.MkdirAll(filepath.Dir(absolute), 0o700); err != nil {
		log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: absolute}, "creating the socket directory failed: %v", err)
		return nil, fmt.Errorf("shim-store server: prepare socket directory for %q: %w", absolute, err)
	}
	switch info, statErr := os.Lstat(absolute); {
	case statErr == nil && info.Mode()&os.ModeSocket != 0:
		if occupied, dialErr := net.DialTimeout("unix", absolute, occupancyDialTimeout); dialErr == nil {
			if closeErr := occupied.Close(); closeErr != nil {
				log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: absolute}, "closing the occupancy probe failed: %v", closeErr)
				return nil, fmt.Errorf("shim-store server: close occupancy probe for %q: %w", absolute, closeErr)
			}
			log.Log(logging.Fields{Operation: "store.listen.occupied", Level: "error", Socket: absolute},
				"another store is already listening on this socket; refusing to steal it — the store is a singleton and the socket is its token")
			return nil, fmt.Errorf("shim-store server: another store is already listening on %q", absolute)
		}
		log.Log(logging.Fields{Operation: "store.listen.reclaim", Level: "warn", Socket: absolute}, "the socket refused a connection, so it is a stale one left by a previous store; reclaiming it")
		if removeErr := os.Remove(absolute); removeErr != nil {
			log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: absolute}, "removing the stale socket failed: %v", removeErr)
			return nil, fmt.Errorf("shim-store server: remove stale socket %q: %w", absolute, removeErr)
		}
	case statErr == nil:
		log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: absolute}, "refusing to replace a non-socket at the listen path mode=%s", info.Mode())
		return nil, fmt.Errorf("shim-store server: refusing to replace non-socket %q (mode %s)", absolute, info.Mode())
	case !errors.Is(statErr, os.ErrNotExist):
		log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: absolute}, "inspecting the listen path failed: %v", statErr)
		return nil, fmt.Errorf("shim-store server: inspect socket path %q: %w", absolute, statErr)
	}
	listener, err := net.Listen("unix", absolute)
	if err != nil {
		log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: absolute}, "binding the socket failed: %v", err)
		return nil, fmt.Errorf("shim-store server: listen on unix socket %q: %w", absolute, err)
	}
	if err := os.Chmod(absolute, 0o600); err != nil {
		log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: absolute}, "restricting the socket to its owner failed: %v", err)
		return nil, abandonListener(listener, absolute, fmt.Errorf("shim-store server: restrict socket %q to its owner: %w", absolute, err), log)
	}
	log.Log(logging.Fields{Operation: "store.listen", Socket: absolute}, "listening on the store socket")
	return listener, nil
}

// abandonListener closes a listener whose setup failed after it was bound, and
// returns the error Listen reports. The setup failure is the cause; a failure
// to close on top of it is a second, separate fault — it can leave the socket
// bound or its file on disk — so it gets its own error record and is joined
// onto the cause rather than dropped.
func abandonListener(listener net.Listener, absolute string, cause error, log *logging.Logger) error {
	closeErr := listener.Close()
	if closeErr == nil {
		return cause
	}
	log.Log(logging.Fields{Operation: "store.listen.abandon", Level: "error", Socket: absolute}, "closing the listener after its setup failed also failed: %v", closeErr)
	return errors.Join(cause, fmt.Errorf("shim-store server: close abandoned listener on %q: %w", absolute, closeErr))
}
