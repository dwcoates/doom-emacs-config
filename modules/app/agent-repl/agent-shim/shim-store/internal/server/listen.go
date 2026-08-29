package server

import (
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"

	"agentrepl/shim-store/internal/logging"
)

// EnvSocket is the environment variable the store's socket path defaults from.
// An explicit --socket flag beats it; it exists so a test harness can point
// every participant at a private store without editing any command line.
const EnvSocket = "AGENT_REPL_STORE_SOCKET"

// Listen binds the store's unix domain socket.
//
// A LEFTOVER SOCKET IS RECLAIMED; ANYTHING ELSE IS REFUSED. A store that died
// holding the path leaves a stale socket which would otherwise make the service
// permanently unstartable — but unlinking whatever happens to sit at an
// operator-supplied path is how a service deletes somebody's file, so the mode
// is proved before the path is removed.
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
		log.Log(logging.Fields{Operation: "store.listen.reclaim", Level: "warn", Socket: absolute}, "removing a stale socket left by a previous store")
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
		listener.Close()
		log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: absolute}, "restricting the socket to its owner failed: %v", err)
		return nil, fmt.Errorf("shim-store server: restrict socket %q to its owner: %w", absolute, err)
	}
	log.Log(logging.Fields{Operation: "store.listen", Socket: absolute}, "listening on the store socket")
	return listener, nil
}
