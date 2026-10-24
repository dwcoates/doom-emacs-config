package server

import (
	"errors"
	"fmt"
	"net"
	"time"

	"agentrepl/logging/singletonsocket"
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
	listener, res, err := singletonsocket.Listen(path, occupancyDialTimeout)
	if err != nil {
		var step *singletonsocket.Error
		if !errors.As(err, &step) {
			log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: path}, "binding the socket failed: %v", err)
			return nil, fmt.Errorf("shim-store server: listen on unix socket %q: %w", path, err)
		}
		at := step.Path
		switch step.Step {
		case singletonsocket.StepResolve:
			log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: path}, "resolving the socket path failed: %v", step.Err)
			return nil, fmt.Errorf("shim-store server: resolve socket path %q: %w", path, step.Err)
		case singletonsocket.StepPrepareDir:
			log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: at}, "creating the socket directory failed: %v", step.Err)
			return nil, fmt.Errorf("shim-store server: prepare socket directory for %q: %w", at, step.Err)
		case singletonsocket.StepCloseProbe:
			log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: at}, "closing the occupancy probe failed: %v", step.Err)
			return nil, fmt.Errorf("shim-store server: close occupancy probe for %q: %w", at, step.Err)
		case singletonsocket.StepOccupied:
			log.Log(logging.Fields{Operation: "store.listen.occupied", Level: "error", Socket: at},
				"another store is already listening on this socket; refusing to steal it — the store is a singleton and the socket is its token")
			return nil, fmt.Errorf("shim-store server: another store is already listening on %q", at)
		case singletonsocket.StepRemoveStale:
			log.Log(logging.Fields{Operation: "store.listen.reclaim", Level: "warn", Socket: at}, "the socket refused a connection, so it is a stale one left by a previous store; reclaiming it")
			log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: at}, "removing the stale socket failed: %v", step.Err)
			return nil, fmt.Errorf("shim-store server: remove stale socket %q: %w", at, step.Err)
		case singletonsocket.StepNotASocket:
			log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: at}, "refusing to replace a non-socket at the listen path mode=%s", step.Mode)
			return nil, fmt.Errorf("shim-store server: refusing to replace non-socket %q (mode %s)", at, step.Mode)
		case singletonsocket.StepInspect:
			log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: at}, "inspecting the listen path failed: %v", step.Err)
			return nil, fmt.Errorf("shim-store server: inspect socket path %q: %w", at, step.Err)
		case singletonsocket.StepBind:
			log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: at}, "binding the socket failed: %v", step.Err)
			return nil, fmt.Errorf("shim-store server: listen on unix socket %q: %w", at, step.Err)
		case singletonsocket.StepRestrict:
			log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: at}, "restricting the socket to its owner failed: %v", step.Err)
			return nil, fmt.Errorf("shim-store server: restrict socket %q to its owner: %w", at, err)
		default:
			panic(fmt.Sprintf("shim-store server: unhandled singleton socket step %q", step.Step))
		}
	}
	if res.Reclaimed {
		log.Log(logging.Fields{Operation: "store.listen.reclaim", Level: "warn", Socket: res.Path}, "the socket refused a connection, so it is a stale one left by a previous store; reclaiming it")
	}
	log.Log(logging.Fields{Operation: "store.listen", Socket: res.Path}, "listening on the store socket")
	return listener, nil
}
