//go:build realtest

package realtest

import (
	"context"
	"crypto/tls"
	"fmt"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
)

// THE SWEEP STOPS THE OWNER'S DAEMON THROUGH THE DAEMON'S OWN DOOR.
//
// Until 2026-09-13 it did not, and the daemon said so. `bin/realtest.sh` stops
// the owner's daemon in three places — the preflight's vendor-guard swap,
// realtest 3's daemon-down world, and the sweep-end handback — and every one of
// them sent a bare SIGTERM. A SIGTERM cancels the daemon's serving context and
// nothing else: no session is stood down on the way out, so every shim the
// daemon was holding SURVIVES it, and the next daemon adopts processes whose
// bounce nobody stated an intent for. That successor then writes, four times in
// the harvest of 2026-09-13:
//
//	WARN daemon.rollout.reconcile "sessions survived a bounce that wrote no
//	     intent manifest; each one is unaccounted for"
//	WARN daemon.rollout.reconcile "a session's bounce disposition needs a human"
//
// The warning is correct. The defect is the signal.
//
// `UpdateShutdownSchedule{now}` is the stop the product itself uses — it is
// what `agent-repl-frontend-daemon-stop` sends from the editor.
// The daemon stops accepting work, STANDS EVERY SESSION DOWN
// (`drain.standEverySessionDown`, then `sweepInFlightSpawns`), flushes its
// in-flight writes and exits itself. Its successor therefore adopts nothing,
// `reconcileWithoutManifest` finds no surviving session, and the boot is the
// ordinary boot it logs at DEBUG rather than a bounce that needs a human.
//
// WHY THIS IS A `go test` AND NOT A CURL. The door is a Connect rpc over h2c on
// the loopback address in `daemon.addr`, and the client for it is generated
// into `agentrepl/proto` and already on this module's path. Spelling the frame
// by hand in bash would be a second implementation of the wire contract; this
// is the same client every other Go caller of the endpoint uses. It runs
// through `run_harness_check`, exactly as the leftover check and the gap scan
// do, so there is one way the sweep reaches Go and one place its bound lives.
//
// EMACS IS NOT USED, deliberately. Two of the three call sites have just quit
// the owner's editor — the preflight quits it BEFORE stopping the daemon so a
// live editor cannot bring an unguarded one straight back up — so an
// Emacs-mediated stop is unavailable at precisely the moments the stop is
// needed. This dials the daemon directly and needs no editor at all.

// daemonStopEnv is what `bin/realtest.sh` sets to ask for the stop. Without it
// the driver skips: this is a maintenance verb the sweep runs at its edges, not
// something a bare `go test ./realtest/` should ever fire at the owner's
// running daemon.
const daemonStopEnv = "AGENT_REPL_REALTEST_DAEMON_STOP"

// DaemonStopNote is the operator reason the stop carries. It names the asker,
// because the daemon puts the reason verbatim into its shutdown announcement
// and into every client's drain banner, and "emacs" — what the editor's own
// stop sends — would be a lie about who asked.
const DaemonStopNote = "bin/realtest.sh"

// daemonStopBound bounds the ask itself: the dial, the request, and the
// daemon's answer.
//
// It is NOT the bound on the daemon going away — `stop_daemons_orderly` in
// bin/realtest.sh owns that wait, off DAEMON_STOP_SECONDS, because it is the
// caller that must decide what a daemon still standing means for the run. This
// bounds only the conversation, and it is generous against a measured answer of
// single-digit milliseconds on loopback: the handler stands every session down
// before it answers (drain.StandBound is 5s per workspace), so a daemon holding
// several sessions legitimately takes seconds to say yes.
const daemonStopBound = 30 * time.Second

// DaemonAddrPath is the advertisement a running daemon publishes under the
// state root. One spelling, so the harness and the editor read the same file.
func DaemonAddrPath(stateDir string) string {
	return filepath.Join(stateDir, "daemon.addr")
}

// DaemonStopAddress reads the address a daemon is answering on.
//
// AN ABSENT OR EMPTY ADVERTISEMENT IS AN ERROR HERE, not an "already gone".
// The caller only ever asks this because it just saw a daemon process in the
// process table, so a file that names nobody means the harness cannot reach the
// door — which is the fallback's cue, and a fallback taken silently is the
// thing this whole change exists to stop.
func DaemonStopAddress(stateDir string) (string, error) {
	path := DaemonAddrPath(stateDir)
	raw, err := os.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("read the daemon advertisement %s: %w", path, err)
	}
	addr := harness.AddrLine(string(raw))
	if addr == "" {
		return "", fmt.Errorf("%s names no address, so nothing states where the daemon's door is", path)
	}
	return addr, nil
}

// StopDaemonOrderly asks the daemon serving stateDir to shut down now, and
// reports whether it accepted. Every line it wants a human to read goes through
// log, which the driver points at t.Log.
//
// IT NEVER SIGNALS ANYTHING. The fallback is the caller's decision and the
// caller's signal; this function's whole contract is "the door was taken and
// accepted, or here is exactly why it was not".
func StopDaemonOrderly(ctx context.Context, stateDir string, log func(string)) error {
	addr, err := DaemonStopAddress(stateDir)
	if err != nil {
		return err
	}
	log(fmt.Sprintf("asking the daemon at %s to stop through its own door: UpdateShutdownSchedule{now}, reason operator %q", addr, DaemonStopNote))

	askCtx, cancel := context.WithTimeout(ctx, daemonStopBound)
	defer cancel()
	res, err := daemonStopClient(addr).UpdateShutdownSchedule(askCtx, connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{
			Now: &agentreplv1.UpdateShutdownScheduleNow{Reason: daemonStopReason()},
		},
	}))
	if err != nil {
		return fmt.Errorf("UpdateShutdownSchedule{now} to the daemon at %s: %w", addr, err)
	}
	switch result := res.Msg.GetResult().(type) {
	case *agentreplv1.UpdateShutdownScheduleResponse_Success:
		log(fmt.Sprintf("the daemon at %s accepted the immediate shutdown; it stands its sessions down and exits itself", addr))
		return nil
	case *agentreplv1.UpdateShutdownScheduleResponse_Error:
		return fmt.Errorf("the daemon at %s refused the immediate shutdown: %v", addr, result.Error)
	default:
		// AN ANSWER THIS HARNESS CANNOT READ IS NOT AN ACCEPTANCE, the same
		// rule `agent-repl-frontend-daemon-stop` applies to the same endpoint.
		return fmt.Errorf("the daemon at %s answered the immediate shutdown with an arm this harness cannot read: %T", addr, result)
	}
}

// daemonStopReason is the typed reason the request carries. The vocabulary is
// the proto's — never a bare string — and the operator arm is the one that
// carries a note.
func daemonStopReason() *agentreplv1.DrainReason {
	return &agentreplv1.DrainReason{
		Kind: &agentreplv1.DrainReason_Operator{
			Operator: &agentreplv1.DrainReasonOperator{Note: DaemonStopNote},
		},
	}
}

// daemonStopClient builds a Connect client against the daemon's loopback
// address. h2c, because that is what the daemon serves; it mirrors the dialers
// in e2e/adoption_e2e_test.go and daemon/integration/drain_rollout_test.go
// rather than inventing a third transport.
func daemonStopClient(addr string) agentreplv1connect.AgentReplClient {
	client := &http.Client{
		Transport: &http2.Transport{
			AllowHTTP: true,
			DialTLSContext: func(ctx context.Context, network, a string, _ *tls.Config) (net.Conn, error) {
				var dialer net.Dialer
				return dialer.DialContext(ctx, network, a)
			},
		},
	}
	return agentreplv1connect.NewAgentReplClient(client, "http://"+addr)
}
