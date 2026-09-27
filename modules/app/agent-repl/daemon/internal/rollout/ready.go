package rollout

import (
	"context"
	"crypto/tls"
	"errors"
	"fmt"
	"net"
	"net/http"
	"time"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
)

// A SUCCESSOR IS SERVING ONLY ONCE IT HAS ANSWERED.
//
// The address report (joining.addr) is written the moment the successor's
// listener is bound -- boot step 5 -- which is BEFORE it opens the state
// database, binds its views, arms its join or reaches `http.Server.Serve`.
// On 2026-09-27 a successor reported its address and exited 3ms later,
// refusing a state database whose layout it could not read; the incumbent
// took the report as "the successor is up", quiesced and transferred five
// workspaces to a dead address, and left their quiesce holds standing. So the
// handover now waits for a REAL ANSWER -- a DaemonHealth round trip, which
// the successor can give only once its whole boot has finished and it is
// serving -- or for the successor's process to end, whichever comes first.

// HealthProbe asks the daemon at address for one DaemonHealth answer. Any
// answer, healthy or not, is proof the daemon is serving: `DaemonHealth`
// answers unhealthy as an answer, never as a transport error.
type HealthProbe func(ctx context.Context, address string) error

// DaemonHealthProbe is the production HealthProbe: one DaemonHealth unary over
// h2c, exactly the call Emacs recognizes a daemon by.
func DaemonHealthProbe(ctx context.Context, address string) error {
	client := &http.Client{Transport: &http2.Transport{
		AllowHTTP: true,
		DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
			var dialer net.Dialer
			return dialer.DialContext(ctx, network, addr)
		},
	}}
	defer client.CloseIdleConnections()
	if _, err := agentreplv1connect.NewAgentReplClient(client, "http://"+address).
		DaemonHealth(ctx, connect.NewRequest(&agentreplv1.DaemonHealthRequest{})); err != nil {
		return fmt.Errorf("rollout: DaemonHealth at %s: %w", address, err)
	}
	return nil
}

// The readiness wait's cadence.
const (
	// DefaultReadyBound bounds the whole wait for a successor to answer. A
	// successor's boot is its reconciliation-free joining boot -- open the
	// state read-only, bind the views, arm the join -- which the integration
	// suite measures in tens of milliseconds; the bound is the spawner's own
	// report timeout, so a successor gets as long to answer as it got to bind.
	DefaultReadyBound = 30 * time.Second
	// readyProbeEvery is the wait between two probes that found nothing
	// answering yet.
	readyProbeEvery = 25 * time.Millisecond
	// readyAttemptBound bounds ONE probe, so a listener that accepts and never
	// answers costs one attempt rather than the whole bound.
	readyAttemptBound = time.Second
)

// SuccessorExitedError is a successor whose process ended before it ever
// proved it was serving.
type SuccessorExitedError struct {
	// PID is the successor's process id.
	PID int
	// Exit is the process's exit as the reap decoded it ("exit status 1",
	// "signal: killed"). The successor's own reason is in its run log and its
	// stdio log.
	Exit string
}

func (e *SuccessorExitedError) Error() string {
	return fmt.Sprintf("rollout: the successor (pid %d) exited before it answered a health probe: %s", e.PID, e.Exit)
}

// awaitAnswer probes address until it answers, the process behind it ends
// (exited closes), or ctx ends. exitErr is read only after exited has closed.
func awaitAnswer(ctx context.Context, probe HealthProbe, address string, exited <-chan struct{}, exitErr func() error, every, attempt time.Duration) error {
	ticker := time.NewTicker(every)
	defer ticker.Stop()
	var last error
	for {
		select {
		case <-exited:
			return exitErr()
		default:
		}
		one, cancel := context.WithTimeout(ctx, attempt)
		last = probe(one, address)
		cancel()
		if last == nil {
			return nil
		}
		select {
		case <-exited:
			return exitErr()
		case <-ctx.Done():
			return fmt.Errorf("rollout: the successor at %s never answered a health probe: %w", address, errors.Join(ctx.Err(), last))
		case <-ticker.C:
		}
	}
}
