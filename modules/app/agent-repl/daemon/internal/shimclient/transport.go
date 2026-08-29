package shimclient

import (
	"context"
	"crypto/tls"
	"net"
	"net/http"
	"time"

	"golang.org/x/net/http2"
)

// udsBaseURL is the base URL every shim connection uses. The authority is
// never resolved: the transport dials the unix socket the client was given.
const udsBaseURL = "http://shim.uds"

// newUDSClient builds the http.Client for one shim socket: h2c over the unix
// domain socket, so one connection carries every unary call and every open
// watch stream at once.
func newUDSClient(udsPath string) *http.Client {
	return &http.Client{
		Transport: &http2.Transport{
			AllowHTTP: true,
			DialTLSContext: func(ctx context.Context, _, _ string, _ *tls.Config) (net.Conn, error) {
				var d net.Dialer
				return d.DialContext(ctx, "unix", udsPath)
			},
		},
	}
}

// backoff is the capped exponential schedule for redialing. It is a VALUE so
// tests can make it instant; nothing about the daemon's behavior depends on
// the delays, only on how long they grow to.
type backoff struct {
	// Initial is the first delay.
	Initial time.Duration
	// Max caps the growth.
	Max time.Duration
	// Factor multiplies the delay per attempt.
	Factor float64
}

// defaultBackoff is the schedule a production supervisor redials on.
var defaultBackoff = backoff{Initial: 100 * time.Millisecond, Max: 5 * time.Second, Factor: 2}

// delay is the wait before attempt n, counting from zero.
func (b backoff) delay(attempt int) time.Duration {
	if b.Initial <= 0 {
		return 0
	}
	factor := b.Factor
	if factor < 1 {
		factor = 1
	}
	d := float64(b.Initial)
	for i := 0; i < attempt; i++ {
		d *= factor
		if b.Max > 0 && d >= float64(b.Max) {
			return b.Max
		}
	}
	if b.Max > 0 && d > float64(b.Max) {
		return b.Max
	}
	return time.Duration(d)
}

// wait sleeps out the attempt's delay, cutting it short when the context ends
// or the process is known dead. It returns the reason it stopped waiting, or
// nil when the delay simply elapsed.
func (b backoff) wait(ctx context.Context, dead <-chan struct{}, attempt int) error {
	d := b.delay(attempt)
	if d <= 0 {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-dead:
			return errProcessDead
		default:
			return nil
		}
	}
	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case <-ctx.Done():
		return ctx.Err()
	case <-dead:
		return errProcessDead
	case <-timer.C:
		return nil
	}
}
