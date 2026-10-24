// Package h2cclient is the daemon's one way to build an HTTP/2 cleartext
// (prior-knowledge h2c) client for a Connect peer: a shim or zettel on a unix
// domain socket, or a daemon on its loopback TCP address. One connection
// carries every unary call and every open stream at once.
package h2cclient

import (
	"context"
	"crypto/tls"
	"net"
	"net/http"

	"golang.org/x/net/http2"
)

// Transport answers an h2c transport that dials each request's own network
// and address: a daemon at http://127.0.0.1:<port>.
func Transport() *http2.Transport {
	return &http2.Transport{
		AllowHTTP: true,
		DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
			var d net.Dialer
			return d.DialContext(ctx, network, addr)
		},
	}
}

// Client answers an http.Client over Transport().
func Client() *http.Client {
	return &http.Client{Transport: Transport()}
}

// UnixTransport answers an h2c transport whose every connection dials
// socketPath, whatever authority a request names. track, when set, is handed
// each dialed connection and answers the one the transport uses (so a caller
// can keep connections it may later hang up on); nil uses the connection as
// dialed.
func UnixTransport(socketPath string, track func(net.Conn) net.Conn) *http2.Transport {
	return &http2.Transport{
		AllowHTTP: true,
		DialTLSContext: func(ctx context.Context, _, _ string, _ *tls.Config) (net.Conn, error) {
			var d net.Dialer
			conn, err := d.DialContext(ctx, "unix", socketPath)
			if err != nil {
				return nil, err
			}
			if track == nil {
				return conn, nil
			}
			return track(conn), nil
		},
	}
}

// UnixClient answers an http.Client over UnixTransport(socketPath, nil).
func UnixClient(socketPath string) *http.Client {
	return &http.Client{Transport: UnixTransport(socketPath, nil)}
}
