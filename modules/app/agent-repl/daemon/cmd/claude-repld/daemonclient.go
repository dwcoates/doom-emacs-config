package main

import (
	"context"
	"crypto/tls"
	"fmt"
	"net"
	"net/http"

	"golang.org/x/net/http2"

	"agentrepl/proto/agentrepl/v1/agentreplv1connect"

	"claude-repld/internal/daemonaddr"
	"claude-repld/internal/stateroot"
)

// The verbs that CALL the serving daemon (deploy, merge-queue, call) share these two
// steps: find the address the daemon advertises in its state root, and dial it.

// newDaemonClient is a client for the daemon at a loopback address: h2c on
// the daemon's one origin.
func newDaemonClient(address string) agentreplv1connect.AgentReplClient {
	return agentreplv1connect.NewAgentReplClient(h2cClient(), "http://"+address)
}

// h2cClient is the transport every verb dials the daemon with: HTTP/2 in
// cleartext over the loopback.
func h2cClient() *http.Client {
	return &http.Client{Transport: &http2.Transport{
		AllowHTTP: true,
		DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
			var dialer net.Dialer
			return dialer.DialContext(ctx, network, addr)
		},
	}}
}

// servingAddress answers the address the state root's daemon.addr advertises.
// No file, or a file naming no address, is NO DAEMON SERVING, and says so.
func servingAddress(layout stateroot.Layout) (string, error) {
	advert, err := daemonaddr.ReadAdvertisement(layout.DaemonAddr())
	if err != nil {
		return "", fmt.Errorf("no daemon is serving: read %s: %w", layout.DaemonAddr(), err)
	}
	if advert.Address == "" {
		return "", fmt.Errorf("no daemon is serving: %s names no address", layout.DaemonAddr())
	}
	return advert.Address, nil
}
