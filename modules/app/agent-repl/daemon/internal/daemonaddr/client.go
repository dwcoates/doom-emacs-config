package daemonaddr

// ClientAddress is the address a client is told to reach the daemon at
// PRIVATE on: SHARED when the daemons share one (the socket holder's,
// docs/investigations/2026-11-18-stable-sockets-design.md), PRIVATE
// otherwise. No daemon (an empty PRIVATE) is told as none. A forward always
// goes to PRIVATE; only what a client is told names SHARED, so no client
// ever learns a private address while one is shared.
func ClientAddress(shared, private string) string {
	if private == "" {
		return ""
	}
	if shared != "" {
		return shared
	}
	return private
}
