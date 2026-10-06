// Package vendortraffic measures agent-repl's vendor network traffic as the
// OPERATING SYSTEM counts it: the bytes every workspace's Claude processes
// actually received and sent (owner ruling, 2026-10-06), summed across
// restarts of those processes.
//
// # WHAT IS MEASURED
//
// The vendor processes are each live shim the workspace fleet holds (it
// probes the vendor's API itself, engine/api-reachability.ts) and that shim's
// DIRECT children: the vendor CLI the SDK spawns. The shim's kernel-lock
// holders (sessionlock.HolderBinary) never touch the network and are not
// subscribed, and a tool the CLI runs (a `git push` in a Bash call) is a
// grandchild and is not vendor traffic. Only sockets whose peer is off this
// machine count: a loopback hop is not traffic.
//
// # HOW (measured 2026-10-06, Darwin 25, unprivileged)
//
// The kernel's network statistics control (`com.apple.network.statistics`,
// the interface nettop reads) answers an unprivileged client. One control
// socket per vendor process subscribes to that pid's TCP and UDP sockets; the
// daemon polls it for the live sockets' cumulative counts on a cadence, and
// every socket that closes sends its FINAL counts the instant it closes. So a
// connection opened and closed between two polls is still counted, exactly
// once (internal/vendortraffic/ledger.go). Measured: three loopback transfers
// of 1, 2 and 4 MB, the last opened and closed between polls, reported
// exactly 7,000,000 bytes; a 259,166-byte HTTPS fetch reported exactly that.
//
// The alternatives were measured and refused:
//
//   - one-shot `nettop -L 1` costs ~10ms of CPU but reports only sockets open
//     at that instant, so every connection's tail after the last sample, and
//     every connection opened and closed between samples, is lost;
//   - a continuous `nettop -L 0` keeps closed sockets but spins: 64s of CPU in
//     60s at a 5s interval, with stdin a pipe, a file or /dev/null, with and
//     without -c, -n, -P or -p;
//   - getrusage / proc_pid_rusage carry no network counters at all.
//
// Cost: one control socket per vendor process, receiving its process's
// updates plus a 24-byte removal notice for every socket closing on the
// machine. Measured with this package's own sampler at the 5s cadence against
// live shims: 6 subscriptions used 194ms of CPU over 30s, 2 used 51ms, none
// 5ms (the process alone), so about 0.8ms of CPU per subscribed process per
// second, ~0.08% of one core.
package vendortraffic

import (
	"time"
)

// Proc is one process as the kernel's process table lists it.
type Proc struct {
	// PID is the process id.
	PID int
	// PPID is its parent's pid.
	PPID int
	// Name is the kernel's command name (at most 16 characters).
	Name string
	// Started is when the process started. With the pid it names the process
	// across pid reuse.
	Started time.Time
}

// ProcessTable lists a process group's members. Every shim leads its own
// group (the spawn contract sets Setpgid), so a shim's pid lists the shim and
// everything it spawned that stayed in its group.
type ProcessTable interface {
	Group(pgid int) ([]Proc, error)
}

// Conn is one statistics control socket. Read answers one datagram.
type Conn interface {
	Read(b []byte) (int, error)
	Write(b []byte) (int, error)
	Close() error
}

// Dialer opens a statistics control socket.
type Dialer func() (Conn, error)

// Sink takes the traffic the sampler counts.
type Sink interface {
	// AddTraffic adds bytes counted since the last call. It is called from
	// each subscription's reader, concurrently.
	AddTraffic(Counts)
	// FlushTraffic ends one sampling round: the sink states what it has
	// accumulated. The sampler calls it once per round, which is what bounds
	// how often the traffic is pushed.
	FlushTraffic()
}
