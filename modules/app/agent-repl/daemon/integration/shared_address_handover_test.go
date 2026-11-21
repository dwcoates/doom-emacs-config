//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

// TestAHandoverOnASharedAddressIsReachedOnlyThroughThatAddress is the stable
// socket design end to end (docs/investigations/
// 2026-11-18-stable-sockets-design.md): every call a client makes goes to the
// one address the socket holder keeps. The incumbent announces that address,
// forwards the transferred workspace's adoption to its successor, and after
// it exits the same address is answered by the successor, with daemon.addr
// naming it throughout. No client ever needs a daemon's private address.
func TestAHandoverOnASharedAddressIsReachedOnlyThroughThatAddress(t *testing.T) {
	t.Parallel()
	// Arrange: a holder, and a daemon serving its shared address.
	holder := harness.StartSocketHolder(t)
	selfRepo := harness.NewRepo(t)
	script := selfRepo.TestAllScript()
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	selfRepo.DeploysAsAgentRepl()
	d := harness.StartDaemon(t, harness.Opts{
		DurableState: true,
		Holder:       holder,
		Timeout:      harness.HandoverChainTimeout,
	})
	d.AwaitAddrMatching(holder.Address, func(addr string) bool { return addr == holder.Address })
	f := drainOpenWorkspace(t, d)
	f.shim.ExpectStartSession()
	f.shim.ExpectWatchSession()
	host := d.WatchHost(f.ws)
	harness.AwaitHostView(t, d.Ctx(), host, "the fresh host push")
	daemonStream := d.WatchDaemonStream()
	shared := drainDial(holder.Address)

	// Act: hand over.
	drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleDaemon)
	announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()

	// Assert: the stand-down names the shared address, never the successor's own.
	if got := announced.GetAddress(); got != holder.Address {
		t.Fatalf("the stand-down named %q, want the shared address %q", got, holder.Address)
	}
	harness.AwaitView(t, d.Ctx(), host, "transferred", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	})

	// Act: the host adopts through the shared address (the workspace has no
	// page, so the host is its one participant); the incumbent forwards it
	// to the successor.
	adopt, err := shared.AdoptHostWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: f.ws}))

	// Assert
	if err != nil || adopt.Msg.GetSuccess() == nil {
		t.Fatalf("AdoptHostWorkspace through the shared address = (%v, %v), want it forwarded and adopted", adopt, err)
	}
	if code := d.AwaitExit(); code != 0 {
		t.Fatalf("the incumbent's exit code = %d, want an orderly 0", code)
	}

	// Assert: the same address is the successor's now, and daemon.addr names it.
	if resp, err := shared.DaemonHealth(d.Ctx(), connect.NewRequest(&agentreplv1.DaemonHealthRequest{})); err != nil || resp == nil {
		t.Fatalf("DaemonHealth through the shared address after the handover = (%v, %v), want the successor's answer", resp, err)
	}
	d.AwaitAddrMatching(holder.Address, func(addr string) bool { return addr == holder.Address })
}
