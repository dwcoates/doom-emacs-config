//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// TestAColdStartedDaemonHandsOverTheSessionsItBroughtUp is the 2026-09-24
// orphaning, end to end. Daemon pid 43501 was COLD-STARTED by Emacs and
// brought three existing workspaces' sessions up at boot without Emacs
// re-registering them, so their serving rows kept naming the dead daemon
// before it. Its handover lists only what this daemon's serving row names:
// it announced `workspaces: 0`, the successor adopted nothing, and the three
// shims were left running, locks held, served by no daemon.
//
// A cold boot reaches a session two ways, and both are rows here: the boot
// bring-up SPAWNS a shim for an open workspace whose shim is gone, and the
// boot reconciliation ADOPTS a survivor still holding its lock. In neither
// does anything re-register the workspace; the first daemon's instance id is
// all the row holds.
func TestAColdStartedDaemonHandsOverTheSessionsItBroughtUp(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		// stopFirst ends the first daemon, deciding which route the cold
		// boot takes: a shim still standing is adopted, a stood-down one is
		// spawned afresh by the boot's bring-up.
		stopFirst func(t *testing.T, f *fixture)
		// coldWarnings are the records the cold daemon's boot states BY
		// DESIGN on this route.
		coldWarnings []string
	}{
		{
			name: "the boot bring-up spawns the session",
			stopFirst: func(t *testing.T, f *fixture) {
				expectSessionKillRecords(f.d)
				if _, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
					Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
						Reason: drainReasonOperator("the cold start under test"),
					}},
				})); err != nil {
					t.Fatalf("UpdateShutdownSchedule{now} = %v, want the immediate shutdown accepted", err)
				}
				f.d.AwaitExit()
			},
		},
		{
			name: "the boot reconciliation adopts the surviving shim",
			stopFirst: func(t *testing.T, f *fixture) {
				f.d.Stop()
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			// Either route leaves exactly one started session on the shim the
			// cold daemon serves: the spawn starts it, the survivor already had.
			const startSessions = 1
			// Arrange: a session on a first daemon, which then goes away.
			first := newDaemon(t, harness.Opts{})
			f := drainOpenWorkspace(t, first)
			f.shim.ExpectStartSession()
			f.shim.ExpectWatchSession()
			tt.stopFirst(t, f)

			// Arrange: a second daemon COLD-STARTS on the same state root and
			// brings the session up with nobody re-registering the workspace.
			selfRepo, cold := coldStartSelfRepoDaemon(t, first)
			if len(tt.coldWarnings) > 0 {
				cold.ExpectWarnings(tt.coldWarnings...)
			}
			// OpenWorkspace is the barrier: it takes the workspace's start gate
			// behind the boot's own bring-up and answers once the session is
			// live. It claims nothing itself -- only a registration did.
			if resp, err := cold.Client().OpenWorkspace(cold.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("OpenWorkspace on the cold daemon = (%v, %v), want the session live", resp, err)
			}
			shim := cold.Shim(f.ws)
			if got := shim.Count(harness.RPCStartSession); got != startSessions {
				t.Fatalf("StartSession on the cold daemon's shim = %d, want %d", got, startSessions)
			}
			watchesBefore := shim.Count(harness.RPCWatchSession)
			host := cold.WatchHost(f.ws)
			harness.AwaitNext(t, cold.Ctx(), host, "the fresh host push")
			daemonStream := cold.WatchDaemonStream()

			// Act: the cold daemon hands over.
			drainTriggerDeploy(t, cold, selfRepo, harness.DeployStaleDaemon)
			announced := harness.AwaitView(t, cold.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
				return r.GetShutdownAnnounced() != nil
			}).GetShutdownAnnounced()
			successor := drainDial(announced.GetAddress())

			// Assert: the session is TRANSFERRED -- the push only a transfer
			// sends -- rather than skipped for the row a dead daemon wrote.
			harness.AwaitView(t, cold.Ctx(), host, "transferred", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
				return r.GetTransferred() != nil
			})

			// Assert: the successor adopts it, dialing the running shim.
			resp, err := successor.AdoptHostWorkspace(cold.Ctx(), connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: f.ws}))
			if err != nil {
				t.Fatalf("AdoptHostWorkspace on the successor = error %v, want success", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("AdoptHostWorkspace on the successor = %v, want success", resp.Msg)
			}
			ftAwaitTrue(t, cold.Ctx(), func() bool {
				return shim.Count(harness.RPCWatchSession) > watchesBefore
			}, "the successor's WatchSession on the transferred shim")
			if got := shim.Count(harness.RPCStartSession); got != startSessions {
				t.Fatalf("StartSession across the handover = %d, want still %d: adoption dials, never re-starts", got, startSessions)
			}
			if code := cold.AwaitExit(); code != 0 {
				t.Fatalf("the cold daemon's exit code = %d, want an orderly 0 after the transfer", code)
			}
		})
	}
}

// coldStartSelfRepoDaemon boots a daemon on `first`'s state root, account root
// and shim profiles -- the same machine, restarted -- whose OWN checkout is a
// fresh fake repository with a passing test gate, so a landing on it fires the
// self-merge handover. See drainSelfRepoDaemon for the gate and the timeout.
func coldStartSelfRepoDaemon(t *testing.T, first *harness.Daemon) (*harness.Repo, *harness.Daemon) {
	t.Helper()
	selfRepo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, selfRepo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	d := harness.StartDaemon(t, harness.Opts{
		StateDir:   first.StateDir,
		ProfileDir: first.ProfileDir,
		ExtraArgs:  []string{"--default-config-dir", first.DefaultConfigDir},
		SelfRepo:   selfRepo.Dir,
		ExtraEnv:   []string{"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path},
		Timeout:    harness.HandoverChainTimeout,
	})
	return selfRepo, d
}
