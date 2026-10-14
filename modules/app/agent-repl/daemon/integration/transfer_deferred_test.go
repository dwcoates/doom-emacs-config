//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/integration/harness"
)

// TestARefusedMidWorkAdoptionAnswersTheHostTransferDeferred pins the wire half
// of the 2026-10-11 adopt storm: the successor's refusal of a mid-work
// adoption reached Emacs as not_yet_adopted, which Emacs retries on a timer,
// and it flipped the webview between the two daemons until the incumbent's
// next transfer. It answers its own arm, which Emacs does not retry.
func TestARefusedMidWorkAdoptionAnswersTheHostTransferDeferred(t *testing.T) {
	t.Parallel()
	// Arrange: a running turn on a shim whose next watches re-announce nothing,
	// so the successor refuses to adopt it mid-work.
	selfRepo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, selfRepo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	d := harness.StartDaemon(t, harness.Opts{
		DurableState: true,
		SelfRepo:     selfRepo.Dir,
		ExtraEnv: []string{
			harness.TestAllScriptEnv(script.Path),
			"AGENT_REPL_HANDOVER_FACTS_BOUND=300ms",
		},
		Timeout: harness.HandoverChainTimeout,
	})
	d.ExpectWarnings("daemon.rollout.handover")
	f := drainOpenWorkspace(t, d)
	f.shim.ExpectStartSession()
	f.shim.ExpectWatchSession()
	host := d.WatchHost(f.ws)
	harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")
	if got := f.submit("forever", "k-deferred-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT); got.GetSuccess() == nil {
		t.Fatalf("SubmitPrompt = %v, want the turn accepted", got)
	}
	f.shim.ExpectStartTurn()
	f.shim.SilenceNextReannouncement()
	f.shim.SilenceNextReannouncement()
	daemonStream := d.WatchDaemonStream()
	drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleDaemon)
	announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	harness.AwaitView(t, d.Ctx(), host, "transferred", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	})
	successor := drainDial(announced.GetAddress())

	// Act: both participants adopt, and the successor refuses mid-work.
	o := adoptBothParticipants(d, successor, f.ws)

	// Assert.
	if o.hostErr != nil {
		t.Fatalf("AdoptHostWorkspace = error %v, want a typed answer", o.hostErr)
	}
	if o.host.Msg.GetError().GetTransferDeferred() == nil {
		t.Fatalf("AdoptHostWorkspace = %v, want error.transfer_deferred", o.host.Msg)
	}
}
