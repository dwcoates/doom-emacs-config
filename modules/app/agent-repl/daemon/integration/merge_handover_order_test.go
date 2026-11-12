//go:build integration

package integration

import (
	"database/sql"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

// merge_handover_order_test.go reproduces 2026-10-14 00:08: a deploy handed
// over the queue's head mid-tests AFTER the merge behind it had been taken
// back, the merge behind resumed as the head, and the real head stood
// stranded with its "tests" step and no run behind it; evicting the stranded
// merge then left its merge lease over no queue entry, and every prompt to
// its workspace was refused. The first half (a merge behind the head resuming
// ahead of it) cannot occur any more: only the head is ever admitted, so a
// merge behind it is taken back waiting and resumes nothing; the eviction
// half and the head's own resume stay covered here.

func TestAHeadHandedOverAfterTheMergeBehindItResumesAsTheHeadAndAMergeTakenBackIsEvictedWithItsLease(t *testing.T) {
	t.Parallel()
	// Arrange: A, the head, inside its gate; B, behind it, waiting in
	// line.
	repo := harness.NewRepo(t)
	gate := newTurnGate(t)
	repo.DeploysAsAgentRepl()
	d := harness.StartDaemon(t, harness.Opts{DurableState: true, Timeout: harness.HandoverChainTimeout,
		ExtraEnv: []string{harness.TestAllScriptEnv(gate.path), adoptAwaitsParticipantsEnv}})
	repoRef := mergeRepositoryRef(t, d, repo)
	a := mergeCreateChild(t, d, repoRef, "alpha", "the first work", nil)
	b := mergeCreateChild(t, d, repoRef, "beta", "the second work", nil)
	harness.CommitWork(t, a.ws.GetDir())
	harness.CommitWork(t, b.ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(a.ws))); err != nil {
		t.Fatalf("FinishWorkspace(alpha): %v", err)
	}
	gate.awaitStart(d, a.ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(b.ws))); err != nil {
		t.Fatalf("FinishWorkspace(beta): %v", err)
	}
	d.AwaitLogRecord(harness.WorkspaceLogPath(b.ws.GetDir(), "daemon"), "beta in line", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.merge.request" && strings.Contains(r.Message, "put a merge in line")
	})
	daemonStream := d.WatchDaemonStream()

	// Act: a deploy hands both over, and B is taken back first.
	d.StageDeployBuild(harness.DeployStaleDaemon)
	if _, err := d.Client().Deploy(d.Ctx(), connect.NewRequest(&agentreplv1.DeployRequest{})); err != nil {
		t.Fatalf("Deploy = error %v, want the handover accepted", err)
	}
	announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	successor := drainDial(announced.GetAddress())
	if o := adoptBothParticipants(d, successor, b.ws); o.hostErr != nil || o.webErr != nil {
		t.Fatalf("adopting beta: host %v, web %v", o.hostErr, o.webErr)
	}

	// Act (the eviction): B, taken back and waiting, is evicted on the successor.
	resp, err := successor.UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{Requester: harness.User(),
		Action: &agentreplv1.UpdateMergeQueueRequest_Evict{Evict: &agentreplv1.UpdateMergeQueueEvict{Workspace: b.ws}},
	}))
	requireRPCSuccess(t, "UpdateMergeQueue(evict beta)", resp, err)

	// Assert (the eviction): B's merge lease went with its entry, and B takes a prompt.
	d.WithDB(func(db *sql.DB) {
		var leases int
		if err := db.QueryRow("SELECT COUNT(*) FROM leases WHERE workspace_id = ? AND holder = 0", b.ws.GetId()).Scan(&leases); err != nil {
			t.Fatalf("counting beta's merge leases: %v", err)
		}
		var entries int
		if err := db.QueryRow("SELECT COUNT(*) FROM merge_queue WHERE workspace_id = ?", b.ws.GetId()).Scan(&entries); err != nil {
			t.Fatalf("counting beta's queue entries: %v", err)
		}
		if leases != 0 || entries != 0 {
			t.Fatalf("after the evict beta holds %d merge leases over %d queue entries, want neither", leases, entries)
		}
	})
	prompt, err := successor.SubmitPrompt(d.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace: b.ws, Said: said("after the evict"), IdempotencyKey: "beta-after-evict", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
	}))
	requireRPCAnswered(t, "SubmitPrompt(beta) after the evict", prompt, err)
	if prompt.Msg.GetError() != nil {
		t.Fatalf("SubmitPrompt(beta) after the evict = %v; want the prompt accepted", prompt.Msg)
	}

	// Act (the head): A is taken back, resumes as the head, and lands.
	if o := adoptBothParticipants(d, successor, a.ws); o.hostErr != nil || o.webErr != nil {
		t.Fatalf("adopting alpha: host %v, web %v", o.hostErr, o.webErr)
	}
	gate.awaitStart(d, a.ws.GetDir())
	gate.release(0)

	// Assert (the head): A landed.
	d.AwaitRunLogRecordFromAnyProcess("alpha's landing on the successor", func(r harness.LogRecord) bool {
		return r.PID != d.PID() && harness.IsLandingDeployed(r)
	})
}
