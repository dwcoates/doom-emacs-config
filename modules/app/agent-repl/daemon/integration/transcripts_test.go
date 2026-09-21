//go:build integration

package integration

import (
	"os"
	"path/filepath"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// answerTranscripts scripts the fake shim's next ReadTranscripts answer.
func answerTranscripts(f *fixture, transcripts ...*shimv1.Transcript) {
	f.t.Helper()
	f.shim.Answer(harness.RPCReadTranscripts, &shimv1.ReadTranscriptsResponse{
		Result: &shimv1.ReadTranscriptsResponse_Success{
			Success: &shimv1.ReadTranscriptsSuccess{Transcripts: transcripts},
		},
	})
}

// listTranscripts asks the real daemon for the workspace's conversations.
func listTranscripts(f *fixture) *agentreplv1.ListWorkspaceTranscriptsResponse {
	f.t.Helper()
	resp, err := f.d.Client().ListWorkspaceTranscripts(f.d.Ctx(),
		connect.NewRequest(&agentreplv1.ListWorkspaceTranscriptsRequest{Workspace: f.ws}))
	if err != nil {
		f.t.Fatalf("ListWorkspaceTranscripts: %v", err)
	}
	return resp.Msg
}

// bind asks the real daemon to point the workspace at one conversation.
func bind(f *fixture, vendorSessionID string) *agentreplv1.BindWorkspaceSessionResponse {
	f.t.Helper()
	resp, err := f.d.Client().BindWorkspaceSession(f.d.Ctx(),
		connect.NewRequest(&agentreplv1.BindWorkspaceSessionRequest{
			Workspace: f.ws, VendorSessionId: vendorSessionID,
		}))
	if err != nil {
		f.t.Fatalf("BindWorkspaceSession: %v", err)
	}
	return resp.Msg
}

// TestListWorkspaceTranscriptsReachesTheShimAndRelaysItsAnswer drives the whole
// hop — client, daemon, real shim socket — so the verb is proved to reach the
// process that owns the transcripts rather than only the verb layer's fake.
func TestListWorkspaceTranscriptsReachesTheShimAndRelaysItsAnswer(t *testing.T) {
	// Arrange.
	f := newOpened(t, harness.Opts{})
	opening := "explain hash tables"
	answerTranscripts(f, &shimv1.Transcript{
		VendorSessionId: "conversation-a", Opening: &opening, Prompts: 3,
	})

	// Act.
	msg := listTranscripts(f)

	// Assert.
	got := msg.GetSuccess().GetTranscripts()
	if len(got) != 1 || got[0].GetVendorSessionId() != "conversation-a" || got[0].GetOpening() != opening {
		t.Fatalf("transcripts = %+v, want the shim's own answer relayed", got)
	}
	if f.shim.Count(harness.RPCReadTranscripts) != 1 {
		t.Fatalf("ReadTranscripts calls = %d, want 1", f.shim.Count(harness.RPCReadTranscripts))
	}
}

// TestListWorkspaceTranscriptsLetsTheDAEMONSRecordDecideWhatIsCurrent proves
// the precedence end to end: the workspace's session record is the binding —
// it is what survives a restart and what the next resume reads — so a
// conversation the record does not name is NOT marked current however the shim
// flags it.
func TestListWorkspaceTranscriptsLetsTheDAEMONSRecordDecideWhatIsCurrent(t *testing.T) {
	// Arrange: an opened workspace already records its own conversation, and
	// the shim claims a DIFFERENT one is the bound one.
	f := newOpened(t, harness.Opts{})
	answerTranscripts(f, &shimv1.Transcript{
		VendorSessionId: "not-the-recorded-one", Bound: &shimv1.TranscriptBound{},
	})

	// Act.
	msg := listTranscripts(f)

	// Assert.
	got := msg.GetSuccess().GetTranscripts()
	if len(got) != 1 || got[0].GetCurrent() != nil {
		t.Fatalf("transcripts = %+v, want the record's binding to decide", got)
	}
}

// TestBindWorkspaceSessionRefusesATranscriptSomethingIsWritingTo proves the
// refusal survives the whole hop with its evidence intact.
func TestBindWorkspaceSessionRefusesATranscriptSomethingIsWritingTo(t *testing.T) {
	// Arrange.
	f := newOpened(t, harness.Opts{})
	answerTranscripts(f, &shimv1.Transcript{
		VendorSessionId: "conversation-b",
		Active:          &shimv1.TranscriptActive{AtMs: 1_700_000_000_000, QuietAfterMs: 120_000},
	})

	// Act.
	msg := bind(f, "conversation-b")

	// Assert: two writers on one conversation is data loss.
	if msg.GetError().GetTranscriptActive().GetAtMs() != 1_700_000_000_000 {
		t.Fatalf("result = %v, want transcript_active carrying the write instant", msg.GetResult())
	}
}

// TestBindWorkspaceSessionRefusesAnIdNoTranscriptCarries proves the bind
// validates against the shim's FRESH listing rather than the client's word.
func TestBindWorkspaceSessionRefusesAnIdNoTranscriptCarries(t *testing.T) {
	// Arrange.
	f := newOpened(t, harness.Opts{})
	answerTranscripts(f, &shimv1.Transcript{VendorSessionId: "conversation-a"})

	// Act.
	msg := bind(f, "invented")

	// Assert.
	if msg.GetError().GetUnknownTranscript().GetVendorSessionId() != "invented" {
		t.Fatalf("result = %v, want unknown_transcript naming the invented id", msg.GetResult())
	}
}

// TestBindWorkspaceSessionStartsTheChosenConversationAsARebind proves the whole
// hop for the fix the bind exists for: the resume the daemon sends after a bind
// carries `rebind`, which is what tells the shim to adopt the chosen
// conversation's identity as the workspace's book. Without it the shim keeps
// the workspace's persisted book and the daemon reads the PREVIOUS
// conversation's history back over the one the user chose.
func TestBindWorkspaceSessionStartsTheChosenConversationAsARebind(t *testing.T) {
	// Arrange: the chosen conversation with the vendor transcript on disk that
	// a real one would have left behind. Without the file the source
	// classifier judges the conversation to have written nothing and brings
	// the workspace up FRESH — and a fresh start carries no resume at all, so
	// there would be nothing for the marker to ride.
	f := newOpened(t, harness.Opts{})
	seedVendorTranscript(t, f, "conversation-b")
	answerTranscripts(f, &shimv1.Transcript{VendorSessionId: "conversation-b"})

	// Act.
	msg := bind(f, "conversation-b")

	// Assert.
	if msg.GetSuccess() == nil {
		t.Fatalf("BindWorkspaceSession = %v, want a success", msg.GetResult())
	}
	// THE BIND STOPS ONE SHIM AND BRINGS ITS SUCCESSOR UP, so the assertion
	// reads the shim's DURABLE request log rather than a control socket whose
	// process the swap replaces underneath it. It keys on the CHOSEN
	// conversation's id because the workspace's opening StartSession is already
	// logged under the same verb.
	req := &shimv1.StartSessionRequest{}
	f.d.AwaitShimLoggedRequestMatching(f.repo.Dir, harness.RPCStartSession,
		"the StartSession the bind itself ran", req,
		func() bool { return req.GetResume().GetVendorSessionId() == "conversation-b" })

	if req.GetResume().GetRebind() == nil {
		t.Fatalf("the bind's start = %v, want the resume marked a REBIND: without the marker the"+
			" shim keeps the workspace's persisted book and replays the conversation the user replaced",
			req.GetResume())
	}
}

// TestARestartIsAPlainResumeAndLeavesTheWorkspacesBookWhereItIs is the other
// half of the rule: the marker moves the shim's book, so a start that is NOT a
// bind must never carry it. A restart resumes the conversation the workspace is
// already on, and a rotated resume handle adopted as the book there would
// orphan every record filed under the name it rotated away from.
func TestARestartIsAPlainResumeAndLeavesTheWorkspacesBookWhereItIs(t *testing.T) {
	// Arrange: an opened workspace with a transcript on disk, so its restart
	// resumes rather than coming up fresh.
	f := newOpened(t, harness.Opts{})
	if req := f.shim.ExpectStartSession(); req.GetFresh() == nil {
		t.Fatalf("the opening StartSession = %v, want fresh", req)
	}
	seedVendorTranscript(t, f, f.shim.Info().VendorSessionID)

	// Act.
	resp, err := f.d.Client().RestartWorkspace(f.d.Ctx(),
		connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: f.ws}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("RestartWorkspace = (%v, %v), want a success", resp, err)
	}

	// Assert.
	req := &shimv1.StartSessionRequest{}
	f.d.AwaitShimLoggedRequestMatching(f.repo.Dir, harness.RPCStartSession,
		"the restart's own StartSession", req,
		func() bool { return req.GetResume() != nil })

	if rebind := req.GetResume().GetRebind(); rebind != nil {
		t.Fatalf("the restart's resume.rebind = %v, want the workspace's book left where it is", rebind)
	}
}

// seedVendorTranscript writes the on-disk vendor transcript a conversation that
// really ran would have left in the workspace's project directory. The source
// classifier probes for it, and a conversation with no file is brought up FRESH
// however the record names it.
func seedVendorTranscript(t *testing.T, f *fixture, vendorSessionID string) {
	t.Helper()
	project := harness.ProjectDir(f.d.DefaultConfigDir, f.ws.GetDir())
	if err := os.MkdirAll(project, 0o755); err != nil {
		t.Fatalf("mkdir the workspace's project dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(project, vendorSessionID+".jsonl"),
		[]byte(`{"type":"summary"}`+"\n"), 0o644); err != nil {
		t.Fatalf("seed the transcript for %q: %v", vendorSessionID, err)
	}
}
