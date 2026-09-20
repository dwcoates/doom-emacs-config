//go:build integration

package integration

import (
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
