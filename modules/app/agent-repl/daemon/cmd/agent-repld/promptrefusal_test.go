package main

import (
	"errors"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"google.golang.org/protobuf/reflect/protoreflect"
)

// promptRefusalCase is one SubmitPromptError arm and the sentence the prompt
// verb says for it.
type promptRefusalCase struct {
	name string
	err  *agentreplv1.SubmitPromptError
	want string
}

// promptRefusalCases holds one case per SubmitPromptError arm, and one per
// bubble_refused kind.
var promptRefusalCases = []promptRefusalCase{
	{name: "merging", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_Merging{Merging: &agentreplv1.SubmitPromptRefusedMerging{}}},
		want: "peer is being merged; send the prompt again once the merge resolves"},
	{name: "unknown_workspace", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.SubmitPromptUnknownWorkspace{}}},
		want: "the daemon has no workspace peer"},
	{name: "workspace_ref_mismatch", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_WorkspaceRefMismatch{WorkspaceRefMismatch: &agentreplv1.SubmitPromptWorkspaceRefMismatch{RegistryDir: "/w/peer"}}},
		want: "peer lives at /w/peer, not at the directory the prompt named"},
	{name: "transferring_away", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_TransferringAway{TransferringAway: &agentreplv1.SubmitPromptTransferringAway{Address: "127.0.0.1:9"}}},
		want: "the daemon is restarting and handed peer to its successor at 127.0.0.1:9; send the prompt again"},
	{name: "not_yet_adopted", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_NotYetAdopted{NotYetAdopted: &agentreplv1.SubmitPromptNotYetAdopted{}}},
		want: "the daemon is restarting and has not taken peer over yet; send the prompt again once the restart finishes"},
	{name: "feed_not_in_workspace", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_FeedNotInWorkspace{FeedNotInWorkspace: &agentreplv1.SubmitPromptFeedNotInWorkspace{}}},
		want: "the prompt named a feed that is not peer's"},
	{name: "feed_undecodable", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_FeedUndecodable{FeedUndecodable: &agentreplv1.SubmitPromptFeedUndecodable{}}},
		want: "the prompt named a feed the daemon cannot read"},
	{name: "no_session", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_NoSession{NoSession: &agentreplv1.SubmitPromptNoSession{}}},
		want: "peer has no live session to deliver to"},
	{name: "duplicate_submission", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_DuplicateSubmission{DuplicateSubmission: &agentreplv1.SubmitPromptDuplicateSubmission{}}},
		want: "peer already took this prompt, so it was not sent twice"},
	{name: "bubble_refused", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_BubbleRefused{BubbleRefused: &agentreplv1.SubmitPromptBubbleRefused{
		Detail: "no route", Kind: &agentreplv1.SubmitPromptBubbleRefused_NotDeliverable{NotDeliverable: &agentreplv1.SubmitPromptBubbleNotDeliverable{}}}}},
		want: "the subagent the prompt addressed in peer cannot be prompted directly (no route)"},
	{name: "bubble_refused agent_busy", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_BubbleRefused{BubbleRefused: &agentreplv1.SubmitPromptBubbleRefused{
		Detail: "turn running", Kind: &agentreplv1.SubmitPromptBubbleRefused_AgentBusy{AgentBusy: &agentreplv1.SubmitPromptBubbleAgentBusy{}}}}},
		want: "the subagent the prompt addressed in peer is already running a turn (turn running)"},
	{name: "cold_gate", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_ColdGate{ColdGate: &agentreplv1.SubmitPromptColdGate{Detail: "the conversation is cold at 571,958 context tokens"}}},
		want: "peer is parked at its cold gate (the conversation is cold at 571,958 context tokens); the user must answer the gate before it can take a prompt"},
	{name: "model_not_in_catalog", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_ModelNotInCatalog{ModelNotInCatalog: &agentreplv1.SubmitPromptModelNotInCatalog{Detail: "no model x"}}},
		want: "peer's session has no such model, so nothing was sent (no model x)"},
	{name: "model_refused", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_ModelRefused{ModelRefused: &agentreplv1.SubmitPromptModelRefused{Detail: "denied"}}},
		want: "the vendor refused peer's model change, so nothing was sent (denied)"},
	{name: "ask_not_standing", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_AskNotStanding{AskNotStanding: &agentreplv1.SubmitPromptAskNotStanding{}}},
		want: "the question this answer was for in peer was already settled, so nothing was sent"},
	{name: "slash_command_from_agent", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_SlashCommandFromAgent{SlashCommandFromAgent: &agentreplv1.SubmitPromptSlashCommandFromAgent{Command: "/clear"}}},
		want: "an agent may not send peer a slash command (/clear); only the user runs those"},
	{name: "unknown_sender", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_UnknownSender{UnknownSender: &agentreplv1.SubmitPromptUnknownSender{SentBy: &workspacev1.WorkspaceRef{Id: "ws-x"}}}},
		want: "the sending workspace is unknown to the daemon or is peer itself, and a workspace cannot prompt itself"},
	{name: "thread_awaiting_first_answer", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_ThreadAwaitingFirstAnswer{ThreadAwaitingFirstAnswer: &agentreplv1.SubmitPromptThreadAwaitingFirstAnswer{}}},
		want: "peer's chat thread is still answering its first question; ask again once that answer lands"},
	{name: "fan_out_refused", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_FanOutRefused{FanOutRefused: &agentreplv1.SubmitPromptFanOutRefused{Cap: 3, WindowMinutes: 60, Detail: "cap"}}},
		want: "this workspace already prompted 3 unrelated workspaces in the last hour; send with -as-user only when the user asked for this send"},
}

func TestDescribePromptRefusalSaysEachArm(t *testing.T) {
	for _, tt := range promptRefusalCases {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := describePromptRefusal("peer", tt.err)

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("describePromptRefusal = %q, %v; want %q", got, err, tt.want)
			}
		})
	}
}

// casedArms answers the set arm's name of every case, and the set kind's name
// of every bubble_refused case.
func casedArms() (arms, kinds map[protoreflect.Name]bool) {
	arms, kinds = map[protoreflect.Name]bool{}, map[protoreflect.Name]bool{}
	reason := (&agentreplv1.SubmitPromptError{}).ProtoReflect().Descriptor().Oneofs().ByName("reason")
	kind := (&agentreplv1.SubmitPromptBubbleRefused{}).ProtoReflect().Descriptor().Oneofs().ByName("kind")
	for _, tt := range promptRefusalCases {
		if set := tt.err.ProtoReflect().WhichOneof(reason); set != nil {
			arms[set.Name()] = true
		}
		if b := tt.err.GetBubbleRefused(); b != nil {
			if set := b.ProtoReflect().WhichOneof(kind); set != nil {
				kinds[set.Name()] = true
			}
		}
	}
	return arms, kinds
}

func TestDescribePromptRefusalCoversEveryArm(t *testing.T) {
	// Arrange
	arms, _ := casedArms()
	fields := (&agentreplv1.SubmitPromptError{}).ProtoReflect().Descriptor().Oneofs().ByName("reason").Fields()

	// Act / Assert
	for i := range fields.Len() {
		if name := fields.Get(i).Name(); !arms[name] {
			t.Errorf("SubmitPromptError arm %s has no case: give describePromptRefusal a sentence for it and a case here", name)
		}
	}
}

func TestDescribePromptRefusalCoversEveryBubbleKind(t *testing.T) {
	// Arrange
	_, kinds := casedArms()
	fields := (&agentreplv1.SubmitPromptBubbleRefused{}).ProtoReflect().Descriptor().Oneofs().ByName("kind").Fields()

	// Act / Assert
	for i := range fields.Len() {
		if name := fields.Get(i).Name(); !kinds[name] {
			t.Errorf("SubmitPromptBubbleRefused kind %s has no case: give describePromptRefusal a sentence for it and a case here", name)
		}
	}
}

func TestDescribePromptRefusalWithNoSentenceIsABug(t *testing.T) {
	tests := []struct {
		name string
		err  *agentreplv1.SubmitPromptError
	}{
		{name: "no reason", err: &agentreplv1.SubmitPromptError{}},
		{name: "bubble_refused with no kind", err: &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_BubbleRefused{BubbleRefused: &agentreplv1.SubmitPromptBubbleRefused{Detail: "d"}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := describePromptRefusal("peer", tt.err)

			// Assert
			if !errors.Is(err, errUndescribedRefusal) || got != "" {
				t.Fatalf("describePromptRefusal = %q, %v; want errUndescribedRefusal", got, err)
			}
		})
	}
}

func TestWindowPhrase(t *testing.T) {
	tests := []struct {
		minutes uint32
		want    string
	}{
		{minutes: 60, want: "hour"},
		{minutes: 120, want: "2 hours"},
		{minutes: 1, want: "minute"},
		{minutes: 45, want: "45 minutes"},
		{minutes: 90, want: "90 minutes"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act
			got := windowPhrase(tt.minutes)

			// Assert
			if got != tt.want {
				t.Fatalf("windowPhrase(%d) = %q; want %q", tt.minutes, got, tt.want)
			}
		})
	}
}

func TestPromptRefusalSaysWhyOnStderr(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.prompted = &agentreplv1.SubmitPromptResponse{Result: &agentreplv1.SubmitPromptResponse_Error{Error: &agentreplv1.SubmitPromptError{
		Reason: &agentreplv1.SubmitPromptError_NoSession{NoSession: &agentreplv1.SubmitPromptNoSession{}}}}}

	// Act
	code := f.run(workspaceNoun, "prompt", "peer", "please rebase")

	// Assert
	if code != exitRefused || !strings.Contains(f.out.String(), "noSession") ||
		f.errOut.String() != "agent-repld workspace prompt: peer has no live session to deliver to\n" {
		t.Fatalf("exit %d, stdout %q, stderr %q; want refused, the arm on stdout, its sentence alone on stderr", code, f.out.String(), f.errOut.String())
	}
}

func TestPromptRefusalWithNoSentenceReportsTheBug(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.prompted = &agentreplv1.SubmitPromptResponse{Result: &agentreplv1.SubmitPromptResponse_Error{Error: &agentreplv1.SubmitPromptError{}}}

	// Act
	code := f.run(workspaceNoun, "prompt", "peer", "please rebase")

	// Assert
	if code != exitRefused || !strings.Contains(f.errOut.String(), errUndescribedRefusal.Error()) {
		t.Fatalf("exit %d, stderr %q; want refused, naming the missing sentence", code, f.errOut.String())
	}
}
