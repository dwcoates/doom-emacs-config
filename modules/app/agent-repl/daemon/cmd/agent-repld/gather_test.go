package main

import (
	"errors"
	"reflect"
	"strings"
	"testing"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

func TestGatherSendsTheCallersQuestionToEveryNamedTarget(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "gather", "-workspaces", "a, b,,c", "-prompt", "what is your status?", "-as-user")

	// Assert
	req := only[*agentreplv1.GatherWorkspacesRequest](t, f)
	if code != exitSuccess || req.GetCaller().GetId() != "ws-caller" || !reflect.DeepEqual(req.GetTargetNames(), []string{"a", "b", "c"}) ||
		req.GetSaid().GetContent().GetBlocks()[0].GetText().GetText() != "what is your status?" || !req.GetAsUser() || req.GetIdempotencyKey() == "" {
		t.Fatalf("exit %d, request %v; want the calling workspace gathering from a, b and c as the user asked", code, req)
	}
}

func TestGatherIsMalformedWithoutTargetsOrAPrompt(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{"no targets", []string{"-prompt", "status?"}},
		{"only blank targets", []string{"-workspaces", " , ", "-prompt", "status?"}},
		{"no prompt", []string{"-workspaces", "a"}},
		{"a positional", []string{"-workspaces", "a", "-prompt", "status?", "extra"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"gather"}, tc.args...)...)

			// Assert
			if code != exitFailure || len(f.client.requests) != 0 {
				t.Fatalf("exit %d, requests %v; want a usage failure and nothing sent", code, f.client.requests)
			}
		})
	}
}

func TestGatherSaysEachRefusedTargetOfAGatherItMade(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.gathered = &agentreplv1.GatherWorkspacesResponse{Result: &agentreplv1.GatherWorkspacesResponse_Success{Success: &agentreplv1.GatherWorkspacesSuccess{
		Gather:   &agentreplv1.GatherRef{Id: "g-1"},
		Accepted: []*agentreplv1.GatherTarget{{Name: "a"}},
		Refused:  []*agentreplv1.GatherRefusedTarget{{Name: "b", Cause: &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_Closed{Closed: &agentreplv1.GatherRefusalClosed{}}}}},
	}}}

	// Act
	code := f.run(workspaceNoun, "gather", "-workspaces", "a,b", "-prompt", "status?")

	// Assert
	if code != exitSuccess || !strings.Contains(f.out.String(), "g-1") || !strings.Contains(f.errOut.String(), "b is closed, so it is not gathered from") {
		t.Fatalf("exit %d, stdout %q, stderr %q; want the gather printed and b's refusal said", code, f.out.String(), f.errOut.String())
	}
}

func TestGatherThatMadeNoGatherExitsRefused(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.gathered = &agentreplv1.GatherWorkspacesResponse{Result: &agentreplv1.GatherWorkspacesResponse_Error{Error: &agentreplv1.GatherWorkspacesError{
		Cause: &agentreplv1.GatherWorkspacesError_CallerClosed{CallerClosed: &agentreplv1.GatherWorkspacesCallerClosed{}}}}}

	// Act
	code := f.run(workspaceNoun, "gather", "-workspaces", "a", "-prompt", "status?")

	// Assert
	if code != exitRefused || !strings.Contains(f.errOut.String(), "this workspace is closed") {
		t.Fatalf("exit %d, stderr %q; want the refusal said", code, f.errOut.String())
	}
}

func TestGatherStatusReadsTheNamedGather(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "gather-status", "g-1")

	// Assert
	req := only[*agentreplv1.GetGatherStatusRequest](t, f)
	if code != exitSuccess || req.GetGather().GetId() != "g-1" {
		t.Fatalf("exit %d, request %v; want g-1 read", code, req)
	}
}

func TestGatherStatusIsMalformedWithoutOneGather(t *testing.T) {
	for _, args := range [][]string{{}, {"g-1", "g-2"}, {" "}} {
		t.Run(strings.Join(args, ","), func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"gather-status"}, args...)...)

			// Assert
			if code != exitFailure || len(f.client.requests) != 0 {
				t.Fatalf("exit %d, requests %v; want a usage failure", code, f.client.requests)
			}
		})
	}
}

var gatherRefusalCases = []struct {
	name string
	err  *agentreplv1.GatherWorkspacesError
	want string
}{
	{name: "unknown_caller", err: &agentreplv1.GatherWorkspacesError{Cause: &agentreplv1.GatherWorkspacesError_UnknownCaller{UnknownCaller: &agentreplv1.GatherWorkspacesUnknownCaller{}}},
		want: "this workspace is unknown to the daemon, so it cannot gather"},
	{name: "caller_closed", err: &agentreplv1.GatherWorkspacesError{Cause: &agentreplv1.GatherWorkspacesError_CallerClosed{CallerClosed: &agentreplv1.GatherWorkspacesCallerClosed{}}},
		want: "this workspace is closed, so no answer could reach it"},
	{name: "none_accepted", err: &agentreplv1.GatherWorkspacesError{Cause: &agentreplv1.GatherWorkspacesError_NoneAccepted{NoneAccepted: &agentreplv1.GatherWorkspacesNoneAccepted{
		Refused: []*agentreplv1.GatherRefusedTarget{
			{Name: "a", Cause: &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_Closed{Closed: &agentreplv1.GatherRefusalClosed{}}}},
			{Name: "b", Cause: &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_Merging{Merging: &agentreplv1.GatherRefusalMerging{}}}},
		}}}},
		want: "no target took the prompt, so no gather was made: a is closed, so it is not gathered from; b is being merged, so it is not gathered from"},
}

func TestDescribeGatherRefusalSaysEachArm(t *testing.T) {
	for _, tt := range gatherRefusalCases {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := describeGatherRefusal(tt.err)

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("describeGatherRefusal = %q, %v; want %q", got, err, tt.want)
			}
		})
	}
}

func TestDescribeGatherRefusalCoversEveryArm(t *testing.T) {
	// Arrange
	var cased []proto.Message
	for _, tt := range gatherRefusalCases {
		cased = append(cased, tt.err)
	}

	// Act / Assert
	for _, name := range uncasedArms(&agentreplv1.GatherWorkspacesError{}, "cause", cased) {
		t.Errorf("GatherWorkspacesError arm %s has no case: give describeGatherRefusal a sentence for it and a case here", name)
	}
}

var gatherTargetRefusalCases = []struct {
	name  string
	cause *agentreplv1.GatherRefusal
	want  string
}{
	{name: "unknown_workspace", cause: &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_UnknownWorkspace{UnknownWorkspace: &agentreplv1.GatherRefusalUnknownWorkspace{}}},
		want: sayNoWorkspace("peer")},
	{name: "closed", cause: &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_Closed{Closed: &agentreplv1.GatherRefusalClosed{}}},
		want: "peer is closed, so it is not gathered from"},
	{name: "is_caller", cause: &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_IsCaller{IsCaller: &agentreplv1.GatherRefusalIsCaller{}}},
		want: "peer is this workspace, which never gathers from itself"},
	{name: "needs_input", cause: &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_NeedsInput{NeedsInput: &agentreplv1.GatherRefusalNeedsInput{}}},
		want: "peer has a question or permission waiting on the user, so it is not gathered from"},
	{name: "merging", cause: &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_Merging{Merging: &agentreplv1.GatherRefusalMerging{}}},
		want: "peer is being merged, so it is not gathered from"},
	{name: "ambiguous_name", cause: &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_AmbiguousName{AmbiguousName: &agentreplv1.GatherRefusalAmbiguousName{
		Workspaces: []*workspacev1.WorkspaceRef{{Id: "1", Dir: "/w/one"}, {Id: "2", Dir: "/w/two"}}}}},
		want: "several open workspaces are named peer (/w/one, /w/two), so it is not gathered from"},
	{name: "prompt_refused", cause: &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_PromptRefused{PromptRefused: &agentreplv1.SubmitPromptError{
		Reason: &agentreplv1.SubmitPromptError_NoSession{NoSession: &agentreplv1.SubmitPromptNoSession{}}}}},
		want: "peer has no live session to deliver to"},
}

func TestDescribeGatherTargetRefusalSaysEachArm(t *testing.T) {
	for _, tt := range gatherTargetRefusalCases {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := describeGatherTargetRefusal(&agentreplv1.GatherRefusedTarget{Name: "peer", Cause: tt.cause})

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("describeGatherTargetRefusal = %q, %v; want %q", got, err, tt.want)
			}
		})
	}
}

func TestDescribeGatherTargetRefusalCoversEveryArm(t *testing.T) {
	// Arrange
	var cased []proto.Message
	for _, tt := range gatherTargetRefusalCases {
		cased = append(cased, tt.cause)
	}

	// Act / Assert
	for _, name := range uncasedArms(&agentreplv1.GatherRefusal{}, "cause", cased) {
		t.Errorf("GatherRefusal arm %s has no case: give describeGatherTargetRefusal a sentence for it and a case here", name)
	}
}

func TestDescribeGatherStatusRefusalSaysEachArm(t *testing.T) {
	// Arrange
	unknown := &agentreplv1.GetGatherStatusError{Cause: &agentreplv1.GetGatherStatusError_UnknownGather{UnknownGather: &agentreplv1.GetGatherStatusUnknownGather{}}}

	// Act
	got, err := describeGatherStatusRefusal("g-1", unknown)

	// Assert
	if err != nil || got != "the daemon has no gather g-1" {
		t.Fatalf("describeGatherStatusRefusal = %q, %v", got, err)
	}
	for _, name := range uncasedArms(&agentreplv1.GetGatherStatusError{}, "cause", []proto.Message{unknown}) {
		t.Errorf("GetGatherStatusError arm %s has no case", name)
	}
}

func TestADescribedGatherRefusalWithNoCauseIsABug(t *testing.T) {
	tests := []struct {
		name     string
		describe func() (string, error)
	}{
		{"the gather's error", func() (string, error) { return describeGatherRefusal(&agentreplv1.GatherWorkspacesError{}) }},
		{"a target's refusal", func() (string, error) {
			return describeGatherTargetRefusal(&agentreplv1.GatherRefusedTarget{Name: "peer", Cause: &agentreplv1.GatherRefusal{}})
		}},
		{"the status read's error", func() (string, error) { return describeGatherStatusRefusal("g", &agentreplv1.GetGatherStatusError{}) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := tt.describe()

			// Assert
			if !errors.Is(err, errUndescribedGatherRefusal) || got != "" {
				t.Fatalf("describe = %q, %v; want errUndescribedGatherRefusal", got, err)
			}
		})
	}
}

func TestGatherTargetNames(t *testing.T) {
	tests := []struct {
		flag string
		want []string
	}{
		{"", nil},
		{"a", []string{"a"}},
		{" a , b ,, c ", []string{"a", "b", "c"}},
	}
	for _, tt := range tests {
		t.Run(tt.flag, func(t *testing.T) {
			// Act, Assert
			if got := gatherTargetNames(tt.flag); !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("gatherTargetNames(%q) = %v, want %v", tt.flag, got, tt.want)
			}
		})
	}
}
