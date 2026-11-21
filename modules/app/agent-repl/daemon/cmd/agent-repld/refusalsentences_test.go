package main

import (
	"errors"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"google.golang.org/protobuf/types/known/timestamppb"
)

func TestTheSharedRefusalSentences(t *testing.T) {
	tests := []struct {
		name string
		got  string
		want string
	}{
		{name: "no workspace", got: sayNoWorkspace("ws"), want: "the daemon has no workspace ws"},
		{name: "lives elsewhere", got: sayLivesElsewhere("ws", "/w/ws", "the request"), want: "ws lives at /w/ws, not at the directory the request named"},
		{name: "handed over", got: sayHandedOver("ws", "127.0.0.1:9", "finish it again"), want: "the daemon is restarting and handed ws to its successor at 127.0.0.1:9; finish it again"},
		{name: "not yet adopted", got: sayNotYetAdopted("ws", "finish it again"), want: "the daemon is restarting and has not taken ws over yet; finish it again once the restart finishes"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Assert
			if tt.got != tt.want {
				t.Fatalf("sentence = %q, want %q", tt.got, tt.want)
			}
		})
	}
}

// TestEveryDescriberSaysTheUnknownWorkspaceArmOneWay pins that every refusal
// describer says the arm the four share through sayNoWorkspace, so a
// describer that hand-rolls its own wording fails here.
func TestEveryDescriberSaysTheUnknownWorkspaceArmOneWay(t *testing.T) {
	tests := []struct {
		name     string
		describe func() (string, error)
	}{
		{name: "prompt", describe: func() (string, error) {
			return describePromptRefusal("ws", &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.SubmitPromptUnknownWorkspace{}}})
		}},
		{name: "finish", describe: func() (string, error) {
			return describeFinishRefusal("ws", &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.FinishWorkspaceUnknownWorkspace{}}})
		}},
		{name: "one-shot", describe: func() (string, error) {
			return describeOneShotRefusal("ws", &agentreplv1.SetWorkspaceOneShotError{Cause: &agentreplv1.SetWorkspaceOneShotError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.SetWorkspaceOneShotUnknownWorkspace{}}})
		}},
		{name: "retarget", describe: func() (string, error) {
			return describeRetargetRefusal("ws", &agentreplv1.SetWorkspaceParentError{Cause: &agentreplv1.SetWorkspaceParentError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.SetWorkspaceParentUnknownWorkspace{}}})
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := tt.describe()

			// Assert
			if err != nil || got != sayNoWorkspace("ws") {
				t.Fatalf("describe = %q, %v; want %q", got, err, sayNoWorkspace("ws"))
			}
		})
	}
}

func TestTheSharedNestedRefusalSentences(t *testing.T) {
	parent := &workspacev1.WorkspaceRef{Dir: "/w/p"}
	resets := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	tests := []struct {
		name string
		say  func() (string, error)
		want string
	}{
		{name: "open children", say: func() (string, error) {
			return sayOpenChildren("ws", &agentreplv1.WorkspaceHasOpenChildren{Children: []*workspacev1.WorkspaceRef{{}}}, "close it again"), nil
		}, want: "ws has 1 open children and 0 open workspaces landing into it, hidden ones included; close or merge each, then close it again"},
		{name: "parent hidden", say: func() (string, error) {
			return sayParentNotOpen(&agentreplv1.WorkspaceParentNotOpen{Parent: parent, State: &agentreplv1.WorkspaceParentNotOpen_Hidden{Hidden: &agentreplv1.WorkspaceParentNotOpenHidden{}}}, "open it again")
		}, want: "the parent /w/p is hidden; show it, then open it again"},
		{name: "parent closed", say: func() (string, error) {
			return sayParentNotOpen(&agentreplv1.WorkspaceParentNotOpen{Parent: parent, State: &agentreplv1.WorkspaceParentNotOpen_Closed{Closed: &agentreplv1.WorkspaceParentNotOpenClosed{}}}, "open it again")
		}, want: "the parent /w/p is closed; reopen it, then open it again"},
		{name: "parent closing", say: func() (string, error) {
			return sayParentNotOpen(&agentreplv1.WorkspaceParentNotOpen{Parent: parent, State: &agentreplv1.WorkspaceParentNotOpen_Closing{Closing: &agentreplv1.WorkspaceParentNotOpenClosing{}}}, "open it again")
		}, want: "the parent /w/p closes once its queued merge lands, so it takes no open child"},
		{name: "invalid last component", say: func() (string, error) {
			return sayInvalidName(&agentreplv1.CreateWorkspaceInvalidName{Name: "a/B", Component: "B", Rule: &agentreplv1.CreateWorkspaceInvalidName_Slug{Slug: &agentreplv1.CreateWorkspaceInvalidNameSlug{}}})
		}, want: `the name "a/B" breaks the naming rule: its last component "B" is not a slug`},
		{name: "invalid prefix", say: func() (string, error) {
			return sayInvalidName(&agentreplv1.CreateWorkspaceInvalidName{Name: "A/b", Component: "A", Rule: &agentreplv1.CreateWorkspaceInvalidName_Prefix{Prefix: &agentreplv1.CreateWorkspaceInvalidNamePrefix{}}})
		}, want: `the name "A/b" breaks the naming rule: its component "A" before the last is not a prefix`},
		{name: "a name in the daemon's namespace", say: func() (string, error) {
			return sayInvalidName(&agentreplv1.CreateWorkspaceInvalidName{Name: "agent-repl/x", Component: "agent-repl", Rule: &agentreplv1.CreateWorkspaceInvalidName_Reserved{Reserved: &agentreplv1.CreateWorkspaceInvalidNameReserved{}}})
		}, want: `the name "agent-repl/x" lies in the daemon's own branch namespace "agent-repl", which no workspace's name may use`},
		{name: "version unexpected, unversioned", say: func() (string, error) {
			return sayVersionUnexpected(&agentreplv1.MergeWorkspaceVersionIncrementUnexpected{Reason: &agentreplv1.MergeWorkspaceVersionIncrementUnexpected_UnversionedRepository{UnversionedRepository: &agentreplv1.MergeWorkspaceVersionIncrementUnexpectedUnversionedRepository{}}})
		}, want: "its repository's deploy config versions nothing"},
		{name: "version unexpected, not the default branch", say: func() (string, error) {
			return sayVersionUnexpected(&agentreplv1.MergeWorkspaceVersionIncrementUnexpected{Reason: &agentreplv1.MergeWorkspaceVersionIncrementUnexpected_NotDefaultBranch{NotDefaultBranch: &agentreplv1.MergeWorkspaceVersionIncrementUnexpectedNotDefaultBranch{}}})
		}, want: "it lands into a parent workspace's checkout, not the main branch"},
		{name: "github unauthorized", say: func() (string, error) {
			return sayGithubFailure(&agentreplv1.PullRequestGitHubFailure{Cause: &agentreplv1.PullRequestGitHubFailure_Unauthorized{Unauthorized: &agentreplv1.PullRequestGitHubFailureUnauthorized{}}})
		}, want: "GitHub refused the gh login's credentials; log in again with `gh auth login`"},
		{name: "github not found", say: func() (string, error) {
			return sayGithubFailure(&agentreplv1.PullRequestGitHubFailure{Cause: &agentreplv1.PullRequestGitHubFailure_NotFound{NotFound: &agentreplv1.PullRequestGitHubFailureNotFound{}}})
		}, want: "GitHub answered not found: it was deleted, renamed, or never readable to the gh login"},
		{name: "github rate limited until a reset", say: func() (string, error) {
			return sayGithubFailure(&agentreplv1.PullRequestGitHubFailure{Cause: &agentreplv1.PullRequestGitHubFailure_RateLimited{RateLimited: &agentreplv1.PullRequestGitHubFailureRateLimited{ResetsAt: timestamppb.New(resets)}}})
		}, want: "GitHub's rate limit is spent until 2026-11-14T12:00:00Z"},
		{name: "github rate limited with no reset named", say: func() (string, error) {
			return sayGithubFailure(&agentreplv1.PullRequestGitHubFailure{Cause: &agentreplv1.PullRequestGitHubFailure_RateLimited{RateLimited: &agentreplv1.PullRequestGitHubFailureRateLimited{}}})
		}, want: "GitHub's rate limit is spent until it resets"},
		{name: "github unreachable", say: func() (string, error) {
			return sayGithubFailure(&agentreplv1.PullRequestGitHubFailure{Cause: &agentreplv1.PullRequestGitHubFailure_Unreachable{Unreachable: &agentreplv1.PullRequestGitHubFailureUnreachable{Detail: "dns"}}})
		}, want: "GitHub could not be reached, or answered a server error (dns)"},
		{name: "github refused", say: func() (string, error) {
			return sayGithubFailure(&agentreplv1.PullRequestGitHubFailure{Cause: &agentreplv1.PullRequestGitHubFailure_Refused{Refused: &agentreplv1.PullRequestGitHubFailureRefused{HttpStatus: 422, Message: "no"}}})
		}, want: "GitHub refused it with 422 (no)"},
		{name: "subscriber closed", say: func() (string, error) { return saySubscriberClosed("ws"), nil },
			want: "ws is closed: it runs no agent to tell, and a subscription taken while closed would never end"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := tt.say()

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("sentence = %q, %v; want %q", got, err, tt.want)
			}
		})
	}
}

func TestASharedNestedSentenceWithNoChoiceIsABug(t *testing.T) {
	tests := []struct {
		name string
		say  func() (string, error)
	}{
		{name: "parent not open with no state", say: func() (string, error) { return sayParentNotOpen(&agentreplv1.WorkspaceParentNotOpen{}, "again") }},
		{name: "version unexpected with no reason", say: func() (string, error) {
			return sayVersionUnexpected(&agentreplv1.MergeWorkspaceVersionIncrementUnexpected{})
		}},
		{name: "github failure with no cause", say: func() (string, error) { return sayGithubFailure(&agentreplv1.PullRequestGitHubFailure{}) }},
		{name: "no github failure at all", say: func() (string, error) { return sayGithubFailure(nil) }},
		{name: "invalid name with no rule", say: func() (string, error) { return sayInvalidName(&agentreplv1.CreateWorkspaceInvalidName{Name: "x"}) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := tt.say()

			// Assert
			if !errors.Is(err, errUndescribedArm) || got != "" {
				t.Fatalf("sentence = %q, %v; want the undescribed-arm bug", got, err)
			}
		})
	}
}
