package main

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
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
