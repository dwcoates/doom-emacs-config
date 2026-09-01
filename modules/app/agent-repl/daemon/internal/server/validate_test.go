package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// said is one well-formed composed prompt.
func said(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{
		Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{{
				Block: &conversationv1.UserContentBlock_Text{
					Text: &conversationv1.TextBlock{Text: text},
				},
			}},
		},
	}
}

// TestUnsetWorkspaceRefIsInvalidArgument pins that an unset non-optional
// message-typed field is refused AT ONCE, naming the field.
func TestUnsetWorkspaceRefIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}

// TestEmptyWorkspaceIDIsInvalidArgument pins that `id` is the sole identifier: a
// ref with only a dir identifies nothing.
func TestEmptyWorkspaceIDIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{
			Workspace: &workspacev1.WorkspaceRef{Dir: testWorkspaceDir},
		}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}

// TestUnsetOneofIsInvalidArgument pins that an unset oneof is an error rather
// than a defaulted arm.
func TestUnsetOneofIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{Workspace: ref()}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}

// TestUnspecifiedPromptOriginIsInvalidArgument pins that SubmitPrompt's origin
// is REQUIRED: the UNSPECIFIED enum value is not a value.
func TestUnspecifiedPromptOriginIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.SubmitPrompt(context.Background(),
		connect.NewRequest(&agentreplv1.SubmitPromptRequest{
			Workspace:      ref(),
			Said:           said("hello"),
			IdempotencyKey: "k1",
		}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}

// TestEmptyPromptBlocksIsInvalidArgument pins that a prompt with no blocks is
// refused: a submission that says nothing is not a submission.
func TestEmptyPromptBlocksIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.SubmitPrompt(context.Background(),
		connect.NewRequest(&agentreplv1.SubmitPromptRequest{
			Workspace:      ref(),
			Said:           &conversationv1.UserSaid{Content: &conversationv1.UserContent{}},
			IdempotencyKey: "k1",
			Origin:         conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}

// TestEmptyFeedIDValueIsInvalidArgument pins that a present-but-empty FeedId is
// a validation failure rather than an undecodable-feed refusal.
func TestEmptyFeedIDValueIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.OpenFeed(context.Background(),
		connect.NewRequest(&agentreplv1.OpenFeedRequest{
			Workspace: ref(),
			Feed:      &frontendv1.FeedId{},
		}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}

// TestUnsetDrainReasonIsInvalidArgument pins that a scheduled drain names its
// reason: the daemon never guesses one.
func TestUnsetDrainReasonIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.UpdateShutdownSchedule(context.Background(),
		connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
			Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{
				Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{AtMs: 1},
			},
		}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}

// TestRepositoryRefNamingNothingIsInvalidArgument pins that a ref naming
// neither an id nor a dir identifies nothing.
func TestRepositoryRefNamingNothingIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.CreateWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
			Repository: &workspacev1.RepositoryRef{},
		}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}

// TestUnsetClientLogLevelIsInvalidArgument pins that a client record names its
// level.
func TestUnsetClientLogLevelIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.ClientLog(context.Background(),
		connect.NewRequest(&agentreplv1.ClientLogRequest{
			Workspace: ref(),
			Record:    &agentreplv1.ClientLogRecord{Operation: "webapp.x", Message: "m"},
		}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}
