package server

import (
	"context"
	"strings"
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

// TestAPresentDeliveryMustNameOneTheDaemonHonors pins SubmitPromptRequest's
// optional delivery: absent is the ordinary delivery, UNSPECIFIED is never
// sent, and an unknown value is never read as the ordinary delivery.
func TestAPresentDeliveryMustNameOneTheDaemonHonors(t *testing.T) {
	tests := []struct {
		name     string
		delivery agentreplv1.SubmitPromptDelivery
	}{
		{name: "UNSPECIFIED is never sent", delivery: agentreplv1.SubmitPromptDelivery_SUBMIT_PROMPT_DELIVERY_UNSPECIFIED},
		{name: "an unknown value is refused", delivery: agentreplv1.SubmitPromptDelivery(99)},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			req := submitRequest()
			req.Delivery = &tc.delivery

			// Act.
			_, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(req))

			// Assert.
			if code := connectCode(t, err); code != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", code)
			}
		})
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

// TestBlankOperatorDrainNoteIsInvalidArgument pins drain_reason.proto's own
// rule: the operator arm's note is REQUIRED NON-BLANK. An operator reason with
// nothing to say is `maintenance`, and the daemon does not accept one dressed
// as the other.
func TestBlankOperatorDrainNoteIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.UpdateShutdownSchedule(context.Background(),
		connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
			Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{
				Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{
					AtMs: 1,
					Reason: &agentreplv1.DrainReason{
						Kind: &agentreplv1.DrainReason_Operator{
							Operator: &agentreplv1.DrainReasonOperator{Note: "   "},
						},
					},
				},
			},
		}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}

// TestOperatorDrainNoteIsAccepted pins that a real note passes: the presence
// check must not refuse the arm it exists to protect.
func TestOperatorDrainNoteIsAccepted(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.UpdateShutdownSchedule(context.Background(),
		connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
			Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{
				Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{
					AtMs: 1,
					Reason: &agentreplv1.DrainReason{
						Kind: &agentreplv1.DrainReason_Operator{
							Operator: &agentreplv1.DrainReasonOperator{Note: "rebooting the host"},
						},
					},
				},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateShutdownSchedule with a real operator note = %v, want a success", err)
	}
}

func TestEditHeldPromptRefusesAMalformedRequest(t *testing.T) {
	tests := []struct {
		name string
		req  func() *agentreplv1.EditHeldPromptRequest
	}{
		{"no turn", func() *agentreplv1.EditHeldPromptRequest {
			req := editRequest("begin")
			req.Turn = nil
			return req
		}},
		{"a blank turn", func() *agentreplv1.EditHeldPromptRequest {
			req := editRequest("begin")
			req.Turn = &conversationv1.TurnId{}
			return req
		}},
		{"no step", func() *agentreplv1.EditHeldPromptRequest {
			req := editRequest("begin")
			req.Action = nil
			return req
		}},
		{"a commit with no content", func() *agentreplv1.EditHeldPromptRequest {
			req := editRequest("begin")
			req.Action = &agentreplv1.EditHeldPromptRequest_Commit{Commit: &agentreplv1.EditHeldPromptCommit{}}
			return req
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.EditHeldPrompt(context.Background(), connect.NewRequest(tt.req()))

			// Assert.
			if code := connectCode(t, err); code != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", code)
			}
		})
	}
}

func TestEachRequestValidatorNamesTheFieldItRefusesWithTargetsAndSources(t *testing.T) {
	tests := []struct {
		name  string
		check func() *connect.Error
		field string
	}{
		{
			name: "command support with no workspace",
			check: func() *connect.Error {
				return validateRequestCommandSupportRequest(&agentreplv1.RequestCommandSupportRequest{})
			},
			field: "workspace",
		},
		{
			name: "command support with no command",
			check: func() *connect.Error {
				return validateRequestCommandSupportRequest(&agentreplv1.RequestCommandSupportRequest{Workspace: ref()})
			},
			field: "command",
		},
		{
			name: "register with no dir",
			check: func() *connect.Error {
				return validateRegisterWorkspaceRequest(&agentreplv1.RegisterWorkspaceRequest{})
			},
			field: "dir",
		},
		{
			name: "set priority with no workspace",
			check: func() *connect.Error {
				return validateSetWorkspacePriorityRequest(&agentreplv1.SetWorkspacePriorityRequest{})
			},
			field: "workspace",
		},
		{
			name: "set priority with a priority that sets no level arm",
			check: func() *connect.Error {
				return validateSetWorkspacePriorityRequest(&agentreplv1.SetWorkspacePriorityRequest{
					Workspace: ref(),
					Priority:  &agentreplv1.WorkspacePriority{},
				})
			},
			field: "priority.level",
		},
		{
			name:  "open external with no workspace",
			check: func() *connect.Error { return validateOpenExternalRequest(&agentreplv1.OpenExternalRequest{}) },
			field: "workspace",
		},
		{
			name: "open external with no url",
			check: func() *connect.Error {
				return validateOpenExternalRequest(&agentreplv1.OpenExternalRequest{Workspace: ref()})
			},
			field: "url",
		},
		{
			name:  "open in editor with no workspace",
			check: func() *connect.Error { return validateOpenInEditorRequest(&agentreplv1.OpenInEditorRequest{}) },
			field: "workspace",
		},
		{
			name: "open in editor with no target",
			check: func() *connect.Error {
				return validateOpenInEditorRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref()})
			},
			field: "target",
		},
		{
			name: "open in editor with an empty workspace file",
			check: func() *connect.Error {
				return validateOpenInEditorRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
					Target: &agentreplv1.OpenInEditorRequest_WorkspaceFile{WorkspaceFile: &agentreplv1.OpenInEditorWorkspaceFile{}}})
			},
			field: "workspace_file.path",
		},
		{
			name: "open in editor with an empty test log token",
			check: func() *connect.Error {
				return validateOpenInEditorRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
					Target: &agentreplv1.OpenInEditorRequest_MergeTestLog{MergeTestLog: &frontendv1.FeedMergeTestLogToken{}}})
			},
			field: "merge_test_log.value",
		},
		{
			name:  "merge with no workspace",
			check: func() *connect.Error { return validateMergeWorkspaceRequest(&agentreplv1.MergeWorkspaceRequest{}) },
			field: "workspace",
		},
		{
			name: "merge with no source",
			check: func() *connect.Error {
				return validateMergeWorkspaceRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ref()})
			},
			field: "source",
		},
		{
			name: "merge of a branch with no name",
			check: func() *connect.Error {
				return validateMergeWorkspaceRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ref(),
					Source: &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_Branch{Branch: &agentreplv1.MergeWorkspaceSourceBranch{}}}})
			},
			field: "source.branch.name",
		},
		{
			name: "merge of another workspace with no ref",
			check: func() *connect.Error {
				return validateMergeWorkspaceRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ref(),
					Source: &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_Workspace{Workspace: &agentreplv1.MergeWorkspaceSourceWorkspace{}}}})
			},
			field: "source.workspace.ref",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			err := tc.check()

			// Assert.
			if err == nil {
				t.Fatal("the validator accepted a request missing a required field")
			}
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", connect.CodeOf(err))
			}
			if !strings.HasPrefix(err.Message(), tc.field+":") {
				t.Fatalf("message = %q, want it to name the field %q", err.Message(), tc.field)
			}
		})
	}
}

func TestEachRequestValidatorAcceptsAWellFormedRequestWithTargetsAndSources(t *testing.T) {
	tests := []struct {
		name  string
		check func() *connect.Error
	}{
		{
			name: "command support",
			check: func() *connect.Error {
				return validateRequestCommandSupportRequest(&agentreplv1.RequestCommandSupportRequest{
					Workspace: ref(), Command: "/merge",
				})
			},
		},
		{
			name: "register",
			check: func() *connect.Error {
				return validateRegisterWorkspaceRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: testWorkspaceDir})
			},
		},
		{
			// An UNSET priority is the CLEAR spelling, which is legal.
			name: "set priority clearing the priority",
			check: func() *connect.Error {
				return validateSetWorkspacePriorityRequest(&agentreplv1.SetWorkspacePriorityRequest{Workspace: ref()})
			},
		},
		{
			name: "open external",
			check: func() *connect.Error {
				return validateOpenExternalRequest(&agentreplv1.OpenExternalRequest{
					Workspace: ref(), Url: "https://example.invalid",
				})
			},
		},
		{
			name: "open in editor of a workspace file",
			check: func() *connect.Error {
				return validateOpenInEditorRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
					Target: &agentreplv1.OpenInEditorRequest_WorkspaceFile{WorkspaceFile: &agentreplv1.OpenInEditorWorkspaceFile{Path: "/tmp/a.go"}}})
			},
		},
		{
			name: "open in editor of a test log",
			check: func() *connect.Error {
				return validateOpenInEditorRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
					Target: &agentreplv1.OpenInEditorRequest_MergeTestLog{MergeTestLog: &frontendv1.FeedMergeTestLogToken{Value: "lease/1"}}})
			},
		},
		{
			name: "merge of the own branch",
			check: func() *connect.Error {
				return validateMergeWorkspaceRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ref(),
					Source: &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_OwnBranch{OwnBranch: &agentreplv1.MergeWorkspaceSourceOwnBranch{}}}})
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			err := tc.check()

			// Assert.
			if err != nil {
				t.Fatalf("a well-formed request was refused: %v", err)
			}
		})
	}
}
