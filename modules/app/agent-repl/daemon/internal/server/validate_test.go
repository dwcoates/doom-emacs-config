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

	"agent-repld/internal/workspace"
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
			Source: &agentreplv1.ClientLogRequest_Workspace{Workspace: ref()},
			Record: &agentreplv1.ClientLogRecord{Operation: "webapp.x", Message: "m"},
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
			name: "set hidden with no workspace",
			check: func() *connect.Error {
				return validateSetWorkspaceHiddenRequest(&agentreplv1.SetWorkspaceHiddenRequest{})
			},
			field: "workspace",
		},
		{
			name: "set principal link with no workspace",
			check: func() *connect.Error {
				return validateSetWorkspacePrincipalLinkRequest(&agentreplv1.SetWorkspacePrincipalLinkRequest{})
			},
			field: "workspace",
		},
		{
			name: "set principal link with no change arm",
			check: func() *connect.Error {
				return validateSetWorkspacePrincipalLinkRequest(&agentreplv1.SetWorkspacePrincipalLinkRequest{Workspace: ref()})
			},
			field: "change",
		},
		{
			name: "set principal link with a blank url",
			check: func() *connect.Error {
				return validateSetWorkspacePrincipalLinkRequest(&agentreplv1.SetWorkspacePrincipalLinkRequest{Workspace: ref(),
					Change: &agentreplv1.SetWorkspacePrincipalLinkRequest_Set{Set: &agentreplv1.SetWorkspacePrincipalLinkSet{Url: " "}}})
			},
			field: "set.url",
		},
		{
			name: "swap with no workspace",
			check: func() *connect.Error {
				return validateSwapWorkspacesRequest(&agentreplv1.SwapWorkspacesRequest{
					Target: &workspacev1.WorkspaceRef{Id: "ws-2"},
				})
			},
			field: "workspace",
		},
		{
			name: "set hidden with no visibility arm",
			check: func() *connect.Error {
				return validateSetWorkspaceHiddenRequest(&agentreplv1.SetWorkspaceHiddenRequest{Workspace: ref()})
			},
			field: "visibility",
		},
		{
			name: "set parent with no workspace",
			check: func() *connect.Error {
				return validateSetWorkspaceParentRequest(&agentreplv1.SetWorkspaceParentRequest{})
			},
			field: "workspace",
		},
		{
			name: "set parent with no parent arm",
			check: func() *connect.Error {
				return validateSetWorkspaceParentRequest(&agentreplv1.SetWorkspaceParentRequest{Workspace: ref()})
			},
			field: "parent",
		},
		{
			name: "set parent as a child of no workspace",
			check: func() *connect.Error {
				return validateSetWorkspaceParentRequest(&agentreplv1.SetWorkspaceParentRequest{Workspace: ref(),
					Parent: &agentreplv1.SetWorkspaceParentRequest_ChildOf{ChildOf: &agentreplv1.SetWorkspaceParentChild{}}})
			},
			field: "child_of.parent",
		},
		{
			name: "move with no workspace",
			check: func() *connect.Error {
				return validateMoveWorkspaceRequest(&agentreplv1.MoveWorkspaceRequest{
					Target: &workspacev1.WorkspaceRef{Id: "ws-2"},
				})
			},
			field: "workspace",
		},
		{
			name: "move with no target",
			check: func() *connect.Error {
				return validateMoveWorkspaceRequest(&agentreplv1.MoveWorkspaceRequest{Workspace: ref()})
			},
			field: "target",
		},
		{
			name: "move of a workspace onto its own tab",
			check: func() *connect.Error {
				return validateMoveWorkspaceRequest(&agentreplv1.MoveWorkspaceRequest{
					Workspace: ref(), Target: ref(),
				})
			},
			field: "target",
		},
		{
			name: "swap with no target",
			check: func() *connect.Error {
				return validateSwapWorkspacesRequest(&agentreplv1.SwapWorkspacesRequest{Workspace: ref()})
			},
			field: "target",
		},
		{
			name: "swap of a workspace with itself",
			check: func() *connect.Error {
				return validateSwapWorkspacesRequest(&agentreplv1.SwapWorkspacesRequest{
					Workspace: ref(), Target: ref(),
				})
			},
			field: "target",
		},
		{name: "create with no requester", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validCreate(func(r *agentreplv1.CreateWorkspaceRequest) { r.Requester = nil }))
		}, field: "requester"},
		{name: "create with a requesting workspace naming nothing", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validCreate(func(r *agentreplv1.CreateWorkspaceRequest) {
				r.Requester = workspaceRequester(&workspacev1.WorkspaceRef{})
			}))
		}, field: "requester.workspace.workspace.id"},
		{name: "create with no context origin", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validCreate(func(r *agentreplv1.CreateWorkspaceRequest) { r.ContextOrigin = nil }))
		}, field: "context_origin"},
		{name: "create forking no workspace", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validCreate(func(r *agentreplv1.CreateWorkspaceRequest) {
				r.ContextOrigin = forkContext(&workspacev1.WorkspaceRef{})
			}))
		}, field: "context_origin.fork.workspace.id"},
		{name: "create with no topology", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validCreate(func(r *agentreplv1.CreateWorkspaceRequest) {
				r.Form = &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{}}
			}))
		}, field: "standard.topology"},
		{name: "create of a child naming no owner", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validCreate(func(r *agentreplv1.CreateWorkspaceRequest) {
				r.Form = &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{Topology: childTopology(&workspacev1.WorkspaceRef{}, "")}}
			}))
		}, field: "standard.topology.child.owner.id"},
		{name: "create of a child with a blank owner notice", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validCreate(func(r *agentreplv1.CreateWorkspaceRequest) {
				topology := childTopology(ref(), "")
				topology.GetChild().OwnerNotice = &agentreplv1.WorkspaceTopologyChildOwnerNotice{Text: " "}
				r.Form = &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{Topology: topology}}
			}))
		}, field: "standard.topology.child.owner_notice.text"},
		{name: "create of a one-shot whose context names no workspace", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validCreate(func(r *agentreplv1.CreateWorkspaceRequest) {
				r.Form = &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
					Prompt: workspace.SaidText("fix the feed"), Topology: primaryTopology(), Context: &agentreplv1.CreateWorkspaceOneShotContext{}}}
			}))
		}, field: "one_shot.context.workspace"},
		{name: "an async merge the user asks for", check: func() *connect.Error {
			req := validAsyncMerge(nil)
			req.Requester = userRequester()
			return validateCreateWorkspaceRequest(req)
		}, field: "requester"},
		{name: "an async merge naming a model", check: func() *connect.Error {
			req := validAsyncMerge(nil)
			req.Model = strPtr("opus")
			return validateCreateWorkspaceRequest(req)
		}, field: "model"},
		{name: "an async merge with no name", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validAsyncMerge(func(a *agentreplv1.CreateWorkspaceAsyncMerge) { a.Name = " " }))
		}, field: "async_merge.name"},
		{name: "an async merge with no branch", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validAsyncMerge(func(a *agentreplv1.CreateWorkspaceAsyncMerge) { a.Source.Branch = "" }))
		}, field: "async_merge.source.branch"},
		{name: "an async merge with no maker", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validAsyncMerge(func(a *agentreplv1.CreateWorkspaceAsyncMerge) { a.Source.Maker = nil }))
		}, field: "async_merge.source.maker"},
		{name: "an async merge of a subagent naming none", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validAsyncMerge(func(a *agentreplv1.CreateWorkspaceAsyncMerge) {
				a.Source.Maker = &agentreplv1.CreateWorkspaceAsyncMergeMaker{Maker: &agentreplv1.CreateWorkspaceAsyncMergeMaker_Subagent{Subagent: &agentreplv1.CreateWorkspaceAsyncMergeMakerSubagent{}}}
			}))
		}, field: "async_merge.source.maker.subagent.agent_id"},
		{name: "an async merge with no target", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validAsyncMerge(func(a *agentreplv1.CreateWorkspaceAsyncMerge) { a.Target = nil }))
		}, field: "async_merge.target"},
		{name: "an async merge with a blank instruction", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validAsyncMerge(func(a *agentreplv1.CreateWorkspaceAsyncMerge) { a.Instruction = strPtr(" ") }))
		}, field: "async_merge.instruction"},
		{name: "an async merge closing a workspace with no ref", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validAsyncMerge(func(a *agentreplv1.CreateWorkspaceAsyncMerge) {
				a.CloseAfter = &agentreplv1.CreateWorkspaceAsyncMergeCloseAfter{}
			}))
		}, field: "async_merge.close_after.workspace"},
		{name: "an async merge with no placement", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validAsyncMerge(func(a *agentreplv1.CreateWorkspaceAsyncMerge) { a.Placement = nil }))
		}, field: "async_merge.placement"},
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
			name: "open in editor of a commit log with no repository dir",
			check: func() *connect.Error {
				return validateOpenInEditorRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
					Target: &agentreplv1.OpenInEditorRequest_CommitLog{CommitLog: &agentreplv1.OpenInEditorCommitLog{Branch: "master", Commit: "abc"}}})
			},
			field: "commit_log.repository_dir",
		},
		{
			name: "open in editor of a commit log with no branch",
			check: func() *connect.Error {
				return validateOpenInEditorRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
					Target: &agentreplv1.OpenInEditorRequest_CommitLog{CommitLog: &agentreplv1.OpenInEditorCommitLog{RepositoryDir: "/repo", Commit: "abc"}}})
			},
			field: "commit_log.branch",
		},
		{
			name: "open in editor of a commit log with no commit",
			check: func() *connect.Error {
				return validateOpenInEditorRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
					Target: &agentreplv1.OpenInEditorRequest_CommitLog{CommitLog: &agentreplv1.OpenInEditorCommitLog{RepositoryDir: "/repo", Branch: "master"}}})
			},
			field: "commit_log.commit",
		},
		{
			name:  "finish with no workspace",
			check: func() *connect.Error { return validateFinishWorkspaceRequest(&agentreplv1.FinishWorkspaceRequest{}) },
			field: "workspace",
		},
		{name: "finish with no requester", check: func() *connect.Error {
			req := ownFinish()
			req.Requester = nil
			return validateFinishWorkspaceRequest(req)
		}, field: "requester"},
		{name: "finish with no placement", check: func() *connect.Error {
			req := ownFinish()
			req.Placement = nil
			return validateFinishWorkspaceRequest(req)
		}, field: "placement"},
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
			name: "set hidden",
			check: func() *connect.Error {
				return validateSetWorkspaceHiddenRequest(&agentreplv1.SetWorkspaceHiddenRequest{
					Workspace:  ref(),
					Visibility: &agentreplv1.SetWorkspaceHiddenRequest_Hide{Hide: &agentreplv1.SetWorkspaceHiddenHide{}},
				})
			},
		},
		{
			name: "set parent to top-level",
			check: func() *connect.Error {
				return validateSetWorkspaceParentRequest(&agentreplv1.SetWorkspaceParentRequest{Workspace: ref(),
					Parent: &agentreplv1.SetWorkspaceParentRequest_TopLevel{TopLevel: &agentreplv1.SetWorkspaceParentTopLevel{}}})
			},
		},
		{
			name: "set parent as a child",
			check: func() *connect.Error {
				return validateSetWorkspaceParentRequest(&agentreplv1.SetWorkspaceParentRequest{Workspace: ref(),
					Parent: &agentreplv1.SetWorkspaceParentRequest_ChildOf{ChildOf: &agentreplv1.SetWorkspaceParentChild{Parent: ref()}}})
			},
		},
		{
			name: "move of one workspace onto another",
			check: func() *connect.Error {
				return validateMoveWorkspaceRequest(&agentreplv1.MoveWorkspaceRequest{
					Workspace: ref(), Target: &workspacev1.WorkspaceRef{Id: "ws-2"},
				})
			},
		},
		{
			name: "swap of one workspace with another",
			check: func() *connect.Error {
				return validateSwapWorkspacesRequest(&agentreplv1.SwapWorkspacesRequest{
					Workspace: ref(), Target: &workspacev1.WorkspaceRef{Id: "ws-2"},
				})
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
			name: "open in editor of a commit log",
			check: func() *connect.Error {
				return validateOpenInEditorRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
					Target: &agentreplv1.OpenInEditorRequest_CommitLog{CommitLog: &agentreplv1.OpenInEditorCommitLog{RepositoryDir: "/repo", Branch: "master", Commit: "abc"}}})
			},
		},
		{
			name: "open in editor of a test log",
			check: func() *connect.Error {
				return validateOpenInEditorRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
					Target: &agentreplv1.OpenInEditorRequest_MergeTestLog{MergeTestLog: &frontendv1.FeedMergeTestLogToken{Value: "lease/1"}}})
			},
		},
		{name: "finish a workspace", check: func() *connect.Error { return validateFinishWorkspaceRequest(ownFinish()) }},
		{name: "create a standard workspace", check: func() *connect.Error { return validateCreateWorkspaceRequest(validCreate(nil)) }},
		{name: "create a child that forks another workspace", check: func() *connect.Error {
			return validateCreateWorkspaceRequest(validCreate(func(r *agentreplv1.CreateWorkspaceRequest) {
				r.Form = &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{Topology: childTopology(ref(), "the parser fix")}}
				r.ContextOrigin = forkContext(&workspacev1.WorkspaceRef{Id: "ws-other"})
			}))
		}},
		{name: "create an async merge", check: func() *connect.Error { return validateCreateWorkspaceRequest(validAsyncMerge(nil)) }},
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

func TestFoldHeldPromptRefusesAMalformedRequest(t *testing.T) {
	tests := []struct {
		name string
		req  func() *agentreplv1.FoldHeldPromptRequest
	}{
		{"no turn", func() *agentreplv1.FoldHeldPromptRequest {
			req := foldRequest()
			req.Turn = nil
			return req
		}},
		{"a blank turn", func() *agentreplv1.FoldHeldPromptRequest {
			req := foldRequest()
			req.Turn = &conversationv1.TurnId{}
			return req
		}},
		{"no entry ahead", func() *agentreplv1.FoldHeldPromptRequest {
			req := foldRequest()
			req.Above = nil
			return req
		}},
		{"a blank entry ahead", func() *agentreplv1.FoldHeldPromptRequest {
			req := foldRequest()
			req.Above = &conversationv1.TurnId{}
			return req
		}},
		{"the entry ahead is the folded entry", func() *agentreplv1.FoldHeldPromptRequest {
			req := foldRequest()
			req.Above = &conversationv1.TurnId{Value: req.GetTurn().GetValue()}
			return req
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.FoldHeldPrompt(context.Background(), connect.NewRequest(tt.req()))

			// Assert.
			if code := connectCode(t, err); code != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", code)
			}
			if len(h.Queue.folds) != 0 {
				t.Fatalf("folds = %v, want a malformed request never to reach the queue", h.Queue.folds)
			}
		})
	}
}

func TestUpdateClassifierPromptRefusesAMalformedRequest(t *testing.T) {
	// Arrange.
	tests := []struct {
		name string
		req  func() *agentreplv1.UpdateClassifierPromptRequest
	}{
		{"a blank instruction", func() *agentreplv1.UpdateClassifierPromptRequest {
			req := classifierPromptRequest()
			req.Instruction = "  \n"
			return req
		}},
		{"no example", func() *agentreplv1.UpdateClassifierPromptRequest {
			req := classifierPromptRequest()
			req.Example = nil
			return req
		}},
		{"an unspecified route", func() *agentreplv1.UpdateClassifierPromptRequest {
			req := classifierPromptRequest()
			req.Example.Route = agentreplv1.ClassifierRoute_CLASSIFIER_ROUTE_UNSPECIFIED
			return req
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			h := newHarness(t)

			// Act.
			_, err := h.Client.UpdateClassifierPrompt(context.Background(), connect.NewRequest(tt.req()))

			// Assert.
			if code := connectCode(t, err); code != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", code)
			}
			if len(h.ClassifierPrompt.updates) != 0 {
				t.Fatalf("a malformed request reached the updater")
			}
		})
	}
}

func TestUpdateClassifierPromptAcceptsAnExampleWithNoText(t *testing.T) {
	// Arrange: a prompt that was only an image still had a verdict.
	h := newHarness(t)
	req := classifierPromptRequest()
	req.Example.Text = ""

	// Act.
	_, err := h.Client.UpdateClassifierPrompt(context.Background(), connect.NewRequest(req))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateClassifierPrompt = %v, want it accepted", err)
	}
}

func TestClientLogWithNoSourceIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.ClientLog(context.Background(),
		connect.NewRequest(&agentreplv1.ClientLogRequest{
			Record: &agentreplv1.ClientLogRecord{
				Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
				Operation: "webapp.x",
				Message:   "m",
			},
		}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}
