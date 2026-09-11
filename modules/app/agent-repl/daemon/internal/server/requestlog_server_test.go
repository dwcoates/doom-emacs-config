package server

import (
	"context"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
)

func TestRepresentativeRPCHandlersRecordRequestBoundaries(t *testing.T) {
	tests := []struct {
		name      string
		operation string
		invoke    func(context.Context, agentreplv1connect.AgentReplClient, string) error
	}{
		{name: "submit prompt", operation: "daemon.server.submit_prompt", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.SubmitPromptRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.SubmitPrompt(ctx, req)
			return err
		}},
		{name: "request command support", operation: "daemon.server.request_command_support", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.RequestCommandSupportRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.RequestCommandSupport(ctx, req)
			return err
		}},
		{name: "open feed", operation: "daemon.server.open_feed", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.OpenFeedRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.OpenFeed(ctx, req)
			return err
		}},
		{name: "get feed page", operation: "daemon.server.get_feed_page", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.GetFeedPageRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.GetFeedPage(ctx, req)
			return err
		}},
		{name: "interrupt", operation: "daemon.server.interrupt", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.InterruptRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.Interrupt(ctx, req)
			return err
		}},
		{name: "answer permission", operation: "daemon.server.answer_permission", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.AnswerPermissionRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.AnswerPermission(ctx, req)
			return err
		}},
		{name: "answer question", operation: "daemon.server.answer_question", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.AnswerQuestionRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.AnswerQuestion(ctx, req)
			return err
		}},
		{name: "answer cold gate", operation: "daemon.server.answer_cold_gate", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.AnswerColdGateRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.AnswerColdGate(ctx, req)
			return err
		}},
		{name: "create workspace", operation: "daemon.server.create_workspace", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.CreateWorkspace(ctx, req)
			return err
		}},
		{name: "open workspace", operation: "daemon.server.open_workspace", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.OpenWorkspace(ctx, req)
			return err
		}},
		{name: "close workspace", operation: "daemon.server.close_workspace", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.CloseWorkspace(ctx, req)
			return err
		}},
		{name: "kill workspace", operation: "daemon.server.kill_workspace", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.KillWorkspaceRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.KillWorkspace(ctx, req)
			return err
		}},
		{name: "nuke workspace", operation: "daemon.server.nuke_workspace", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.NukeWorkspaceRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.NukeWorkspace(ctx, req)
			return err
		}},
		{name: "merge workspace", operation: "daemon.server.merge_workspace", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.MergeWorkspace(ctx, req)
			return err
		}},
		{name: "restart workspace", operation: "daemon.server.restart_workspace", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.RestartWorkspace(ctx, req)
			return err
		}},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			log := &recordingLogger{}
			h := newHarness(t, func(deps *Deps) {
				deps.Log = &fakeSurfaces{global: log, workspace: log}
			})
			requestID := "request-" + strings.ReplaceAll(test.name, " ", "-")

			// Act.
			err := test.invoke(context.Background(), h.Client, requestID)

			// Assert.
			if err == nil {
				t.Fatal("the deliberately incomplete request succeeded")
			}
			var entry, completion *logRecord
			for i := range log.records {
				record := &log.records[i]
				if record.Operation != test.operation {
					continue
				}
				switch record.Message {
				case "entered the rpc handler":
					entry = record
				case "completed the rpc handler":
					completion = record
				}
			}
			if entry == nil || completion == nil {
				t.Fatalf("records = %v, want entry and completion for %s", log.records, test.operation)
			}
			if entry.Level != "DEBUG" || entry.Context["request_id"] != requestID {
				t.Fatalf("entry = %+v, want debug with request_id %q", entry, requestID)
			}
			if completion.Level != "DEBUG" || completion.Context["outcome"] != "error" {
				t.Fatalf("completion = %+v, want debug error outcome", completion)
			}
			if _, ok := completion.Context["duration_ms"]; !ok {
				t.Fatalf("completion = %+v, want duration_ms", completion)
			}
		})
	}
}
