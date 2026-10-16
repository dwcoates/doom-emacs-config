package server

import (
	"context"
	"errors"
	"os"
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
		{name: "set workspace parent", operation: "daemon.server.set_workspace_parent", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.SetWorkspaceParentRequest{})
			req.Header().Set(requestIDHeader, id)
			_, err := client.SetWorkspaceParent(ctx, req)
			return err
		}},
		{name: "watch footer, a streaming rpc", operation: "daemon.server.watch_footer", invoke: func(ctx context.Context, client agentreplv1connect.AgentReplClient, id string) error {
			req := connect.NewRequest(&agentreplv1.WatchFooterRequest{})
			req.Header().Set(requestIDHeader, id)
			stream, err := client.WatchFooter(ctx, req)
			if err != nil {
				return err
			}
			defer stream.Close()
			for stream.Receive() {
			}
			return stream.Err()
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

// TestRequestBoundaryReachesTheHandlerWhenTheWorkspaceSinkCannotBeResolved
// pins the boundary's totality: a workspace whose directory is a scratch path
// or has been deleted cannot host a durable sink, and that must cost the
// record's PLACEMENT, never the request's answer. Refusing here answered every
// per-workspace rpc on such a workspace with an internal error raised by
// logging, in place of the handler's own answer.
func TestRequestBoundaryReachesTheHandlerWhenTheWorkspaceSinkCannotBeResolved(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(d *Deps) {
		d.Log = &fakeSurfaces{workspaceErr: errors.New("the workspace directory is gone")}
	})

	// Act.
	resp, err := h.Client.OpenWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenWorkspace = %v, want the handler's own answer", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenWorkspace answered %v, want success", resp.Msg.GetResult())
	}
}

// TestRequestBoundaryRecordsNoErrorWhenTheWorkspaceSinkCannotBeResolved pins
// that the fallback is an ORDINARY outcome: it is not reported as a fault
// beside every request.
func TestRequestBoundaryRecordsNoErrorWhenTheWorkspaceSinkCannotBeResolved(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(d *Deps) {
		d.Log = &fakeSurfaces{global: log, workspaceErr: errors.New("the workspace directory is gone")}
	})

	// Act.
	if _, err := h.Client.OpenWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ref()})); err != nil {
		t.Fatalf("OpenWorkspace: %v", err)
	}

	// Assert.
	for _, rec := range log.at("ERROR") {
		if rec.Operation == "daemon.server.request_boundary" {
			t.Fatalf("the boundary recorded an error for an unsinkable workspace: %+v", rec)
		}
	}
}

// TestEveryWrapperIsOneCallToTheSharedBoundary fails a wrapper that hand-rolls
// its own boundary instead of delegating through serveUnary or serveStream.
func TestEveryWrapperIsOneCallToTheSharedBoundary(t *testing.T) {
	// Arrange.
	raw, err := os.ReadFile("requestlog_server.go")
	if err != nil {
		t.Fatalf("read requestlog_server.go: %v", err)
	}
	source := string(raw)

	// Act.
	wrappers := strings.Count(source, "func (s *requestLoggingServer) ")
	shared := strings.Count(source, "return serveUnary(s, ctx, ") + strings.Count(source, "return serveStream(s, ctx, ")

	// Assert.
	if wrappers == 0 || wrappers != shared {
		t.Fatalf("%d wrappers and %d delegate through serveUnary or serveStream; every wrapper must", wrappers, shared)
	}
	if got := strings.Count(source, "s.server.beginRequest("); got != 2 {
		t.Fatalf("beginRequest is called %d times, want once in each shared boundary", got)
	}
}

// TestBoundaryEnterRecordsEntryAndCompletionUnderTheRPCsOperation pins the
// two records every recorded rpc's boundary writes, under the operation its
// name spells.
func TestBoundaryEnterRecordsEntryAndCompletionUnderTheRPCsOperation(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	boundary := requestBoundary{log: log, rpc: "SetWorkspaceParent", requestID: "r-1"}
	failed := errors.New("refused")

	// Act.
	boundary.enter()(&failed)

	// Assert.
	want := []string{"entered the rpc handler", "completed the rpc handler"}
	if len(log.records) != len(want) {
		t.Fatalf("records = %+v, want %d", log.records, len(want))
	}
	for i, record := range log.records {
		if record.Operation != "daemon.server.set_workspace_parent" || record.Message != want[i] || record.Level != "DEBUG" {
			t.Fatalf("record %d = %+v, want DEBUG %q under daemon.server.set_workspace_parent", i, record, want[i])
		}
	}
	if log.records[1].Context["outcome"] != "error" {
		t.Fatalf("completion = %+v, want the error outcome", log.records[1])
	}
}

// TestAnUnrecordedRPCsBoundaryRecordsNothing pins that ClientLog's boundary
// writes no records of its own, failed or not.
func TestAnUnrecordedRPCsBoundaryRecordsNothing(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	boundary := requestBoundary{log: log, rpc: "ClientLog", unrecorded: unrecordedRPCs["ClientLog"]}
	failed := errors.New("refused")

	// Act.
	boundary.enter()(&failed)

	// Assert.
	if len(log.records) != 0 {
		t.Fatalf("records = %+v, want none", log.records)
	}
}
