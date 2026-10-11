package server

import (
	"context"
	"strings"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

func slashCatalog(names ...string) *conversationv1.SessionCommandCatalog {
	catalog := &conversationv1.SessionCommandCatalog{}
	for _, name := range names {
		catalog.Commands = append(catalog.Commands, &conversationv1.SessionCommandCatalogEntry{Name: name})
	}
	return catalog
}

// TestListSlashCommandsAnswersTheStandingCatalog pins that the composer is
// served the catalog the workspace's session last stated, whole.
func TestListSlashCommandsAnswersTheStandingCatalog(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	stated := slashCatalog("merge-queue", "compact")
	h.SlashCommands.OnCommandCatalog(testWorkspaceID, stated)

	// Act.
	resp, err := h.Client.ListSlashCommands(context.Background(), connect.NewRequest(&agentreplv1.ListSlashCommandsRequest{
		Workspace: ref(),
	}))

	// Assert.
	if err != nil {
		t.Fatalf("ListSlashCommands: %v", err)
	}
	got := resp.Msg.GetSuccess().GetStated().GetCatalog()
	if !proto.Equal(got, stated) {
		t.Fatalf("catalog = %v, want %v", resp.Msg.GetResult(), stated)
	}
}

// TestListSlashCommandsAnswersUnstatedBeforeAnyStatement pins that a workspace
// whose session has stated nothing is an ANSWER, never an error.
func TestListSlashCommandsAnswersUnstatedBeforeAnyStatement(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) {
		deps.Log = &fakeSurfaces{workspace: log}
	})

	// Act.
	resp, err := h.Client.ListSlashCommands(context.Background(), connect.NewRequest(&agentreplv1.ListSlashCommandsRequest{
		Workspace: ref(),
	}))

	// Assert.
	if err != nil {
		t.Fatalf("ListSlashCommands: %v", err)
	}
	if resp.Msg.GetSuccess().GetUnstated() == nil {
		t.Fatalf("result = %v, want success.unstated", resp.Msg.GetResult())
	}
	found := false
	for _, rec := range log.records {
		if rec.Operation == "daemon.server.list_slash_commands" && rec.Context["branch"] == "unstated" {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %v, want the unstated answer recorded", log.records)
	}
}

// TestListSlashCommandsRefusesAnUnknownWorkspace pins the typed refusal for a
// workspace the registry does not hold, and that no other workspace's catalog
// leaks into the answer.
func TestListSlashCommandsRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.SlashCommands.OnCommandCatalog(testWorkspaceID, slashCatalog("compact"))

	// Act.
	resp, err := h.Client.ListSlashCommands(context.Background(), connect.NewRequest(&agentreplv1.ListSlashCommandsRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: "ws-forgotten", Dir: testWorkspaceDir},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("ListSlashCommands: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want error.unknown_workspace", resp.Msg.GetResult())
	}
}

// TestListSlashCommandsRefusesAMissingWorkspace pins that a request naming no
// workspace is invalid input, never an arm.
func TestListSlashCommandsRefusesAMissingWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.ListSlashCommands(context.Background(), connect.NewRequest(&agentreplv1.ListSlashCommandsRequest{}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}

// TestNewRefusesMissingSlashCommandCatalogs pins that the catalog table is
// required: a composer asking for completions would otherwise reach a nil.
func TestNewRefusesMissingSlashCommandCatalogs(t *testing.T) {
	// Arrange.
	var complete Deps
	newHarness(t, func(deps *Deps) { complete = *deps })
	complete.SlashCommands = nil

	// Act.
	_, err := New(complete)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the slash-command catalogs") {
		t.Fatalf("New = %v, want the refusal naming the slash-command catalogs", err)
	}
}
