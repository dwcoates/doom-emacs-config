package server

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/envc"
	"agent-repld/internal/login"
	"agent-repld/internal/merge"
	"agent-repld/internal/workspace"
)

// TestOpenLoginAnswersTheConfigDir pins that the caller learns which account
// root the flow runs under.
func TestOpenLoginAnswersTheConfigDir(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Login.configDir = "/accounts/one"

	// Act.
	resp, err := h.Client.OpenLogin(context.Background(),
		connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenLogin: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetConfigDir(); got != "/accounts/one" {
		t.Fatalf("config_dir = %q, want /accounts/one", got)
	}
}

// TestOpenLoginWhoseSpawnFailsAnswersSpawnFailed pins the arm a failure that
// is not a typed refusal reaches the caller as, its detail carrying the
// failure's text: the binary that would not spawn, or the vendor guard's
// refusal naming its site (asRefusal does not recognize *envc.ForbiddenError).
func TestOpenLoginWhoseSpawnFailsAnswersSpawnFailed(t *testing.T) {
	tests := []struct {
		name    string
		openErr error
		want    string
	}{
		{name: "a vendor binary that will not spawn names its path",
			openErr: errors.New("fork/exec /no/such/claude: no such file or directory"), want: "/no/such/claude"},
		{name: "the vendor guard's refusal names the login site",
			openErr: &envc.ForbiddenError{Site: "login"}, want: "login"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Login.openErr = tt.openErr

			// Act.
			resp, err := h.Client.OpenLogin(context.Background(),
				connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: ref()}))

			// Assert.
			if err != nil {
				t.Fatalf("OpenLogin: %v", err)
			}
			spawnFailed := resp.Msg.GetError().GetSpawnFailed()
			if spawnFailed == nil || !strings.Contains(spawnFailed.GetDetail(), tt.want) {
				t.Fatalf("result = %v, want spawn_failed whose detail names %q", resp.Msg.GetResult(), tt.want)
			}
		})
	}
}

// TestSendLoginInputRefusesWithNoLoginOpen pins the unary arm the contract
// carries for an absent pty.
func TestSendLoginInputRefusesWithNoLoginOpen(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Login.sendErr = login.ErrNoSession

	// Act.
	resp, err := h.Client.SendLoginInput(context.Background(),
		connect.NewRequest(&agentreplv1.SendLoginInputRequest{
			Workspace: ref(),
			Input: &agentreplv1.SendLoginInputRequest_Keystrokes{
				Keystrokes: &agentreplv1.LoginTerminalKeystrokes{Data: []byte("y")},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SendLoginInput: %v", err)
	}
	if resp.Msg.GetError().GetNoLoginOpen() == nil {
		t.Fatalf("result = %v, want no_login_open", resp.Msg.GetResult())
	}
}

// TestCloseLoginOnAnAbsentSessionIsSuccess pins the contract: closing a login
// that is not open is a success, not an error.
func TestCloseLoginOnAnAbsentSessionIsSuccess(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Login.closeErr = login.ErrNoSession

	// Act.
	resp, err := h.Client.CloseLogin(context.Background(),
		connect.NewRequest(&agentreplv1.CloseLoginRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("CloseLogin: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
}

// TestWatchLoginTerminalStreamsBytes pins that the pty's raw output reaches the
// client verbatim.
func TestWatchLoginTerminalStreamsBytes(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Login.watchFrames = make(chan login.Output, 1)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, err := h.Client.WatchLoginTerminal(ctx,
		connect.NewRequest(&agentreplv1.WatchLoginTerminalRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("WatchLoginTerminal: %v", err)
	}
	h.Login.watchFrames <- login.Output{Bytes: []byte("Enter code:")}
	if !stream.Receive() {
		t.Fatalf("receive the bytes: %v", stream.Err())
	}

	// Assert.
	if got := string(stream.Msg().GetBytes().GetData()); got != "Enter code:" {
		t.Fatalf("bytes = %q, want the pty's output", got)
	}
}

// TestWatchLoginTerminalEndsOnTheClosedFrame pins that `closed` is the stream's
// LAST frame.
func TestWatchLoginTerminalEndsOnTheClosedFrame(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Login.watchFrames = make(chan login.Output, 1)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, err := h.Client.WatchLoginTerminal(ctx,
		connect.NewRequest(&agentreplv1.WatchLoginTerminalRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("WatchLoginTerminal: %v", err)
	}

	// Act.
	h.Login.watchFrames <- login.Output{Closed: true}
	if !stream.Receive() {
		t.Fatalf("receive the closed frame: %v", stream.Err())
	}

	// Assert.
	if stream.Msg().GetClosed() == nil {
		t.Fatalf("frame = %v, want closed", stream.Msg().GetOutput())
	}
	if stream.Receive() {
		t.Fatal("the stream kept going after the closed frame")
	}
}

// TestWatchLoginTerminalRefusesWithNoLoginOpen pins the STREAM's unlanded arm:
// WatchLoginTerminal has no error message, so a refused open is the sentinel.
func TestWatchLoginTerminalRefusesWithNoLoginOpen(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Login.watchErr = login.ErrNoSession
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, err := h.Client.WatchLoginTerminal(ctx,
		connect.NewRequest(&agentreplv1.WatchLoginTerminalRequest{Workspace: ref()}))
	err = firstStreamErr(stream, err)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "WatchLoginTerminal closed the stream: no_login_open") {
		t.Fatalf("error = %v, want the transport-closed no_login_open cause", err)
	}
}

// TestOpenInEditorRelaysOntoTheHostStream pins the ruling: the daemon validates
// the workspace and RELAYS the click; it opens nothing itself.
func TestOpenInEditorRelaysOntoTheHostStream(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := dialHostStream(t, h, ctx)

	// Act: the verbs relay through the server's own relay face.
	line := uint32(42)
	h.Server.Relay().OpenInEditor(testWorkspaceID, "lisp/core.el", &line)
	push := receiveHostEvent(t, stream)

	// Assert.
	got := push.GetOpenInEditor()
	if got.GetPath() != "lisp/core.el" || got.GetLine() != 42 {
		t.Fatalf("open_in_editor = %v, want lisp/core.el:42", got)
	}
}

// TestOpenInEditorRelaysTheSameClickTwice pins the defect of 2026-10-14: the
// host stream's events rode a Topic, which dropped a push identical to its
// latest value, so clicking the same link a second time opened nothing.
func TestOpenInEditorRelaysTheSameClickTwice(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := dialHostStream(t, h, ctx)

	// Act.
	h.Server.Relay().OpenInEditor(testWorkspaceID, "lisp/core.el", nil)
	first := receiveHostEvent(t, stream)
	h.Server.Relay().OpenInEditor(testWorkspaceID, "lisp/core.el", nil)
	second := receiveHostEvent(t, stream)

	// Assert.
	for i, push := range []*agentreplv1.WatchHostWorkspaceResponse{first, second} {
		if push.GetOpenInEditor().GetPath() != "lisp/core.el" {
			t.Fatalf("event %d = %v, want open_in_editor lisp/core.el", i+1, push)
		}
	}
}

// TestOpenInEditorIsNotReplayedToALateHostStream pins that a click belongs to
// the moment it was made: a host stream opened after it never reopens the file.
func TestOpenInEditorIsNotReplayedToALateHostStream(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	h.Server.Relay().OpenInEditor(testWorkspaceID, "lisp/old.el", nil)
	stream := dialHostStream(t, h, ctx)

	// Act.
	h.Server.Relay().OpenInEditor(testWorkspaceID, "lisp/new.el", nil)
	push := receiveHostEvent(t, stream)

	// Assert.
	if push.GetOpenInEditor().GetPath() != "lisp/new.el" {
		t.Fatalf("first event = %v, want lisp/new.el (the earlier click is never replayed)", push)
	}
}

func TestOpenInEditorOpensAWorkspaceFileThroughTheVerb(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
		Target: &agentreplv1.OpenInEditorRequest_WorkspaceFile{WorkspaceFile: &agentreplv1.OpenInEditorWorkspaceFile{Path: "lisp/core.el"}}}))

	// Assert.
	if err != nil || len(h.Verbs.editorOpens) != 1 || h.Verbs.editorOpens[0] != "file:lisp/core.el" {
		t.Fatalf("OpenInEditor = %v, opens %v, want the workspace file relayed", err, h.Verbs.editorOpens)
	}
}

// TestOpenInEditorAnswersTheVerbsRefusalOfAWorkspaceFile pins that a verb
// refusing the file (it lies outside the workspace) is answered as its typed
// arm, and recorded.
func TestOpenInEditorAnswersTheVerbsRefusalOfAWorkspaceFile(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	h.Verbs.openInEditorErr = &workspace.Refusal{Arm: workspace.ArmPathEscapesWorkspace, Reason: "/etc/passwd lies outside the workspace"}

	// Act.
	resp, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
		Target: &agentreplv1.OpenInEditorRequest_WorkspaceFile{WorkspaceFile: &agentreplv1.OpenInEditorWorkspaceFile{Path: "/etc/passwd"}}}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenInEditor: %v", err)
	}
	if resp.Msg.GetError().GetPathEscapesWorkspace() == nil {
		t.Fatalf("result = %v, want path_escapes_workspace", resp.Msg.GetResult())
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: "OpenInEditor", Message: "answered a typed refusal",
		Match: func(r dlog.Record) bool { return r.Context["arm"] == workspace.ArmPathEscapesWorkspace }}); !ok {
		t.Fatalf("records = %+v, want the path_escapes_workspace refusal recorded", log.Records())
	}
}

func TestOpenInEditorOpensACommitLogThroughTheVerb(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
		Target: &agentreplv1.OpenInEditorRequest_CommitLog{CommitLog: &agentreplv1.OpenInEditorCommitLog{RepositoryDir: "/repo", Branch: "master", Commit: "abc123"}}}))

	// Assert.
	if err != nil || len(h.Verbs.editorOpens) != 1 || h.Verbs.editorOpens[0] != "commit:/repo@master#abc123" {
		t.Fatalf("OpenInEditor = %v, opens %v, want the commit log relayed", err, h.Verbs.editorOpens)
	}
}

// TestOpenCommitLogRelaysOntoTheHostStream pins the relay face's push: the
// target travels verbatim as the open_commit_log arm.
func TestOpenCommitLogRelaysOntoTheHostStream(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := dialHostStream(t, h, ctx)

	// Act.
	h.Server.Relay().OpenCommitLog(testWorkspaceID, &agentreplv1.OpenInEditorCommitLog{RepositoryDir: "/repo", Branch: "master", Commit: "abc123"})
	push := receiveHostEvent(t, stream)

	// Assert.
	got := push.GetOpenCommitLog()
	if got.GetRepositoryDir() != "/repo" || got.GetBranch() != "master" || got.GetCommit() != "abc123" {
		t.Fatalf("open_commit_log = %v, want /repo master abc123", got)
	}
}

func TestOpenInEditorRunsATerminalCommandThroughTheVerb(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
		Target: &agentreplv1.OpenInEditorRequest_TerminalCommand{TerminalCommand: &agentreplv1.OpenInEditorTerminalCommand{Command: "ls -la"}}}))

	// Assert.
	if err != nil || len(h.Verbs.editorOpens) != 1 || h.Verbs.editorOpens[0] != "terminal:ls -la" {
		t.Fatalf("OpenInEditor = %v, opens %v, want the command relayed", err, h.Verbs.editorOpens)
	}
}

// TestRunInTerminalRelaysOntoTheHostStream pins the relay face's push: the
// command travels verbatim as the run_in_terminal arm.
func TestRunInTerminalRelaysOntoTheHostStream(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := dialHostStream(t, h, ctx)

	// Act.
	h.Server.Relay().RunInTerminal(testWorkspaceID, &agentreplv1.OpenInEditorTerminalCommand{Command: "ls -la"})
	push := receiveHostEvent(t, stream)

	// Assert.
	if got := push.GetRunInTerminal().GetCommand(); got != "ls -la" {
		t.Fatalf("run_in_terminal = %q, want ls -la", got)
	}
}

func TestOpenInEditorOpensAMergeTestLogByItsToken(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Merge.logPaths = map[string]string{"lease-1/2": "/state/merge-logs/lease-1-tests-2.log"}

	// Act.
	_, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
		Target: &agentreplv1.OpenInEditorRequest_MergeTestLog{MergeTestLog: &frontendv1.FeedMergeTestLogToken{Value: "lease-1/2"}}}))

	// Assert.
	if err != nil || len(h.Verbs.editorOpens) != 1 || h.Verbs.editorOpens[0] != "daemon:/state/merge-logs/lease-1-tests-2.log" {
		t.Fatalf("OpenInEditor = %v, opens %v, want the log relayed", err, h.Verbs.editorOpens)
	}
}

func TestOpenInEditorAnswersAnUnknownMergeTestLog(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Merge.logErr = &merge.RefusalError{Arm: merge.ArmUnknownMergeTestLog, Reason: "no such log"}

	// Act.
	resp, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
		Target: &agentreplv1.OpenInEditorRequest_MergeTestLog{MergeTestLog: &frontendv1.FeedMergeTestLogToken{Value: "x"}}}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenInEditor: %v", err)
	}
	if resp.Msg.GetError().GetUnknownMergeTestLog() == nil || len(h.Verbs.editorOpens) != 0 {
		t.Fatalf("result = %v, opens %v, want unknown_merge_test_log and nothing relayed", resp.Msg.GetResult(), h.Verbs.editorOpens)
	}
}

// TestALinkClickRecordsTheUsersInteraction pins that every link a page
// follows, outward or into the editor, is recorded as the user's interaction
// with the workspace, so its row is viewed and most recent.
func TestALinkClickRecordsTheUsersInteraction(t *testing.T) {
	tests := []struct {
		name  string
		click func(h *harness) error
	}{
		{
			name: "OpenExternal",
			click: func(h *harness) error {
				_, err := h.Client.OpenExternal(context.Background(), connect.NewRequest(&agentreplv1.OpenExternalRequest{
					Workspace: ref(), Url: "https://example.com/a"}))
				return err
			},
		},
		{
			name: "OpenInEditor workspace_file",
			click: func(h *harness) error {
				_, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
					Target: &agentreplv1.OpenInEditorRequest_WorkspaceFile{WorkspaceFile: &agentreplv1.OpenInEditorWorkspaceFile{Path: "lisp/core.el"}}}))
				return err
			},
		},
		{
			name: "OpenInEditor terminal_command",
			click: func(h *harness) error {
				_, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
					Target: &agentreplv1.OpenInEditorRequest_TerminalCommand{TerminalCommand: &agentreplv1.OpenInEditorTerminalCommand{Command: "ls"}}}))
				return err
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			holdHostStream(t, h)

			// Act.
			err := tt.click(h)

			// Assert.
			if err != nil {
				t.Fatalf("%s: %v", tt.name, err)
			}
			want := []interactionCall{{ws: testWorkspaceID, kind: workspace.InteractionLinkFollowed}}
			if !slices.Equal(h.Verbs.interactions, want) {
				t.Fatalf("interactions = %v, want %v", h.Verbs.interactions, want)
			}
		})
	}
}

func TestOpenExternalStillOpensTheLinkAfterRecordingTheInteraction(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	holdHostStream(t, h)

	// Act.
	_, err := h.Client.OpenExternal(context.Background(), connect.NewRequest(&agentreplv1.OpenExternalRequest{
		Workspace: ref(), Url: "https://example.com/a"}))

	// Assert.
	if err != nil || len(h.Verbs.externalOpens) != 1 || h.Verbs.externalOpens[0] != "https://example.com/a" {
		t.Fatalf("OpenExternal = %v, opens %v, want the link handed to the browser", err, h.Verbs.externalOpens)
	}
}

// TestOpenExternalAnswersTheVerbsRefusalsOnTheirArms pins each refusal the
// open raises reaching OpenExternalError by name, with its fields.
func TestOpenExternalAnswersTheVerbsRefusalsOnTheirArms(t *testing.T) {
	tests := []struct {
		name    string
		refusal *workspace.Refusal
		got     func(*agentreplv1.OpenExternalError) bool
	}{
		{name: "an unparseable url",
			refusal: &workspace.Refusal{Rpc: "OpenExternal", Arm: workspace.ArmInvalidUrl, Reason: "not an absolute url"},
			got:     func(e *agentreplv1.OpenExternalError) bool { return e.GetInvalidUrl() != nil }},
		{name: "no browser configured",
			refusal: &workspace.Refusal{Rpc: "OpenExternal", Arm: workspace.ArmNoBrowserConfigured, Reason: "no browser"},
			got:     func(e *agentreplv1.OpenExternalError) bool { return e.GetNoBrowserConfigured() != nil }},
		{name: "a launcher that failed",
			refusal: &workspace.Refusal{Rpc: "OpenExternal", Arm: workspace.ArmLaunchFailed, Reason: "did not open",
				Fields: map[string]any{"detail": "exit status 1"}},
			got: func(e *agentreplv1.OpenExternalError) bool { return e.GetLaunchFailed().GetDetail() == "exit status 1" }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			holdHostStream(t, h)
			h.Verbs.externalOpenErr = tt.refusal

			// Act.
			resp, err := h.Client.OpenExternal(context.Background(), connect.NewRequest(&agentreplv1.OpenExternalRequest{
				Workspace: ref(), Url: "https://example.com/a"}))

			// Assert.
			if err != nil {
				t.Fatalf("OpenExternal = transport error %v, want the in-band arm", err)
			}
			if !tt.got(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm", resp.Msg.GetResult(), tt.refusal.Arm)
			}
		})
	}
}

func TestALinkFollowedIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	holdHostStream(t, h)

	// Act.
	_, err := h.Client.OpenExternal(context.Background(), connect.NewRequest(&agentreplv1.OpenExternalRequest{
		Workspace: ref(), Url: "https://example.com/a"}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenExternal: %v", err)
	}
	for _, rec := range log.Records() {
		if rec.Operation == "OpenExternal" && rec.Level == "info" &&
			rec.Message == "recorded the followed link as the user's interaction with the workspace" {
			return
		}
	}
	t.Fatalf("records = %+v, want the link followed at info", log.Records())
}
