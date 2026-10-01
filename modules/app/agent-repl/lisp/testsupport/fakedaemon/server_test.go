package main

import (
	"context"
	"net/http"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"connectrpc.com/connect"
)

func TestRegisterWorkspaceMintsRefFromCleanedDir(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	resp, err := client.RegisterWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: "/tmp/ws/./one"}))
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}

	// Assert: the daemon normalizes the path it was handed and returns the
	// minted ref (elisp.md, "The HOST section").
	got := resp.Msg.GetSuccess().GetWorkspace()
	if got.GetDir() != "/tmp/ws/one" {
		t.Fatalf("dir = %q, want the cleaned spelling", got.GetDir())
	}
	if got.GetId() == "" {
		t.Fatalf("id is empty; the daemon mints the identity")
	}
}

func TestRegisterWorkspaceIsIdempotentByDir(t *testing.T) {
	// Arrange: two spellings of one directory.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	first, err := client.RegisterWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: "/tmp/ws/one"}))
	if err != nil {
		t.Fatalf("first RegisterWorkspace: %v", err)
	}
	second, err := client.RegisterWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: "/tmp/ws/two/../one"}))
	if err != nil {
		t.Fatalf("second RegisterWorkspace: %v", err)
	}

	// Assert: RegisterWorkspace is IDEMPOTENT BY DIR — re-registration after
	// a reconnect must reconcile to the same id.
	if first.Msg.GetSuccess().GetWorkspace().GetId() != second.Msg.GetSuccess().GetWorkspace().GetId() {
		t.Fatalf("ids differ across spellings of one dir: %q vs %q",
			first.Msg.GetSuccess().GetWorkspace().GetId(),
			second.Msg.GetSuccess().GetWorkspace().GetId())
	}
}

func TestDefaultDaemonHealthIsHealthy(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	resp, err := client.DaemonHealth(context.Background(),
		connect.NewRequest(&agentreplv1.DaemonHealthRequest{}))
	if err != nil {
		t.Fatalf("DaemonHealth: %v", err)
	}

	// Assert: UNHEALTHY IS AN ANSWER, so the healthy default must arrive as a
	// resolved success arm rather than an empty message.
	if resp.Msg.GetSuccess().GetHealthy() == nil {
		t.Fatalf("DaemonHealth default is %v, want success.healthy", resp.Msg)
	}
}

func TestDefaultSelectWorkspaceIsSuccess(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	resp, err := client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{
			Workspace: &workspaceRef}))
	if err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}

	// Assert: success is EMPTY wherever the new state arrives as a push.
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("SelectWorkspace default is %v, want the success arm", resp.Msg)
	}
}

func TestDefaultSubmitPromptCarriesATurnId(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	resp, err := client.SubmitPrompt(context.Background(), connect.NewRequest(completeSubmit("abc")))
	if err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert: SubmitPromptTurn.turn is a non-optional message field, so a
	// default that left it unset would make every client raise.
	if resp.Msg.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("SubmitPrompt default is %v, want success.turn.turn.value set", resp.Msg)
	}
}

func TestDefaultPlanRollbackCarriesAToken(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	resp, err := client.PlanRollback(context.Background(),
		connect.NewRequest(&agentreplv1.PlanRollbackRequest{
			Workspace: &workspaceRef,
			Files:     &agentreplv1.PlanRollbackRequest_KeepFiles{KeepFiles: &agentreplv1.PlanRollbackKeepFiles{}},
		}))
	if err != nil {
		t.Fatalf("PlanRollback: %v", err)
	}

	// Assert: the token is daemon-minted and a client refuses an empty one.
	if resp.Msg.GetSuccess().GetPlan().GetToken().GetValue() == "" {
		t.Fatalf("PlanRollback default is %v, want success.plan.token.value set", resp.Msg)
	}
}

func TestDefaultRollBackIsSuccess(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	resp, err := client.RollBack(context.Background(),
		connect.NewRequest(&agentreplv1.RollBackRequest{
			Workspace: &workspaceRef,
			Token:     &agentreplv1.RollbackToken{Value: defaultRollbackToken},
		}))
	if err != nil {
		t.Fatalf("RollBack: %v", err)
	}

	// Assert: RollBackSuccess.prompt is non-optional, so it must be present.
	if resp.Msg.GetSuccess().GetPrompt() == nil {
		t.Fatalf("RollBack default is %v, want success.prompt set", resp.Msg)
	}
}

func TestScriptedResponseWinsOverTheDefault(t *testing.T) {
	// Arrange: script the error arm the default synthesis never produces.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	status, body := controlPost(t, baseURL, "/_fake/script",
		`{"method":"RegisterWorkspace","response":{"error":{}}}`)
	if status != http.StatusOK {
		t.Fatalf("/_fake/script = %d %s", status, body)
	}

	// Act.
	resp, err := client.RegisterWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: "/tmp/ws"}))
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}

	// Assert.
	if resp.Msg.GetError() == nil {
		t.Fatalf("response is %v, want the scripted error arm", resp.Msg)
	}
}

func TestScriptRefusesAnUnknownMethod(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/script",
		`{"method":"NoSuchRpc","response":{}}`)

	// Assert.
	if status != http.StatusBadRequest {
		t.Fatalf("/_fake/script for an unknown method = %d %s, want 400", status, body)
	}
}

func TestScriptRefusesAResponseTheSchemaRejects(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act: `succes` is not a field of RegisterWorkspaceResponse.
	status, body := controlPost(t, baseURL, "/_fake/script",
		`{"method":"RegisterWorkspace","response":{"succes":{}}}`)

	// Assert: validation happens at script time so a suite never discovers a
	// bad script as a mysterious client-side wire error.
	if status != http.StatusBadRequest {
		t.Fatalf("/_fake/script with an invalid response = %d %s, want 400", status, body)
	}
}

func TestUnaryRefusesAnUnknownRequestField(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := rawUnary(t, baseURL, "RegisterWorkspace", `{"dir":"/tmp/ws","bogus":1}`)

	// Assert: the round trip through the generated types is the check on the
	// client's encoders; DiscardUnknown would make a misspelling invisible.
	if status != http.StatusBadRequest || !strings.Contains(body, "bogus") {
		t.Fatalf("unknown-field request = %d %s, want 400 naming the field", status, body)
	}
}

func TestCallsAreRecordedInOrder(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	if _, err := client.RegisterWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: "/a"})); err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	if _, err := client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: &workspaceRef})); err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}

	// Assert.
	status, body := controlGet(t, baseURL, "/_fake/calls")
	if status != http.StatusOK {
		t.Fatalf("/_fake/calls = %d %s", status, body)
	}
	calls := decodeCalls(t, body)
	if len(calls) != 2 || calls[0].Method != "RegisterWorkspace" || calls[1].Method != "SelectWorkspace" {
		t.Fatalf("recorded %v, want RegisterWorkspace then SelectWorkspace", calls)
	}
}

func TestRecordedBodyIsTheRequestProtojson(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	if _, err := client.RegisterWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: "/a/b"})); err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}

	// Assert: the recording carries the request as the wire saw it, so a
	// suite can assert on lowerCamel keys and echoed values.
	_, body := controlGet(t, baseURL, "/_fake/calls")
	calls := decodeCalls(t, body)
	if len(calls) != 1 || string(calls[0].Body) != `{"dir":"/a/b"}` {
		t.Fatalf("recorded body = %v, want the request protojson", calls)
	}
}

func TestCallsIsAnEmptyArrayBeforeAnyRequest(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlGet(t, baseURL, "/_fake/calls")

	// Assert: an empty recording is `[]`, never `null` — a client parsing it
	// must not have to special-case the empty case.
	if status != http.StatusOK || strings.TrimSpace(body) != "[]" {
		t.Fatalf("/_fake/calls = %d %s, want 200 []", status, body)
	}
}

func TestWebappStreamIsRefusedAsUnimplemented(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act: WatchFooter belongs to the webapp; this fake mocks Emacs's
	// neighbor only, and never composes another system.
	stream, err := client.WatchFooter(context.Background(),
		connect.NewRequest(&agentreplv1.WatchFooterRequest{Workspace: &workspaceRef}))
	if err != nil {
		t.Fatalf("open WatchFooter: %v", err)
	}
	defer stream.Close()
	stream.Receive()

	// Assert.
	if connect.CodeOf(stream.Err()) != connect.CodeUnimplemented {
		t.Fatalf("WatchFooter error = %v, want unimplemented", stream.Err())
	}
}

func TestSubmitPromptRefusedWithoutAWorkspace(t *testing.T) {
	// Arrange: everything but the ref (landing 2 made it REQUIRED).
	_, baseURL := newTestServer(t)

	// Act.
	status, body := rawUnary(t, baseURL, "SubmitPrompt",
		`{"said":{"content":{}},"idempotencyKey":"k","origin":"PROMPT_ORIGIN_USER_SENT"}`)

	// Assert: protojson accepts the body (a missing message field is legal
	// JSON), so the fake enforces the validation invariant itself.
	if status != http.StatusBadRequest || !strings.Contains(body, "workspace") {
		t.Fatalf("SubmitPrompt without a workspace = %d %s, want 400 naming workspace", status, body)
	}
}

func TestSubmitPromptRefusedWithAnUnspecifiedOrigin(t *testing.T) {
	// Arrange: origin omitted, so it decodes to the UNSPECIFIED zero.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := rawUnary(t, baseURL, "SubmitPrompt",
		`{"workspace":{"id":"ws-test","dir":"/tmp/ws-test"},"said":{"content":{}},"idempotencyKey":"k"}`)

	// Assert: SubmitPromptRequest.origin is REQUIRED and never UNSPECIFIED.
	if status != http.StatusBadRequest || !strings.Contains(body, "UNSPECIFIED") {
		t.Fatalf("SubmitPrompt with no origin = %d %s, want 400 naming UNSPECIFIED", status, body)
	}
}

func TestSubmitPromptAcceptedWithoutAFeed(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act: `feed` is optional — the ROOT composer omits it.
	status, body := rawUnary(t, baseURL, "SubmitPrompt",
		`{"workspace":{"id":"ws-test","dir":"/tmp/ws-test"},"said":{"content":{}},"idempotencyKey":"k","origin":"PROMPT_ORIGIN_USER_SENT"}`)

	// Assert.
	if status != http.StatusOK {
		t.Fatalf("root-composer SubmitPrompt = %d %s, want 200", status, body)
	}
}

func TestRequestRefusedForAnUnsetOneof(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act: UpdateShutdownScheduleRequest.action has no arm set.
	status, body := rawUnary(t, baseURL, "UpdateShutdownSchedule", `{}`)

	// Assert: an unset oneof is an error by default.
	if status != http.StatusBadRequest || !strings.Contains(body, "action") {
		t.Fatalf("UpdateShutdownSchedule with no action = %d %s, want 400 naming action", status, body)
	}
}

func TestRequestRefusedForAnUnsetNestedField(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act: the schedule arm is set but its own reason is missing (the reason
	// is REQUIRED on schedule per the 2026-08-29 increment).
	status, body := rawUnary(t, baseURL, "UpdateShutdownSchedule",
		`{"schedule":{"atMs":"1735689600000"}}`)

	// Assert: validation is recursive, so a hole one level down is still loud.
	if status != http.StatusBadRequest || !strings.Contains(body, "reason") {
		t.Fatalf("schedule without a reason = %d %s, want 400 naming reason", status, body)
	}
}

// Every stream in the schema that belongs to the WEBAPP is refused, not
// stubbed: this fake mocks Emacs's neighbor and never composes another
// system, so a wrong caller has to fail loudly rather than hang on silence.
func TestEveryWebappStreamIsRefusedAsUnimplemented(t *testing.T) {
	tests := []struct {
		name string
		open func(context.Context, agentreplv1connectClient) (interface{ Err() error }, func() error)
	}{
		{
			name: "WatchFeed",
			open: func(ctx context.Context, c agentreplv1connectClient) (interface{ Err() error }, func() error) {
				s, err := c.WatchFeed(ctx, connect.NewRequest(&agentreplv1.WatchFeedRequest{}))
				if err != nil {
					t.Fatalf("open WatchFeed: %v", err)
				}
				s.Receive()
				return s, s.Close
			},
		},
		{
			name: "WatchTopbar",
			open: func(ctx context.Context, c agentreplv1connectClient) (interface{ Err() error }, func() error) {
				s, err := c.WatchTopbar(ctx, connect.NewRequest(&agentreplv1.WatchTopbarRequest{}))
				if err != nil {
					t.Fatalf("open WatchTopbar: %v", err)
				}
				s.Receive()
				return s, s.Close
			},
		},
		{
			name: "WatchDaemonHolds",
			open: func(ctx context.Context, c agentreplv1connectClient) (interface{ Err() error }, func() error) {
				s, err := c.WatchDaemonHolds(ctx, connect.NewRequest(&agentreplv1.WatchDaemonHoldsRequest{}))
				if err != nil {
					t.Fatalf("open WatchDaemonHolds: %v", err)
				}
				s.Receive()
				return s, s.Close
			},
		},
		{
			name: "WatchWebWorkspace",
			open: func(ctx context.Context, c agentreplv1connectClient) (interface{ Err() error }, func() error) {
				s, err := c.WatchWebWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchWebWorkspaceRequest{}))
				if err != nil {
					t.Fatalf("open WatchWebWorkspace: %v", err)
				}
				s.Receive()
				return s, s.Close
			},
		},
		{
			name: "WatchLoginTerminal",
			open: func(ctx context.Context, c agentreplv1connectClient) (interface{ Err() error }, func() error) {
				s, err := c.WatchLoginTerminal(ctx, connect.NewRequest(&agentreplv1.WatchLoginTerminalRequest{}))
				if err != nil {
					t.Fatalf("open WatchLoginTerminal: %v", err)
				}
				s.Receive()
				return s, s.Close
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			_, baseURL := newTestServer(t)
			client := newTestClient(t, baseURL)

			// Act.
			stream, closeStream := tc.open(context.Background(), client)
			defer closeStream()

			// Assert: the refusal is typed, and the attempt is still recorded
			// so a suite can see WHICH wrong stream a client reached for.
			if connect.CodeOf(stream.Err()) != connect.CodeUnimplemented {
				t.Fatalf("%s error = %v, want unimplemented", tc.name, stream.Err())
			}
			_, listing := controlGet(t, baseURL, "/_fake/calls")
			calls := decodeCalls(t, listing)
			if len(calls) != 1 || calls[0].Method != tc.name {
				t.Fatalf("recorded calls = %v, want one %s", calls, tc.name)
			}
		})
	}
}

func TestCreateWorkspaceMintsRefFromTheRepositoryDir(t *testing.T) {
	// Arrange: the daemon names and creates everything, so the answer's ref
	// is minted from the REPOSITORY dir the caller asked to create in.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	resp, err := client.CreateWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
			Repository: &workspacev1.RepositoryRef{Dir: "/tmp/repo/./one"},
			Form: &agentreplv1.CreateWorkspaceRequest_Standard{
				Standard: &agentreplv1.CreateWorkspaceStandard{},
			},
		}))
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}

	// Assert.
	got := resp.Msg.GetSuccess().GetWorkspace()
	if got.GetDir() != "/tmp/repo/one" {
		t.Fatalf("dir = %q, want the cleaned repository dir", got.GetDir())
	}
	if got.GetId() == "" {
		t.Fatalf("id is empty; the daemon mints the created workspace's identity")
	}
}

// A CREATED workspace is a different workspace from the one REGISTERED at the
// same dir, so the two must never mint the same id.
func TestCreateWorkspaceIdIsDistinctFromTheRegisteredIdForOneDir(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	created, err := client.CreateWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
			Repository: &workspacev1.RepositoryRef{Dir: "/tmp/repo/one"},
			Form: &agentreplv1.CreateWorkspaceRequest_Standard{
				Standard: &agentreplv1.CreateWorkspaceStandard{},
			},
		}))
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	registered, err := client.RegisterWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: "/tmp/repo/one"}))
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}

	// Assert.
	if created.Msg.GetSuccess().GetWorkspace().GetId() == registered.Msg.GetSuccess().GetWorkspace().GetId() {
		t.Fatalf("create and register minted one id for one dir: %q",
			created.Msg.GetSuccess().GetWorkspace().GetId())
	}
}
