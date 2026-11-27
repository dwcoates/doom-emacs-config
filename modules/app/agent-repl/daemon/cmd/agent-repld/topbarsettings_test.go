package main

import (
	"context"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

// topbarAnswers are the answers the fixture's topbar setters give; a nil
// field is the rpc's success.
type topbarAnswers struct {
	permission   *agentreplv1.SetPermissionModeResponse
	effort       *agentreplv1.SetEffortResponse
	account      *agentreplv1.SelectAccountResponse
	defaultModel *agentreplv1.SetDefaultModelResponse
	wifi         *agentreplv1.UpdatePersistentWifiModeResponse
}

func (c *cliClient) SetPermissionMode(_ context.Context, req *connect.Request[agentreplv1.SetPermissionModeRequest]) (*connect.Response[agentreplv1.SetPermissionModeResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.topbar.permission != nil {
		return connect.NewResponse(c.topbar.permission), nil
	}
	return connect.NewResponse(&agentreplv1.SetPermissionModeResponse{Result: &agentreplv1.SetPermissionModeResponse_Success{Success: &agentreplv1.SetPermissionModeSuccess{}}}), nil
}

func (c *cliClient) SetEffort(_ context.Context, req *connect.Request[agentreplv1.SetEffortRequest]) (*connect.Response[agentreplv1.SetEffortResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.topbar.effort != nil {
		return connect.NewResponse(c.topbar.effort), nil
	}
	return connect.NewResponse(&agentreplv1.SetEffortResponse{Result: &agentreplv1.SetEffortResponse_Success{Success: &agentreplv1.SetEffortSuccess{}}}), nil
}

func (c *cliClient) SelectAccount(_ context.Context, req *connect.Request[agentreplv1.SelectAccountRequest]) (*connect.Response[agentreplv1.SelectAccountResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.topbar.account != nil {
		return connect.NewResponse(c.topbar.account), nil
	}
	return connect.NewResponse(&agentreplv1.SelectAccountResponse{Result: &agentreplv1.SelectAccountResponse_Success{Success: &agentreplv1.SelectAccountSuccess{LoggedIn: true}}}), nil
}

func (c *cliClient) SetDefaultModel(_ context.Context, req *connect.Request[agentreplv1.SetDefaultModelRequest]) (*connect.Response[agentreplv1.SetDefaultModelResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.topbar.defaultModel != nil {
		return connect.NewResponse(c.topbar.defaultModel), nil
	}
	return connect.NewResponse(&agentreplv1.SetDefaultModelResponse{Result: &agentreplv1.SetDefaultModelResponse_Success{Success: &agentreplv1.SetDefaultModelSuccess{}}}), nil
}

func (c *cliClient) UpdatePersistentWifiMode(_ context.Context, req *connect.Request[agentreplv1.UpdatePersistentWifiModeRequest]) (*connect.Response[agentreplv1.UpdatePersistentWifiModeResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.topbar.wifi != nil {
		return connect.NewResponse(c.topbar.wifi), nil
	}
	return connect.NewResponse(&agentreplv1.UpdatePersistentWifiModeResponse{Result: &agentreplv1.UpdatePersistentWifiModeResponse_Success{Success: &agentreplv1.UpdatePersistentWifiModeSuccess{}}}), nil
}

func TestSetPermissionModeSendsTheModeForTheNamedWorkspace(t *testing.T) {
	tests := []struct {
		name   string
		ws     string
		wantID string
	}{
		{name: "another workspace by its name", ws: "peer", wantID: "ws-peer"},
		{name: "the calling workspace, as here", ws: "here", wantID: "ws-caller"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, "set", tt.ws, "-permission-mode", "plan")

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			req := only[*agentreplv1.SetPermissionModeRequest](t, f)
			if req.GetWorkspace().GetId() != tt.wantID || req.GetMode() != "plan" {
				t.Errorf("request = %v, want mode plan for %s", req, tt.wantID)
			}
		})
	}
}

func TestSetPermissionModeRefusesABlankModeBeforeSending(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "set", "peer", "-permission-mode", " ")

	// Assert
	if code != exitFailure || len(f.client.requests) != 0 || !strings.Contains(f.errOut.String(), "-permission-mode names a mode") {
		t.Fatalf("exit %d, requests %v, stderr %q; want a usage failure and nothing sent", code, f.client.requests, f.errOut.String())
	}
}

func TestSetEffortSendsTheLevel(t *testing.T) {
	tests := []struct {
		word string
		want conversationv1.AgentEffortLevel
	}{
		{"low", conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW},
		{"medium", conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MEDIUM},
		{"high", conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH},
		{"xhigh", conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_XHIGH},
		{"max", conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MAX},
	}
	for _, tt := range tests {
		t.Run(tt.word, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, "set", "peer", "-effort", tt.word)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			req := only[*agentreplv1.SetEffortRequest](t, f)
			if req.GetWorkspace().GetId() != "ws-peer" || req.GetEffort() != tt.want {
				t.Errorf("request = %v, want %v for ws-peer", req, tt.want)
			}
		})
	}
}

func TestSetEffortRefusesALevelTheSelectorDoesNotServe(t *testing.T) {
	tests := []struct {
		name string
		word string
	}{
		{name: "a misspelling", word: "extreme"},
		{name: "a different case", word: "High"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, "set", "peer", "-effort", tt.word)

			// Assert
			if code != exitFailure || len(f.client.requests) != 0 || !strings.Contains(f.errOut.String(), "-effort is one of low, medium, high, xhigh, max") {
				t.Fatalf("exit %d, requests %v, stderr %q; want a usage failure and nothing sent", code, f.client.requests, f.errOut.String())
			}
		})
	}
}

func TestSetAccountSendsTheLoginRoot(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "set", "peer", "-account", "/home/u/.claude-work")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	req := only[*agentreplv1.SelectAccountRequest](t, f)
	if req.GetWorkspace().GetId() != "ws-peer" || req.GetConfigDir() != "/home/u/.claude-work" {
		t.Errorf("request = %v, want the work root for ws-peer", req)
	}
}

func TestSetDefaultModelSendsTheModel(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "set", "peer", "-default-model", "claude-opus-5-5")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	req := only[*agentreplv1.SetDefaultModelRequest](t, f)
	if req.GetWorkspace().GetId() != "ws-peer" || req.GetModel().GetName() != "claude-opus-5-5" {
		t.Errorf("request = %v, want the model for ws-peer", req)
	}
}

func TestTopbarSettersFailOnAWorkspaceTheRosterLacks(t *testing.T) {
	for _, flag := range []string{"-permission-mode", "-effort", "-account", "-default-model"} {
		t.Run(flag, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, "set", "nobody", flag, "high")

			// Assert
			if code != exitFailure || len(f.client.requests) != 0 || !strings.Contains(f.errOut.String(), `no workspace named "nobody"`) {
				t.Fatalf("exit %d, requests %v, stderr %q; want the unknown name and nothing sent", code, f.client.requests, f.errOut.String())
			}
		})
	}
}

func TestTopbarSettersPrintTheDaemonsRefusalByName(t *testing.T) {
	tests := []struct {
		name  string
		flag  string
		value string
		set   func(*topbarAnswers)
		want  string
	}{
		{name: "permission mode not served", flag: "-permission-mode", value: "bypass", want: `"modeNotServed"`,
			set: func(a *topbarAnswers) {
				a.permission = &agentreplv1.SetPermissionModeResponse{Result: &agentreplv1.SetPermissionModeResponse_Error{Error: &agentreplv1.SetPermissionModeError{
					Cause: &agentreplv1.SetPermissionModeError_ModeNotServed{ModeNotServed: &agentreplv1.SetPermissionModeModeNotServed{}}}}}
			}},
		{name: "permission mode ungated without consent", flag: "-permission-mode", value: "bypass", want: `"ungatedWithoutConsent"`,
			set: func(a *topbarAnswers) {
				a.permission = &agentreplv1.SetPermissionModeResponse{Result: &agentreplv1.SetPermissionModeResponse_Error{Error: &agentreplv1.SetPermissionModeError{
					Cause: &agentreplv1.SetPermissionModeError_UngatedWithoutConsent{UngatedWithoutConsent: &agentreplv1.SetPermissionModeUngatedWithoutConsent{}}}}}
			}},
		{name: "effort not supported", flag: "-effort", value: "max", want: `"notSupported"`,
			set: func(a *topbarAnswers) {
				a.effort = &agentreplv1.SetEffortResponse{Result: &agentreplv1.SetEffortResponse_Error{Error: &agentreplv1.SetEffortError{
					Cause: &agentreplv1.SetEffortError_NotSupported{NotSupported: &agentreplv1.SetEffortNotSupported{}}}}}
			}},
		{name: "account unknown", flag: "-account", value: "/nowhere", want: `"unknownAccount"`,
			set: func(a *topbarAnswers) {
				a.account = &agentreplv1.SelectAccountResponse{Result: &agentreplv1.SelectAccountResponse_Error{Error: &agentreplv1.SelectAccountError{
					Cause: &agentreplv1.SelectAccountError_UnknownAccount{UnknownAccount: &agentreplv1.SelectAccountUnknownAccount{}}}}}
			}},
		{name: "default model not in catalog", flag: "-default-model", value: "nope", want: `"notInCatalog"`,
			set: func(a *topbarAnswers) {
				a.defaultModel = &agentreplv1.SetDefaultModelResponse{Result: &agentreplv1.SetDefaultModelResponse_Error{Error: &agentreplv1.SetDefaultModelError{
					Cause: &agentreplv1.SetDefaultModelError_NotInCatalog{NotInCatalog: &agentreplv1.SetDefaultModelNotInCatalog{}}}}}
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			tt.set(&f.client.topbar)

			// Act
			code := f.run(workspaceNoun, "set", "peer", tt.flag, tt.value)

			// Assert
			if code != exitRefused || !strings.Contains(f.out.String(), tt.want) || !strings.Contains(f.errOut.String(), "peer") {
				t.Fatalf("exit %d, printed %q, stderr %q; want exit %d, %s printed and the refusal said about peer", code, f.out.String(), f.errOut.String(), exitRefused, tt.want)
			}
		})
	}
}

func TestRuntimePersistentWifiSendsTheMode(t *testing.T) {
	tests := []struct {
		mode   string
		wantOn bool
	}{
		{mode: "on", wantOn: true},
		{mode: "off"},
	}
	for _, tt := range tests {
		t.Run(tt.mode, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(runtimeNoun, "persistent-wifi", tt.mode)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			req := only[*agentreplv1.UpdatePersistentWifiModeRequest](t, f)
			if (req.GetOn() != nil) != tt.wantOn || (req.GetOff() != nil) == tt.wantOn {
				t.Errorf("request = %v, want %s", req, tt.mode)
			}
		})
	}
}

func TestRuntimePersistentWifiRefusesAnythingButOnOrOff(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "a toggle, which is the editor's", args: []string{"toggle"}},
		{name: "no mode", args: nil},
		{name: "two modes", args: []string{"on", "off"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(runtimeNoun, append([]string{"persistent-wifi"}, tt.args...)...)

			// Assert
			if code != exitFailure || len(f.client.requests) != 0 {
				t.Fatalf("exit %d, requests %v, stderr %q; want a usage failure and nothing sent", code, f.client.requests, f.errOut.String())
			}
		})
	}
}

func TestRuntimePersistentWifiPrintsTheDaemonsRefusalByName(t *testing.T) {
	tests := []struct {
		name  string
		cause *agentreplv1.UpdatePersistentWifiModeError
		want  string
	}{
		{name: "the sudo grant refused", cause: &agentreplv1.UpdatePersistentWifiModeError{
			Cause: &agentreplv1.UpdatePersistentWifiModeError_PowerSettingsRefused{PowerSettingsRefused: &agentreplv1.UpdatePersistentWifiModePowerSettingsRefused{Detail: "sudo: a password is required"}}},
			want: `"powerSettingsRefused"`},
		{name: "the mode unreadable", cause: &agentreplv1.UpdatePersistentWifiModeError{
			Cause: &agentreplv1.UpdatePersistentWifiModeError_ModeUnreadable{ModeUnreadable: &agentreplv1.UpdatePersistentWifiModeModeUnreadable{Detail: "pmset failed"}}},
			want: `"modeUnreadable"`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			f.client.topbar.wifi = &agentreplv1.UpdatePersistentWifiModeResponse{Result: &agentreplv1.UpdatePersistentWifiModeResponse_Error{Error: tt.cause}}

			// Act
			code := f.run(runtimeNoun, "persistent-wifi", "on")

			// Assert
			if code != exitRefused || !strings.Contains(f.out.String(), tt.want) {
				t.Fatalf("exit %d, printed %q; want exit %d and %s printed", code, f.out.String(), exitRefused, tt.want)
			}
		})
	}
}

// topbarSettingRefusals is each topbar setter's describer with its cases.
var topbarSettingRefusals = func() []describedRefusal {
	permission := &agentreplv1.SetPermissionModeError{}
	effort := &agentreplv1.SetEffortError{}
	account := &agentreplv1.SelectAccountError{}
	defaultModel := &agentreplv1.SetDefaultModelError{}
	wifi := &agentreplv1.UpdatePersistentWifiModeError{}
	return []describedRefusal{
		{name: "permission mode", empty: permission, oneof: "cause",
			cases: append(sharedWorkspaceCases(permission, "set it again", theFourWorkspaceArms...),
				refusalCase{name: "mode_not_served", err: armed(permission, "mode_not_served", nil),
					want: `the mode "plan" is not among the permission modes ws's picker serves`},
				refusalCase{name: "ungated_without_consent", err: armed(permission, "ungated_without_consent", nil),
					want: `the mode "plan" is ungated, and ws's creation recorded no consent to an ungated mode`},
				refusalCase{name: "no_session", err: armed(permission, "no_session", nil), want: "ws has no session to set a permission mode on"},
				refusalCase{name: "vendor_refused", err: armedWith(permission, "vendor_refused", map[string]any{"detail": "no"}),
					want: `the vendor refused the permission mode "plan" for ws (no)`}),
			describe: func(e proto.Message) (string, error) {
				return describePermissionModeRefusal("ws", "plan", e.(*agentreplv1.SetPermissionModeError))
			}},
		{name: "effort", empty: effort, oneof: "cause",
			cases: append(sharedWorkspaceCases(effort, "set it again", theFourWorkspaceArms...),
				refusalCase{name: "no_session", err: armed(effort, "no_session", nil), want: "ws has no session to set an effort on"},
				refusalCase{name: "not_supported", err: armed(effort, "not_supported", nil),
					want: `ws's model does not accept the effort "high", or takes no effort level at all`},
				refusalCase{name: "vendor_refused", err: armedWith(effort, "vendor_refused", map[string]any{"detail": "no"}),
					want: `the vendor refused the effort "high" for ws (no)`}),
			describe: func(e proto.Message) (string, error) {
				return describeEffortRefusal("ws", "high", e.(*agentreplv1.SetEffortError))
			}},
		{name: "account", empty: account, oneof: "cause",
			cases: append(sharedWorkspaceCases(account, "set it again", theFourWorkspaceArms...),
				refusalCase{name: "unknown_account", err: armed(account, "unknown_account", nil),
					want: `the login root "/r" is not one the daemon knows, so ws still spends as its own account`}),
			describe: func(e proto.Message) (string, error) {
				return describeSelectAccountRefusal("ws", "/r", e.(*agentreplv1.SelectAccountError))
			}},
		{name: "default model", empty: defaultModel, oneof: "cause",
			cases: append(sharedWorkspaceCases(defaultModel, "set it again", theFourWorkspaceArms...),
				refusalCase{name: "no_session", err: armed(defaultModel, "no_session", nil), want: "ws has no session whose login the default model could be set for"},
				refusalCase{name: "not_in_catalog", err: armed(defaultModel, "not_in_catalog", nil),
					want: `the model "m" is not in the catalog of ws's login, so the default is unchanged`}),
			describe: func(e proto.Message) (string, error) {
				return describeDefaultModelRefusal("ws", "m", e.(*agentreplv1.SetDefaultModelError))
			}},
		{name: "persistent wifi", empty: wifi, oneof: "cause",
			cases: []refusalCase{
				{name: "power_settings_refused", err: armedWith(wifi, "power_settings_refused", map[string]any{"detail": "sudo: no grant"}),
					want: "changing the power settings was refused (sudo: no grant); the passwordless sudo grant for the exact pmset invocations is absent or refused"},
				{name: "mode_unreadable", err: armedWith(wifi, "mode_unreadable", map[string]any{"detail": "pmset failed"}),
					want: "the current persistent wifi mode could not be read (pmset failed), so nothing was changed"},
			},
			describe: func(e proto.Message) (string, error) {
				return describePersistentWifiRefusal(e.(*agentreplv1.UpdatePersistentWifiModeError))
			}},
	}
}()

func TestDescribeTopbarSettingRefusals(t *testing.T) {
	testDescribedRefusals(t, topbarSettingRefusals)
}
