package main

import (
	"bytes"
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/types/dynamicpb"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// callFake records what the verb sent and answers with a canned response.
type callFake struct {
	sentMethod string
	sentJSON   string
	answer     string
	invokeErr  error
	lookupDir  string
	lookupRef  *workspacev1.WorkspaceRef
	lookupErr  error
	addressErr error
}

func (f *callFake) deps() callDeps {
	return callDeps{
		invoke: func(_ context.Context, _ string, method protoreflect.MethodDescriptor, req *dynamicpb.Message) (*dynamicpb.Message, error) {
			f.sentMethod = string(method.Name())
			raw, err := protojson.Marshal(req)
			if err != nil {
				return nil, err
			}
			f.sentJSON = string(raw)
			if f.invokeErr != nil {
				return nil, f.invokeErr
			}
			resp := dynamicpb.NewMessage(method.Output())
			if err := protojson.Unmarshal([]byte(f.answer), resp); err != nil {
				return nil, err
			}
			return resp, nil
		},
		lookup: func(_ context.Context, _ string, dir string) (*workspacev1.WorkspaceRef, error) {
			f.lookupDir = dir
			return f.lookupRef, f.lookupErr
		},
		address: func(string) (string, error) {
			if f.addressErr != nil {
				return "", f.addressErr
			}
			return "127.0.0.1:1", nil
		},
	}
}

func TestCallVerb(t *testing.T) {
	cases := []struct {
		name       string
		args       []string
		fake       callFake
		wantCode   int
		wantMethod string
		wantSent   string
		wantOut    string
		wantErr    string
	}{
		{
			name:       "a unary method with no JSON sends an empty request",
			args:       []string{"DaemonHealth"},
			fake:       callFake{answer: `{"success":{}}`},
			wantCode:   exitSuccess,
			wantMethod: "DaemonHealth",
			wantSent:   `{}`,
			wantOut:    `"success"`,
		},
		{
			name:       "the response's error arm exits non-zero after printing it",
			args:       []string{"DaemonHealth"},
			fake:       callFake{answer: `{"error":{}}`},
			wantCode:   exitFailure,
			wantMethod: "DaemonHealth",
			wantOut:    `"error"`,
			wantErr:    "error arm",
		},
		{
			name:     "an unknown method is refused before any call",
			args:     []string{"NoSuchMethod"},
			wantCode: exitFailure,
			wantErr:  `no method "NoSuchMethod"`,
		},
		{
			name:     "a streaming method is refused by name",
			args:     []string{"WatchWorkspaceRoster"},
			wantCode: exitFailure,
			wantErr:  "is a stream",
		},
		{
			name:     "a request that is not the method's input is refused",
			args:     []string{"DaemonHealth", `{"bogus":1}`},
			wantCode: exitFailure,
			wantErr:  "is not a agentrepl.v1.DaemonHealthRequest",
		},
		{
			name:     "no serving daemon is a failure naming why",
			args:     []string{"DaemonHealth"},
			fake:     callFake{addressErr: errors.New("no daemon is serving")},
			wantCode: exitFailure,
			wantErr:  "no daemon is serving",
		},
		{
			name:       "a failed call is a failure naming the method",
			args:       []string{"DaemonHealth"},
			fake:       callFake{invokeErr: errors.New("unavailable")},
			wantCode:   exitFailure,
			wantMethod: "DaemonHealth",
			wantErr:    "failed DaemonHealth: unavailable",
		},
		{
			name:       "-workspace fills the request's workspace from the roster",
			args:       []string{"-workspace", "/tmp/ws", "RestartWorkspace"},
			fake:       callFake{answer: `{"success":{}}`, lookupRef: &workspacev1.WorkspaceRef{Id: "w1", Dir: "/tmp/ws"}},
			wantCode:   exitSuccess,
			wantMethod: "RestartWorkspace",
			wantSent:   `{"workspace":{"id":"w1","dir":"/tmp/ws"}}`,
		},
		{
			name:       "-workspace leaves a workspace the JSON already set",
			args:       []string{"-workspace", "/tmp/ws", "RestartWorkspace", `{"workspace":{"id":"given"}}`},
			fake:       callFake{answer: `{"success":{}}`, lookupRef: &workspacev1.WorkspaceRef{Id: "w1"}},
			wantCode:   exitSuccess,
			wantMethod: "RestartWorkspace",
			wantSent:   `{"workspace":{"id":"given"}}`,
		},
		{
			name:     "-workspace on a request with no workspace field is refused",
			args:     []string{"-workspace", "/tmp/ws", "DaemonHealth"},
			wantCode: exitFailure,
			wantErr:  "has no workspace field",
		},
		{
			name:     "-workspace naming no roster row is a failure",
			args:     []string{"-workspace", "/tmp/ws", "RestartWorkspace"},
			fake:     callFake{lookupErr: errors.New("the daemon's roster holds no workspace at /tmp/ws")},
			wantCode: exitFailure,
			wantErr:  "holds no workspace",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			fake := tc.fake
			var out, errOut bytes.Buffer

			// Act
			code := runCallVerb(context.Background(), tc.args, fake.deps(), &out, &errOut)

			// Assert
			if code != tc.wantCode {
				t.Fatalf("exit = %d, want %d (stderr %q)", code, tc.wantCode, errOut.String())
			}
			if fake.sentMethod != tc.wantMethod {
				t.Errorf("sent method = %q, want %q", fake.sentMethod, tc.wantMethod)
			}
			if tc.wantSent != "" && compactJSON(fake.sentJSON) != compactJSON(tc.wantSent) {
				t.Errorf("sent %s, want %s", fake.sentJSON, tc.wantSent)
			}
			if tc.wantOut != "" && !strings.Contains(out.String(), tc.wantOut) {
				t.Errorf("stdout %q lacks %q", out.String(), tc.wantOut)
			}
			if tc.wantErr != "" && !strings.Contains(errOut.String(), tc.wantErr) {
				t.Errorf("stderr %q lacks %q", errOut.String(), tc.wantErr)
			}
		})
	}
}

// compactJSON drops whitespace so two protojson renderings compare equal.
func compactJSON(s string) string {
	return strings.Join(strings.Fields(s), "")
}

func TestInvokeUnaryOverSendsADynamicRequestAndDecodesTheAnswer(t *testing.T) {
	// Arrange
	mux := http.NewServeMux()
	mux.Handle("/agentrepl.v1.AgentRepl/DaemonHealth", connect.NewUnaryHandler(
		"/agentrepl.v1.AgentRepl/DaemonHealth",
		func(_ context.Context, _ *connect.Request[agentreplv1.DaemonHealthRequest]) (*connect.Response[agentreplv1.DaemonHealthResponse], error) {
			return connect.NewResponse(&agentreplv1.DaemonHealthResponse{
				Result: &agentreplv1.DaemonHealthResponse_Error{Error: &agentreplv1.DaemonHealthError{}},
			}), nil
		}))
	server := httptest.NewServer(mux)
	defer server.Close()
	method := agentReplService().Methods().ByName("DaemonHealth")

	// Act
	resp, err := invokeUnaryOver(context.Background(), server.Client(), server.URL, method, dynamicpb.NewMessage(method.Input()))

	// Assert
	if err != nil {
		t.Fatalf("invokeUnaryOver = %v", err)
	}
	if !answeredError(resp) {
		t.Fatalf("answer %v, want the error arm decoded", resp)
	}
}
