package main

import (
	"encoding/json"
	"fmt"
	"net/http"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/protojson"
)

// The control plane rides the SAME mux as agentrepl.v1 under /_fake/, so a
// test drives the fake over the one address it already discovered from
// daemon.addr.

type scriptRequest struct {
	Method   string          `json:"method"`
	Response json.RawMessage `json:"response"`
}

type pushRequest struct {
	Stream      string          `json:"stream"`
	WorkspaceID string          `json:"workspace_id"`
	Message     json.RawMessage `json:"message"`
	Snapshot    bool            `json:"snapshot"`
}

type endErrorSpec struct {
	Code    string `json:"code"`
	Message string `json:"message"`
}

type endRequestBody struct {
	Stream      string        `json:"stream"`
	WorkspaceID string        `json:"workspace_id"`
	Error       *endErrorSpec `json:"error"`
	Abort       bool          `json:"abort"`
}

func writeJSON(w http.ResponseWriter, status int, body any) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	if err := json.NewEncoder(w).Encode(body); err != nil {
		logError("fakedaemon.control.write-failed", "could not write a control-plane answer",
			map[string]any{"error": err.Error()})
	}
}

func badRequest(w http.ResponseWriter, operation string, err error) {
	logWarn(operation, "control-plane request refused", map[string]any{"error": err.Error()})
	writeJSON(w, http.StatusBadRequest, map[string]string{"error": err.Error()})
}

// decodeControlBody parses a control-plane body STRICTLY: an unknown field is
// a caller mistake, not something to ignore.
func decodeControlBody(r *http.Request, into any) error {
	dec := json.NewDecoder(r.Body)
	dec.DisallowUnknownFields()
	if err := dec.Decode(into); err != nil {
		return fmt.Errorf("parse control body: %w", err)
	}
	return nil
}

func (s *fakeServer) handleScript(w http.ResponseWriter, r *http.Request) {
	var body scriptRequest
	if err := decodeControlBody(r, &body); err != nil {
		badRequest(w, "fakedaemon.control.script-unparseable", err)
		return
	}
	factory, ok := unaryResponseTypes[body.Method]
	if !ok {
		badRequest(w, "fakedaemon.control.script-unknown-method",
			fmt.Errorf("unknown unary method %q", body.Method))
		return
	}
	// Validate by unmarshalling into the GENERATED response type: a scripted
	// body that the real schema rejects must never reach a client.
	if err := protojson.Unmarshal(body.Response, factory()); err != nil {
		badRequest(w, "fakedaemon.control.script-invalid-response",
			fmt.Errorf("response is not a valid %s: %w", body.Method, err))
		return
	}
	s.script(body.Method, body.Response)
	writeJSON(w, http.StatusOK, map[string]string{"scripted": body.Method})
}

func (s *fakeServer) handlePush(w http.ResponseWriter, r *http.Request) {
	var body pushRequest
	if err := decodeControlBody(r, &body); err != nil {
		badRequest(w, "fakedaemon.control.push-unparseable", err)
		return
	}
	factory, ok := streamResponseTypes[body.Stream]
	if !ok {
		badRequest(w, "fakedaemon.control.push-unknown-stream",
			fmt.Errorf("unknown stream %q", body.Stream))
		return
	}
	if body.Stream == streamHost && body.WorkspaceID == "" {
		badRequest(w, "fakedaemon.control.push-missing-workspace",
			fmt.Errorf("the host stream is keyed by workspace id; workspace_id is required"))
		return
	}
	if body.Stream != streamHost && body.WorkspaceID != "" {
		badRequest(w, "fakedaemon.control.push-unexpected-workspace",
			fmt.Errorf("stream %q is workspace-independent; workspace_id must be omitted", body.Stream))
		return
	}
	msg := factory()
	if err := protojson.Unmarshal(body.Message, msg); err != nil {
		badRequest(w, "fakedaemon.control.push-invalid-message",
			fmt.Errorf("message is not a valid %s push: %w", body.Stream, err))
		return
	}
	delivered := s.push(body.Stream, body.WorkspaceID, msg, body.Snapshot)
	writeJSON(w, http.StatusOK, map[string]any{"delivered": delivered, "snapshot": body.Snapshot})
}

// connectCodes is the closed set of Connect error codes /_fake/end accepts.
var connectCodes = map[string]connect.Code{
	"canceled":            connect.CodeCanceled,
	"unknown":             connect.CodeUnknown,
	"invalid_argument":    connect.CodeInvalidArgument,
	"deadline_exceeded":   connect.CodeDeadlineExceeded,
	"not_found":           connect.CodeNotFound,
	"already_exists":      connect.CodeAlreadyExists,
	"permission_denied":   connect.CodePermissionDenied,
	"resource_exhausted":  connect.CodeResourceExhausted,
	"failed_precondition": connect.CodeFailedPrecondition,
	"aborted":             connect.CodeAborted,
	"out_of_range":        connect.CodeOutOfRange,
	"unimplemented":       connect.CodeUnimplemented,
	"internal":            connect.CodeInternal,
	"unavailable":         connect.CodeUnavailable,
	"data_loss":           connect.CodeDataLoss,
	"unauthenticated":     connect.CodeUnauthenticated,
}

func (s *fakeServer) handleEnd(w http.ResponseWriter, r *http.Request) {
	var body endRequestBody
	if err := decodeControlBody(r, &body); err != nil {
		badRequest(w, "fakedaemon.control.end-unparseable", err)
		return
	}
	if _, ok := streamResponseTypes[body.Stream]; !ok {
		badRequest(w, "fakedaemon.control.end-unknown-stream",
			fmt.Errorf("unknown stream %q", body.Stream))
		return
	}
	if body.Abort && body.Error != nil {
		badRequest(w, "fakedaemon.control.end-abort-with-error",
			fmt.Errorf("abort drops the connection without an end frame; it cannot carry an error"))
		return
	}
	req := endRequest{abort: body.Abort}
	if body.Error != nil {
		code, ok := connectCodes[body.Error.Code]
		if !ok {
			badRequest(w, "fakedaemon.control.end-unknown-code",
				fmt.Errorf("unknown connect code %q", body.Error.Code))
			return
		}
		req.err = connect.NewError(code, fmt.Errorf("%s", body.Error.Message))
	}
	ended := s.endStreams(body.Stream, body.WorkspaceID, req)
	writeJSON(w, http.StatusOK, map[string]any{"ended": ended})
}

func (s *fakeServer) handleCalls(w http.ResponseWriter, _ *http.Request) {
	calls := s.recordedCalls()
	if calls == nil {
		calls = []recordedCall{}
	}
	writeJSON(w, http.StatusOK, calls)
}

func (s *fakeServer) handleSubscribers(w http.ResponseWriter, _ *http.Request) {
	writeJSON(w, http.StatusOK, s.subscriberInfos())
}

// controlMux registers every /_fake/ endpoint on MUX.  EXIT is the orderly
// shutdown the process performs after answering /_fake/exit.
func (s *fakeServer) registerControlPlane(mux *http.ServeMux, exit func()) {
	post := func(name string, h http.HandlerFunc) http.HandlerFunc {
		return func(w http.ResponseWriter, r *http.Request) {
			if r.Method != http.MethodPost {
				badRequest(w, "fakedaemon.control.wrong-method",
					fmt.Errorf("%s requires POST, got %s", name, r.Method))
				return
			}
			h(w, r)
		}
	}
	mux.HandleFunc("/_fake/script", post("/_fake/script", s.handleScript))
	mux.HandleFunc("/_fake/push", post("/_fake/push", s.handlePush))
	mux.HandleFunc("/_fake/end", post("/_fake/end", s.handleEnd))
	mux.HandleFunc("/_fake/calls", s.handleCalls)
	mux.HandleFunc("/_fake/subscribers", s.handleSubscribers)
	mux.HandleFunc("/_fake/exit", post("/_fake/exit", func(w http.ResponseWriter, _ *http.Request) {
		writeJSON(w, http.StatusOK, map[string]string{"exiting": "ok"})
		logInfo("fakedaemon.control.exit", "orderly exit requested", nil)
		go exit()
	}))
}
