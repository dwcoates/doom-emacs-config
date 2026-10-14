package server

import (
	"context"
	"errors"
	"strings"
	"time"
	"unicode"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	"agent-repld/internal/ctxend"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

const requestIDHeader = "X-Agent-Repl-Request-Id"

func boundaryOperation(rpc string) string {
	var name strings.Builder
	name.WriteString("daemon.server.")
	for i, letter := range rpc {
		if i > 0 && unicode.IsUpper(letter) {
			name.WriteByte('_')
		}
		name.WriteRune(unicode.ToLower(letter))
	}
	return name.String()
}

type requestBoundary struct {
	log       dlog.Logger
	rpc       string
	requestID string
	workspace string
	started   time.Time
}

func (s *server) beginRequest(
	ctx context.Context,
	rpc string,
	headerValue string,
	msg proto.Message,
) (requestBoundary, error) {
	log := s.log
	workspaceIDText := requestWorkspaceID(msg)
	if workspaceIDText != "" {
		workspaceID := ids.WorkspaceID(workspaceIDText)
		standing, err := s.deps.Ownership.Standing(ctx, workspaceID)
		switch {
		case err != nil:
			s.log.Error("daemon.server.request_boundary", "could not resolve the request's serving standing",
				dlog.Context{"rpc": rpc, "workspace_id": workspaceIDText, "cause": err.Error()})
			return requestBoundary{}, err
		case standing != workspace.StandingOwned:
			log = log.With(dlog.Context{"workspace_id": workspaceIDText, "workspace_owned": false})
		default:
			// THE BOUNDARY'S READ IS ORDERED AGAINST CLOSE, as every
			// resolution read is (readRegistry): a lifetime check before it
			// left the state client free to close in between, and a ClientLog
			// arriving during a drain's exit met "sql: database is closed" at
			// ERROR here.
			record, ended, err := s.registryWorkspace(ctx, workspaceID)
			switch {
			case ended:
				log = log.With(dlog.Context{"workspace_id": workspaceIDText, "workspace_owned": false})
			case err == nil:
				// THE BOUNDARY NEVER FAILS A REQUEST OVER ITS OWN LOGGING. A
				// workspace whose directory is a scratch path or has been
				// deleted resolves to the central sink with the workspace
				// named on the record; the handler then answers the request on
				// its own terms — a typed refusal where one is owed — instead
				// of the client meeting an internal error raised by logging.
				log = s.deps.Log.WorkspaceOrCentral(record.Dir).
					With(dlog.Context{dlog.KeyWorkspaceID: workspaceIDText, dlog.KeyWorkspaceDir: record.Dir})
			case errors.Is(err, wsm.ErrNotFound):
				log = log.With(dlog.Context{"workspace_id": workspaceIDText, "workspace_known": false})
			case ctxend.Is(err):
				// The caller went away before its workspace was read: the
				// request is over, and that is not a fault of the daemon's.
				s.log.Info("daemon.server.request_boundary", "the request's workspace was not read; the request's context ended",
					dlog.Context{"rpc": rpc, "workspace_id": workspaceIDText, "cause": err.Error()})
				return requestBoundary{}, err
			default:
				s.log.Error("daemon.server.request_boundary", "could not resolve the request's workspace",
					dlog.Context{"rpc": rpc, "workspace_id": workspaceIDText, "cause": err.Error()})
				return requestBoundary{}, err
			}
		}
	}
	if headerValue != "" {
		log = log.With(dlog.Context{dlog.KeyRequestID: headerValue})
	}
	return requestBoundary{
		log:       log,
		rpc:       rpc,
		requestID: headerValue,
		workspace: workspaceIDText,
		started:   time.Now(),
	}, nil
}

func requestWorkspaceID(msg proto.Message) string {
	if msg == nil {
		return ""
	}
	ref := msg.ProtoReflect()
	field := ref.Descriptor().Fields().ByName(protoreflect.Name("workspace"))
	if field == nil || field.Kind() != protoreflect.MessageKind || !ref.Has(field) {
		return ""
	}
	workspace := ref.Get(field).Message()
	id := workspace.Descriptor().Fields().ByName(protoreflect.Name("id"))
	if id == nil || id.Kind() != protoreflect.StringKind || !workspace.Has(id) {
		return ""
	}
	return workspace.Get(id).String()
}

// enter records the rpc's entry under its boundary operation and answers the
// record of its completion, which the caller defers over its named error.
func (b requestBoundary) enter() func(*error) {
	operation := boundaryOperation(b.rpc)
	b.log.Debug(operation, "entered the rpc handler", b.entryContext())
	b.log.Debug(operation, "resolved the rpc request scope", b.entryContext())
	b.log.Debug(operation, "delegated the rpc request", b.entryContext())
	return func(err *error) {
		b.log.Debug(operation, "completed the rpc handler", b.completionContext(*err))
	}
}

func (b requestBoundary) entryContext() dlog.Context {
	return dlog.Context{
		"rpc":          b.rpc,
		"request_id":   b.requestID,
		"workspace_id": b.workspace,
	}
}

func (b requestBoundary) completionContext(err error) dlog.Context {
	outcome := "success"
	if err != nil {
		outcome = "error"
		if ctxend.Is(err) {
			outcome = "cancelled"
		}
	}
	ctx := dlog.Context{
		"rpc":          b.rpc,
		"request_id":   b.requestID,
		"workspace_id": b.workspace,
		"outcome":      outcome,
		"duration_ms":  time.Since(b.started).Milliseconds(),
	}
	if err != nil {
		ctx["cause"] = err.Error()
	}
	return ctx
}

func requestMessage[T any](req *connect.Request[T]) proto.Message {
	msg, _ := any(req.Msg).(proto.Message)
	return msg
}
