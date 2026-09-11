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

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
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
		case standing != workspace.StandingOwned || s.life.Err() != nil:
			log = log.With(dlog.Context{"workspace_id": workspaceIDText, "workspace_owned": false})
		default:
			record, err := s.deps.DB.Workspace(ctx, workspaceID)
			switch {
			case err == nil:
				log, err = s.deps.Log.Workspace(record.Dir)
				if err != nil {
					s.log.Error("daemon.server.request_boundary", "could not resolve the request's workspace log sink",
						dlog.Context{"rpc": rpc, "workspace_id": workspaceIDText, "workspace_dir": record.Dir, "cause": err.Error()})
					return requestBoundary{}, err
				}
				log = log.With(dlog.Context{dlog.KeyWorkspaceID: workspaceIDText, dlog.KeyWorkspaceDir: record.Dir})
			case errors.Is(err, wsm.ErrNotFound):
				log = log.With(dlog.Context{"workspace_id": workspaceIDText, "workspace_known": false})
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
		if errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded) {
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
