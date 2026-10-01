package main

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"path/filepath"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
)

// maxFillDepth bounds the recursive default synthesis.  No agentrepl.v1
// response nests message fields deeper than this; the cap only exists so a
// future self-referential message cannot hang the fake.
const maxFillDepth = 8

// applyDefault synthesizes "a sensible success" for METHOD into OUT.
//
// The generic rule, which covers every verb: a response's `result` oneof gets
// its `success` arm, and every non-optional message field reachable from
// there is filled with a legal empty value (an unset non-optional field is
// ILLEGAL on this contract, so a default that left one unset would make every
// client raise instead of exercising the path under test).  A oneof inside
// the success message resolves to its FIRST arm — DaemonHealthSuccess.health
// becomes `healthy`, SubmitPromptSuccess.outcome becomes `turn`.
//
// Methods whose success carries daemon-minted identity are then specialized
// below, because an empty WorkspaceRef is not a legal echo token.
func applyDefault(method string, req, out proto.Message) error {
	m := out.ProtoReflect()
	desc := m.Descriptor()
	if od := desc.Oneofs().ByName("result"); od != nil {
		fd := od.Fields().ByName("success")
		if fd == nil {
			return fmt.Errorf("%s: response `result` oneof has no `success` arm", method)
		}
		if fd.Kind() != protoreflect.MessageKind {
			return fmt.Errorf("%s: `success` arm is not a message", method)
		}
		success := m.NewField(fd)
		fillRequired(success.Message(), 0)
		m.Set(fd, success)
	}
	return specializeDefault(method, req, out)
}

// fillRequired populates every non-optional message field of M (recursively)
// and resolves every oneof to its first arm.  Scalars keep their proto3
// defaults, which protojson omits — exactly what a real daemon emits.
func fillRequired(m protoreflect.Message, depth int) {
	if depth >= maxFillDepth {
		return
	}
	desc := m.Descriptor()
	for i := 0; i < desc.Oneofs().Len(); i++ {
		od := desc.Oneofs().Get(i)
		if od.IsSynthetic() {
			// A synthetic oneof is the `optional` keyword's encoding; absence
			// is a legal value there and must stay absent.
			continue
		}
		fd := od.Fields().Get(0)
		if fd.Kind() != protoreflect.MessageKind {
			continue
		}
		arm := m.NewField(fd)
		fillRequired(arm.Message(), depth+1)
		m.Set(fd, arm)
	}
	for i := 0; i < desc.Fields().Len(); i++ {
		fd := desc.Fields().Get(i)
		if fd.ContainingOneof() != nil || fd.IsList() || fd.IsMap() {
			continue
		}
		if fd.HasOptionalKeyword() || fd.Kind() != protoreflect.MessageKind {
			continue
		}
		child := m.NewField(fd)
		fillRequired(child.Message(), depth+1)
		m.Set(fd, child)
	}
}

// workspaceID mints the id the fake hands back for DIR: `ws-<sha of dir>`.
// It is opaque to every client by contract (workspace.v1's opacity comment),
// so the only property that matters is that it is stable per directory.
func workspaceID(dir string) string {
	sum := sha256.Sum256([]byte(filepath.Clean(dir)))
	return "ws-" + hex.EncodeToString(sum[:])[:12]
}

// setWorkspaceRef writes {id, dir} into the message field named FIELD of M.
func setWorkspaceRef(m protoreflect.Message, field, id, dir string) error {
	fd := m.Descriptor().Fields().ByName(protoreflect.Name(field))
	if fd == nil || fd.Kind() != protoreflect.MessageKind {
		return fmt.Errorf("no message field %q", field)
	}
	ref := m.NewField(fd)
	rm := ref.Message()
	idFd := rm.Descriptor().Fields().ByName("id")
	dirFd := rm.Descriptor().Fields().ByName("dir")
	if idFd == nil || dirFd == nil {
		return fmt.Errorf("field %q is not a workspace ref", field)
	}
	rm.Set(idFd, protoreflect.ValueOfString(id))
	rm.Set(dirFd, protoreflect.ValueOfString(dir))
	m.Set(fd, ref)
	return nil
}

// successMessage returns the `success` arm of OUT's `result` oneof.
func successMessage(out proto.Message) (protoreflect.Message, error) {
	m := out.ProtoReflect()
	od := m.Descriptor().Oneofs().ByName("result")
	if od == nil {
		return nil, fmt.Errorf("response has no `result` oneof")
	}
	fd := od.Fields().ByName("success")
	if fd == nil {
		return nil, fmt.Errorf("`result` oneof has no `success` arm")
	}
	return m.Mutable(fd).Message(), nil
}

// stringField reads a top-level string field off a request message.
func stringField(req proto.Message, field string) string {
	m := req.ProtoReflect()
	fd := m.Descriptor().Fields().ByName(protoreflect.Name(field))
	if fd == nil || fd.Kind() != protoreflect.StringKind {
		return ""
	}
	return m.Get(fd).String()
}

// repositoryDir reads CreateWorkspaceRequest.repository.dir.
func repositoryDir(req proto.Message) string {
	m := req.ProtoReflect()
	fd := m.Descriptor().Fields().ByName("repository")
	if fd == nil || fd.Kind() != protoreflect.MessageKind {
		return ""
	}
	repo := m.Get(fd).Message()
	dirFd := repo.Descriptor().Fields().ByName("dir")
	if dirFd == nil {
		return ""
	}
	return repo.Get(dirFd).String()
}

// specializeDefault overrides the generic synthesis where the contract wants
// a daemon-minted value rather than an empty one.
func specializeDefault(method string, req, out proto.Message) error {
	switch method {
	case "RegisterWorkspace":
		// RegisterWorkspace is IDEMPOTENT BY DIR and the daemon normalizes,
		// mints and returns: the same dir must always mint the same id.
		dir := filepath.Clean(stringField(req, "dir"))
		success, err := successMessage(out)
		if err != nil {
			return fmt.Errorf("RegisterWorkspace: %w", err)
		}
		return setWorkspaceRef(success, "workspace", workspaceID(dir), dir)
	case "RegisterRepository":
		// RegisterRepository is IDEMPOTENT BY THE RESOLVED DIR, and the fake
		// has no git: the path IS the repository here, cleaned, so the same
		// path always mints the same id. `already_known` stays false unless a
		// scenario scripts the answer itself.
		dir := filepath.Clean(stringField(req, "path"))
		success, err := successMessage(out)
		if err != nil {
			return fmt.Errorf("RegisterRepository: %w", err)
		}
		return setWorkspaceRef(success, "repository", workspaceID(dir), dir)
	case "CreateWorkspace":
		// The daemon names and creates everything; the answer carries the
		// minted ref and the roster push is what actually opens the tab.
		dir := filepath.Clean(repositoryDir(req))
		success, err := successMessage(out)
		if err != nil {
			return fmt.Errorf("CreateWorkspace: %w", err)
		}
		return setWorkspaceRef(success, "workspace", workspaceID(dir+"#created"), dir)
	case "SubmitPrompt":
		// SubmitPromptSuccess.outcome resolved to `turn` by fillRequired; give
		// its TurnId a value so the client has an id to correlate against.
		success, err := successMessage(out)
		if err != nil {
			return fmt.Errorf("SubmitPrompt: %w", err)
		}
		turnFd := success.Descriptor().Fields().ByName("turn")
		if turnFd == nil {
			return fmt.Errorf("SubmitPromptSuccess has no `turn` arm")
		}
		turn := success.Mutable(turnFd).Message()
		idFd := turn.Descriptor().Fields().ByName("turn")
		if idFd == nil {
			return fmt.Errorf("SubmitPromptTurn has no `turn` field")
		}
		turnID := turn.Mutable(idFd).Message()
		valueFd := turnID.Descriptor().Fields().ByName("value")
		if valueFd == nil {
			return fmt.Errorf("TurnId has no `value` field")
		}
		turnID.Set(valueFd, protoreflect.ValueOfString("turn-"+stringField(req, "idempotency_key")))
		return nil
	case "PlanRollback":
		// PlanRollbackSuccess.outcome resolved to `plan` by fillRequired; its
		// token is daemon-minted and a client refuses an empty one, so the
		// default mints a stable opaque value a RollBack can echo.
		plan := out.(*agentreplv1.PlanRollbackResponse).GetSuccess().GetPlan()
		if plan == nil || plan.GetToken() == nil {
			return fmt.Errorf("PlanRollback: the default success carries no plan token")
		}
		plan.Token.Value = defaultRollbackToken
		return nil
	default:
		return nil
	}
}

// defaultRollbackToken is the opaque token the default PlanRollback plan
// carries.
const defaultRollbackToken = "fake-rollback-token"
