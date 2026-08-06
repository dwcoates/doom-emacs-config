package frontend

import (
	"fmt"
	"net/http"
	"net/url"
	"path/filepath"
)

// scopequery.go — deriving a workspace-addressed Scope from a request's query
// string.
//
// A WebSocket that renders one workspace names it in the URL:
//
//	GET /workspace-stream?workspace=<URL-encoded absolute directory path>
//
// The value is the RAW directory path, not a hash or an opaque handle. Both
// Scope.Workspace and FrontendCommand.workspace carry the directory string, so
// a handle would make the browser the only participant keyed differently, and
// the routing key stays greppable in a URL bar, a daemon log, and a frame
// alike.
//
// The workspace path is the connection's WHOLE address. A viewer holds no
// session identity: the daemon resolves which session the workspace owns, and
// a session that rotates under a live view never invalidates the URL.
//
// EVERY way a query string can fail to name a servable workspace is a typed
// refusal (ScopeRefusal), never an unscoped connection: a client served with no
// scope receives every workspace's frames, so a silent widening here is a
// cross-workspace leak. A refusal names the reason, so a caller reports the
// distinct HTTP statuses of "the client asked wrongly" and "the daemon could
// not answer" without re-deriving either.

// scopeWorkspaceParam is the query key a workspace-addressed connection names
// its workspace with.
const scopeWorkspaceParam = "workspace"

// ScopeRefusalReason classifies why a query string does not name a servable
// scope. It is a stable, machine-readable tag: callers log it and map it to a
// status without parsing prose.
type ScopeRefusalReason string

const (
	// ScopeRefusalMalformedQuery — the query string is not decodable
	// (a bad percent-escape, say).
	ScopeRefusalMalformedQuery ScopeRefusalReason = "malformed_query"
	// ScopeRefusalRepeatedWorkspace — the query repeats the workspace key.
	// Two values cannot both address one connection.
	ScopeRefusalRepeatedWorkspace ScopeRefusalReason = "repeated_workspace"
	// ScopeRefusalMissingWorkspace — the query carries no workspace value.
	ScopeRefusalMissingWorkspace ScopeRefusalReason = "missing_workspace"
	// ScopeRefusalRelativeWorkspace — the workspace value is not an absolute
	// directory path, so it names no directory on its own.
	ScopeRefusalRelativeWorkspace ScopeRefusalReason = "relative_workspace"
	// ScopeRefusalUnknownWorkspace — the workspace is a well-formed path the
	// daemon holds no state for.
	ScopeRefusalUnknownWorkspace ScopeRefusalReason = "unknown_workspace"
	// ScopeRefusalWorkspaceLookup — the daemon's workspace lookup itself
	// failed, so whether the workspace exists is UNDECIDED. Kept apart from
	// ScopeRefusalUnknownWorkspace: reporting a broken lookup as "no such
	// workspace" would blame the caller for the daemon's fault.
	ScopeRefusalWorkspaceLookup ScopeRefusalReason = "workspace_lookup_failed"
)

// ScopeRefusal is the typed refusal to serve a scoped connection.
type ScopeRefusal struct {
	Reason ScopeRefusalReason
	// Workspace is the value that was asked for, empty when none was given.
	Workspace string
	// Err is the underlying failure for the reasons that wrap one
	// (ScopeRefusalMalformedQuery, ScopeRefusalWorkspaceLookup), nil otherwise.
	Err error
	// detail is the human half of the message.
	detail string
}

func (e *ScopeRefusal) Error() string {
	msg := fmt.Sprintf("frontend: %s: %s", e.Reason, e.detail)
	if e.Err != nil {
		return msg + ": " + e.Err.Error()
	}
	return msg
}

func (e *ScopeRefusal) Unwrap() error { return e.Err }

// HTTPStatus is the response status the refusal warrants: 400 for a query the
// client got wrong, 404 for a workspace the daemon does not have, 500 for a
// lookup the daemon could not complete.
func (e *ScopeRefusal) HTTPStatus() int {
	switch e.Reason {
	case ScopeRefusalUnknownWorkspace:
		return http.StatusNotFound
	case ScopeRefusalWorkspaceLookup:
		return http.StatusInternalServerError
	default:
		return http.StatusBadRequest
	}
}

// WorkspaceKnown reports whether the daemon holds any state for a workspace.
// A false report means the daemon has no such workspace; an error means the
// lookup could not be completed, which is a different answer and is reported
// as one.
type WorkspaceKnown func(workspace string) (bool, error)

// WorkspaceScopeFromQuery derives the workspace-addressed Scope a connection
// serves from rawQuery (the URL's query string, still percent-encoded), and
// admits it only for a workspace known reports.
//
// known is required: without it there is no authority to check a workspace
// against, and a connection scoped to an unchecked string is exactly the silent
// widening this refuses. A nil known is a programmer error and panics.
func WorkspaceScopeFromQuery(rawQuery string, known WorkspaceKnown) (Scope, error) {
	if known == nil {
		panic("frontend: WorkspaceScopeFromQuery requires a WorkspaceKnown")
	}
	values, err := url.ParseQuery(rawQuery)
	if err != nil {
		return Scope{}, &ScopeRefusal{
			Reason: ScopeRefusalMalformedQuery,
			Err:    err,
			detail: "the query string is not decodable",
		}
	}
	workspaces := values[scopeWorkspaceParam]
	if len(workspaces) > 1 {
		return Scope{}, &ScopeRefusal{
			Reason: ScopeRefusalRepeatedWorkspace,
			detail: fmt.Sprintf("the query names %d workspaces (%q); one connection addresses one workspace",
				len(workspaces), workspaces),
		}
	}
	workspace := values.Get(scopeWorkspaceParam)
	if workspace == "" {
		return Scope{}, &ScopeRefusal{
			Reason: ScopeRefusalMissingWorkspace,
			detail: fmt.Sprintf("the query carries no %q value", scopeWorkspaceParam),
		}
	}
	if !filepath.IsAbs(workspace) {
		return Scope{}, &ScopeRefusal{
			Reason:    ScopeRefusalRelativeWorkspace,
			Workspace: workspace,
			detail:    fmt.Sprintf("workspace %q is not an absolute directory path", workspace),
		}
	}
	switch ok, err := known(workspace); {
	case err != nil:
		return Scope{}, &ScopeRefusal{
			Reason:    ScopeRefusalWorkspaceLookup,
			Workspace: workspace,
			Err:       err,
			detail:    fmt.Sprintf("looking workspace %q up failed", workspace),
		}
	case !ok:
		return Scope{}, &ScopeRefusal{
			Reason:    ScopeRefusalUnknownWorkspace,
			Workspace: workspace,
			detail:    fmt.Sprintf("the daemon holds no state for workspace %q", workspace),
		}
	}
	return Scope{Workspace: workspace}, nil
}
