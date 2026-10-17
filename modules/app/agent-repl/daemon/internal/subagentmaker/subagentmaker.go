// Package subagentmaker answers which workspace SPAWNED a subagent: the
// maker an async merge of the subagent's branch forks and may land into
// (agentrepl.v1.CreateWorkspaceAsyncMergeMakerSubagent).
//
// THE FACT IS READ FROM DISK, never stated by the request a second time. The
// vendor files a subagent's transcript under its spawning session, at
// `<session's sidecar dir>/subagents/agent-<agent id>.jsonl`, so the maker is
// the one registered workspace of the repository whose session's sidecar
// holds that file. None holding it is an answer (ErrNotFound); two holding it
// breaks the vendor's one-file filing and fails hard.
package subagentmaker

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"path/filepath"
	"strings"

	"agent-repld/internal/account"
	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

const op = "daemon.subagentmaker.find"

// Deps are the finder's reads.
type Deps struct {
	Workspaces func(ctx context.Context) ([]wsm.Workspace, error)
	Session    func(ctx context.Context, id wsm.WorkspaceID) (wsm.Session, bool, error)
	Transcript func(ctx context.Context, workspaceDir, vendorSessionID string) (account.Transcript, error)
	// Stat reports whether a path exists (os.Stat in production).
	Stat func(path string) (fs.FileInfo, error)
	Log  dlog.Logger
}

// Finder is the maker lookup.
type Finder struct{ deps Deps }

// New builds a finder.
func New(deps Deps) *Finder { return &Finder{deps: deps} }

// ErrNotFound is the answer for a subagent no workspace's session holds.
var ErrNotFound = errors.New("subagentmaker: no workspace's session holds the subagent")

// TranscriptFile is the subagent's transcript under a session's sidecar dir.
func TranscriptFile(sidecarDir, agentID string) string {
	return filepath.Join(sidecarDir, "subagents", "agent-"+agentID+".jsonl")
}

// Find answers the workspace of repository REPO that spawned subagent
// AGENTID, ErrNotFound when none did, and an error recorded at ERROR when the
// registry, a session or the disk cannot be read, or two workspaces claim it.
func (f *Finder) Find(ctx context.Context, repo wsm.RepoID, agentID string) (wsm.Workspace, error) {
	fields := dlog.Context{"repo": string(repo), "agent_id": agentID}
	if agentID == "" || strings.ContainsAny(agentID, `/\`) {
		err := fmt.Errorf("subagentmaker: %q is no subagent id", agentID)
		f.deps.Log.Error(op, "refused a subagent id that cannot name a transcript file", dlog.WithCause(fields, err))
		return wsm.Workspace{}, err
	}
	workspaces, err := f.deps.Workspaces(ctx)
	if err != nil {
		f.deps.Log.Error(op, "could not read the registry to find a subagent's maker", dlog.WithCause(fields, err))
		return wsm.Workspace{}, fmt.Errorf("subagentmaker: read the registry: %w", err)
	}
	var found []wsm.Workspace
	for _, ws := range workspaces {
		if ws.Repo != repo {
			continue
		}
		holds, err := f.holds(ctx, ws, agentID, fields)
		if err != nil {
			return wsm.Workspace{}, err
		}
		if holds {
			found = append(found, ws)
		}
	}
	switch len(found) {
	case 0:
		f.deps.Log.Info(op, "no workspace's session of the repository holds the subagent", fields)
		return wsm.Workspace{}, ErrNotFound
	case 1:
		f.deps.Log.Debug(op, "found the workspace that spawned the subagent", withWS(fields, found[0]))
		return found[0], nil
	}
	names := make([]string, len(found))
	for i, ws := range found {
		names[i] = string(ws.ID)
	}
	err = fmt.Errorf("subagentmaker: subagent %s is filed under %d workspaces' sessions: %s", agentID, len(found), strings.Join(names, ", "))
	f.deps.Log.Error(op, "a subagent is filed under more than one session", dlog.WithCause(fields, err))
	return wsm.Workspace{}, err
}

// holds reports whether WS's session files subagent AGENTID. A workspace with
// no session, or whose transcript is not on disk, holds nothing.
func (f *Finder) holds(ctx context.Context, ws wsm.Workspace, agentID string, fields dlog.Context) (bool, error) {
	session, ok, err := f.deps.Session(ctx, ws.ID)
	if err != nil {
		f.deps.Log.Error(op, "could not read a workspace's session to find a subagent's maker", dlog.WithCause(withWS(fields, ws), err))
		return false, fmt.Errorf("subagentmaker: read the session of %s: %w", ws.ID, err)
	}
	if !ok || session.VendorSessionID == "" {
		return false, nil
	}
	transcript, err := f.deps.Transcript(ctx, ws.Dir, session.VendorSessionID)
	var missing *account.NotFoundError
	if errors.As(err, &missing) {
		return false, nil
	}
	if err != nil {
		f.deps.Log.Error(op, "could not locate a workspace's transcript to find a subagent's maker", dlog.WithCause(withWS(fields, ws), err))
		return false, fmt.Errorf("subagentmaker: locate the transcript of %s: %w", ws.ID, err)
	}
	if transcript.SidecarDir == "" {
		return false, nil
	}
	_, err = f.deps.Stat(TranscriptFile(transcript.SidecarDir, agentID))
	if errors.Is(err, fs.ErrNotExist) {
		return false, nil
	}
	if err != nil {
		f.deps.Log.Error(op, "could not read whether a session holds the subagent", dlog.WithCause(withWS(fields, ws), err))
		return false, fmt.Errorf("subagentmaker: stat the subagent under %s: %w", ws.ID, err)
	}
	return true, nil
}

func withWS(fields dlog.Context, ws wsm.Workspace) dlog.Context {
	out := dlog.Context{"workspace": string(ws.ID), "dir": ws.Dir}
	for k, v := range fields {
		out[k] = v
	}
	return out
}
