package server

import (
	"context"
	"fmt"
	"slices"
	"strings"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/workspacefind"
	"agent-repld/internal/wsm"
)

// FindWorkspaces answers the registry's agent workspaces, open or closed,
// narrowed as the request asks (internal/workspacefind), each with its name,
// its stored summary, its config dir and its last agent turn.
func (s *server) FindWorkspaces(
	ctx context.Context,
	req *connect.Request[agentreplv1.FindWorkspacesRequest],
) (*connect.Response[agentreplv1.FindWorkspacesResponse], error) {
	const rpc = "FindWorkspaces"
	if err := validateFindWorkspacesRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.FindWorkspacesResponse{}
	// THE WORKSPACES AND THE REPOSITORIES ARE ONE SNAPSHOT (wsm.ReadRegistry),
	// so every workspace's repository is among those read: a repository and
	// its first workspace registered between two reads left the workspace
	// naming a repository the first read did not hold.
	registry, err := s.deps.DB.ReadRegistry(ctx)
	if err != nil {
		return nil, fail(s.log, rpc, fmt.Errorf("read the registry: %w", err))
	}
	repositories, records := registry.Repositories, registry.Workspaces
	query := findQueryOf(req.Msg)
	if ref := req.Msg.Repository; ref != nil {
		repo, ok := wsm.RepositoryNamed(repositories, wsm.RepoID(ref.GetId()), ref.GetDir())
		if !ok {
			return answer(resp, s.refuse(s.log, rpc, resp, s.fill(refusal{
				Arm: "unknown_repository", Reason: fmt.Sprintf("no registered repository is named by id %q dir %q", ref.GetId(), ref.GetDir()), Info: true,
			})))
		}
		query.Repository = &repo.ID
	}
	byID := make(map[wsm.WorkspaceID]wsm.Workspace, len(records))
	for _, ws := range records {
		byID[ws.ID] = ws
	}
	if ref := req.Msg.CreatedBy; ref != nil {
		creator, ok := registeredRef(byID, ref)
		if !ok {
			return answer(resp, s.refuse(s.log, rpc, resp, s.fill(refusal{
				Arm: "unknown_creator", Reason: fmt.Sprintf("no registered workspace is named by id %q dir %q", ref.GetId(), ref.GetDir()), Info: true,
			})))
		}
		query.CreatedBy = &creator.ID
	}
	var caller *wsm.WorkspaceID
	if ref := req.Msg.Caller; ref != nil {
		record, ok := registeredRef(byID, ref)
		if !ok {
			return answer(resp, s.refuse(s.log, rpc, resp, s.fill(refusal{
				Arm: "unknown_caller", Reason: fmt.Sprintf("no registered workspace is named by id %q dir %q", ref.GetId(), ref.GetDir()), Info: true,
			})))
		}
		caller = &record.ID
	}
	if login := req.Msg.Login; login != nil {
		var root string
		switch arm := login.GetLogin().(type) {
		case *agentreplv1.FindWorkspacesLogin_OfWorkspace:
			ref := arm.OfWorkspace
			record, ok := registeredRef(byID, ref)
			if !ok {
				return answer(resp, s.refuse(s.log, rpc, resp, s.fill(refusal{
					Arm: "unknown_login_workspace", Reason: fmt.Sprintf("no registered workspace is named by id %q dir %q", ref.GetId(), ref.GetDir()), Info: true,
				})))
			}
			own, ok := wsm.RepositoryWithID(repositories, record.Repo)
			if !ok {
				panic(fmt.Sprintf("server: workspace %s names repository %s, which the registry listing does not hold", record.ID, record.Repo))
			}
			root = s.deps.AccountRootFor(own.Dir)
		case *agentreplv1.FindWorkspacesLogin_AccountRoot:
			known := s.deps.KnownAccountRoots()
			if !slices.Contains(known, arm.AccountRoot) {
				return answer(resp, s.refuse(s.log, rpc, resp, s.fill(refusal{
					Arm: "unknown_login_root", Reason: fmt.Sprintf("%q is not one of the account roots this daemon knows (%s)", arm.AccountRoot, strings.Join(known, ", ")),
					Fields: map[string]any{"known": known}, Info: true,
				})))
			}
			root = arm.AccountRoot
		default:
			panic(fmt.Sprintf("server: find login arm %T has no root", arm))
		}
		query.LoginRepos = s.loginRepos(repositories, root)
	}
	// THE MERGED ROWS AND THE PUSH ARE READ TOGETHER, under the sidebar's one
	// lock, so no workspace that merged between two reads is drawn twice.
	merged, roster, ok := s.deps.Sidebar.MergedRows()
	if !ok {
		return nil, fail(s.log, rpc, fmt.Errorf("read the roster: the daemon has published no roster yet"))
	}
	drawn, err := workspacefind.Placements(roster, merged)
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	summaries, err := s.deps.DB.ListWorkspaceSummaries(ctx)
	if err != nil {
		return nil, fail(s.log, rpc, fmt.Errorf("list the workspace summaries: %w", err))
	}
	titles, err := s.deps.DB.ListVendorTitles(ctx)
	if err != nil {
		return nil, fail(s.log, rpc, fmt.Errorf("list the vendor titles: %w", err))
	}
	sessions, err := wsm.SessionsByWorkspace(ctx, s.deps.DB)
	if err != nil {
		return nil, fail(s.log, rpc, fmt.Errorf("list the sessions: %w", err))
	}
	turns, err := s.deps.DB.TurnStarts(ctx)
	if err != nil {
		return nil, fail(s.log, rpc, fmt.Errorf("list the turn starts: %w", err))
	}
	result, err := workspacefind.Find(workspacefind.Records{
		Workspaces: records, Repositories: repositories, Summaries: workspacefind.Summaries(summaries, titles),
		Drawn: drawn, Sessions: sessions, Turns: turns,
	}, query)
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	found := make([]*agentreplv1.FoundWorkspace, 0, len(result.Found))
	for _, f := range result.Found {
		found = append(found, foundWorkspaceOf(f, byID, caller))
	}
	success := &agentreplv1.FindWorkspacesSuccess{}
	if result.Omitted > 0 {
		success.Extent = &agentreplv1.FindWorkspacesSuccess_Partial{Partial: &agentreplv1.FindWorkspacesSuccessPartial{
			Workspaces: found, Omitted: uint32(result.Omitted)}}
	} else {
		success.Extent = &agentreplv1.FindWorkspacesSuccess_Complete{Complete: &agentreplv1.FindWorkspacesSuccessComplete{Workspaces: found}}
	}
	s.log.Debug("daemon.server.find_workspaces", "answered a workspace find", dlog.Context{
		"found": len(found), "omitted": result.Omitted, "repository": req.Msg.GetRepository().GetId() + req.Msg.GetRepository().GetDir(),
		"text": req.Msg.GetText(), "standing": fmt.Sprint(query.Standing), "created_by": req.Msg.GetCreatedBy().GetId(),
		"on_roster": query.OnRoster, "caller": req.Msg.GetCaller().GetId(), "login_workspace": req.Msg.GetLogin().GetOfWorkspace().GetId(),
		"login_root": req.Msg.GetLogin().GetAccountRoot(), "login_repositories": len(query.LoginRepos), "roster_revision": roster.GetRevision(),
		"turned": req.Msg.Turned != nil, "turned_since_ms": req.Msg.GetTurned().GetSinceMs(), "turned_until_ms": req.Msg.GetTurned().GetUntilMs()})
	resp.Result = &agentreplv1.FindWorkspacesResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// loginRepos answers the repositories of the login whose config dir is ROOT:
// the repositories whose main checkout AccountRootFor routes to it.
func (s *server) loginRepos(repositories []wsm.Repository, root string) map[wsm.RepoID]bool {
	repos := map[wsm.RepoID]bool{}
	for _, repo := range repositories {
		if s.deps.AccountRootFor(repo.Dir) == root {
			repos[repo.ID] = true
		}
	}
	return repos
}

// registeredRef answers the record in BYID that REF names: its id held, and
// its dir, when REF states one, the record's own. A ref the registry does not
// hold by both answers false, which the caller refuses with its own arm.
func registeredRef(byID map[wsm.WorkspaceID]wsm.Workspace, ref *workspacev1.WorkspaceRef) (wsm.Workspace, bool) {
	record, ok := byID[wsm.WorkspaceID(ref.GetId())]
	if !ok || (ref.GetDir() != "" && ref.GetDir() != record.Dir) {
		return wsm.Workspace{}, false
	}
	return record, true
}

// validateFindWorkspacesRequest is FindWorkspacesRequest's base function.
func validateFindWorkspacesRequest(req *agentreplv1.FindWorkspacesRequest) *connect.Error {
	if req.Repository != nil {
		if err := validateRepositoryRef("repository", req.GetRepository()); err != nil {
			return err
		}
	}
	if w := req.Window; w != nil && w.SinceMs != nil && w.UntilMs != nil && w.GetUntilMs() < w.GetSinceMs() {
		return invalid("window", "a window's end precedes its start")
	}
	if req.Standing != nil && req.GetStanding().GetStanding() == nil {
		return invalid("standing", "a standing names its arm")
	}
	if req.Text != nil && strings.TrimSpace(req.GetText()) == "" {
		return invalid("text", "a text filter is not blank")
	}
	if req.Limit != nil && req.GetLimit() == 0 {
		return invalid("limit", "a limit is at least 1")
	}
	if req.CreatedBy != nil {
		if err := validateWorkspaceRef("created_by", req.GetCreatedBy()); err != nil {
			return err
		}
	}
	if req.Caller != nil {
		if err := validateWorkspaceRef("caller", req.GetCaller()); err != nil {
			return err
		}
	}
	if req.Login != nil {
		if req.GetLogin().GetLogin() == nil {
			return invalid("login", "a login names its arm")
		}
		switch arm := req.GetLogin().GetLogin().(type) {
		case *agentreplv1.FindWorkspacesLogin_OfWorkspace:
			if err := validateWorkspaceRef("login.of_workspace", arm.OfWorkspace); err != nil {
				return err
			}
		case *agentreplv1.FindWorkspacesLogin_AccountRoot:
			if strings.TrimSpace(arm.AccountRoot) == "" {
				return invalid("login.account_root", "an account root is not blank")
			}
		default:
			panic(fmt.Sprintf("server: find login arm %T is not validated", arm))
		}
	}
	if w := req.Turned; w != nil && w.SinceMs != nil && w.UntilMs != nil && w.GetUntilMs() < w.GetSinceMs() {
		return invalid("turned", "a turn window's end precedes its start")
	}
	return nil
}

// findQueryOf reads a validated request's narrowing, all but its repository,
// which the handler resolves against the registry.
func findQueryOf(req *agentreplv1.FindWorkspacesRequest) workspacefind.Query {
	var q workspacefind.Query
	if w := req.Window; w != nil {
		q.Since, q.Until = instantOf(w.SinceMs), instantOf(w.UntilMs)
	}
	if req.Standing != nil {
		switch req.GetStanding().GetStanding().(type) {
		case *agentreplv1.FindWorkspacesStanding_Open:
			q.Standing = workspacefind.OpenStanding
		case *agentreplv1.FindWorkspacesStanding_Closed:
			q.Standing = workspacefind.ClosedStanding
		case *agentreplv1.FindWorkspacesStanding_Landed:
			q.Standing = workspacefind.LandedStanding
		default:
			panic(fmt.Sprintf("server: find standing arm %T has no query standing", req.GetStanding().GetStanding()))
		}
	}
	if req.Text != nil {
		text := req.GetText()
		q.Text = &text
	}
	if req.Limit != nil {
		limit := int(req.GetLimit())
		q.Limit = &limit
	}
	q.OnRoster = req.OnRoster != nil
	if w := req.Turned; w != nil {
		q.Turned = &workspacefind.TurnWindow{Since: instantOf(w.SinceMs), Until: instantOf(w.UntilMs)}
	}
	return q
}

// instantOf reads an optional unix-millisecond bound, nil when unset.
func instantOf(ms *int64) *time.Time {
	if ms == nil {
		return nil
	}
	at := time.UnixMilli(*ms)
	return &at
}

// foundWorkspaceOf renders one found workspace, naming its creator and its
// parent (and a merge workspace's requester) by their records in BYID, the
// same listing, and answering calling
// for the record of CALLER, nil when the request names none. Both references are SET
// NULL when their row is forgotten, so one the listing does not hold is an
// invariant violation and panics.
func foundWorkspaceOf(f workspacefind.Found, byID map[wsm.WorkspaceID]wsm.Workspace, caller *wsm.WorkspaceID) *agentreplv1.FoundWorkspace {
	ws := f.Workspace
	standing := &agentreplv1.FoundWorkspaceStanding{}
	switch {
	case ws.MergedAt != nil:
		standing.Standing = &agentreplv1.FoundWorkspaceStanding_Landed{Landed: &agentreplv1.FoundWorkspaceStandingLanded{LandedAtMs: ws.MergedAt.UnixMilli()}}
	case ws.Closed:
		closed := &agentreplv1.FoundWorkspaceStandingClosed{}
		if ws.Closure != nil {
			at := ws.Closure.At.UnixMilli()
			closed.ClosedAtMs = &at
		}
		standing.Standing = &agentreplv1.FoundWorkspaceStanding_Closed{Closed: closed}
	default:
		standing.Standing = &agentreplv1.FoundWorkspaceStanding_Open{Open: &agentreplv1.FoundWorkspaceStandingOpen{}}
	}
	refOf := func(id *wsm.WorkspaceID) *workspacev1.WorkspaceRef {
		if id == nil {
			return nil
		}
		record, ok := byID[*id]
		if !ok {
			panic(fmt.Sprintf("server: workspace %s names %s, which the registry listing does not hold", ws.ID, *id))
		}
		return record.Ref()
	}
	var merge *agentreplv1.FoundWorkspaceMerge
	if ws.MergeOf != nil {
		merge = &agentreplv1.FoundWorkspaceMerge{}
		// A MERGE'S REQUESTER IS RECORDED BY ID ALONE (no foreign key), so a
		// requester the listing no longer holds is a state the registry
		// allows, answered as no requester.
		if requester, ok := byID[ws.MergeOf.Requester]; ok {
			merge.Requester = requester.Ref()
		}
	}
	var lastTurnAtMs *int64
	if f.LastTurnAt != nil {
		ms := f.LastTurnAt.UnixMilli()
		lastTurnAtMs = &ms
	}
	return &agentreplv1.FoundWorkspace{
		Merge:          merge,
		CreatedBy:      refOf(ws.RequestedBy),
		Parent:         refOf(ws.Parent),
		Workspace:      ws.Ref(),
		Name:           ws.Name,
		Branch:         ws.Branch,
		Repository:     &workspacev1.RepositoryRef{Id: string(f.Repository.ID), Dir: f.Repository.Dir},
		Summary:        f.Summary,
		CreatedAtMs:    ws.CreatedAt.UnixMilli(),
		LastActiveAtMs: f.LastActiveAt.UnixMilli(),
		Standing:       standing,
		Roster:         foundRosterOf(f.Roster),
		Calling:        caller != nil && *caller == ws.ID,
		ConfigDir:      f.ConfigDir,
		LastTurnAtMs:   lastTurnAtMs,
	}
}

// foundRosterOf renders how the roster draws a found workspace, nil when it
// draws no row of it.
func foundRosterOf(drawn *workspacefind.Drawn) *agentreplv1.FoundWorkspaceRoster {
	if drawn == nil {
		return nil
	}
	section := &agentreplv1.FoundWorkspaceRosterSection{}
	switch drawn.Section {
	case workspacefind.OpenSection:
		section.Section = &agentreplv1.FoundWorkspaceRosterSection_Open{Open: &agentreplv1.FoundWorkspaceRosterSectionOpen{}}
	case workspacefind.BacklogSection:
		section.Section = &agentreplv1.FoundWorkspaceRosterSection_Backlog{Backlog: &agentreplv1.FoundWorkspaceRosterSectionBacklog{}}
	case workspacefind.MergedSection:
		section.Section = &agentreplv1.FoundWorkspaceRosterSection_Merged{Merged: &agentreplv1.FoundWorkspaceRosterSectionMerged{}}
	default:
		panic(fmt.Sprintf("server: roster section %d has no FoundWorkspaceRosterSection arm", int(drawn.Section)))
	}
	return &agentreplv1.FoundWorkspaceRoster{Section: section, Status: &agentreplv1.FoundWorkspaceRosterStatus{Arm: drawn.Status}}
}
