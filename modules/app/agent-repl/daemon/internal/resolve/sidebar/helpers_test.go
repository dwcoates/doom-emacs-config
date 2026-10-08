package sidebar_test

import (
	"context"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/vocab"
	"claude-repld/internal/wsm"
)

// epoch is the instant every test's timestamps are relative to, so no
// assertion depends on wall-clock.
var epoch = time.UnixMilli(1_700_000_000_000)

// at is epoch plus d, addressable so it can be assigned to a *time.Time field.
func at(d time.Duration) *time.Time {
	t := epoch.Add(d)
	return &t
}

// testColors is the render-colors double: every roster_status arm painted and
// every merge arm glyphed, which is exactly what the resolver asserts. The
// values are irrelevant — the resolver reads the tables' KEYS, never a color.
func testColors() vocab.RenderColors {
	status := map[string]string{}
	for _, arm := range []string{
		"submitting", "thinking", "clearing", "compacting", "permission", "done",
		"interrupted", "turn_failed", "ready", "idle_async", "vendor_blocked", "vendor_fault", "network_fault", "api_retrying", "init", "severed",
		"start_failed", "degraded", "dead", "turn_died", "merging",
		"merge_queued", "merge_failed", "merged", "none",
		"inactive", "closing", "daemon_impaired", "waiting",
	} {
		status[arm] = "grey"
	}
	glyphs := map[string]string{}
	for _, arm := range []string{
		"merging", "merge_queued",
		"merge_failed", "merged",
	} {
		glyphs[arm] = "recycle"
	}
	return vocab.RenderColors{RosterStatus: status, MergeGlyphs: glyphs}
}

// newResolver builds a resolver plus the surfaces its records land in. The
// roster projects the footer's status, so the resolver is a fanned pair: a
// real footer resolver beside the roster, every status fact delivered to both
// exactly as the daemon delivers it (fanned).
func newResolver(t *testing.T, opts ...sidebar.Option) (sidebar.Resolver, *dlog.TestSurfaces) {
	t.Helper()
	colors, err := vocab.LoadRenderColors(repoVocabDir)
	if err != nil {
		t.Fatalf("LoadRenderColors: %v", err)
	}
	// The footer's status edge redraws the roster, as the daemon wires it.
	var r sidebar.Resolver
	f, err := footer.New(colors, dlog.NewTestSurfaces(), footer.WithMomentaryDwell(time.Hour),
		footer.WithStatusChanged(func(ws ids.WorkspaceID) { r.FooterStatusChanged(ws) }))
	if err != nil {
		t.Fatalf("footer.New: %v", err)
	}
	surfaces := dlog.NewTestSurfaces()
	r, err = sidebar.New(testColors(), surfaces, f, opts...)
	if err != nil {
		t.Fatalf("sidebar.New: %v", err)
	}
	return &fanned{Resolver: r, footer: f, dir: t.TempDir(), bound: map[ids.WorkspaceID]bool{}}, surfaces
}

// fanned is the roster with the footer whose status it projects, fed the way
// the daemon feeds them: every fact both resolvers take reaches both, the
// footer first, so the roster's own render projects the footer's status for
// the same fact rather than waiting on the footer's status edge.
type fanned struct {
	sidebar.Resolver
	footer footer.Resolver
	dir    string

	mu    sync.Mutex
	bound map[ids.WorkspaceID]bool
}

// SetRegistry binds every registered workspace on the footer, as registration
// does (SetWorkspaceDir), with its two client hops held.
func (f *fanned) SetRegistry(reg sidebar.Registry) {
	f.mu.Lock()
	for _, ws := range reg.Workspaces {
		if f.bound[ws.ID] {
			continue
		}
		f.bound[ws.ID] = true
		if err := f.footer.SetWorkspaceDir(ws.ID, f.dir); err != nil {
			panic(err)
		}
		f.footer.SetParticipants(ws.ID, true, true)
	}
	f.mu.Unlock()
	// A HIBERNATED RECORD IS THE IDLE SWEEP'S PARK, which the sweep tells the
	// footer in the same breath as it publishes the record.
	for _, session := range reg.Sessions {
		if session.Terminal != nil && session.Terminal.Kind == "hibernated" {
			f.footer.SetParked(session.Workspace, true)
		}
	}
	f.Resolver.SetRegistry(reg)
}

func (f *fanned) AckTurn(ws ids.WorkspaceID) {
	f.footer.AckTurn(ws)
	f.Resolver.AckTurn(ws)
}

func (f *fanned) OnTurnRunningAtAttach(ws ids.WorkspaceID, turn ids.TurnID, startedAt *time.Time) {
	f.footer.OnTurnRunningAtAttach(ws, turn, startedAt)
	f.Resolver.OnTurnRunningAtAttach(ws, turn, startedAt)
}

func (f *fanned) OnSessionStarted(ws ids.WorkspaceID, started *conversationv1.SessionStarted) {
	f.footer.OnSessionStarted(ws, started)
	f.Resolver.OnSessionStarted(ws, started)
}

func (f *fanned) OnAgentTerminal(ws ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) {
	f.footer.OnAgentTerminal(ws, agent, turn, success, failure)
	f.Resolver.OnAgentTerminal(ws, agent, turn, success, failure)
}

func (f *fanned) OnActivity(ws ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity) {
	f.footer.OnActivity(ws, agent, act)
	f.Resolver.OnActivity(ws, agent, act)
}

func (f *fanned) OnDetachedWork(ws ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork) {
	f.footer.OnDetachedWork(ws, agent, work)
	f.Resolver.OnDetachedWork(ws, agent, work)
}

func (f *fanned) OnPermission(ws ids.WorkspaceID, agent *conversationv1.AgentId, perm *conversationv1.AgentPermission) {
	f.footer.OnPermission(ws, agent, perm)
	f.Resolver.OnPermission(ws, agent, perm)
}

func (f *fanned) OnApiError(ws ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed) {
	f.footer.OnApiError(ws, agent, failed)
	f.Resolver.OnApiError(ws, agent, failed)
}

func (f *fanned) OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	f.footer.OnSessionUpdate(ws, update)
	f.Resolver.OnSessionUpdate(ws, update)
}

func (f *fanned) OnLink(ws ids.WorkspaceID, link shimclient.LinkState) {
	f.footer.OnLink(ws, link)
	f.Resolver.OnLink(ws, link)
}

func (f *fanned) OnLiveWorkChanged(ws ids.WorkspaceID, live sidebar.LiveWorkSet) {
	f.footer.OnLiveWorkChanged(ws, live)
	f.Resolver.OnLiveWorkChanged(ws, live)
}

func (f *fanned) SetMerge(ws ids.WorkspaceID, facts footer.MergeFacts) {
	f.footer.SetMerge(ws, facts)
	f.Resolver.SetMerge(ws, facts)
}

func (f *fanned) SetStateUnreported(ws ids.WorkspaceID, unreported bool) {
	f.footer.SetStateUnreported(ws, unreported)
	f.Resolver.SetStateUnreported(ws, unreported)
}

func (f *fanned) SetBringingUp(ws ids.WorkspaceID, bringingUp bool) {
	f.footer.SetBringingUp(ws, bringingUp)
	f.Resolver.SetBringingUp(ws, bringingUp)
}

func (f *fanned) SetTurn(ws ids.WorkspaceID, turn *sidebar.TurnStarted) {
	f.footer.SetTurn(ws, turn)
	f.Resolver.SetTurn(ws, turn)
}

func (f *fanned) SetTurnEnded(ws ids.WorkspaceID, how sidebar.TurnClose) {
	f.footer.SetTurnEnded(ws, how)
	f.Resolver.SetTurnEnded(ws, how)
}

// NetworkFaultOpened is the network fault health.ObserveFaults opens: the
// footer's standing fault and the roster's own record of it.
func (f *fanned) NetworkFaultOpened(ws ids.WorkspaceID, id string) {
	f.openFault(ws, id, health.KindNetworkUnreachable)
	f.Resolver.NetworkFaultOpened(ws, id)
}

func (f *fanned) FaultClosed(ws ids.WorkspaceID, id string) {
	f.footer.CloseFault(ws, id)
	f.Resolver.FaultClosed(ws, id)
}

// SetVendorStart is the vendor-start run the fleet records as faults: the
// footer takes the run's fault, the roster its state.
func (f *fanned) SetVendorStart(ws ids.WorkspaceID, state sidebar.VendorStart) {
	id := "vendor-start-" + string(ws)
	f.footer.CloseFault(ws, id)
	switch state {
	case sidebar.VendorStartRetrying:
		f.openFault(ws, id, health.KindVendorStartRetrying)
	case sidebar.VendorStartStopped:
		f.openFault(ws, id, health.KindVendorStartFailed)
	}
	f.Resolver.SetVendorStart(ws, state)
}

// openFault opens a standing fault on the footer in the cell the health
// partition gives its kind, as the fault surfaces do.
func (f *fanned) openFault(ws ids.WorkspaceID, id, kind string) {
	cell, ok := health.FaultFooterCell(kind, false)
	if !ok {
		panic("no footer cell for fault kind " + kind)
	}
	f.footer.OpenFault(ws, footer.Fault{
		ID: id, Kind: kind, Status: string(cell.Status), SubStatus: cell.SubStatus, At: epoch,
	})
}

// latest reads the roster the resolver last published, failing when it
// published none.
func latest(t *testing.T, r sidebar.Resolver) *frontendv1.WorkspaceRoster {
	t.Helper()
	roster, ok := r.Topic().Latest()
	if !ok {
		t.Fatal("the roster published nothing")
	}
	return roster
}

// subscribe opens a subscription that is torn down with the test.
func subscribe(t *testing.T, r sidebar.Resolver) <-chan *frontendv1.WorkspaceRoster {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	return r.Topic().Subscribe(ctx)
}

// repo is the repository every test workspace belongs to unless it names
// another.
var repo = wsm.Repository{
	ID:            ids.RepoID("repo-1"),
	Dir:           "/repos/one",
	Name:          "alpha",
	DefaultBranch: "master",
}

// workspace builds a registered workspace record with a distinct dir and
// branch derived from its id, cut from the repository's default branch.
func workspace(id, name string) wsm.Workspace {
	return wsm.Workspace{
		ID:           ids.WorkspaceID(id),
		Repo:         repo.ID,
		Dir:          "/repos/one/" + id,
		Name:         name,
		Branch:       "branch/" + id,
		ParentBranch: repo.DefaultBranch,
		CreatedAt:    epoch,
	}
}

// registry builds a snapshot carrying one repository and the given workspaces.
func registry(workspaces ...wsm.Workspace) sidebar.Registry {
	return sidebar.Registry{
		Workspaces:   workspaces,
		Repositories: []wsm.Repository{repo},
		View:         wsm.DefaultSidebarView,
	}
}

// prioritized stamps a priority on a workspace.
func prioritized(ws wsm.Workspace, p wsm.Priority) wsm.Workspace {
	ws.Priority = &p
	return ws
}

// repoRows reads the rows of the repository grouping's only section.
func repoRows(t *testing.T, roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
	t.Helper()
	sections := roster.GetRepository().GetSections()
	if len(sections) != 1 {
		t.Fatalf("the repository view carried %d sections, want exactly 1", len(sections))
	}
	return sections[0].GetRows().GetRows()
}

// rowNames reads a row list's display names, in the order they render.
func rowNames(rows []*frontendv1.RosterRow) []string {
	out := make([]string, 0, len(rows))
	for _, row := range rows {
		out = append(out, row.GetName().GetText())
	}
	return out
}

// rowFor finds the row for a workspace id anywhere in a row list's tree.
func rowFor(rows []*frontendv1.RosterRow, id string) *frontendv1.RosterRow {
	for _, row := range rows {
		if row.GetWorkspace().GetWorkspace().GetId() == id {
			return row
		}
		if found := rowFor(row.GetChildren(), id); found != nil {
			return found
		}
	}
	return nil
}

// onlyRow reads the repository grouping's single row.
func onlyRow(t *testing.T, r sidebar.Resolver) *frontendv1.RosterRow {
	t.Helper()
	rows := repoRows(t, latest(t, r))
	if len(rows) != 1 {
		t.Fatalf("the section carried %d rows, want exactly 1", len(rows))
	}
	return rows[0]
}

// statusName names the status arm a row carries, empty when the oneof is unset
// — which is itself a contract breach a test asserts against.
func statusName(row *frontendv1.RosterRow) string {
	switch row.GetStatus().(type) {
	case *frontendv1.RosterRow_VendorFault:
		return "vendor_fault"
	case *frontendv1.RosterRow_NetworkFault:
		return "network_fault"
	case *frontendv1.RosterRow_Submitting:
		return "submitting"
	case *frontendv1.RosterRow_Thinking:
		return "thinking"
	case *frontendv1.RosterRow_Clearing:
		return "clearing"
	case *frontendv1.RosterRow_Compacting:
		return "compacting"
	case *frontendv1.RosterRow_Permission:
		return "permission"
	case *frontendv1.RosterRow_Done:
		return "done"
	case *frontendv1.RosterRow_Interrupted:
		return "interrupted"
	case *frontendv1.RosterRow_TurnFailed:
		return "turn_failed"
	case *frontendv1.RosterRow_Ready:
		return "ready"
	case *frontendv1.RosterRow_IdleAsync:
		return "idle_async"
	case *frontendv1.RosterRow_VendorBlocked:
		return "vendor_blocked"
	case *frontendv1.RosterRow_ApiRetrying:
		return "api_retrying"
	case *frontendv1.RosterRow_Init:
		return "init"
	case *frontendv1.RosterRow_Severed:
		return "severed"
	case *frontendv1.RosterRow_StartFailed:
		return "start_failed"
	case *frontendv1.RosterRow_Degraded:
		return "degraded"
	case *frontendv1.RosterRow_Dead:
		return "dead"
	case *frontendv1.RosterRow_TurnDied:
		return "turn_died"
	case *frontendv1.RosterRow_Merging:
		return "merging"
	case *frontendv1.RosterRow_MergeQueued:
		return "merge_queued"
	case *frontendv1.RosterRow_MergeFailed:
		return "merge_failed"
	case *frontendv1.RosterRow_Merged:
		return "merged"
	case *frontendv1.RosterRow_None:
		return "none"
	case *frontendv1.RosterRow_Inactive:
		return "inactive"
	case *frontendv1.RosterRow_Closing:
		return "closing"
	case *frontendv1.RosterRow_DaemonImpaired:
		return "daemon_impaired"
	case *frontendv1.RosterRow_Waiting:
		return "waiting"
	default:
		return ""
	}
}

// agent is the agent id every session frame in these tests carries.
func agent(id string) *conversationv1.AgentId {
	return &conversationv1.AgentId{Value: id}
}

// permissionAsk builds an OPEN consent ask with the given identity.
func permissionAsk(id string) *conversationv1.AgentPermission {
	return &conversationv1.AgentPermission{
		Id: &conversationv1.AgentPermissionId{Value: id},
		Result: &conversationv1.AgentPermission_Start{
			Start: &conversationv1.AgentPermissionStart{}},
	}
}

// permissionDecided builds the same ask once the gate has been decided.
func permissionDecided(id string) *conversationv1.AgentPermission {
	return &conversationv1.AgentPermission{
		Id: &conversationv1.AgentPermissionId{Value: id},
		Result: &conversationv1.AgentPermission_Success{
			Success: &conversationv1.AgentPermissionSuccess{}},
	}
}

// detachedWork builds a detached-work announcement with the given handle.
func detachedWork(id string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: id},
	}
}

// hasError reports whether any captured record was an error under operation.
func hasError(records []dlog.Record, operation string) bool {
	for _, rec := range records {
		if rec.Level == "error" && rec.Operation == operation {
			return true
		}
	}
	return false
}
