package sidebar

import (
	"maps"
	"slices"
	"time"

	"google.golang.org/protobuf/proto"

	"fmt"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/deployedversion"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/protoarm"
	"agent-repld/internal/publish"
	"agent-repld/internal/repodivergence"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/ladder"
	"agent-repld/internal/semver"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/viewquiet"
	"agent-repld/internal/vocab"
	"agent-repld/internal/wsm"
)

// resolver is the roster resolver. ONE accumulation and ONE topic serve every
// webview: the roster is editor-global, so there is nothing to key by
// workspace except the live half of each row.
type resolver struct {
	colors vocab.RenderColors
	log    dlog.Surfaces
	// footer is the one resolver of a workspace's status, which every row
	// projects (statusArm).
	footer FooterStatus

	mu    sync.Mutex
	state *rosterState
	topic publish.Topic[*frontendv1.RosterPush]
	// revision is the last push's RosterPush.revision: raised for every push
	// whose streams differ from the last, never lowered for the daemon's
	// life.
	revision uint64
	// last is the push last resolved and published, which Row reads. EVERY
	// STREAM IS SET on it: the topic hands a new subscriber its latest value
	// first, so a partial one would hand it an incomplete roster. A stream
	// unchanged since the push before keeps that push's own message, so the
	// stream server can tell what changed by pointer (streams.go).
	last *frontendv1.RosterPush
	// mergedSet is the merged set the last push was resolved from (its ids,
	// sorted), and mergedRevision its token
	// (frontend.v1.RosterMergedSection.merged_revision): the revision of the
	// push in which the set last changed.
	mergedSet      []ids.WorkspaceID
	mergedRevision uint64

	// results is told every change of a workspace's last turn result, so the
	// daemon keeps it durable (wsm.SetResult). nil tells nobody.
	results ResultSink
	// pendingResults are the changes a render found, reported after the lock
	// is released.
	pendingResults []resultChange

	// statusChanges is told every instant a workspace's published status arm
	// moves, so the daemon keeps it durable (wsm.SetStatusChangedAt). nil
	// tells nobody.
	statusChanges StatusChangeSink
	// pendingStatusChanges are the arm moves a render found, reported after
	// the lock is released.
	pendingStatusChanges []statusChange

	// visibilities is told every change of a workspace's visibility, so the
	// daemon keeps it durable (wsm.SetVisibilities). nil tells nobody.
	visibilities VisibilitySink
	// pendingVisibilities are the visibility changes a render found, reported
	// after the lock is released.
	pendingVisibilities map[ids.WorkspaceID]wsm.Visibility

	// now is the clock a status change is stamped by (WithClock).
	now func() time.Time

	// quiet is the lid-closed quiet state's view gate (WithQuiet): while it
	// stands, a change derives every row's facts but resolves and publishes
	// no roster, and quietStale records that one was skipped, so leaving
	// publishes the current roster once.
	quiet      *viewquiet.Gate
	quietStale bool
	// departures are the rows a render moved onto a departure arm
	// (departureArm), collected for the span of one combined commit so a
	// quiet commit can tell whether it must publish anyway. Caller holds mu.
	departures []departure
	// factsOnly is set for the span of a renderFacts, and of each merged
	// workspace's facts in a render (rowFacts): row derives its facts and
	// builds no row.
	factsOnly bool
	// readOnly is set for the span of an on-demand row (readRow): row builds
	// the row from the facts last derived and derives none, so a read feeds
	// no durable sink and no departure.
	readOnly bool
	// batchCauses names, for the span of a combined commit's render, the
	// operation that last changed each workspace in the batch.
	batchCauses map[ids.WorkspaceID]string

	// The combined commit (combine.go): mutations wait in queue, and the one
	// leader applies and publishes them together.
	qmu       sync.Mutex
	queue     []*mutation
	combining bool
	// onQueued is the tests' one look at the queue: told its depth each time
	// a change joins it, under qmu. nil outside a test.
	onQueued func(depth int)
}

// resultChange is one workspace's changed last turn result.
type resultChange struct {
	ws     ids.WorkspaceID
	result *wsm.TurnResult
}

// departure is one row a render moved onto a departure arm.
type departure struct {
	ws  ids.WorkspaceID
	arm string
}

// statusChange is one workspace's published status arm moving, and when.
type statusChange struct {
	ws ids.WorkspaceID
	at time.Time
}

// newResolver builds the resolver, asserting the render-colors tables against
// the arms this resolver emits. An arm with no color fails HERE rather than
// drawing an unpainted dot, and a table row no arm claims fails too: a colored
// state nothing can reach means the table and the oneof have drifted.
func newResolver(colors vocab.RenderColors, log dlog.Surfaces, footer FooterStatus) (*resolver, error) {
	if log == nil {
		return nil, fmt.Errorf("sidebar resolver needs log surfaces")
	}
	if footer == nil {
		return nil, fmt.Errorf("sidebar resolver needs the footer's status to project")
	}
	if err := assertTables(colors); err != nil {
		return nil, fmt.Errorf("sidebar resolver refuses to serve an unpainted state: %w", err)
	}
	return &resolver{
		colors: colors, log: log, footer: footer, state: newRosterState(),
		now: time.Now,
	}, nil
}

// assertTables checks the roster_status table row for row against the
// proto's own arms (vocab.RosterStatusArms), and merge_glyphs row for row
// against the merge arms, through the vocabulary's OWN assertions. Both directions are checked: a missing row would draw an
// unpainted dot, and a surplus row is a state the vocabulary paints and the
// resolver can never emit.
func assertTables(colors vocab.RenderColors) error {
	if err := colors.AssertRosterStatusArms(vocab.RosterStatusArms()); err != nil {
		return err
	}
	return colors.AssertMergeGlyphArms(vocab.RosterMergeArms())
}

// FooterStatusChanged redraws the roster on the footer's status edge, in line,
// so the row shows the footer's new status before the edge returns.
//
// THE LOCK ORDER IS ROSTER, THEN FOOTER. A render holds the roster's lock and
// reads the footer's status under the footer's; the footer tells this edge
// only after releasing its own lock, and never from the one path that reaches
// it while the roster's lock is held — a roster warning teed onto the strip,
// which moves the activity line alone (footer.mutateLine).
func (r *resolver) FooterStatusChanged(ws ids.WorkspaceID) {
	r.mutate("daemon.sidebar.footer_status_changed", "the roster re-projected the footer's status",
		dlog.Context{"workspace_id": string(ws)}, r.log.Global(), func() {})
}

// Topic is the one editor-global roster publication.
func (r *resolver) Topic() *publish.Topic[*frontendv1.RosterPush] { return &r.topic }

// logFor answers the logger a workspace-scoped record goes to. It resolves the
// workspace's OWN durable sink from the registry's directory; a fact for a
// workspace the registry does not carry is an invariant violation, recorded as
// one rather than written globally by default.
//
// A REGISTERED WORKSPACE WHOSE DIRECTORY CANNOT HOST A SINK IS ORDINARY: a
// landed workspace keeps its registry row after its worktree is reaped, and
// the roster renders it on. Its records go to the central sink naming it
// (dlog.Surfaces.WorkspaceOrCentral, the surface for callers that render
// every workspace the registry holds), and nothing about it is a violation.
func (r *resolver) logFor(ws ids.WorkspaceID) dlog.Logger {
	for _, rec := range r.state.reg.Workspaces {
		if rec.ID != ws {
			continue
		}
		return r.log.WorkspaceOrCentral(rec.Dir).With(dlog.Context{"workspace_id": string(ws)})
	}
	return r.log.Global().With(dlog.Context{
		"workspace_id":        string(ws),
		"invariant_violation": "roster fact for a workspace the registry does not carry",
		"remediation":         "install the registry before routing the workspace's frames",
	})
}

// assertArm records an arm the render-colors table cannot paint. The table is
// asserted whole at construction, so this catches only a table mutated
// underneath the resolver — loudly, never silently.
func (r *resolver) assertArm(arm string, log dlog.Logger) {
	if _, ok := r.colors.RosterStatus[arm]; ok {
		return
	}
	log.Error("daemon.sidebar.assert_arm", "the roster resolved a status arm the render-colors table cannot paint",
		dlog.Context{
			"arm":                 arm,
			"invariant_violation": "roster_status has no row for the arm",
			"remediation":         "add the arm to proto/vocab/render-colors.json",
		})
}

// mutate runs one accumulation change under the lock and republishes the WHOLE
// roster. Every sink method and every setter goes through it, so there is
// exactly one publication site and exactly one readiness gate.
//
// NOTHING PUBLISHES BEFORE A REGISTRY. The roster is a view of the registry: a
// roster built from live frames alone would name no workspaces, which is not a
// smaller roster but a wrong one.
func (r *resolver) mutate(operation, message string, ctx dlog.Context, log dlog.Logger, apply func()) {
	r.mutateIf(operation, message, ctx, log, func() bool {
		apply()
		return true
	})
}

// mutateIf is mutate for a change that may decline under the lock: when
// APPLY answers false it changed nothing, and nothing is rendered,
// published or recorded.
func (r *resolver) mutateIf(operation, message string, ctx dlog.Context, log dlog.Logger, apply func() bool) {
	r.commit(&mutation{
		operation: operation, message: message, ctx: ctx, log: log,
		apply: func(dlog.Logger) bool { return apply() },
	})
}

// applyBatch applies every mutation of one combined commit (combine.go) under
// the lock, and resolves and publishes the roster ONCE for all of them. It
// answers what the caller reports off the lock.
func (r *resolver) applyBatch(batch []*mutation) batchOutcome {
	r.mu.Lock()
	var last *mutation
	causes := map[ids.WorkspaceID]string{}
	for _, m := range batch {
		if m.scoped {
			// THE WORKSPACE'S OWN SINK, resolved under the lock that reads the
			// registry it is looked up in.
			m.log = r.logFor(m.ws)
		}
		m.changed = m.apply(m.log)
		if !m.changed {
			continue
		}
		last = m
		if m.scoped {
			causes[m.ws] = m.operation
		}
	}
	if last == nil {
		r.mu.Unlock()
		return batchOutcome{}
	}
	ready := r.state.regSeen
	// A COMBINED RENDER NAMES EACH ROW'S OWN CAUSE: the last operation of the
	// batch that changed that workspace, and the batch's last operation for a
	// row no workspace-scoped change in it touched.
	r.batchCauses = causes
	defer func() { r.batchCauses = nil }()
	// THE QUIET STATE STOPS THE ROSTER, NEVER ITS FACTS: the rows' facts are
	// derived by a render that builds no row (renderFacts), so every status
	// arm move and turn result still reaches its durable sink, and the roster
	// is resolved and published once when the gate opens (resyncAfterQuiet).
	// The gate is read under the lock the resync takes, so a change is either
	// published now or left stale for the resync, never lost between them.
	//
	// A DEPARTURE IS PUBLISHED EVEN WHILE QUIET. A row moving onto a departure
	// arm (departureArm) is a fact the editor acts on, not a picture of one:
	// the merge that moved it removes the workspace's worktree, and an editor
	// still holding the roster from before the lid closed reads the vanished
	// directory as a registry row that outlived its worktree. On 2026-11-16 a
	// merge ran from admission to teardown with the lid closed, Emacs's roster
	// stood eight minutes old, and its log routing raised the
	// stale-registration warning for the workspace that had just landed.
	quiet := ready && r.quiet.Quiet()
	r.departures = nil
	if quiet {
		r.renderFacts(last.log, last.operation)
		// THE MERGED SET IS PUBLISHED EVEN WHILE QUIET. A page of merged rows
		// is resolved from the current merged set and carries the last
		// push's token (MergedPage), so a set the roster took and did not
		// publish would be paged under the token of the set before it.
		// Before any push no page is answered (ErrMergedNotReady), so there
		// is no token yet to disagree with.
		mergedMoved := r.last != nil && !slices.Equal(mergedSetOf(r.partition().merged), r.mergedSet)
		switch {
		case len(r.departures) > 0:
			for _, d := range r.departures {
				r.logFor(d.ws).Info("daemon.sidebar.quiet_departure",
					"the lid is closed and quiet, but a row moved onto a departure arm, so the roster was published",
					dlog.Context{"arm": d.arm})
			}
			quiet = false
			r.quietStale = false
		case mergedMoved:
			last.log.Info("daemon.sidebar.quiet_merged_set",
				"the lid is closed and quiet, but the set of merged workspaces changed, so the roster was published",
				dlog.Context{"merged": len(r.partition().merged)})
			quiet = false
			r.quietStale = false
		default:
			r.quietStale = true
			r.quiet.Skipped(QuietViewName)
		}
	}
	r.departures = nil
	var push *frontendv1.RosterPush
	if ready && !quiet {
		push = r.resolvePush(last.log, last.operation)
	}
	out := batchOutcome{
		applied:       true,
		ready:         ready,
		quiet:         quiet,
		results:       r.pendingResults,
		statusChanges: r.pendingStatusChanges,
		visibilities:  r.pendingVisibilities,
	}
	r.pendingResults = nil
	r.pendingStatusChanges = nil
	r.pendingVisibilities = nil
	// PUBLISHED UNDER THE LOCK THAT RENDERED IT, so views reach the topic in
	// the order the changes were made. Published after the unlock, a change
	// rendered first could be published second, and a stale view overwrote a
	// newer one on the wire (2026-10-02: the roster walked submitting, ready,
	// thinking for an accepted prompt). Topic.Publish only enqueues, so it
	// never waits on a subscriber.
	if push != nil {
		r.topic.Publish(push)
	}
	r.mu.Unlock()
	return out
}

// resolvePush renders the roster's four streams and answers the push to
// publish: every stream set, each one whose snapshot is equal to the last
// push's REUSING THAT PUSH'S MESSAGE, and the next revision when any stream
// differs. A render identical to the last one answers the last push itself,
// which the topic drops as the duplicate it is. Caller holds mu.
func (r *resolver) resolvePush(log dlog.Logger, cause string) *frontendv1.RosterPush {
	streams, set, token := r.render(log, cause)
	if r.last == nil {
		r.revision++
		streams.Revision = r.revision
		r.last, r.mergedSet, r.mergedRevision = streams, set, token
		return streams
	}
	changed := false
	reuse := func(fresh, last proto.Message) bool {
		if proto.Equal(fresh, last) {
			return true
		}
		changed = true
		return false
	}
	if reuse(streams.Workspaces, r.last.Workspaces) {
		streams.Workspaces = r.last.Workspaces
	}
	if reuse(streams.MergeQueue, r.last.MergeQueue) {
		streams.MergeQueue = r.last.MergeQueue
	}
	if reuse(streams.RecentlyMerged, r.last.RecentlyMerged) {
		streams.RecentlyMerged = r.last.RecentlyMerged
	}
	if reuse(streams.Closed, r.last.Closed) {
		streams.Closed = r.last.Closed
	}
	if !changed {
		return r.last
	}
	// EVERY PUSH WHOSE STREAMS DIFFER TAKES THE NEXT REVISION, under the lock
	// that rendered it, so the revision orders pushes exactly as they were
	// resolved (frontend.v1 RosterPush.revision).
	r.revision++
	streams.Revision = r.revision
	r.last, r.mergedSet, r.mergedRevision = streams, set, token
	return streams
}

// report hands a combined commit's results and status changes to their
// sinks, off the lock, and records each applied mutation as mutate always
// has.
func (r *resolver) report(batch []*mutation, out batchOutcome) {
	for _, change := range out.results {
		r.results(change.ws, change.result)
	}
	for _, change := range out.statusChanges {
		r.statusChanges(change.ws, change.at)
	}
	if len(out.visibilities) > 0 {
		r.visibilities(out.visibilities)
	}
	if !out.applied {
		return
	}
	for _, m := range batch {
		if !m.changed {
			continue
		}
		ctx := m.ctx
		if ctx == nil {
			ctx = dlog.Context{}
		}
		switch {
		case !out.ready:
			m.log.Debug(m.operation, "the roster took a fact before any registry and published nothing", ctx)
		case out.quiet:
			m.log.Debug(m.operation, "the roster took a fact while the lid is closed and quiet; its facts are derived and the roster waits for the lid to open", ctx)
		default:
			m.log.Debug(m.operation, m.message, ctx)
		}
	}
}

// QuietViewName names the roster in the quiet gate's records and counters.
const QuietViewName = "sidebar"

// renderFacts is render with no row built: the same walk, so exactly the
// workspaces a roster would row have their status arm and turn result
// derived (row), and the roster it answers is discarded. Caller holds mu.
func (r *resolver) renderFacts(log dlog.Logger, cause string) {
	r.factsOnly = true
	defer func() { r.factsOnly = false }()
	_, _, _ = r.render(log, cause)
}

// followQuiet is the quiet gate's follower: leaving the quiet state publishes
// the current roster once, when one was skipped. Entering needs nothing: the
// next change is the first one held back.
func (r *resolver) followQuiet(quiet bool) {
	if quiet {
		return
	}
	log := r.log.Global()
	r.mutateIf("daemon.sidebar.quiet_resync", "the lid opened; the roster was resolved and published at its current state", nil, log, func() bool {
		stale := r.quietStale
		r.quietStale = false
		return stale
	})
}

// mutateWorkspace is mutate for a fact about ONE workspace, resolved onto that
// workspace's own log sink.
func (r *resolver) mutateWorkspace(ws ids.WorkspaceID, operation, message string, ctx dlog.Context, apply func(*wsState)) {
	r.mutateWorkspaceLogged(ws, operation, message, ctx, func(s *wsState, _ dlog.Logger) { apply(s) })
}

// mutateWorkspaceLogged is mutateWorkspace for a change that records a
// transition of its own: apply is handed the workspace's log sink, and runs
// under the same lock as the render it precedes.
func (r *resolver) mutateWorkspaceLogged(ws ids.WorkspaceID, operation, message string, ctx dlog.Context, apply func(*wsState, dlog.Logger)) {
	r.mutateWorkspaceIf(ws, operation, message, ctx, func(s *wsState, log dlog.Logger) bool {
		apply(s, log)
		return true
	})
}

// mutateWorkspaceIf is mutateWorkspaceLogged for a change that may decline
// under the lock: when APPLY answers false it changed nothing about the
// workspace, and nothing is rendered, published or recorded for it.
func (r *resolver) mutateWorkspaceIf(ws ids.WorkspaceID, operation, message string, ctx dlog.Context, apply func(*wsState, dlog.Logger) bool) {
	if ctx == nil {
		ctx = dlog.Context{}
	}
	ctx["workspace_id"] = string(ws)
	r.commit(&mutation{
		operation: operation, message: message, ctx: ctx, ws: ws, scoped: true,
		apply: func(log dlog.Logger) bool {
			return apply(r.state.workspace(ws), log)
		},
	})
}

// rowContext is what every row of one resolve shares; CAUSE is the
// operation whose fact is being rendered. Caller holds mu.
func (r *resolver) rowContext(cause string) rowContext {
	return rowContext{
		sessions: r.state.sessions(),
		selected: r.state.selected,
		cause:    cause,
		causes:   r.batchCauses,
		children: childrenOf(r.state.reg.Workspaces),
	}
}

// render resolves the roster's FOUR STREAMS in one walk under the one lock
// (frontend.v1.RosterPush): the workspaces stream (the open repository view,
// the hidden section, the selection and the rail's chrome), the merge queue,
// the Recently Merged summary and the closed rows. It answers them unstamped
// (no revision), with the merged set they were resolved from and that set's
// token. CAUSE is the operation whose fact is being rendered, named on every
// status arm change the render publishes.
//
// A MERGED WORKSPACE HAS NO ROW IN ANY STREAM, BUT ITS FACTS ARE DERIVED ON
// EVERY RENDER (rowFacts), exactly as when its row was drawn in the band: its
// status arm still moves after it merged (merged, then inactive once its
// session ends), and each move is a status change the durable record keeps
// and a departure a quiet roster publishes.
func (r *resolver) render(log dlog.Logger, cause string) (*frontendv1.RosterPush, []ids.WorkspaceID, uint64) {
	if dlog.Enabled(log, dlog.LevelDebug) {
		if r.state.renderLogs == nil {
			r.state.renderLogs = map[string]string{}
		}
		filter := newRepeatFilter(log, r.state.renderLogs)
		defer filter.flush()
		log = filter
	}
	rc := r.rowContext(cause)
	lists := r.partition()
	set := mergedSetOf(lists.merged)
	token := r.mergedToken(set)

	// THE ARMS AND THE VISIBILITY COME FIRST, before any row is drawn: a row's
	// tab is numbered by whether it is visible, which reads every row's arm
	// and the Open tree's folds (visibility.go).
	reg := r.state.reg
	reg.Current = r.state.selected
	repos, orphans := openTree(reg, log)
	rc.arms = r.resolveArms(rc)
	r.resolveVisibility(repos, rc, rc.arms)

	workspaces := &frontendv1.WorkspaceRoster{
		Repository: r.repositoryView(repos, orphans, rc, log),
		Hidden:     r.hiddenSection(lists.hidden, rc, log),
		Open:       r.openSection(),
	}
	for _, ws := range sortMergedNewestFirst(lists.merged) {
		r.rowFacts(ws, rc, log)
	}
	out := &frontendv1.RosterPush{
		Workspaces:     workspaces,
		RecentlyMerged: r.mergedSection(lists.merged, token, log),
		MergeQueue:     r.mergeQueueSection(log),
		Closed:         r.closedRows(lists.closed, rc, log),
	}
	workspaces.AppSwitcher = r.appSwitcher()
	workspaces.Threads = r.threadSection(lists.threads)
	workspaces.ColdGates = r.coldGates()
	if cur := r.currentWorkspace(); cur != nil {
		workspaces.Current = cur
	}
	if v := r.state.version; v != nil {
		workspaces.Version = &frontendv1.RosterVersion{Text: v.String()}
	}
	return out, set, token
}

// rowFacts derives WS's row facts and builds no row (row under factsOnly).
// Caller holds mu.
func (r *resolver) rowFacts(ws wsm.Workspace, rc rowContext, log dlog.Logger) {
	was := r.factsOnly
	r.factsOnly = true
	defer func() { r.factsOnly = was }()
	r.row(ws, rc, log)
}

// currentWorkspace resolves the roster's selection, or nil when nothing is
// selected. It is stated by IDENTITY, never by a display name, because names
// collide across repositories.
func (r *resolver) currentWorkspace() *frontendv1.RosterCurrentWorkspace {
	ws := r.selectedWorkspace()
	if ws == nil {
		return nil
	}
	out := &frontendv1.RosterCurrentWorkspace{Workspace: ws.Ref()}
	if st := r.state.stamp; st != nil && st.ws == ws.ID {
		out.SelectionId = st.stamp.ID
		if at := st.stamp.SwitchStartedAt; at != nil {
			ms := at.UnixMilli()
			out.SwitchStartedAtMs = &ms
		}
	}
	return out
}

// selectedWorkspace is the registry's record of the selection, or nil when
// nothing is selected or the registry does not carry it yet.
func (r *resolver) selectedWorkspace() *wsm.Workspace {
	if r.state.selected == nil {
		return nil
	}
	for i := range r.state.reg.Workspaces {
		if r.state.reg.Workspaces[i].ID == *r.state.selected {
			return &r.state.reg.Workspaces[i]
		}
	}
	return nil
}

// ---- daemon-fact setters --------------------------------------------------

// SetRegistry installs the durable half, whole.
//
// A VIEW WHOSE GROUPING IS NONE OF THE THREE IS A DEFECT, never a state to
// draw around: the store refuses one on write and on read, so a registry
// carrying one was composed by a caller that skipped the store.
func (r *resolver) SetRegistry(reg Registry) {
	r.assertArrangement(reg)
	r.mutate("daemon.sidebar.set_registry", "the roster took a registry snapshot",
		registryContext(reg), r.log.Global(), func() { r.installRegistry(reg) })
}

// SetRegistrySelected installs the durable half and the user's selection in
// ONE mutation, so a select reaches every client as ONE roster push.
//
// THIS IS AN OPTIMIZATION of SetRegistry followed by SetSelected, which
// published two whole rosters per switch: Emacs and every page decoded,
// applied and repainted both (2026-10-08, the laggy-switch report). The
// selection is installed AFTER the registry, exactly as the two calls did, so
// a registry that no longer carries ws leaves the roster with no current.
func (r *resolver) SetRegistrySelected(reg Registry, ws ids.WorkspaceID, stamp SelectStamp) {
	r.assertArrangement(reg)
	ctx := stampContext(registryContext(reg), stamp)
	ctx["workspace_id"] = string(ws)
	r.mutate("daemon.sidebar.set_registry_selected",
		"the roster took a registry snapshot and the selection, and cleared the workspace's attention marker",
		ctx, r.log.Global(), func() {
			r.installRegistry(reg)
			r.installSelected(ws, stamp)
		})
}

// SetSelection installs a select's change into the registry the roster
// holds, and the selection, with no registry read.
//
// THE CHANGE IS THE STORE'S OWN READ-BACK (wsm.Store.Select): the rows its
// write left, read in the same transaction, replace the roster's copies
// whole, so the registry held here and the store can never disagree about
// a selection. The store makes at most one workspace current, and the
// change names it, so every other row the roster holds stops being current.
//
// THE CALLER ORDERS IT AGAINST EVERY WHOLE REGISTRY INSTALL (the workspace
// verbs' selection lock and select count): a read taken before this select's
// write can never be installed after it.
func (r *resolver) SetSelection(sel wsm.Selection, stamp SelectStamp) bool {
	ctx := stampContext(dlog.Context{"workspace_id": string(sel.Current), "again": sel.Again, "rows": len(sel.Workspaces)}, stamp)
	applied := false
	r.mutateIf("daemon.sidebar.set_selection",
		"the roster took the select's change and the selection, and cleared the workspace's attention marker",
		ctx, r.log.Global(), func() bool {
			reg, ok := patchedRegistry(r.state.reg, r.state.regSeen, sel)
			if !ok {
				return false
			}
			r.state.reg = reg
			// A RE-SELECTION CHANGES NO VIEW, so its stamp is not stated:
			// the selection's standing one stays, and the roster renders
			// as it did and is dropped as the duplicate it is.
			if sel.Again {
				r.installSelected(sel.Current, r.standingStamp(sel.Current))
			} else {
				r.installSelected(sel.Current, stamp)
			}
			applied = true
			return true
		})
	if !applied {
		r.log.Global().Info("daemon.sidebar.set_selection",
			"the roster's registry cannot take the select's change; the caller installs a whole registry", ctx)
	}
	return applied
}

// patchedRegistry answers REG with the select's change SEL installed, or
// false when REG cannot take it: none was installed yet (SEEN), or it
// carries no row, repository or subsection SEL names. It copies every slice
// it changes: the registry it was handed is shared.
func patchedRegistry(reg Registry, seen bool, sel wsm.Selection) (Registry, bool) {
	if !seen {
		return Registry{}, false
	}
	rows := make(map[ids.WorkspaceID]wsm.Workspace, len(sel.Workspaces))
	for _, row := range sel.Workspaces {
		rows[row.ID] = row
	}
	workspaces := slices.Clone(reg.Workspaces)
	for i := range workspaces {
		if row, ok := rows[workspaces[i].ID]; ok {
			workspaces[i] = row
			delete(rows, row.ID)
			continue
		}
		workspaces[i].Current = false
	}
	if len(rows) > 0 {
		return Registry{}, false
	}
	repositories := slices.Clone(reg.Repositories)
	at := slices.IndexFunc(repositories, func(repo wsm.Repository) bool { return repo.ID == sel.Repository.ID })
	if at < 0 {
		return Registry{}, false
	}
	repositories[at] = sel.Repository
	subsections := reg.Subsections
	if sel.Subsection != nil {
		subsections = slices.Clone(reg.Subsections)
		at := slices.IndexFunc(subsections, func(sub wsm.Subsection) bool { return sub.ID == sel.Subsection.ID })
		if at < 0 {
			return Registry{}, false
		}
		subsections[at] = *sel.Subsection
	}
	current := sel.Current
	reg.Workspaces, reg.Repositories, reg.Subsections, reg.Current = workspaces, repositories, subsections, &current
	return reg, true
}

// standingStamp is the stamp the roster states for WS now, the zero stamp
// when it states none; it runs under the lock.
func (r *resolver) standingStamp(ws ids.WorkspaceID) SelectStamp {
	if st := r.state.stamp; st != nil && st.ws == ws {
		return st.stamp
	}
	return SelectStamp{}
}

// installSelected makes WS the selection under STAMP; it runs under the lock.
func (r *resolver) installSelected(ws ids.WorkspaceID, stamp SelectStamp) {
	selected := ws
	r.state.selected = &selected
	r.state.stamp = &stampedSelection{ws: ws, stamp: stamp}
}

// stampContext adds a select's stamp to a record's context.
func stampContext(ctx dlog.Context, stamp SelectStamp) dlog.Context {
	if stamp.ID != "" {
		ctx["selection_id"] = stamp.ID
	}
	if stamp.SwitchStartedAt != nil {
		ctx["switch_started_at"] = stamp.SwitchStartedAt.UTC()
	}
	return ctx
}

// Selected answers the roster's selection, read under the lock every mutation
// takes, so it is never a selection half-installed.
func (r *resolver) Selected() (ids.WorkspaceID, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.state.selected == nil {
		return "", false
	}
	return *r.state.selected, true
}

// registryContext is the record context every registry install carries.
func registryContext(reg Registry) dlog.Context {
	return dlog.Context{
		"workspaces":   len(reg.Workspaces),
		"repositories": len(reg.Repositories),
		"tasks":        len(reg.Tasks),
		"sessions":     len(reg.Sessions),
	}
}

// installRegistry is the registry half of a mutation; it runs under the lock.
func (r *resolver) installRegistry(reg Registry) {
	r.state.reg = reg
	r.state.regSeen = true
	// The registry's selection is the durable one. A selection the daemon
	// stamped a moment ago stands until WSM catches up, so a registry that
	// carries none does NOT clear it.
	if reg.Current != nil {
		current := *reg.Current
		r.state.selected = &current
		// A SELECTION THE REGISTRY MOVED is one no select of this daemon's
		// stamped, so the stamp of the one it left is not stated for it.
		if r.state.stamp != nil && r.state.stamp.ws != current {
			r.state.stamp = nil
		}
	}
	r.forgetNuked()
}

// forgetNuked drops the live half of every workspace the registry no longer
// carries. A NUKED WORKSPACE LEAVES THE ROSTER, and its accumulation leaves
// with it rather than lingering as a leak that would resurrect the row if the
// id were ever reused.
func (r *resolver) forgetNuked() {
	known := make(map[ids.WorkspaceID]struct{}, len(r.state.reg.Workspaces))
	for _, ws := range r.state.reg.Workspaces {
		known[ws.ID] = struct{}{}
	}
	for ws := range r.state.workspaces {
		if _, ok := known[ws]; ok {
			continue
		}
		delete(r.state.workspaces, ws)
	}
	if r.state.selected != nil {
		if _, ok := known[*r.state.selected]; !ok {
			r.state.selected = nil
			r.state.stamp = nil
		}
	}
}

// SetMerge installs one workspace's merge facts.
func (r *resolver) SetMerge(ws ids.WorkspaceID, facts footer.MergeFacts) {
	r.mutateWorkspaceLogged(ws, "daemon.sidebar.set_merge", "the roster took the merge facts",
		dlog.Context{"state": facts.State}, func(s *wsState, log dlog.Logger) {
			s.merge = facts
		})
}

// SetMergeQueue replaces REPO's merge queue as the roster draws it.
func (r *resolver) SetMergeQueue(repo wsm.RepoKey, queue MergeQueue) {
	fields := dlog.Context{"repo": string(repo), "paused": queue.Pause != nil, "held_for_deploy_check": queue.DeployCheck != nil,
		"workspaces": ids.Strings(queue.Workspaces), "line": ids.Strings(queue.LineWorkspaces()), "async_requesters": ids.Strings(queue.AsyncRequesters)}
	r.mutate("daemon.sidebar.set_merge_queue", "the roster took a repository's merge queue",
		fields,
		r.log.Global(),
		func() {
			q := newMergeQueue(queue)
			if q.empty() {
				delete(r.state.queues, repo)
				return
			}
			r.state.queues[repo] = q
		})
}

// SetGithubQueue replaces the GitHub merge queues' share as the roster draws
// it.
func (r *resolver) SetGithubQueue(entries []GithubQueueEntry) {
	ws := make([]ids.WorkspaceID, 0, len(entries))
	for _, e := range entries {
		ws = append(ws, e.Workspace)
	}
	r.mutate("daemon.sidebar.set_github_queue", "the roster took the workspaces standing in GitHub merge queues",
		dlog.Context{"workspaces": ids.Strings(ws)},
		r.log.Global(),
		func() { r.state.github = entries })
}

// SetOffQueue replaces the merges off their queues as the roster draws them.
func (r *resolver) SetOffQueue(entries []OffQueueEntry) {
	ws := make([]ids.WorkspaceID, 0, len(entries))
	for _, e := range entries {
		ws = append(ws, e.Workspace)
	}
	r.mutate("daemon.sidebar.set_off_queue", "the roster took the merges standing off their queues",
		dlog.Context{"workspaces": ids.Strings(ws)},
		r.log.Global(),
		func() { r.state.offQueue = entries })
}

// SetColdGate installs, or with nil lifts, WS's shown cold gate.
func (r *resolver) SetColdGate(ws ids.WorkspaceID, row *frontendv1.FeedId, gate *frontendv1.FeedColdGate) {
	if (row == nil) != (gate == nil) {
		panic(fmt.Sprintf("sidebar: SetColdGate(%s) was handed a row id without a card, or a card without one", ws))
	}
	r.mutateWorkspace(ws, "daemon.sidebar.set_cold_gate", "the roster took the workspace's shown cold gate",
		dlog.Context{"standing": gate != nil}, func(*wsState) {
			if gate == nil {
				delete(r.state.gates, ws)
				return
			}
			r.state.gates[ws] = coldGate{row: row, gate: gate}
		})
}

// coldGates resolves the shown cold gates of the workspaces listed open, in
// the registry's order: a closed or hidden workspace's gate has no page to be
// shown over.
func (r *resolver) coldGates() []*frontendv1.RosterColdGate {
	var out []*frontendv1.RosterColdGate
	for _, ws := range r.state.reg.Workspaces {
		if ws.Closed || ws.Hidden || ws.IsThread() {
			continue
		}
		g, ok := r.state.gates[ws.ID]
		if !ok {
			continue
		}
		out = append(out, &frontendv1.RosterColdGate{Workspace: ws.Ref(), Row: g.row, Gate: g.gate})
	}
	return out
}

// SetRepoDivergence installs a repository's latest ahead/behind measure.
func (r *resolver) SetRepoDivergence(repo ids.RepoID, measure repodivergence.Measure) {
	if measure == nil {
		panic(fmt.Sprintf("sidebar: SetRepoDivergence(%s) was handed no measure", repo))
	}
	r.mutate("daemon.sidebar.set_repo_divergence", "the roster took a repository's divergence measure",
		dlog.Context{"repo_id": string(repo), "measure": fmt.Sprintf("%T", measure)}, r.log.Global(),
		func() { r.state.divergence[repo] = measure })
}

// DeployedVersion installs the release the rail's header states. Only the
// release is drawn: a build between landings states the last landing's
// version, so the commit it is past by is not the roster's to carry.
func (r *resolver) DeployedVersion(v *deployedversion.Version) {
	ctx := dlog.Context{}
	var release *semver.Version
	if v != nil {
		own := v.Release
		release = &own
		ctx["version"] = own.String()
	}
	r.mutate("daemon.sidebar.set_version", "the roster took the deployed version", ctx, r.log.Global(),
		func() { r.state.version = release })
}

// SetSelected records the user's selection.
func (r *resolver) SetSelected(ws ids.WorkspaceID) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_selected",
		"the roster took the selection and cleared the workspace's attention marker",
		nil, func(*wsState) {
			// THE SELECTION LEAVING A WORKSPACE ENDS ITS DWELL: the user is no
			// longer looking at it, so a later return counts a fresh one
			// rather than drawing this one's elapsed fade.
			if previous := r.state.selected; previous != nil && *previous != ws {
				r.state.workspace(*previous).dwell = nil
			}
			selected := ws
			r.state.selected = &selected
		})
}

// SetViewed records that the user has READ the last turn's result, which
// draws the row PARTIAL — but only when the report lands on a TURN-END row
// (done, interrupted or turn_failed), or on a VENDOR_BLOCKED row, whose block
// a failed turn raised: viewing it reads that turn's result (owner ruling,
// 2026-09-28), though the marker itself is still drawn only on a turn end.
// It also reads an UNREAD result from an IDLE_ASYNC row, which live detached
// work draws over that turn end (idleArm): the user viewing it is viewing the
// finished turn.
//
// The editor is the only caller (MarkWorkspaceViewed): dwell is an editor
// fact, and the editor reports it whatever the status. Whether it takes is
// the roster's decision, made against the arm the row last PUBLISHED — the
// one the user was looking at — under the same lock as the render this
// mutation runs. A report on a turn-end row, or on an `idle_async` row over an
// unread result, reads the result, and the row comes back PARTIAL on its
// turn-end arm once detached work ends. A report on any other row is refused
// and recorded, and changes nothing.
// There is no companion lowering setter, deliberately: a new turn is what
// makes the next result unread (startTurn, SetTurnEnded).
func (r *resolver) SetViewed(ws ids.WorkspaceID) {
	r.mutateWorkspaceLogged(ws, "daemon.sidebar.set_viewed",
		"the roster took the editor's viewed report; it reads the result only on a turn-end row",
		nil, func(s *wsState, log dlog.Logger) {
			// THE VERDICT ENDS THE DWELL, whether or not it reads the result:
			// the editor has stopped counting either way.
			s.dwell = nil
			if !s.lastArmSeen || !s.viewReadsResult(s.lastArm) {
				log.Debug("daemon.sidebar.row_viewed_refused",
					"the row is not on a turn-end arm, so the viewed report was dropped and the row stays FULL",
					dlog.Context{"status": s.lastArm, "result": s.result.String()})
				return
			}
			if s.result == resultRead {
				return
			}
			log.Info("daemon.sidebar.result_read",
				"the user viewed the turn-end row, so the last turn's result is read", dlog.Context{
					"status":     s.lastArm,
					"was":        s.result.String(),
					"async_live": s.asyncLive(),
				})
			s.result = resultRead
			s.resultSettled = true
		})
}

// SetViewDwell implements Resolver.
func (r *resolver) SetViewDwell(ws ids.WorkspaceID, length time.Duration) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_view_dwell",
		"the roster took the editor's view dwell; the row carries it while it will recede the name",
		dlog.Context{"length_ms": length.Milliseconds()}, func(s *wsState) {
			s.dwell = &viewDwell{startedAt: r.now(), length: length}
		})
}

// StopViewDwell implements Resolver. Stopping a workspace with no dwell is a
// success that changes nothing, so it publishes nothing.
func (r *resolver) StopViewDwell(ws ids.WorkspaceID) {
	r.mutateWorkspaceIf(ws, "daemon.sidebar.stop_view_dwell",
		"the roster ended the editor's view dwell before it completed",
		nil, func(s *wsState, _ dlog.Logger) bool {
			if s.dwell == nil {
				return false
			}
			s.dwell = nil
			return true
		})
}

// SetBringingUp raises or lowers the workspace's bring-up-under-way fact,
// which holds its availability at `pending` until a link connects.
func (r *resolver) SetBringingUp(ws ids.WorkspaceID, bringingUp bool) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_bringing_up",
		"the roster took whether a bring-up is under way",
		dlog.Context{"bringing_up": bringingUp}, func(s *wsState) { s.bringingUp = bringingUp })
}

// SetIncumbentArms implements Resolver. The map is copied, so the caller's
// later writes to it cannot reach a render.
func (r *resolver) SetIncumbentArms(arms map[ids.WorkspaceID]string) {
	var copied map[ids.WorkspaceID]string
	if len(arms) > 0 {
		copied = make(map[ids.WorkspaceID]string, len(arms))
		for ws, arm := range arms {
			copied[ws] = arm
		}
	}
	r.mutateIf("daemon.sidebar.set_incumbent_arms",
		"the roster took the incumbent's status arms for the workspaces not yet adopted",
		dlog.Context{"mirrored": len(copied)}, r.log.Global(), func() bool {
			if maps.Equal(r.state.incumbentArms, copied) {
				return false
			}
			r.state.incumbentArms = copied
			return true
		})
}

// SetOpening raises or lowers the workspace's open-in-flight fact, which
// draws the `inactive` arm and the row's `opening` marker over every other.
func (r *resolver) SetOpening(ws ids.WorkspaceID, opening bool) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_opening",
		"the roster took whether an open of the workspace is in flight",
		dlog.Context{"opening": opening}, func(s *wsState) { s.opening = opening })
}

// SetThreadOutline installs a chat thread's outline, nil once it has none.
func (r *resolver) SetThreadOutline(ws ids.WorkspaceID, outline *frontendv1.RosterThreadOutline) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_thread_outline",
		"the roster took a chat thread's outline",
		dlog.Context{"rows": len(outline.GetRows())}, func(s *wsState) { s.outline = outline })
}

// SetVendorStart installs where the vendor-start run stands.
func (r *resolver) SetVendorStart(ws ids.WorkspaceID, state VendorStart) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_vendor_start",
		"the roster took where the vendor-start run stands",
		dlog.Context{"vendor_start": state.String()}, func(s *wsState) { s.vendorStart = state })
}

// NetworkFaultOpened installs a standing network fault.
func (r *resolver) NetworkFaultOpened(ws ids.WorkspaceID, id string) {
	r.mutateWorkspace(ws, "daemon.sidebar.network_fault_opened",
		"the roster took a standing network fault",
		dlog.Context{"fault": id}, func(s *wsState) {
			if s.networkFaults == nil {
				s.networkFaults = map[string]bool{}
			}
			s.networkFaults[id] = true
		})
}

// FaultClosed retracts a standing fault by id; one the roster does not hold
// changes nothing and publishes nothing.
func (r *resolver) FaultClosed(ws ids.WorkspaceID, id string) {
	if !r.holdsNetworkFault(ws, id) {
		return
	}
	r.mutateWorkspace(ws, "daemon.sidebar.fault_closed",
		"the roster retracted a standing network fault",
		dlog.Context{"fault": id}, func(s *wsState) { delete(s.networkFaults, id) })
}

// SetReviving raises or lowers the workspace's REVIVING marker, which draws a
// shimmer across the row's name while its parked session comes back up.
//
// Both edges are the workspace verbs' (startRevival): the raise when a
// revival is decided, the lower when it ends whichever way it ended. A marker
// left standing past its revival would say "coming back" about a session that
// already did, or never will.
func (r *resolver) SetReviving(ws ids.WorkspaceID, reviving bool) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_reviving",
		"the roster took the workspace's revival edge",
		dlog.Context{"reviving": reviving}, func(s *wsState) { s.reviving = reviving })
}

// SetTurn installs the accepted turn.
func (r *resolver) SetTurn(ws ids.WorkspaceID, turn *footer.TurnStarted) {
	ctx := dlog.Context{"in_flight": turn != nil}
	if turn != nil {
		ctx["act"] = int(turn.Act)
	}
	r.mutateWorkspaceLogged(ws, "daemon.sidebar.set_turn", "the roster took the accepted turn", ctx,
		func(s *wsState, log dlog.Logger) {
			if turn != nil && s.result == resultUnread {
				log.Info("daemon.sidebar.result_unread_cleared",
					"a new prompt started a turn, so the last turn's unread result no longer holds the row", nil)
			}
			s.startTurn(turn)
		})
}

// OnTurnRunningAtAttach stands the turn an adopted shim was already running
// when its watcher attached (sessionwatcher.SidebarSink), so a daemon that took
// a busy workspace over draws it `thinking` rather than the idle rung. A turn
// that already stands is this daemon's own and stays as it is. The turn is
// running, so it is acknowledged; the roster draws no clock, so the start is
// kept only when it is known.
func (r *resolver) OnTurnRunningAtAttach(ws ids.WorkspaceID, turn ids.TurnID, startedAt *time.Time) {
	r.mutateWorkspaceLogged(ws, "daemon.sidebar.on_turn_running_at_attach", "the roster took a turn the adopted shim is running",
		dlog.Context{"turn_id": string(turn), "row_known": startedAt != nil}, func(s *wsState, log dlog.Logger) {
			if s.turn != nil {
				log.Debug("daemon.sidebar.on_turn_running_at_attach",
					"a turn already stands; the adopted one is this daemon's own", dlog.Context{"turn_id": string(turn)})
				return
			}
			started := &footer.TurnStarted{Act: footer.ActPrompt}
			if startedAt != nil {
				started.At = *startedAt
			}
			s.startTurn(started)
			s.sawActivity = true
		})
}

// AckTurn records the shim's acceptance of the turn, ending `submitting`.
func (r *resolver) AckTurn(ws ids.WorkspaceID) {
	r.mutateWorkspace(ws, "daemon.sidebar.ack_turn", "the roster took the turn's ack", nil,
		func(s *wsState) {
			if s.turn != nil {
				s.sawActivity = true
			}
		})
}

// SetTurnEnded installs how the last turn ended.
func (r *resolver) SetTurnEnded(ws ids.WorkspaceID, how TurnClose) {
	r.mutateWorkspaceLogged(ws, "daemon.sidebar.set_turn_ended", "the roster took the turn's close",
		dlog.Context{"close": int(how)}, func(s *wsState, log dlog.Logger) {
			cut := contextCut(s.turn)
			s.turn = nil
			// THE TURN'S END IS THE RETRY'S END: nothing is left to retry for.
			s.retrying = ""
			s.turnEverRan = true
			s.lastClose = how
			s.endAnswered = false
			s.compacting = false
			s.settleResult()
			// A COMPLETED, INTERRUPTED or FAILED turn leaves a result the
			// user has not read. A close this build does not know is a
			// contract breach: it is recorded loudly and leaves no tracked
			// result, and since it is still a NEW ending, a read state from
			// before it does not carry over.
			fault, faultKnown := ladder.ResolveTurnFault(how, s.lastFailure)
			if faultKnown {
				s.turnFault = fault
			}
			end, known := ladder.ResolveTurnEnd(how, s.lastFailure)
			if !known {
				s.result = resultNone
				log.Error("daemon.sidebar.set_turn_ended",
					"the roster took a turn close it has no turn-end arm for", dlog.Context{
						"close":               int(how),
						"invariant_violation": "every turn close resolves to a turn-end arm",
						"remediation":         "add the close to ladder.ResolveTurnEnd",
					})
				return
			}
			// A CONTEXT CUT THAT SUCCEEDED HAS NOTHING TO READ (owner ruling,
			// 2026-10-02): a /clear or a compaction the user asked for leaves no
			// response, only the cut, so its `done` is read the moment it lands
			// and the row is drawn PARTIAL on the very push that ends it — no
			// dwell, so no editor timer is involved. A cut that was interrupted
			// or failed is a result like any other turn's and stays unread.
			if cut != "" && end == ladder.TurnEndDone {
				s.result = resultRead
				log.Info("daemon.sidebar.result_read_on_cut",
					"a context cut completed, so its result is read at once and the row is drawn PARTIAL with no dwell", dlog.Context{
						"act":        cut,
						"status":     end.String(),
						"async_live": s.asyncLive(),
					})
				return
			}
			// AN INTERRUPTED TURN IS READ THE MOMENT IT ENDS (owner ruling,
			// 2026-10-13): the user stopped it, so there is nothing new to
			// look at, and the row is drawn PARTIAL on the very push that
			// ends it, in the sidebar and the tab bar alike.
			if end == ladder.TurnEndInterrupted {
				s.result = resultRead
				log.Info("daemon.sidebar.result_read_on_interrupt",
					"the turn was interrupted, so its result is read at once and the row is drawn PARTIAL with no dwell", dlog.Context{
						"status":     end.String(),
						"async_live": s.asyncLive(),
					})
				return
			}
			s.result = resultUnread
			log.Info("daemon.sidebar.result_unread",
				"the turn ended, so its result is unread until the user views the row", dlog.Context{
					"status":     end.String(),
					"async_live": s.asyncLive(),
				})
		})
}

// contextCut names the context cut the turn carries — "clear" or "compact" —
// and is empty for an ordinary prompt or when no turn stands. A vendor's
// auto-compaction runs INSIDE an ordinary prompt's turn, so it is not a cut
// the user asked for and is not named here.
func contextCut(turn *footer.TurnStarted) string {
	if turn == nil {
		return ""
	}
	switch turn.Act {
	case footer.ActClear:
		return "clear"
	case footer.ActCompact:
		return "compact"
	default:
		return ""
	}
}

// SetSummary installs the row detail's summary line.
func (r *resolver) SetSummary(ws ids.WorkspaceID, text string) {
	line := firstLine(text)
	r.mutateWorkspace(ws, "daemon.sidebar.set_summary", "the roster took the row's summary line",
		dlog.Context{"empty": line == ""}, func(s *wsState) { s.summary = line })
}

// SetOpenOwnedPullRequest installs whether the workspace owns a pull request
// still open. An unchanged fact renders nothing.
func (r *resolver) SetOpenOwnedPullRequest(ws ids.WorkspaceID, open bool) {
	r.mutateWorkspaceIf(ws, "daemon.sidebar.set_open_owned_pull_request", "the roster took whether the workspace owns an open pull request",
		dlog.Context{"open": open}, func(s *wsState, _ dlog.Logger) bool {
			if s.openOwnedPullRequest == open {
				return false
			}
			s.openOwnedPullRequest = open
			return true
		})
}

// ---- SidebarSink ----------------------------------------------------------

// OnSessionStarted marks the row live.
func (r *resolver) OnSessionStarted(ws ids.WorkspaceID, started *conversationv1.SessionStarted) {
	r.mutateWorkspace(ws, "daemon.sidebar.on_session_started", "the roster took a session start",
		dlog.Context{"vendor_session_id": started.GetVendorSessionId()}, func(s *wsState) {
			s.started = true
			s.liftVendorBlock()
			s.stateUnreported = false
		})
}

// SetStateUnreported installs, or lifts, the fact that a shim taken back after
// a failed handover has not re-reported its session state. A session start
// lifts it too (OnSessionStarted), which is the re-report it waits for.
func (r *resolver) SetStateUnreported(ws ids.WorkspaceID, unreported bool) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_state_unreported", "the roster took whether the shim's session state is unreported",
		dlog.Context{"unreported": unreported}, func(s *wsState) { s.stateUnreported = unreported })
}

// OnActivity moves the row to thinking.
//
// AN ACTIVITY CHANGES THE ROW'S FACTS ONLY TWICE: the turn's first activity
// (sawActivity) and the answer to a retried call (retrying). Every other
// streamed frame declines, so it renders and publishes nothing: rendered, it
// was a whole-roster render the topic dropped as a duplicate, on every frame
// (2026-11-11, agent-repld's top cost in a `sample`). A footer status the
// frame moved reaches the row through FooterStatusChanged, not here.
func (r *resolver) OnActivity(ws ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity) {
	r.mutateWorkspaceIf(ws, "daemon.sidebar.on_activity", "the roster took a turn's activity",
		dlog.Context{"agent_id": agent.GetValue()}, func(s *wsState, log dlog.Logger) bool {
			changed := !s.sawActivity
			s.sawActivity = true
			if s.retrying != "" && ladder.RetryAnsweredBy(s.retrying, agent.GetValue(), act) {
				log.Debug("daemon.sidebar.retry_cleared", "the retried call was answered; the row leaves api_retrying",
					dlog.Context{"agent_id": agent.GetValue()})
				s.retrying = ""
				changed = true
			}
			return changed
		})
}

// OnApiError stands the vendor's mid-turn retry of AGENT's call: the row is
// `api_retrying` (turquoise) until that agent is answered, the turn ends, or a
// new turn opens — the same lifetime the footer's `vendor_fault ·
// api_retrying` has.
func (r *resolver) OnApiError(ws ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed) {
	if failed == nil {
		return
	}
	r.mutateWorkspace(ws, "daemon.sidebar.on_api_error", "the roster took mid-turn api failure evidence",
		dlog.Context{"agent_id": agent.GetValue()}, func(s *wsState) { s.retrying = agent.GetValue() })
}

// OnPermission moves the row to waiting, and moves it off waiting again when
// the gate is DECIDED.
//
// The ask's own result oneof is what says which: `start` opens the gate and
// either `success` or `failure` closes it — opened or closed, a decided gate is
// no longer waiting on the user. Tracking the ask by identity rather than as a
// flag is what lets two gated calls both be answered before the row leaves
// `permission`.
func (r *resolver) OnPermission(ws ids.WorkspaceID, agent *conversationv1.AgentId, p *conversationv1.AgentPermission) {
	key := permissionKey(agent, p)
	_, open := p.GetResult().(*conversationv1.AgentPermission_Start)
	r.mutateWorkspace(ws, "daemon.sidebar.on_permission", "the roster took a permission ask",
		dlog.Context{"agent_id": agent.GetValue(), "open": open}, func(s *wsState) {
			if open {
				s.permissions[key] = struct{}{}
				return
			}
			delete(s.permissions, key)
		})
}

// OnDetachedWork moves the row to background.
func (r *resolver) OnDetachedWork(ws ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork) {
	// THE KIND IS REQUIRED, and the session watcher refuses an announcement
	// without one before any view takes it (resolveDetachedLocked).
	protoarm.Must(work.GetKind().ProtoReflect(), "kind", "daemon.sidebar.on_detached_work")
	r.mutateWorkspace(ws, "daemon.sidebar.on_detached_work", "the roster took a detached-work announcement",
		dlog.Context{"agent_id": agent.GetValue()}, func(s *wsState) {
			s.detached[detachedKey(agent, work)] = struct{}{}
		})
}

// OnLiveWorkChanged takes the watcher's authoritative live-work set, which is
// what RETIRES `idle_async`. An announcement can only raise the arm: nothing on
// the agent's stream states that a detached item ended, and the watcher — which
// reaps each item's watch at its terminal — is the one party that knows.
func (r *resolver) OnLiveWorkChanged(ws ids.WorkspaceID, live LiveWorkSet) {
	r.mutateWorkspace(ws, "daemon.sidebar.on_live_work_changed", "the roster took the live-work set",
		dlog.Context{
			"agents": len(live.Agents), "shells": len(live.Shells), "monitors": len(live.Monitors),
		}, func(s *wsState) {
			s.liveWork = live
			s.liveWorkSeen = true
		})
}

// OnAgentTerminal retires the row's thinking state.
//
// It retires the AGENT, not the turn: a turn's own close is SetTurnEnded's, and
// the two are different facts — a subagent can end while its turn runs on.
func (r *resolver) OnAgentTerminal(ws ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) {
	ctx := dlog.Context{
		"agent_id": agent.GetValue(),
		"outcome":  terminalOutcome(success, failure),
	}
	if turn != nil {
		ctx["turn_id"] = string(*turn)
	}
	// ONLY THE TURN'S OWN TERMINAL SPEAKS FOR THE WORKSPACE. A subagent's
	// failure ends the subagent, not the session, and the footer has only
	// ever read the main thread's terminal: reading every agent's here let a
	// failed subagent paint the whole row blocked beside a strip that was not.
	class := ladder.NoFailure
	if turn != nil {
		class = ladder.ClassifyFailure(failure)
	}
	ctx["failure_class"] = class.String()
	r.mutateWorkspace(ws, "daemon.sidebar.on_agent_terminal", "the roster took an agent terminal", ctx,
		func(s *wsState) {
			for key := range s.permissions {
				if agentOf(key) == agent.GetValue() {
					delete(s.permissions, key)
				}
			}
			if turn == nil {
				return
			}
			s.lastFailure = class
			s.lastFailureSignInCures = ladder.FailureCuredBySignIn(failure)
			if class == ladder.VendorBlocked {
				s.vendorBlocked = true
			}
		})
}

// OnSessionUpdate carries the terminals and faults the row reflects.
func (r *resolver) OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	if update == nil {
		return
	}
	arm, apply := sessionUpdateArm(update)
	r.mutateWorkspace(ws, "daemon.sidebar.on_session_update", "the roster took a session update",
		dlog.Context{"arm": arm}, apply)
}

// sessionUpdateArm names the update's arm and returns what it changes. EVERY
// arm has a branch, including the ones the roster deliberately draws nothing
// from, so a new arm is a compile-time question rather than a silent default.
func sessionUpdateArm(update *conversationv1.SessionUpdate) (string, func(*wsState)) {
	switch u := update.GetUpdate().(type) {
	case *conversationv1.SessionUpdate_QueryDied:
		// A DEAD QUERY IS A FAILED TURN, NOT A BLOCK (owner ruling,
		// 2026-09-28): the watcher closes the turn it cut as failed, which
		// the roster draws `turn_failed`, and nothing about the vendor or the
		// account refuses the session.
		return "query_died", func(s *wsState) {
			// THE TURN THE DEATH CUT DIED OF IT: its close raises the
			// agent-repl turn fault (ladder.ResolveTurnFault), as the footer's
			// does from the same close.
			if s.turn != nil {
				s.lastFailure = ladder.AgentReplFailed
			}
			s.turn = nil
			s.retrying = ""
		}
	case *conversationv1.SessionUpdate_RateLimitStatus:
		return "rate_limit_status", func(s *wsState) {
			s.vendorBlocked = s.rateLimits.Observe(u.RateLimitStatus)
		}
	case *conversationv1.SessionUpdate_Compacting:
		return "compacting", func(s *wsState) { s.compacting = true }
	case *conversationv1.SessionUpdate_Diagnostics:
		return "diagnostics", func(s *wsState) { s.degraded = ladder.DegradedWindowOpen(u.Diagnostics) }
	case *conversationv1.SessionUpdate_ModelChanged:
		return "model_changed", func(*wsState) {}
	case *conversationv1.SessionUpdate_PermissionModeChanged:
		return "permission_mode_changed", func(*wsState) {}
	case *conversationv1.SessionUpdate_IdentityRotated:
		return "identity_rotated", func(*wsState) {}
	case *conversationv1.SessionUpdate_FastMode:
		return "fast_mode", func(*wsState) {}
	case *conversationv1.SessionUpdate_McpServer:
		return "mcp_server", func(*wsState) {}
	case *conversationv1.SessionUpdate_ContextUsage:
		return "context_usage", func(*wsState) {}
	default:
		return "unset", func(*wsState) {}
	}
}

// OnLink drives the init, severed, dead and start_failed arms.
func (r *resolver) OnLink(ws ids.WorkspaceID, link sessionwatcher.LinkState) {
	r.mutateWorkspace(ws, "daemon.sidebar.on_link", "the roster took a link state",
		dlog.Context{"link": linkName(link)}, func(s *wsState) {
			s.link = link
			s.linkSeen = true
			if link == shimclient.LinkConnected {
				s.everConnected = true
			}
		})
}

// ---- naming ---------------------------------------------------------------

// permissionKey identifies one open consent ask. It is agent-scoped so an
// agent's terminal retires exactly that agent's asks.
func permissionKey(agent *conversationv1.AgentId, p *conversationv1.AgentPermission) string {
	return agent.GetValue() + "\x00" + p.GetId().GetValue()
}

// detachedKey identifies one detached-work item.
func detachedKey(agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork) string {
	return agent.GetValue() + "\x00" + work.GetWork().GetValue()
}

// agentOf reads the agent half of a composite key.
func agentOf(key string) string {
	for i := 0; i < len(key); i++ {
		if key[i] == 0 {
			return key[:i]
		}
	}
	return key
}

// terminalOutcome names how an agent's stream ended, for the record.
func terminalOutcome(success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) string {
	switch {
	case success != nil:
		return "success"
	case failure != nil:
		return "failure"
	default:
		return "unset"
	}
}

// linkName spells a link state for the record.
func linkName(link sessionwatcher.LinkState) string {
	switch link {
	case shimclient.LinkDialing:
		return "dialing"
	case shimclient.LinkConnected:
		return "connected"
	case shimclient.LinkRedialing:
		return "redialing"
	case shimclient.LinkDead:
		return "dead"
	default:
		return "unknown"
	}
}

// holdsNetworkFault reports whether the workspace stands on the network fault
// with this id.
func (r *resolver) holdsNetworkFault(ws ids.WorkspaceID, id string) bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.state.workspace(ws).networkFaults[id]
}

// OnSignedIn takes a completed sign-in for the workspace's account root. When
// the last turn failed on an account cause a sign-in cures, that failure is
// answered: its fault and its block lift, its unread result is retired, and
// the row draws `ready` until the next turn. Any other last turn is left
// exactly as it stands.
func (r *resolver) OnSignedIn(ws ids.WorkspaceID) {
	r.mutateWorkspaceLogged(ws, "daemon.sidebar.on_signed_in", "the roster took a completed sign-in",
		nil, func(s *wsState, log dlog.Logger) {
			if !s.lastFailureSignInCures {
				log.Debug("daemon.sidebar.signed_in", "a sign-in completed and the last turn did not fail on an account cause; the row is unchanged", nil)
				return
			}
			s.lastFailure = ladder.NoFailure
			s.lastFailureSignInCures = false
			s.turnFault = ladder.NoTurnFault
			// A SIGN-IN ANSWERS THE ACCOUNT CAUSE ONLY: a window the vendor
			// rejected stays rejected until its own verdict lifts it, as the
			// footer's sign-in leaves its rate-limit rejections standing.
			s.vendorBlocked = false
			s.result = resultNone
			s.endAnswered = true
			s.settleResult()
			log.Info("daemon.sidebar.signed_in", "a completed sign-in answered the last turn's account failure; the row draws ready", nil)
		})
}
