package server

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"net/http/httptest"
	"slices"
	"testing"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/vocab"
	"agent-repld/internal/wsm"
)

// mergedRow is a merged page's row of workspace ID.
func mergedRow(id string) *frontendv1.RosterMergedRow {
	return &frontendv1.RosterMergedRow{Row: &frontendv1.RosterRow{Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: id}}}}
}

// byMerge is the most-recently-merged order.
func byMerge() *agentreplv1.ListMergedWorkspacesOrder {
	return &agentreplv1.ListMergedWorkspacesOrder{Order: &agentreplv1.ListMergedWorkspacesOrder_MostRecentlyMerged{MostRecentlyMerged: &agentreplv1.ListMergedWorkspacesOrderMerged{}}}
}

// mergedHarness is a harness whose sidebar answers every page with PAGE and
// ERR, its records in the returned logger.
func mergedHarness(t *testing.T, page sidebar.MergedPage, err error) (*harness, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	h.Sidebar.page = func(sidebar.MergedPageQuery) (sidebar.MergedPage, error) { return page, err }
	return h, log
}

func TestListMergedWorkspacesAnswersThePage(t *testing.T) {
	tests := []struct {
		name string
		page sidebar.MergedPage
		want *agentreplv1.ListMergedWorkspacesSuccess
	}{
		{
			name: "a page with more after it is the more arm",
			page: sidebar.MergedPage{Revision: 4, Rows: []*frontendv1.RosterMergedRow{mergedRow("a")}, More: true, Omitted: 3},
			want: &agentreplv1.ListMergedWorkspacesSuccess{Revision: &frontendv1.RosterMergedRevision{Revision: 4},
				Extent: &agentreplv1.ListMergedWorkspacesSuccess_More{More: &agentreplv1.ListMergedWorkspacesSuccessMore{
					Rows: &frontendv1.RosterMergedRows{Rows: []*frontendv1.RosterMergedRow{mergedRow("a")}}, Omitted: 3}}},
		},
		{
			name: "the last page is the last arm",
			page: sidebar.MergedPage{Revision: 4, Rows: []*frontendv1.RosterMergedRow{mergedRow("a")}},
			want: &agentreplv1.ListMergedWorkspacesSuccess{Revision: &frontendv1.RosterMergedRevision{Revision: 4},
				Extent: &agentreplv1.ListMergedWorkspacesSuccess_Last{Last: &agentreplv1.ListMergedWorkspacesSuccessLast{
					Rows: &frontendv1.RosterMergedRows{Rows: []*frontendv1.RosterMergedRow{mergedRow("a")}}}}},
		},
		{
			name: "a page past the end is a last page with no rows",
			page: sidebar.MergedPage{Revision: 4},
			want: &agentreplv1.ListMergedWorkspacesSuccess{Revision: &frontendv1.RosterMergedRevision{Revision: 4},
				Extent: &agentreplv1.ListMergedWorkspacesSuccess_Last{Last: &agentreplv1.ListMergedWorkspacesSuccessLast{
					Rows: &frontendv1.RosterMergedRows{}}}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h, _ := mergedHarness(t, tt.page, nil)

			// Act.
			resp, err := h.Client.ListMergedWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.ListMergedWorkspacesRequest{Order: byMerge(), Page: 2}))

			// Assert.
			if err != nil || !proto.Equal(resp.Msg.GetSuccess(), tt.want) {
				t.Fatalf("ListMergedWorkspaces = %v, %v; want %v", resp, err, tt.want)
			}
		})
	}
}

func TestListMergedWorkspacesAsksTheSidebarWhatTheRequestNames(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.ListMergedWorkspacesRequest
		want sidebar.MergedPageQuery
	}{
		{name: "every repository by merge", req: &agentreplv1.ListMergedWorkspacesRequest{Order: byMerge(), Page: 1},
			want: sidebar.MergedPageQuery{Order: sidebar.MergedByMerge, Page: 1}},
		{name: "one repository by creation", req: &agentreplv1.ListMergedWorkspacesRequest{
			Repository: &workspacev1.RepositoryRef{Id: "repo-1"},
			Order: &agentreplv1.ListMergedWorkspacesOrder{Order: &agentreplv1.ListMergedWorkspacesOrder_MostRecentlyCreated{
				MostRecentlyCreated: &agentreplv1.ListMergedWorkspacesOrderCreated{}}}},
			want: sidebar.MergedPageQuery{Repository: &workspacev1.RepositoryRef{Id: "repo-1"}, Order: sidebar.MergedByCreation}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h, _ := mergedHarness(t, sidebar.MergedPage{}, nil)

			// Act.
			if _, err := h.Client.ListMergedWorkspaces(context.Background(), connect.NewRequest(tt.req)); err != nil {
				t.Fatalf("ListMergedWorkspaces: %v", err)
			}

			// Assert.
			if len(h.Sidebar.pages) != 1 {
				t.Fatalf("pages asked = %d, want 1", len(h.Sidebar.pages))
			}
			got := h.Sidebar.pages[0]
			if got.Order != tt.want.Order || got.Page != tt.want.Page || !proto.Equal(got.Repository, tt.want.Repository) {
				t.Fatalf("query = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestListMergedWorkspacesRefusals(t *testing.T) {
	ref := &workspacev1.RepositoryRef{Id: "repo-missing"}
	tests := []struct {
		name    string
		err     error
		check   func(*agentreplv1.ListMergedWorkspacesError) bool
		message string
	}{
		{name: "no registry yet is not_ready", err: sidebar.ErrMergedNotReady,
			check:   func(e *agentreplv1.ListMergedWorkspacesError) bool { return e.GetNotReady() != nil },
			message: "answered not_ready: the roster has published no registry yet"},
		{name: "an unregistered repository is unknown_repository", err: &sidebar.UnknownRepositoryError{Ref: ref},
			check: func(e *agentreplv1.ListMergedWorkspacesError) bool {
				return proto.Equal(e.GetUnknownRepository().GetRepository(), ref)
			},
			message: "answered unknown_repository: the registry holds no repository the request names"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h, log := mergedHarness(t, sidebar.MergedPage{}, tt.err)

			// Act.
			resp, err := h.Client.ListMergedWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.ListMergedWorkspacesRequest{Repository: ref, Order: byMerge()}))

			// Assert: the typed answer, and its record with the request's context.
			if err != nil || !tt.check(resp.Msg.GetError()) {
				t.Fatalf("ListMergedWorkspaces = %v, %v; want the %s answer", resp, err, tt.name)
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: "ListMergedWorkspaces", Message: tt.message})
			if !ok || rec.Context["repository"] != "repo-missing" || rec.Context["order"] != "most_recently_merged" || rec.Context["cause"] != tt.err.Error() {
				t.Fatalf("record = %v (%v), want one naming the repository, the order and the cause", rec.Context, ok)
			}
		})
	}
}

func TestListMergedWorkspacesFailsAndRecordsAnUnexpectedError(t *testing.T) {
	// Arrange.
	h, log := mergedHarness(t, sidebar.MergedPage{}, errors.New("the resolver broke"))

	// Act.
	resp, err := h.Client.ListMergedWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.ListMergedWorkspacesRequest{Order: byMerge(), Page: 3}))

	// Assert.
	if connectCode(t, err) != connect.CodeInternal || resp != nil {
		t.Fatalf("ListMergedWorkspaces = %v, %v; want an internal failure", resp, err)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "ListMergedWorkspaces"})
	if !ok || rec.Context["cause"] != "resolve the merged page: the resolver broke" || rec.Context["page"] != uint32(3) {
		t.Fatalf("ERROR record = %v (%v), want the cause and the page", rec.Context, ok)
	}
}

func TestListMergedWorkspacesRefusesAMalformedRequest(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.ListMergedWorkspacesRequest
	}{
		{name: "no order", req: &agentreplv1.ListMergedWorkspacesRequest{}},
		{name: "an order that names no arm", req: &agentreplv1.ListMergedWorkspacesRequest{Order: &agentreplv1.ListMergedWorkspacesOrder{}}},
		{name: "a repository ref naming nothing", req: &agentreplv1.ListMergedWorkspacesRequest{Repository: &workspacev1.RepositoryRef{}, Order: byMerge()}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h, _ := mergedHarness(t, sidebar.MergedPage{}, nil)

			// Act.
			_, err := h.Client.ListMergedWorkspaces(context.Background(), connect.NewRequest(tt.req))

			// Assert: refused before the sidebar is asked.
			if connectCode(t, err) != connect.CodeInvalidArgument || len(h.Sidebar.pages) != 0 {
				t.Fatalf("err = %v, pages asked %d; want invalid_argument and none", err, len(h.Sidebar.pages))
			}
		})
	}
}

// ---- the endpoint's latency, measured over the real Connect transport ----

// infoSurfaces are test surfaces at the daemon's default threshold, info:
// their loggers drop every debug record, as a production daemon's do
// (dlog.WithoutDebug), and keep the rest.
type infoSurfaces struct{ *dlog.TestSurfaces }

func (s infoSurfaces) Global() dlog.Logger { return dlog.WithoutDebug(s.TestSurfaces.Global()) }
func (s infoSurfaces) WorkspaceOrCentral(dir string) dlog.Logger {
	return dlog.WithoutDebug(s.TestSurfaces.WorkspaceOrCentral(dir))
}

// liveShapeRegistry is the owner's live registry shape (2026-11-16): about
// 660 merged workspaces across 8 repositories and 500 open, backlogged or closed
// ones, every one at its own place, merged ones a minute apart, each with a
// commit count.
func liveShapeRegistry() sidebar.Registry {
	epoch := time.UnixMilli(1_700_000_000_000)
	var reg sidebar.Registry
	for i := range 8 {
		reg.Repositories = append(reg.Repositories, wsm.Repository{
			ID: wsm.RepoID(fmt.Sprintf("repo-%d", i)), Dir: fmt.Sprintf("/repos/%d", i),
			Name: fmt.Sprintf("repo-%d", i), DefaultBranch: "main", Place: int64(i + 1)})
	}
	place := int64(0)
	add := func(ws wsm.Workspace) {
		place++
		ws.Place = place
		reg.Workspaces = append(reg.Workspaces, ws)
	}
	base := func(kind string, i int) wsm.Workspace {
		repo := reg.Repositories[i%len(reg.Repositories)]
		id := fmt.Sprintf("%s-%04d", kind, i)
		commits := uint32(i%7 + 1)
		return wsm.Workspace{ID: wsm.WorkspaceID(id), Repo: repo.ID, Dir: repo.Dir + "/" + id, Name: id,
			Branch: "branch/" + id, ParentBranch: "main", CreatedAt: epoch.Add(time.Duration(i) * time.Second), CommitCount: &commits}
	}
	for i := range 660 {
		ws := base("merged", i)
		at := epoch.Add(-time.Duration(i) * time.Minute)
		ws.Closed, ws.MergedAt = true, &at
		add(ws)
	}
	for i := range 500 {
		ws := base("other", i)
		switch {
		case i%5 == 0:
			backlogged := epoch
			ws.Backlogged, ws.BackloggedAt = true, &backlogged
		case i%5 == 1 || i%5 == 2:
			ws.Closed = true
		}
		add(ws)
	}
	reg.View = wsm.DefaultSidebarView
	return reg
}

// mergedPageServer serves ListMergedWorkspaces from a real sidebar resolver
// holding the live-shaped registry, over httptest with h2c, and answers a
// real Connect client dialed to it.
func mergedPageServer(b *testing.B) agentreplv1connect.AgentReplClient {
	b.Helper()
	colors, err := vocab.LoadRenderColors("../../../proto/vocab")
	if err != nil {
		b.Fatalf("load render colors: %v", err)
	}
	surfaces := infoSurfaces{dlog.NewTestSurfaces()}
	foot, err := footer.New(colors, surfaces)
	if err != nil {
		b.Fatalf("footer.New: %v", err)
	}
	roster, err := sidebar.New(colors, surfaces, foot)
	if err != nil {
		b.Fatalf("sidebar.New: %v", err)
	}
	reg := liveShapeRegistry()
	dir := b.TempDir()
	for _, ws := range reg.Workspaces {
		if err := foot.SetWorkspaceDir(ws.ID, dir); err != nil {
			b.Fatalf("SetWorkspaceDir: %v", err)
		}
	}
	roster.SetRegistry(reg)
	s := &server{deps: Deps{Sidebar: roster}, log: surfaces.Global()}
	path, handler := agentreplv1connect.NewAgentReplHandler(mergedOnly{s: s})
	mux := http.NewServeMux()
	mux.Handle(path, handler)
	srv := httptest.NewServer(H2C(mux, surfaces.Global()))
	b.Cleanup(srv.Close)
	return agentreplv1connect.NewAgentReplClient(h2cClient(), srv.URL)
}

// mergedOnly is the AgentRepl service answering ListMergedWorkspaces alone,
// through the real handler.
type mergedOnly struct {
	agentreplv1connect.UnimplementedAgentReplHandler
	s *server
}

func (m mergedOnly) ListMergedWorkspaces(ctx context.Context, req *connect.Request[agentreplv1.ListMergedWorkspacesRequest]) (*connect.Response[agentreplv1.ListMergedWorkspacesResponse], error) {
	return m.s.ListMergedWorkspaces(ctx, req)
}

// BenchmarkListMergedWorkspaces measures one page's round trip over the
// real Connect transport against the live registry shape, per scope, order
// and page, reporting each call's p50, p90 and max. Run with -benchtime=200x
// for the owner's 200-call figure.
func BenchmarkListMergedWorkspaces(b *testing.B) {
	client := mergedPageServer(b)
	orders := map[string]*agentreplv1.ListMergedWorkspacesOrder{
		"merged":  byMerge(),
		"created": {Order: &agentreplv1.ListMergedWorkspacesOrder_MostRecentlyCreated{MostRecentlyCreated: &agentreplv1.ListMergedWorkspacesOrderCreated{}}},
	}
	scopes := map[string]*workspacev1.RepositoryRef{"every-repository": nil, "one-repository": {Id: "repo-3"}}
	for _, scope := range []string{"every-repository", "one-repository"} {
		for _, order := range []string{"merged", "created"} {
			for _, page := range []uint32{0, 5, 1000} {
				b.Run(fmt.Sprintf("%s/%s/page-%d", scope, order, page), func(b *testing.B) {
					req := &agentreplv1.ListMergedWorkspacesRequest{Repository: scopes[scope], Order: orders[order], Page: page}
					took := make([]time.Duration, 0, b.N)
					for b.Loop() {
						start := time.Now()
						resp, err := client.ListMergedWorkspaces(context.Background(), connect.NewRequest(req))
						took = append(took, time.Since(start))
						if err != nil || resp.Msg.GetSuccess() == nil {
							b.Fatalf("ListMergedWorkspaces = %v, %v", resp, err)
						}
					}
					slices.Sort(took)
					b.ReportMetric(float64(took[len(took)/2].Microseconds()), "p50-µs")
					b.ReportMetric(float64(took[len(took)*9/10].Microseconds()), "p90-µs")
					b.ReportMetric(float64(took[len(took)-1].Microseconds()), "max-µs")
				})
			}
		}
	}
}
