package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/resolve/sidebar"
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
