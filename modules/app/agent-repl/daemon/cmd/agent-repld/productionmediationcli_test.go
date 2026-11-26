package main

import (
	"context"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

func (c *cliClient) UpdateProductionMediation(_ context.Context, req *connect.Request[agentreplv1.UpdateProductionMediationRequest]) (*connect.Response[agentreplv1.UpdateProductionMediationResponse], error) {
	return connect.NewResponse(&agentreplv1.UpdateProductionMediationResponse{}), c.record(req.Msg)
}

// twoChildrenJSON is a dispatch of a named remediation and an unnamed
// investigation, as the production mediator writes it.
const twoChildrenJSON = `[
  {"name": "fix-tail", "kind": "remediation", "findings": [{"statement": "the tail lags", "sites": ["0123456789ab"], "evidence": ["tail lag 9s"]}]},
  {"kind": "investigation", "findings": [{"statement": "the footer flickers", "evidence": ["redraw x40"]}]}
]`

func TestProductionMediationAsksEachDecision(t *testing.T) {
	tests := []struct {
		name  string
		args  []string
		stdin string
		check func(*agentreplv1.UpdateProductionMediationRequest) bool
	}{
		{name: "nothing to do", args: []string{"decide", "-summary", "quiet"},
			check: func(r *agentreplv1.UpdateProductionMediationRequest) bool {
				return r.GetDecide().GetNothingToDo() != nil && r.GetDecide().GetSummary() == "quiet"
			}},
		{name: "a dispatch reads its children from standard input", args: []string{"dispatch", "-summary", "two problems"}, stdin: twoChildrenJSON,
			check: func(r *agentreplv1.UpdateProductionMediationRequest) bool {
				children := r.GetDecide().GetDispatch().GetChildren()
				return len(children) == 2 && children[0].GetName() == "fix-tail" && children[0].GetRemediation() != nil &&
					children[0].GetFindings()[0].GetSites()[0].GetHex() == "0123456789ab" &&
					children[1].Name == nil && children[1].GetInvestigation() != nil
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			f.stdin = tt.stdin

			// Act
			code := f.run(productionMediationNoun, tt.args...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			if got := only[*agentreplv1.UpdateProductionMediationRequest](t, f); !tt.check(got) || got.GetMediator() == nil {
				t.Fatalf("request = %v, want the decision asked as the calling workspace", got)
			}
		})
	}
}

func TestProductionMediationRefusesInputItCannotRead(t *testing.T) {
	tests := []struct {
		name  string
		args  []string
		stdin string
	}{
		{name: "a decision with no summary", args: []string{"decide"}},
		{name: "a dispatch with no children", args: []string{"dispatch", "-summary", "s"}, stdin: `[]`},
		{name: "a child of no known kind", args: []string{"dispatch", "-summary", "s"}, stdin: `[{"kind": "repair", "findings": []}]`},
		{name: "a child with a field the input does not know", args: []string{"dispatch", "-summary", "s"}, stdin: `[{"kind": "remediation", "owner": "me"}]`},
		{name: "a fold naming one side", args: []string{"fold", "-folded", "a"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			f.stdin = tt.stdin

			// Act
			code := f.run(productionMediationNoun, tt.args...)

			// Assert
			if code == exitSuccess || len(f.client.requests) != 0 {
				t.Fatalf("exit %d, requests %v; want a usage failure and nothing asked", code, f.client.requests)
			}
		})
	}
}

// productionRefusals is the production mediation's refusal, with a sentence
// for each of its arms.
var productionRefusals = func() []describedRefusal {
	child := &workspacev1.WorkspaceRef{Id: "c", Dir: "/w/c"}
	site := &agentreplv1.LogSiteKey{Hex: "0123456789ab"}
	e := &agentreplv1.UpdateProductionMediationError{}
	cases := []refusalCase{
		{name: "unknown_workspace", err: &agentreplv1.UpdateProductionMediationError{Cause: &agentreplv1.UpdateProductionMediationError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.UpdateProductionMediationUnknownWorkspace{}}},
			want: "the daemon serves no such workspace"},
		{name: "workspace_ref_mismatch", err: &agentreplv1.UpdateProductionMediationError{Cause: &agentreplv1.UpdateProductionMediationError_WorkspaceRefMismatch{WorkspaceRefMismatch: &agentreplv1.UpdateProductionMediationWorkspaceRefMismatch{RegistryDir: "/w/m"}}},
			want: "the workspace lives at /w/m, not where the request said"},
		{name: "transferring_away", err: &agentreplv1.UpdateProductionMediationError{Cause: &agentreplv1.UpdateProductionMediationError_TransferringAway{TransferringAway: &agentreplv1.UpdateProductionMediationTransferringAway{Address: "127.0.0.1:9"}}},
			want: "this daemon handed the workspace to 127.0.0.1:9; ask there"},
		{name: "not_production_mediator", err: &agentreplv1.UpdateProductionMediationError{Cause: &agentreplv1.UpdateProductionMediationError_NotProductionMediator{NotProductionMediator: &agentreplv1.UpdateProductionMediationNotProductionMediator{}}},
			want: "this workspace is no repository's production mediator"},
		{name: "no_check_owed", err: &agentreplv1.UpdateProductionMediationError{Cause: &agentreplv1.UpdateProductionMediationError_NoCheckOwed{NoCheckOwed: &agentreplv1.UpdateProductionMediationNoCheckOwed{}}},
			want: "no production span you were told is owed"},
		{name: "site_owned", err: &agentreplv1.UpdateProductionMediationError{Cause: &agentreplv1.UpdateProductionMediationError_SiteOwned{SiteOwned: &agentreplv1.UpdateProductionMediationSiteOwned{Site: site,
			Holder: &agentreplv1.UpdateProductionMediationSiteOwned_OpenChild{OpenChild: child}}}},
			want: "the log site 0123456789ab is owned by the open child at /w/c; leave it to that child, and dispatch nothing"},
		{name: "not_foldable", err: &agentreplv1.UpdateProductionMediationError{Cause: &agentreplv1.UpdateProductionMediationError_NotFoldable{NotFoldable: &agentreplv1.UpdateProductionMediationNotFoldable{Workspace: child, Detail: "closed"}}},
			want: "the workspace at /w/c cannot be folded: closed"},
		{name: "child_refused", err: &agentreplv1.UpdateProductionMediationError{Cause: &agentreplv1.UpdateProductionMediationError_ChildRefused{ChildRefused: &agentreplv1.UpdateProductionMediationChildRefused{Child: 1, Detail: "the name is taken"}}},
			want: "child 1 could not be made (the name is taken); no child of the dispatch was kept"},
		{name: "paused", err: &agentreplv1.UpdateProductionMediationError{Cause: &agentreplv1.UpdateProductionMediationError_Paused{Paused: &agentreplv1.UpdateProductionMediationPaused{}}},
			want: "the repository's production checks are paused: decide, dispatch and fold nothing, and do not work around the pause"},
	}
	pause := &agentreplv1.UpdateProductionPauseError{}
	return []describedRefusal{{name: "production", cases: cases, empty: e, oneof: "cause",
		describe: func(m proto.Message) (string, error) {
			return describeProductionRefusal(m.(*agentreplv1.UpdateProductionMediationError))
		}},
		{name: "production pause", empty: pause, oneof: "cause",
			cases: []refusalCase{
				{name: "already_paused", err: armed(pause, "already_paused", nil), want: "the repository's production checks are already paused"},
				{name: "not_paused", err: armed(pause, "not_paused", nil), want: "the repository's production checks are not paused"},
				{name: "unknown_repository", err: armed(pause, "unknown_repository", nil), want: "the daemon has no repository by the one -repository named"},
				{name: "checks_off", err: armed(pause, "checks_off", nil), want: "the repository's lifecycle config turns no production checks on, so there is nothing to pause or resume"},
			},
			describe: func(m proto.Message) (string, error) {
				return describeProductionPauseRefusal(m.(*agentreplv1.UpdateProductionPauseError))
			}}}
}()

func TestProductionMediationPausesOrResumesTheNamedRepository(t *testing.T) {
	tests := []struct {
		name   string
		verb   string
		args   []string
		code   int
		resume bool
	}{
		{name: "pause", verb: "pause", args: []string{"-repository", "here"}, code: exitSuccess},
		{name: "resume", verb: "resume", args: []string{"-repository", "here"}, code: exitSuccess, resume: true},
		{name: "no repository", verb: "pause", code: exitFailure},
		{name: "a positional", verb: "resume", args: []string{"-repository", "here", "now"}, code: exitFailure},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(productionMediationNoun, append([]string{tt.verb}, tt.args...)...)

			// Assert
			if code != tt.code {
				t.Fatalf("exit %d, stderr %q; want %d", code, f.errOut.String(), tt.code)
			}
			if tt.code != exitSuccess {
				return
			}
			req := only[*agentreplv1.UpdateProductionPauseRequest](t, f)
			if req.GetRepository().GetId() != "repo-1" || (req.GetResume() != nil) != tt.resume || (req.GetPause() != nil) == tt.resume {
				t.Fatalf("request = %v, want the caller's repository, resume %v", req, tt.resume)
			}
		})
	}
}

func TestDescribeProductionRefusals(t *testing.T) {
	testDescribedRefusals(t, productionRefusals)
}
