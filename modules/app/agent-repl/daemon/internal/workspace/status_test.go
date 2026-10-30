package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func footerIdle() *frontendv1.FooterStatus {
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Idle{Idle: &frontendv1.FooterStatusIdle{}}}
}

func TestStatusAnswersTheWorkspaceAndWhereItLands(t *testing.T) {
	owner := ids.WorkspaceID("ws-owner")
	maker := ids.WorkspaceID("ws-maker")
	primary := &agentreplv1.WorkspaceStatusMergeTarget{Target: &agentreplv1.WorkspaceStatusMergeTarget_Primary{Primary: &agentreplv1.WorkspaceStatusMergeTargetPrimary{}}}
	into := func(id ids.WorkspaceID, dir string) *agentreplv1.WorkspaceStatusMergeTarget {
		return &agentreplv1.WorkspaceStatusMergeTarget{Target: &agentreplv1.WorkspaceStatusMergeTarget_Workspace{Workspace: &agentreplv1.WorkspaceStatusMergeTargetWorkspace{
			Workspace: &workspacev1.WorkspaceRef{Id: string(id), Dir: dir}}}}
	}
	tests := []struct {
		name   string
		record wsm.Workspace
		want   *agentreplv1.WorkspaceStatusMergeTarget
	}{
		{name: "a workspace landing on the primary branch",
			record: wsm.Workspace{ID: "ws-1", Name: "one", Branch: "agent/one", Dir: "/wt/one"},
			want:   primary},
		{name: "a child landing into its parent",
			record: wsm.Workspace{ID: "ws-1", Name: "one", Branch: "agent/one", Dir: "/wt/one", Parent: &owner, LandsInto: &owner, OwnerLands: true},
			want:   into(owner, "/wt/owner")},
		{name: "a child landing on the primary branch",
			record: wsm.Workspace{ID: "ws-1", Name: "one", Branch: "agent/one", Dir: "/wt/one", Parent: &owner},
			want:   primary},
		{name: "a merge workspace landing in its maker",
			record: wsm.Workspace{ID: "ws-1", Name: "one", Branch: "agent/one", Dir: "/wt/one", Parent: &owner, LandsInto: &maker, MergeOf: &wsm.MergeOrigin{Requester: owner}},
			want:   into(maker, "/wt/maker")},
		{name: "a merge workspace landing on the primary branch",
			record: wsm.Workspace{ID: "ws-1", Name: "one", Branch: "agent/one", Dir: "/wt/one", Parent: &owner, MergeOf: &wsm.MergeOrigin{Requester: owner}},
			want:   primary},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.db.workspaces[tt.record.ID] = tt.record
			f.db.workspaces[owner] = wsm.Workspace{ID: owner, Dir: "/wt/owner"}
			f.db.workspaces[maker] = wsm.Workspace{ID: maker, Dir: "/wt/maker"}
			f.db.jobs[tt.record.ID] = wsm.CreationJob{Workspace: tt.record.ID}
			f.footer.statuses = map[ids.WorkspaceID]*frontendv1.FooterStatus{tt.record.ID: footerIdle()}

			// Act
			got, err := f.verbs.Status(context.Background(), tt.record.ID)

			// Assert
			if err != nil {
				t.Fatalf("Status: %v", err)
			}
			if got.GetName() != "one" || got.GetBranch() != "agent/one" || got.GetWorkspace().GetDir() != "/wt/one" {
				t.Errorf("Status = %+v, want the workspace's own facts", got)
			}
			if !proto.Equal(got.GetMergeTarget(), tt.want) {
				t.Errorf("merge target = %v, want %v", got.GetMergeTarget(), tt.want)
			}
		})
	}
}

func TestStatusAnswersTheRelationship(t *testing.T) {
	parent := ids.WorkspaceID("ws-parent")
	creator := ids.WorkspaceID("ws-creator")
	parentRef := &workspacev1.WorkspaceRef{Id: string(parent), Dir: "/wt/parent"}
	creatorRef := &workspacev1.WorkspaceRef{Id: string(creator), Dir: "/wt/creator"}
	tests := []struct {
		name        string
		record      wsm.Workspace
		wantKind    *agentreplv1.WorkspaceStatusRelationship
		wantOwner   *agentreplv1.WorkspaceStatusOwner
		wantCreator *workspacev1.WorkspaceRef
	}{
		{name: "a peer the user asked for",
			record:   wsm.Workspace{ID: "ws-1"},
			wantKind: &agentreplv1.WorkspaceStatusRelationship{Kind: &agentreplv1.WorkspaceStatusRelationship_Peer{Peer: &agentreplv1.WorkspaceStatusRelationshipPeer{}}}},
		{name: "a peer another workspace asked for",
			record:      wsm.Workspace{ID: "ws-1", RequestedBy: &creator},
			wantKind:    &agentreplv1.WorkspaceStatusRelationship{Kind: &agentreplv1.WorkspaceStatusRelationship_Peer{Peer: &agentreplv1.WorkspaceStatusRelationshipPeer{}}},
			wantCreator: creatorRef},
		{name: "a child its parent lands",
			record:      wsm.Workspace{ID: "ws-1", Parent: &parent, OwnerLands: true, RequestedBy: &creator},
			wantKind:    &agentreplv1.WorkspaceStatusRelationship{Kind: &agentreplv1.WorkspaceStatusRelationship_Child{Child: &agentreplv1.WorkspaceStatusRelationshipChild{Parent: parentRef}}},
			wantOwner:   &agentreplv1.WorkspaceStatusOwner{Workspace: parentRef, OwnerLands: true},
			wantCreator: creatorRef},
		{name: "a child that lands itself",
			record:    wsm.Workspace{ID: "ws-1", Parent: &parent},
			wantKind:  &agentreplv1.WorkspaceStatusRelationship{Kind: &agentreplv1.WorkspaceStatusRelationship_Child{Child: &agentreplv1.WorkspaceStatusRelationshipChild{Parent: parentRef}}},
			wantOwner: &agentreplv1.WorkspaceStatusOwner{Workspace: parentRef}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.db.workspaces[tt.record.ID] = tt.record
			f.db.workspaces[parent] = wsm.Workspace{ID: parent, Dir: "/wt/parent"}
			f.db.workspaces[creator] = wsm.Workspace{ID: creator, Dir: "/wt/creator"}
			f.footer.statuses = map[ids.WorkspaceID]*frontendv1.FooterStatus{tt.record.ID: footerIdle()}

			// Act
			got, err := f.verbs.Status(context.Background(), tt.record.ID)

			// Assert
			if err != nil {
				t.Fatalf("Status: %v", err)
			}
			if !proto.Equal(got.GetRelationship(), tt.wantKind) {
				t.Errorf("relationship = %v, want %v", got.GetRelationship(), tt.wantKind)
			}
			if !proto.Equal(got.GetOwner(), tt.wantOwner) {
				t.Errorf("owner = %v, want %v", got.GetOwner(), tt.wantOwner)
			}
			if !proto.Equal(got.GetCreatedBy(), tt.wantCreator) {
				t.Errorf("created by = %v, want %v", got.GetCreatedBy(), tt.wantCreator)
			}
		})
	}
}

func TestStatusListsTheOpenChildrenInPlaceOrder(t *testing.T) {
	// Arrange
	f := newFixture(t)
	parent := ids.WorkspaceID("ws-parent")
	f.db.workspaces[parent] = wsm.Workspace{ID: parent, Dir: "/wt/parent"}
	f.db.workspaces["ws-b"] = wsm.Workspace{ID: "ws-b", Dir: "/wt/b", Parent: &parent, Place: 2}
	f.db.workspaces["ws-a"] = wsm.Workspace{ID: "ws-a", Dir: "/wt/a", Parent: &parent, Place: 1, Hidden: true}
	f.db.workspaces["ws-closed"] = wsm.Workspace{ID: "ws-closed", Dir: "/wt/closed", Parent: &parent, Closed: true}
	f.footer.statuses = map[ids.WorkspaceID]*frontendv1.FooterStatus{parent: footerIdle()}

	// Act
	got, err := f.verbs.Status(context.Background(), parent)

	// Assert
	if err != nil {
		t.Fatalf("Status: %v", err)
	}
	want := []*workspacev1.WorkspaceRef{{Id: "ws-a", Dir: "/wt/a"}, {Id: "ws-b", Dir: "/wt/b"}}
	if len(got.GetChildren()) != len(want) {
		t.Fatalf("children = %v, want %v", got.GetChildren(), want)
	}
	for i := range want {
		if !proto.Equal(got.GetChildren()[i], want[i]) {
			t.Errorf("children[%d] = %v, want %v", i, got.GetChildren()[i], want[i])
		}
	}
}

func TestStatusFailsWhenItsChildrenCannotBeRead(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.workspaces["ws-1"] = wsm.Workspace{ID: "ws-1"}
	f.db.openChildrenErr = errors.New("database is locked")
	f.footer.statuses = map[ids.WorkspaceID]*frontendv1.FooterStatus{"ws-1": footerIdle()}

	// Act
	_, err := f.verbs.Status(context.Background(), "ws-1")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "database is locked") {
		t.Fatalf("Status err = %v, want the failed children read", err)
	}
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opStatus, Message: "could not read the open children of the workspace whose status was asked"})
	if !ok || record.Context["workspace"] != "ws-1" {
		t.Errorf("records = %+v, want the failed read recorded at error with the workspace", f.log.logger.Records())
	}
}

func TestStatusAnswersAClosedOrHiddenWorkspace(t *testing.T) {
	tests := []struct {
		name       string
		record     wsm.Workspace
		wantClosed bool
		wantHidden bool
	}{
		{name: "a closed workspace", record: wsm.Workspace{ID: "ws-1", Closed: true}, wantClosed: true},
		{name: "a hidden workspace", record: wsm.Workspace{ID: "ws-1", Hidden: true}, wantHidden: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.db.workspaces[tt.record.ID] = tt.record
			f.footer.statuses = map[ids.WorkspaceID]*frontendv1.FooterStatus{tt.record.ID: footerIdle()}

			// Act
			got, err := f.verbs.Status(context.Background(), tt.record.ID)

			// Assert
			if err != nil {
				t.Fatalf("Status: %v", err)
			}
			if (got.GetStanding().GetClosed() != nil) != tt.wantClosed || got.GetHidden() != tt.wantHidden {
				t.Errorf("Status = %+v, want closed %v hidden %v", got, tt.wantClosed, tt.wantHidden)
			}
		})
	}
}

func TestStatusNamesAStandingColdGateByItsRow(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.workspaces["ws-1"] = wsm.Workspace{ID: "ws-1"}
	f.cards.coldGate = &ServedColdGate{VendorSessionID: "vendor-1"}
	f.footer.statuses = map[ids.WorkspaceID]*frontendv1.FooterStatus{"ws-1": footerIdle()}

	// Act
	got, err := f.verbs.Status(context.Background(), "ws-1")

	// Assert
	if err != nil {
		t.Fatalf("Status: %v", err)
	}
	gate := got.GetStanding().GetOpen().GetActivity().GetColdGate().GetGate()
	if !proto.Equal(gate, coldGateRowID("ws-1", "vendor-1")) {
		t.Errorf("cold gate = %v, want the gate row", gate)
	}
}

func TestActivityOfProjectsEveryFooterArm(t *testing.T) {
	tests := []struct {
		name   string
		status *frontendv1.FooterStatus
		want   string
	}{
		{name: "idle", status: footerIdle(), want: "idle"},
		{name: "interrupted", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Interrupted{Interrupted: &frontendv1.FooterStatusInterrupted{}}}, want: "idle"},
		{name: "background work only", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Background{Background: &frontendv1.FooterStatusBackground{}}}, want: "idle"},
		{name: "a merge that failed", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_MergeFailed{MergeFailed: &frontendv1.FooterStatusMergeFailed{}}}, want: "idle"},
		{name: "a turn working", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Working{Working: &frontendv1.FooterStatusWorking{}}}, want: "turn_running"},
		{name: "a turn being interrupted", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Waiting{Waiting: &frontendv1.FooterStatusWaiting{
			Substatus: &frontendv1.FooterStatusWaiting_Interrupting{Interrupting: &frontendv1.FooterSubStatusWaitingInterrupting{}}}}}, want: "turn_running"},
		{name: "a wakeup counting down", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Waiting{Waiting: &frontendv1.FooterStatusWaiting{
			Substatus: &frontendv1.FooterStatusWaiting_Wakeup{Wakeup: &frontendv1.FooterSubStatusWaitingWakeup{}}}}}, want: "idle"},
		{name: "a spent cold gate's answer", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Waiting{Waiting: &frontendv1.FooterStatusWaiting{
			Substatus: &frontendv1.FooterStatusWaiting_ColdGate{ColdGate: &frontendv1.FooterSubStatusWaitingColdGate{}}}}}, want: "starting"},
		{name: "a permission", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Permission{Permission: &frontendv1.FooterStatusPermission{}}}, want: "needs_input"},
		{name: "a question", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Question{Question: &frontendv1.FooterStatusQuestion{}}}, want: "needs_input"},
		{name: "merging", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Merging{Merging: &frontendv1.FooterStatusMerging{}}}, want: "merging"},
		{name: "starting", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Starting{Starting: &frontendv1.FooterStatusStarting{}}}, want: "starting"},
		{name: "loading", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Loading{Loading: &frontendv1.FooterStatusLoading{}}}, want: "starting"},
		{name: "a vendor fault", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_VendorFault{VendorFault: &frontendv1.FooterStatusVendorFault{}}}, want: "fault"},
		{name: "a degraded link", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Degraded{Degraded: &frontendv1.FooterStatusDegraded{}}}, want: "fault"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := ActivityOf(tt.status)

			// Assert
			if !ok {
				t.Fatal("ActivityOf found no activity")
			}
			field := got.ProtoReflect().WhichOneof(got.ProtoReflect().Descriptor().Oneofs().ByName("activity"))
			if string(field.Name()) != tt.want {
				t.Errorf("activity = %s, want %s", field.Name(), tt.want)
			}
		})
	}
}

func TestStatusFails(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		arm     string
		message string
	}{
		{name: "an unregistered workspace", arrange: func(*fixture) {}, arm: ArmUnknownWorkspace},
		{name: "an unreadable registry", arrange: func(f *fixture) { f.db.workspaceErr = errors.New("db gone") },
			message: "could not read the workspace whose status was asked"},
		{name: "a footer with no status", arrange: func(f *fixture) { f.db.workspaces["ws-1"] = wsm.Workspace{ID: "ws-1"} },
			message: "a workspace's footer status has no activity"},
		{name: "a merge workspace with no creation job", arrange: func(f *fixture) {
			f.db.workspaces["ws-1"] = wsm.Workspace{ID: "ws-1", MergeOf: &wsm.MergeOrigin{Requester: "ws-0"}}
		}, message: "a merge workspace has no creation job to say where it lands"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			tt.arrange(f)

			// Act
			_, err := f.verbs.Status(context.Background(), "ws-1")

			// Assert
			if tt.arm != "" {
				if refusal, ok := AsRefusal(err); !ok || refusal.Arm != tt.arm {
					t.Fatalf("Status = %v, want the %s refusal", err, tt.arm)
				}
				return
			}
			if _, refused := AsRefusal(err); err == nil || refused {
				t.Fatalf("Status = %v, want a hard failure", err)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opStatus, Message: tt.message}); !ok {
				t.Errorf("no error record %q: %+v", tt.message, f.log.logger.Records())
			}
		})
	}
}

func TestStatusReportsTheWorkspacesStandings(t *testing.T) {
	// Arrange
	f := newFixture(t)
	awaited := ids.WorkspaceID("ws-planner")
	f.db.workspaces["ws-1"] = wsm.Workspace{ID: "ws-1", Repo: "repo-1", OneShot: true, Awaiting: &awaited}
	f.db.workspaces[awaited] = wsm.Workspace{ID: awaited, Dir: "/wt/planner"}
	f.footer.statuses = map[ids.WorkspaceID]*frontendv1.FooterStatus{"ws-1": footerIdle()}

	// Act
	got, err := f.verbs.Status(context.Background(), "ws-1")

	// Assert
	if err != nil {
		t.Fatalf("Status: %v", err)
	}
	if !got.GetOneShot() || got.GetAwaiting().GetId() != string(awaited) || got.GetAwaiting().GetDir() != "/wt/planner" {
		t.Errorf("one-shot %v awaiting %v, want a one-shot awaiting %s", got.GetOneShot(), got.GetAwaiting(), awaited)
	}
	if got.GetRepository().GetId() != "repo-1" {
		t.Errorf("repository = %v, want repo-1", got.GetRepository())
	}
}

func TestStatusFailsLoudlyWhenAWorkspaceItNamesCannotBeRead(t *testing.T) {
	tests := []struct {
		name    string
		record  wsm.Workspace
		message string
	}{
		{name: "its owner", record: wsm.Workspace{ID: "ws-1", Parent: ptrTo(ids.WorkspaceID("ws-gone"))},
			message: "could not read its owner to name it in a workspace's status"},
		{name: "the workspace it awaits", record: wsm.Workspace{ID: "ws-1", Awaiting: ptrTo(ids.WorkspaceID("ws-gone"))},
			message: "could not read the workspace it awaits to name it in a workspace's status"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.db.workspaces[tt.record.ID] = tt.record
			f.footer.statuses = map[ids.WorkspaceID]*frontendv1.FooterStatus{"ws-1": footerIdle()}

			// Act
			_, err := f.verbs.Status(context.Background(), "ws-1")

			// Assert
			if err == nil {
				t.Fatal("Status answered a workspace whose named workspace cannot be read")
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opStatus, Message: tt.message}); !ok {
				t.Errorf("no error record %q: %+v", tt.message, f.log.logger.Records())
			}
		})
	}
}

func ptrTo[T any](v T) *T { return &v }

func TestStatusAnswersTheLinks(t *testing.T) {
	doc := wsm.WorkspaceLink{URL: "https://example.com/ticket", Label: "The tracking ticket", Shown: true}
	tests := []struct {
		name  string
		links []wsm.WorkspaceLink
		want  *agentreplv1.WorkspaceLinks
	}{
		{name: "stored links are answered with their placement", links: []wsm.WorkspaceLink{doc},
			want: LinksView([]wsm.WorkspaceLink{doc})},
		{name: "no links answer an empty list with room", links: nil, want: LinksView(nil)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.db.workspaces["ws-1"] = wsm.Workspace{ID: "ws-1", Repo: "repo-1"}
			f.db.links = map[ids.WorkspaceID][]wsm.WorkspaceLink{"ws-1": tt.links}
			f.footer.statuses = map[ids.WorkspaceID]*frontendv1.FooterStatus{"ws-1": footerIdle()}

			// Act.
			got, err := f.verbs.Status(context.Background(), "ws-1")

			// Assert.
			if err != nil {
				t.Fatalf("Status: %v", err)
			}
			if !proto.Equal(got.GetLinks(), tt.want) {
				t.Errorf("links = %v, want %v", got.GetLinks(), tt.want)
			}
		})
	}
}

func TestStatusRefusesWhenTheLinksCannotBeRead(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.workspaces["ws-1"] = wsm.Workspace{ID: "ws-1", Repo: "repo-1"}
	f.db.linksReadErr = errors.New("disk gone")

	// Act.
	got, err := f.verbs.Status(context.Background(), "ws-1")

	// Assert.
	if err == nil || got != nil || !strings.Contains(err.Error(), "disk gone") {
		t.Fatalf("Status = %v, %v; want the links read failure and no status", got, err)
	}
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opStatus})
	if !ok || r.Context["workspace"] != "ws-1" {
		t.Errorf("record = %+v, want an ERROR naming the workspace", r)
	}
}
