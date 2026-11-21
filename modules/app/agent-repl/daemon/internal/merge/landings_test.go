package merge

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// directHead is the main branch's head a direct landing reports.
const directHead = "d1ec7d1ec7d1ec7d1ec7d1ec7d1ec7d1ec7d1ec7"

// landingsOf reads every landing recorded on KEY.
func landingsOf(t *testing.T, h *harness, key wsm.RepoKey) []wsm.MergeLanding {
	t.Helper()
	landings, _, err := h.db.checks.MergeLandingsIn(context.Background(), key, time.Unix(0, 0), time.Now().Add(24*time.Hour))
	if err != nil {
		t.Fatalf("MergeLandingsIn: %v", err)
	}
	return landings
}

// landingRecord answers the INFO daemon.merge.record_landing record naming
// HEAD, if one was written.
func landingRecord(h *harness, head string) (dlog.Record, bool) {
	for _, r := range h.logs.Records() {
		if r.Level == "info" && r.Operation == opRecordLanding && r.Context["head"] == head {
			return r, true
		}
	}
	return dlog.Record{}, false
}

// landingSites is how each landing path is made, one per wsm.LandingPaths
// entry: TestEveryLandingPathWritesItsRecord asserts every path has one, so
// a new path cannot land without its record.
var landingSites = map[wsm.LandingPath]struct {
	// land makes a landing by this path and answers the repository and head
	// it landed.
	land func(t *testing.T) (*harness, wsm.RepoKey, string)
	// workspace is the workspace the record names.
	workspace ids.WorkspaceID
}{
	wsm.LandingThroughQueue: {workspace: theWorkspace, land: func(t *testing.T) (*harness, wsm.RepoKey, string) {
		h := newHarness(t)
		h.emacsRepo()
		h.landsCleanly("abc123def4567")
		h.gatePasses("daemon")
		enqueue(t, h)
		if err := h.admit(context.Background()); err != nil {
			t.Fatalf("the merge failed: %v", err)
		}
		return h, h.repoKey(), "abc123def4567"
	}},
	wsm.LandingDeployMediator: {workspace: "ws-fixer", land: func(t *testing.T) (*harness, wsm.RepoKey, string) {
		h := finishing(t)
		if err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch); err != nil {
			t.Fatalf("FinishRemediation: %v", err)
		}
		return h, mediationKey(h), tipAfter
	}},
	wsm.LandingQueueBypass: {workspace: theWorkspace, land: func(t *testing.T) (*harness, wsm.RepoKey, string) {
		h := directLanding(t)
		h.db.mu.Lock()
		h.db.bypassHolders = map[ids.WorkspaceID]ids.RepoID{theWorkspace: theRepository}
		h.db.mu.Unlock()
		reportDirect(t, h, theWorkspace, "feature")
		return h, scopedKey(h), directHead
	}},
	wsm.LandingDirect: {workspace: theWorkspace, land: func(t *testing.T) (*harness, wsm.RepoKey, string) {
		h := directLanding(t)
		reportDirect(t, h, theWorkspace, "feature")
		return h, scopedKey(h), directHead
	}},
}

// directLanding is a harness whose repository a direct landing names.
func directLanding(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.register(theWorkspace, "the-workspace")
	return h
}

// reportDirect reports land-direct.sh's landing of BRANCH by CALLER.
func reportDirect(t *testing.T, h *harness, caller ids.WorkspaceID, branch string) wsm.LandingPath {
	t.Helper()
	path, err := h.o.RecordDirectLanding(context.Background(), DirectLanding{
		Repository: RepositoryScope{ID: theRepository}, Branch: branch, Head: directHead, Caller: caller})
	if err != nil {
		t.Fatalf("RecordDirectLanding: %v", err)
	}
	return path
}

func TestEveryLandingPathWritesItsRecord(t *testing.T) {
	for _, path := range wsm.LandingPaths {
		t.Run(string(path), func(t *testing.T) {
			// Arrange
			site, ok := landingSites[path]
			if !ok {
				t.Fatalf("landing path %q has no landing site in landingSites; every path must write its record", path)
			}

			// Act
			h, key, head := site.land(t)

			// Assert
			landings := landingsOf(t, h, key)
			if len(landings) != 1 || landings[0].Path != path || landings[0].Head != head || landings[0].Workspace != site.workspace {
				t.Fatalf("landings %+v; want one of %s by path %q naming %s", landings, head, path, site.workspace)
			}
			if r, ok := landingRecord(h, head); !ok || r.Context["path"] != string(path) {
				t.Fatalf("no INFO %s naming path %q in %+v", opRecordLanding, path, h.logs.Records())
			}
		})
	}
}

func TestADirectLandingIsClassifiedByTheDaemonsState(t *testing.T) {
	tests := []struct {
		name          string
		arrange       func(h *harness)
		caller        ids.WorkspaceID
		branch        string
		wantPath      wsm.LandingPath
		wantWorkspace ids.WorkspaceID
	}{
		{name: "the mediator's branch is the mediator's landing whoever ran it", caller: "ws-main", branch: "feature",
			arrange: func(h *harness) {
				h.register("ws-fixer", mediatorName)
				withMediator(t, h)
				h.db.mu.Lock()
				record := h.db.workspaces["ws-fixer"]
				record.Branch = "feature"
				h.db.workspaces["ws-fixer"] = record
				h.db.mu.Unlock()
				h.register("ws-main", "main")
			},
			wantPath: wsm.LandingDeployMediator, wantWorkspace: "ws-fixer"},
		{name: "a holder of another repository's bypass lands directly", caller: theWorkspace, branch: "feature",
			arrange: func(h *harness) {
				h.db.bypassHolders = map[ids.WorkspaceID]ids.RepoID{theWorkspace: "repo-other"}
			},
			wantPath: wsm.LandingDirect, wantWorkspace: theWorkspace},
		{name: "a landing no workspace ran is direct and names none", branch: "feature",
			arrange:  func(h *harness) {},
			wantPath: wsm.LandingDirect},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := directLanding(t)
			tt.arrange(h)

			// Act
			path := reportDirect(t, h, tt.caller, tt.branch)

			// Assert
			landings := landingsOf(t, h, scopedKey(h))
			if path != tt.wantPath || len(landings) != 1 || landings[0].Path != tt.wantPath || landings[0].Workspace != tt.wantWorkspace {
				t.Fatalf("answered %q, recorded %+v; want path %q naming %q", path, landings, tt.wantPath, tt.wantWorkspace)
			}
		})
	}
}

func TestAFailedLandingRecordIsAnsweredAndLogged(t *testing.T) {
	// Arrange
	h := directLanding(t)
	h.db.landingErr = errors.New("disk full")

	// Act
	_, err := h.o.RecordDirectLanding(context.Background(), DirectLanding{
		Repository: RepositoryScope{ID: theRepository}, Branch: "feature", Head: directHead, Caller: theWorkspace})

	// Assert
	if err == nil {
		t.Fatal("RecordDirectLanding succeeded with its record failing")
	}
	found := false
	for _, r := range h.logs.Records() {
		found = found || (r.Level == "error" && r.Operation == opRecordLanding && r.Context["head"] == directHead && r.Context["path"] == string(wsm.LandingDirect))
	}
	if !found {
		t.Fatalf("no ERROR %s naming the landing in %+v", opRecordLanding, h.logs.Records())
	}
	if landings := landingsOf(t, h, scopedKey(h)); len(landings) != 0 {
		t.Fatalf("landings %+v after a failed record, want none", landings)
	}
}

func TestADirectLandingOfAnUnknownRepositoryIsRefused(t *testing.T) {
	// Arrange
	h := directLanding(t)

	// Act
	_, err := h.o.RecordDirectLanding(context.Background(), DirectLanding{
		Repository: RepositoryScope{ID: "repo-unknown"}, Branch: "feature", Head: directHead})

	// Assert
	if refusal, ok := Refused(err); !ok || refusal.Arm != ArmUnknownRepository {
		t.Fatalf("RecordDirectLanding = %v, want refused %s", err, ArmUnknownRepository)
	}
}

func TestLandingsSplitsTheQueuesFromEveryOther(t *testing.T) {
	// Arrange
	h := directLanding(t)
	key := scopedKey(h)
	at := func(s int64) time.Time { return time.Unix(s, 0).UTC() }
	for _, l := range []wsm.MergeLanding{
		{Repo: key, Head: "q1", Path: wsm.LandingThroughQueue, Branch: "a", LandedAt: at(100)},
		{Repo: key, Head: "b1", Path: wsm.LandingQueueBypass, Branch: "b", LandedAt: at(110)},
		{Repo: key, Head: "q2", Path: wsm.LandingThroughQueue, Branch: "c", LandedAt: at(120)},
		{Repo: key, Head: "m1", Path: wsm.LandingDeployMediator, Branch: "d", LandedAt: at(130)},
		{Repo: key, Head: "q3", Path: wsm.LandingThroughQueue, Branch: "e", LandedAt: at(200)},
	} {
		if _, err := h.db.checks.RecordMergeLanding(context.Background(), l); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	window, err := h.o.Landings(context.Background(), RepositoryScope{ID: theRepository}, at(100), at(130))

	// Assert
	heads := func(ls []wsm.MergeLanding) []string {
		var out []string
		for _, l := range ls {
			out = append(out, l.Head)
		}
		return out
	}
	if err != nil || !slices.Equal(heads(window.ThroughQueue), []string{"q1", "q2"}) || !slices.Equal(heads(window.Otherwise), []string{"b1", "m1"}) {
		t.Fatalf("Landings = queue %v, otherwise %v (%v); want [q1 q2] and [b1 m1]", heads(window.ThroughQueue), heads(window.Otherwise), err)
	}
	if window.RecordingBegan.IsZero() || window.Repository.ID != theRepository {
		t.Fatalf("window %+v; want it to name its repository and when recording began", window)
	}
}
