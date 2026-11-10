package prwatch

import (
	"context"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ghclient"
	"agent-repld/internal/merge"
	"agent-repld/internal/wsm"
)

// own anchors and records the rig's workspace's owned-link subscription to
// prK: the workspace owns the pull request.
func (r *rig) own(t *testing.T) {
	t.Helper()
	sub, _, err := r.w.Anchor(context.Background(), r.ws, prK, wsm.AllPullRequestEvents(), wsm.PullRequestOriginOwnedLink)
	if err != nil {
		t.Fatalf("Anchor: %v", err)
	}
	if _, err := r.db.SubscribePullRequest(context.Background(), sub); err != nil {
		t.Fatalf("SubscribePullRequest: %v", err)
	}
}

func TestAnOwnedPullRequestInAGithubMergeQueueIsToldAsADeployment(t *testing.T) {
	joined := remoteQueueEvent{ID: "a1", Actor: "dodge", At: t0}
	tests := []struct {
		name    string
		arrange func(t *testing.T, r *rig, g *gql)
		want    []merge.GithubDeployment
	}{
		{name: "an owned pull request at the second of three places is told there", arrange: func(t *testing.T, r *rig, g *gql) {
			r.own(t)
			queuedPR(g, "QUEUED", "", []remoteQueueEntry{entry(3, "QUEUED"), entry(prK.Number, "QUEUED"), entry(9, "QUEUED")}, joined)
		}, want: []merge.GithubDeployment{{Workspace: "", PullRequest: prK, Place: 2, Total: 3, QueuedAt: t0}}},
		{name: "a pull request only subscribed to is no deployment", arrange: func(t *testing.T, r *rig, g *gql) {
			r.subscribe(t, wsm.PullRequestEventMergeQueue)
			queuedPR(g, "QUEUED", "", []remoteQueueEntry{entry(prK.Number, "QUEUED")}, joined)
		}},
		{name: "an owned pull request in no queue is no deployment", arrange: func(t *testing.T, r *rig, g *gql) {
			r.own(t)
			g.setFacts(prK.Number, factsJSON("main", nil))
		}},
		{name: "an owned pull request whose read fails stays at its last place", arrange: func(t *testing.T, r *rig, g *gql) {
			r.own(t)
			queuedPR(g, "QUEUED", "", []remoteQueueEntry{entry(prK.Number, "QUEUED")}, joined)
			r.tick()
			g.fail["PullRequestQueues"] = &ghclient.Failure{Kind: ghclient.KindUnreachable, Method: "POST", Path: "graphql", Message: "down"}
		}, want: []merge.GithubDeployment{{PullRequest: prK, Place: 1, Total: 1, QueuedAt: t0}}},
		{name: "an owned pull request no longer owned is forgotten", arrange: func(t *testing.T, r *rig, g *gql) {
			r.own(t)
			queuedPR(g, "QUEUED", "", []remoteQueueEntry{entry(prK.Number, "QUEUED")}, joined)
			r.tick()
			if _, err := r.db.UnsubscribePullRequest(context.Background(), r.ws, prK); err != nil {
				t.Fatal(err)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			g := r.withGQL()
			g.setFacts(prK.Number, factsJSON("main", nil))
			tt.arrange(t, r, g)

			// Act
			r.tick()

			// Assert
			got := r.deploys.last(t)
			for i := range tt.want {
				tt.want[i].Workspace = r.ws
			}
			if len(got) != len(tt.want) {
				t.Fatalf("deployments = %+v, want %+v", got, tt.want)
			}
			for i := range got {
				if got[i] != tt.want[i] {
					t.Fatalf("deployments = %+v, want %+v", got, tt.want)
				}
			}
		})
	}
}

func TestAQueuedPullRequestWhoseTimelineNamesNoJoiningIsRecordedAndNotTold(t *testing.T) {
	// Arrange
	r := newRig(t)
	g := r.withGQL()
	g.setFacts(prK.Number, factsJSON("main", nil))
	r.own(t)
	queuedPR(g, "QUEUED", "", []remoteQueueEntry{entry(prK.Number, "QUEUED")})

	// Act
	r.tick()

	// Assert
	if got := r.deploys.last(t); len(got) != 0 {
		t.Fatalf("deployments = %+v, want none", got)
	}
	rec, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "error", Operation: opDeployments})
	if !ok || rec.Context["pull_request"] != prK.String() {
		t.Fatalf("records = %+v, want the missing joining recorded at error naming the pull request", r.log.Records())
	}
}

func TestADaemonThatDoesNotServeTellsNoDeployments(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.own(t)
	r.serves = false

	// Act
	r.tick()

	// Assert
	if len(r.deploys.told) != 0 {
		t.Fatalf("told = %+v, want nothing", r.deploys.told)
	}
}
