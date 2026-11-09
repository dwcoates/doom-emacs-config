package merge

import (
	"context"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

var (
	ghQueuedAt = time.Unix(1700000500, 0).UTC()
	ghPR       = wsm.PullRequestKey{Owner: "o", Repo: "r", Number: 7}
)

// ghDeploy is THEWORKSPACE's pull request NUMBER at PLACE of TOTAL.
func ghDeploy(ws ids.WorkspaceID, number uint32, place, total int) GithubDeployment {
	return GithubDeployment{Workspace: ws, PullRequest: wsm.PullRequestKey{Owner: "o", Repo: "r", Number: number}, Place: place, Total: total, QueuedAt: ghQueuedAt}
}

func TestAWorkspaceStandingInAGithubMergeQueueIsDrawnDeploying(t *testing.T) {
	deploying := MergeFacts{State: StateMerging, Step: footer.StepGithubMergeQueue, QueuedAt: ghQueuedAt, GithubPlace: 3, GithubTotal: 5}
	tests := []struct {
		name string
		// native is the merge the orchestrator runs for the workspace, nil
		// for none.
		native  *MergeFacts
		sets    [][]GithubDeployment
		want    MergeFacts
		wantGH  []sidebar.GithubQueueEntry
		wantTot int
	}{
		{name: "a deployment draws its place", sets: [][]GithubDeployment{{ghDeploy(theWorkspace, 7, 3, 5)}},
			want: deploying, wantGH: []sidebar.GithubQueueEntry{{Workspace: theWorkspace, Place: 3, Total: 5, QueuedAt: ghQueuedAt}}, wantTot: 1},
		{name: "a workspace owning two queued pull requests stands where it lands first",
			sets: [][]GithubDeployment{{ghDeploy(theWorkspace, 8, 4, 5), ghDeploy(theWorkspace, 7, 3, 5)}},
			want: deploying, wantGH: []sidebar.GithubQueueEntry{{Workspace: theWorkspace, Place: 3, Total: 5, QueuedAt: ghQueuedAt}}, wantTot: 1},
		{name: "a deployment that left the queue draws no merge",
			sets: [][]GithubDeployment{{ghDeploy(theWorkspace, 7, 3, 5)}, nil},
			want: MergeFacts{State: "none"}, wantGH: []sidebar.GithubQueueEntry{}, wantTot: 2},
		{name: "an unchanged deployment is not restated",
			sets: [][]GithubDeployment{{ghDeploy(theWorkspace, 7, 3, 5)}, {ghDeploy(theWorkspace, 7, 3, 5)}},
			want: deploying, wantGH: []sidebar.GithubQueueEntry{{Workspace: theWorkspace, Place: 3, Total: 5, QueuedAt: ghQueuedAt}}, wantTot: 1},
		{name: "a merge the orchestrator runs wins over the deployment",
			native: &MergeFacts{State: StateMerging, Step: footer.StepTesting, QueuedAt: ghQueuedAt},
			sets:   [][]GithubDeployment{{ghDeploy(theWorkspace, 7, 3, 5)}},
			want:   MergeFacts{State: StateMerging, Step: footer.StepTesting, QueuedAt: ghQueuedAt},
			wantGH: []sidebar.GithubQueueEntry{{Workspace: theWorkspace, Place: 3, Total: 5, QueuedAt: ghQueuedAt}}, wantTot: 1},
		{name: "a deployment wins over a merge that already failed",
			native: &MergeFacts{State: StateFailed, FailedArea: footer.FailedTests},
			sets:   [][]GithubDeployment{{ghDeploy(theWorkspace, 7, 3, 5)}},
			want:   deploying, wantGH: []sidebar.GithubQueueEntry{{Workspace: theWorkspace, Place: 3, Total: 5, QueuedAt: ghQueuedAt}}, wantTot: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if tt.native != nil {
				h.o.mu.Lock()
				h.o.facts[theWorkspace] = *tt.native
				h.o.mu.Unlock()
			}

			// Act
			for _, set := range tt.sets {
				h.o.SetGithubDeployments(context.Background(), set)
			}

			// Assert
			if got := h.footer.last(); got.State != tt.want.State || got.Step != tt.want.Step || got.GithubPlace != tt.want.GithubPlace ||
				got.GithubTotal != tt.want.GithubTotal || !got.QueuedAt.Equal(tt.want.QueuedAt) || got.FailedArea != tt.want.FailedArea {
				t.Fatalf("footer facts = %+v, want %+v", got, tt.want)
			}
			told := h.sidebar.githubTold()
			if len(told) != tt.wantTot {
				t.Fatalf("GitHub shares told = %d (%+v), want %d", len(told), told, tt.wantTot)
			}
			last := told[len(told)-1]
			if len(last) != len(tt.wantGH) {
				t.Fatalf("GitHub share = %+v, want %+v", last, tt.wantGH)
			}
			for i := range last {
				if last[i] != tt.wantGH[i] {
					t.Fatalf("GitHub share = %+v, want %+v", last, tt.wantGH)
				}
			}
		})
	}
}

func TestTheGithubShareIsInQueueOrder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	other := ids.WorkspaceID("ws-2")

	// Act
	h.o.SetGithubDeployments(context.Background(), []GithubDeployment{ghDeploy(theWorkspace, 7, 4, 4), ghDeploy(other, 8, 2, 4)})

	// Assert
	told := h.sidebar.githubTold()
	if len(told) != 1 || len(told[0]) != 2 || told[0][0].Workspace != other || told[0][1].Workspace != theWorkspace {
		t.Fatalf("GitHub share = %+v, want %s at 2 then %s at 4", told, other, theWorkspace)
	}
}

func TestForgettingANativeMergeLeavesTheDeploymentDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.o.mu.Lock()
	h.o.facts[theWorkspace] = MergeFacts{State: StateMerging, Step: footer.StepTesting, QueuedAt: ghQueuedAt}
	h.o.mu.Unlock()
	h.o.SetGithubDeployments(context.Background(), []GithubDeployment{ghDeploy(theWorkspace, ghPR.Number, 1, 1)})

	// Act
	h.o.forget(theWorkspace)

	// Assert
	if got := h.footer.last(); got.Step != footer.StepGithubMergeQueue || got.GithubPlace != 1 {
		t.Fatalf("footer facts = %+v, want the GitHub standing", got)
	}
}
