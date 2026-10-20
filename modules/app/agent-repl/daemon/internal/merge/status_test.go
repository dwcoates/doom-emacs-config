package merge

import (
	"context"
	"errors"
	"reflect"
	"testing"

	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// statusHarness is a harness whose registered repository keys the queue its
// workspaces' merges are in, with one async merge running and one own-branch
// merge waiting at the front.
func statusHarness(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.git.mu.Lock()
	h.git.commonDirs[h.repoRoot] = string(h.repoKey())
	h.git.mu.Unlock()
	h.registerOther(otherWorkspace, "ws-two", "agent/two")
	h.db.mu.Lock()
	merging := h.db.workspaces[theWorkspace]
	merging.MergeOf = &wsm.MergeOrigin{Requester: "ws-asker"}
	h.db.workspaces[theWorkspace] = merging
	h.db.queues[h.repoKey()] = []wsm.MergeQueueEntry{
		{Repo: h.repoKey(), Workspace: theWorkspace, State: wsm.MergeAdmitted, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, Branch: "agent/one"}},
		{Repo: h.repoKey(), Workspace: otherWorkspace, State: wsm.MergeQueued, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, Branch: "agent/two", Front: true}},
	}
	h.db.mu.Unlock()
	h.o.events.mu.Lock()
	h.o.events.steps[theWorkspace] = footer.StepTesting
	h.o.events.mu.Unlock()
	return h
}

func TestStatusAnswersEachQueueWithItsMembersAndSteps(t *testing.T) {
	tests := []struct {
		name  string
		scope *RepositoryScope
	}{
		{name: "every registered repository"},
		{name: "the one repository a scope names", scope: &RepositoryScope{ID: theRepository}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := statusHarness(t)

			// Act
			got, err := h.o.Status(context.Background(), tt.scope)

			// Assert
			if err != nil {
				t.Fatalf("Status: %v", err)
			}
			if len(got) != 1 || got[0].Repository.ID != theRepository || got[0].Queue.Repo != h.repoKey() {
				t.Fatalf("Status = %+v, want the one registered repository's queue", got)
			}
			members := got[0].Queue.Members
			want := []QueueMember{
				{Workspace: theWorkspace, Name: members[0].Name, Branch: "agent/one", Mode: MergeModeAsync, Started: true, Requester: "ws-asker", Step: footer.StepTesting},
				{Workspace: otherWorkspace, Name: members[1].Name, Branch: "agent/two", Mode: members[1].Mode, Front: true},
			}
			if !reflect.DeepEqual(members, want) {
				t.Errorf("members = %+v, want %+v", members, want)
			}
		})
	}
}

func TestStatusRefusesAnUnknownRepository(t *testing.T) {
	// Arrange
	h := statusHarness(t)

	// Act
	_, err := h.o.Status(context.Background(), &RepositoryScope{ID: "repo-9"})

	// Assert
	if refusal, ok := Refused(err); !ok || refusal.Arm != ArmUnknownRepository {
		t.Fatalf("Status = %v, want the unknown_repository refusal", err)
	}
	if !loggedLike(h, logLike{op: "daemon.merge.status", text: "refused a merge queue status for a repository the registry does not hold"}) {
		t.Errorf("no record of the refusal: %+v", h.logs.Records())
	}
}

func TestStatusFailsHardWhenAQueueCannotBeRead(t *testing.T) {
	// Arrange
	h := statusHarness(t)
	h.git.mu.Lock()
	h.git.commonDirErr = errors.New("git gone")
	h.git.mu.Unlock()

	// Act
	_, err := h.o.Status(context.Background(), nil)

	// Assert
	if _, refused := Refused(err); err == nil || refused {
		t.Fatalf("Status = %v, want a hard failure", err)
	}
	if !loggedLike(h, logLike{level: "error", op: "daemon.merge.status", text: "could not resolve a repository's queue key"}) {
		t.Errorf("no error record: %+v", h.logs.Records())
	}
}
