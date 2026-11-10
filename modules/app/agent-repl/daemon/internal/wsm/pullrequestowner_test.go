package wsm

import (
	"context"
	"errors"
	"testing"
)

const ownedURL = "https://github.com/ChessCom/explanation-engine/pull/42"

// ownedSubscriptions answers every owned-link subscription to prKey.
func ownedSubscriptions(t *testing.T, s *store) []WorkspaceID {
	t.Helper()
	rows, err := s.db().QueryContext(context.Background(),
		`SELECT subscriber FROM pull_request_subscriptions WHERE origin = 'owned_link' AND owner = ? AND repo = ? AND number = ? ORDER BY subscriber`,
		prKey.Owner, prKey.Repo, prKey.Number)
	if err != nil {
		t.Fatal(err)
	}
	defer rows.Close()
	var out []WorkspaceID
	for rows.Next() {
		var ws WorkspaceID
		if err := rows.Scan(&ws); err != nil {
			t.Fatal(err)
		}
		out = append(out, ws)
	}
	return out
}

func TestAPullRequestHasOneOwningWorkspace(t *testing.T) {
	tests := []struct {
		name string
		// release is what the first owner does before the second owns, nil
		// for nothing.
		release   func(t *testing.T, s *store, owner WorkspaceID)
		wantOwned bool
	}{
		{name: "a second open workspace is refused while the first owns it"},
		{name: "a second workspace owns it once the first stores it a reference", release: func(t *testing.T, s *store, owner WorkspaceID) {
			mustSetLink(t, s, owner, LinkSet{URL: ownedURL, Label: "the pull request", PullRequest: &prKey})
		}, wantOwned: true},
		{name: "a second workspace owns it once the first removes it", release: func(t *testing.T, s *store, owner WorkspaceID) {
			if _, err := s.RemoveWorkspaceLink(context.Background(), owner, ownedURL); err != nil {
				t.Fatal(err)
			}
		}, wantOwned: true},
		{name: "a second workspace owns it once the first closes", release: func(t *testing.T, s *store, owner WorkspaceID) {
			if err := s.MarkClosed(context.Background(), owner, Closure{At: instant, By: CloseByUser}); err != nil {
				t.Fatal(err)
			}
		}, wantOwned: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			first, second := testWorkspace(t, s), testWorkspaceNamed(t, s, "other")
			mustSetLink(t, s, first.ID, ownedSet(first.ID, ownedURL, prKey))
			if tt.release != nil {
				tt.release(t, s, first.ID)
			}

			// Act
			_, err := s.SetWorkspaceLink(context.Background(), second.ID, ownedSet(second.ID, ownedURL, prKey))

			// Assert
			if tt.wantOwned {
				if err != nil {
					t.Fatalf("SetWorkspaceLink = %v, want the second workspace to own it", err)
				}
				if got := ownedSubscriptions(t, s); len(got) != 1 || got[0] != second.ID {
					t.Fatalf("owners = %v, want only %s", got, second.ID)
				}
				return
			}
			var elsewhere *PullRequestOwnedElsewhereError
			if !errors.As(err, &elsewhere) || elsewhere.Owner != first.ID || elsewhere.OwnerName != first.Name || elsewhere.PullRequest != prKey {
				t.Fatalf("SetWorkspaceLink = %v, want owned elsewhere by %s (%s)", err, first.Name, first.ID)
			}
			if links := linksNow(t, s, second.ID); len(links) != 0 {
				t.Fatalf("the refused workspace's links = %+v, want nothing written", links)
			}
			if got := ownedSubscriptions(t, s); len(got) != 1 || got[0] != first.ID {
				t.Fatalf("owners = %v, want only %s", got, first.ID)
			}
		})
	}
}

func TestOwningAPullRequestTakesOverTheSubscriptionHeld(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	requested := PullRequestSubscription{Subscriber: ws, PullRequest: prKey, Events: NewPullRequestEvents(PullRequestEventComment),
		Origin: PullRequestOriginRequested, SubscribedAt: instant, Told: "a"}
	if _, err := s.SubscribePullRequest(context.Background(), requested); err != nil {
		t.Fatal(err)
	}
	set := ownedSet(ws, ownedURL, prKey)
	set.Subscribe = nil

	// Act
	_, err := s.SetWorkspaceLink(context.Background(), ws, set)

	// Assert
	subs := subsOf(t, s, ws)
	if err != nil || len(subs) != 1 || subs[0].Origin != PullRequestOriginOwnedLink || subs[0].Events != AllPullRequestEvents() {
		t.Fatalf("SetWorkspaceLink = %v; subscriptions %+v; want the held one owned, asking for every event", err, subs)
	}
}

func TestTheStoreHoldsNoSecondOwnedSubscription(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	first, second := testWorkspace(t, s), testWorkspaceNamed(t, s, "other")
	mustSetLink(t, s, first.ID, ownedSet(first.ID, ownedURL, prKey))

	// Act
	_, err := s.db().ExecContext(context.Background(),
		`INSERT INTO pull_request_subscriptions (subscriber, owner, repo, number, events, origin, subscribed_at, told) VALUES (?, ?, ?, ?, ?, 'owned_link', 0, 'a')`,
		second.ID, prKey.Owner, prKey.Repo, prKey.Number, int64(AllPullRequestEvents()))

	// Assert
	if err == nil {
		t.Fatal("a second owned-link subscription was written, want the index to refuse it")
	}
}

func TestTheOnePullRequestOwnerStepKeepsTheEarliestOwner(t *testing.T) {
	// Arrange: the index dropped, as before the step, and two owners plus a
	// workspace owning through its link with only a requested subscription.
	s, _ := testStore(t)
	ctx := context.Background()
	first, second, third := testWorkspace(t, s), testWorkspaceNamed(t, s, "other"), testWorkspaceNamed(t, s, "third")
	other := PullRequestKey{Owner: "ChessCom", Repo: "explanation-engine", Number: 43}
	if _, err := s.db().ExecContext(ctx, `DROP INDEX pull_request_subscriptions_one_owner`); err != nil {
		t.Fatal(err)
	}
	insert := func(ws WorkspaceID, pr PullRequestKey, origin string, at int64) {
		t.Helper()
		if _, err := s.db().ExecContext(ctx,
			`INSERT INTO pull_request_subscriptions (subscriber, owner, repo, number, events, origin, subscribed_at, told) VALUES (?, ?, ?, ?, 2, ?, ?, 'a')`,
			ws, pr.Owner, pr.Repo, pr.Number, origin, at); err != nil {
			t.Fatal(err)
		}
	}
	insert(second.ID, prKey, "owned_link", 2)
	insert(first.ID, prKey, "owned_link", 1)
	insert(third.ID, other, "requested", 1)
	if _, err := s.db().ExecContext(ctx, `INSERT INTO workspace_links (workspace_id, url, label, set_seq, shown, owned, pr_owner, pr_repo, pr_number) VALUES (?, ?, 'the pull request', 1, 1, 1, ?, ?, ?)`,
		third.ID, "https://github.com/ChessCom/explanation-engine/pull/43", other.Owner, other.Repo, other.Number); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := s.db().ExecContext(ctx, onePullRequestOwnerDDL)

	// Assert
	if err != nil {
		t.Fatalf("the step: %v", err)
	}
	if got := ownedSubscriptions(t, s); len(got) != 1 || got[0] != first.ID {
		t.Fatalf("owners of %s = %v, want only the earliest, %s", prKey, got, first.ID)
	}
	if subs := subsOf(t, s, second.ID); len(subs) != 1 || subs[0].Origin != PullRequestOriginRequested {
		t.Fatalf("the later owner's subscriptions = %+v, want one demoted to requested", subs)
	}
	if subs := subsOf(t, s, third.ID); len(subs) != 1 || subs[0].Origin != PullRequestOriginOwnedLink || subs[0].Events != AllPullRequestEvents()|NewPullRequestEvents(PullRequestEventComment) {
		t.Fatalf("the link owner's subscriptions = %+v, want its requested one owned, asking for every event", subs)
	}
}

func TestTheOnePullRequestOwnerStepSpellsEveryEvent(t *testing.T) {
	// Arrange, Act
	all := int64(AllPullRequestEvents())

	// Assert: the step's literal is every event's bits.
	if all != 126 {
		t.Fatalf("AllPullRequestEvents() = %d, but onePullRequestOwnerDDL spells 126; update the step", all)
	}
}
