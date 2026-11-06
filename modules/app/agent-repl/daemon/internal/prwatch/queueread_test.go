package prwatch

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/ghclient"
	"agent-repld/internal/wsm"
)

var repoOR = repository{"o", "r"}

func TestABatchedQueryAsksEveryPullRequestUnderItsAlias(t *testing.T) {
	// Act
	got := batchedPullRequests("PullRequestQueues", []uint32{7, 12}, prQueueFragment)

	// Assert
	for _, want := range []string{"query PullRequestQueues($owner: String!, $repo: String!)", "pr7: pullRequest(number: 7) { ...facts }", "pr12: pullRequest(number: 12) { ...facts }", "fragment facts on PullRequest"} {
		if !strings.Contains(got, want) {
			t.Fatalf("query %q lacks %q", got, want)
		}
	}
}

func TestReadPRQueuesDecodesEachPullRequestsFacts(t *testing.T) {
	// Arrange
	gh := newGH()
	g := newGQL()
	gh.graphql = g.answer
	later := t0.Add(time.Minute)
	g.setFacts(7, factsJSON("main", &remoteEntryFacts{State: "AWAITING_CHECKS", GroupHead: "group1"},
		remoteQueueEvent{ID: "e2", Removed: true, Actor: "bot", Reason: ptr("checks failed"), At: later},
		remoteQueueEvent{ID: "e1", Actor: "dodge", At: t0}))
	g.setFacts(8, factsJSON("main", nil))

	// Act
	facts, failures, err := readPRQueues(context.Background(), gh, repoOR, []uint32{7, 8})

	// Assert
	want := map[uint32]remotePRQueue{
		7: {Base: "main", Entry: &remoteEntryFacts{State: "AWAITING_CHECKS", GroupHead: "group1"}, Events: []remoteQueueEvent{
			{ID: "e1", Actor: "dodge", At: t0}, {ID: "e2", Removed: true, Actor: "bot", Reason: ptr("checks failed"), At: later}}},
		8: {Base: "main"},
	}
	if err != nil || len(failures) != 0 || !reflect.DeepEqual(facts, want) {
		t.Fatalf("readPRQueues = %+v, %v, %v; want %+v", facts, failures, err, want)
	}
}

func TestReadPRQueuesAnswersANulledPullRequestAsItsOwnFailure(t *testing.T) {
	// Arrange
	gh := newGH()
	g := newGQL()
	gh.graphql = g.answer
	g.setFacts(7, factsJSON("main", nil))

	// Act
	facts, failures, err := readPRQueues(context.Background(), gh, repoOR, []uint32{7, 9})

	// Assert
	var f *ghclient.Failure
	if err != nil || len(facts) != 1 || !errors.As(failures[9], &f) || f.Kind != ghclient.KindNotFound {
		t.Fatalf("readPRQueues = %+v, %v, %v; want #7 read and #9 not found", facts, failures, err)
	}
}

func TestABatchedReadFails(t *testing.T) {
	tests := []struct {
		name string
		data string
		errs []ghclient.GraphQLError
		want string
	}{
		{name: "a nulled repository is its error's failure", data: `{"repository":null}`,
			errs: []ghclient.GraphQLError{{Type: "NOT_FOUND", Message: "no repository", Path: []string{"repository"}}}, want: "no repository"},
		{name: "a nulled field with no error is a defect of the answer", data: `{"repository":{"pr7":null}}`, want: "GitHub nulled repository.pr7 and reported no error for it"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			gh := newGH()
			gh.graphql = func(string, map[string]any) (ghclient.GraphQLResponse, error) {
				return ghclient.GraphQLResponse{Data: []byte(tt.data), Errors: tt.errs}, nil
			}

			// Act
			_, failures, err := readPRQueues(context.Background(), gh, repoOR, []uint32{7})

			// Assert
			if err == nil {
				err = failures[7]
			}
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want one saying %q", err, tt.want)
			}
		})
	}
}

func TestDecodePRQueueRefusesAnUnknownTimelineEvent(t *testing.T) {
	// Act
	_, err := decodePRQueue([]byte(`{"baseRefName":"main","timelineItems":{"nodes":[{"__typename":"MergedEvent","id":"x"}]}}`))

	// Assert
	if err == nil || !strings.Contains(err.Error(), `"MergedEvent"`) {
		t.Fatalf("err = %v, want the unknown type named", err)
	}
}

func TestReadQueueReadsEveryPageHeadFirst(t *testing.T) {
	// Arrange
	gh := newGH()
	var afters []any
	gh.graphql = func(_ string, vars map[string]any) (ghclient.GraphQLResponse, error) {
		afters = append(afters, vars["after"])
		if vars["after"] == nil {
			return ghclient.GraphQLResponse{Data: []byte(`{"repository":{"defaultBranchRef":{"name":"main"},"mergeQueue":{"url":"u","entries":{"pageInfo":{"hasNextPage":true,"endCursor":"c1"},"nodes":[{"state":"QUEUED","pullRequest":{"number":1,"title":"a","url":"u1"}}]}}}}`)}, nil
		}
		return ghclient.GraphQLResponse{Data: []byte(`{"repository":{"defaultBranchRef":{"name":"main"},"mergeQueue":{"url":"u","entries":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[{"state":"QUEUED","headCommit":{"oid":"g2"},"pullRequest":{"number":2,"title":"b","url":"u2"}}]}}}}`)}, nil
	}

	// Act
	q, err := readQueue(context.Background(), gh, repoOR, "")

	// Assert
	want := remoteQueue{URL: "u", Branch: "main", Entries: []remoteQueueEntry{{Number: 1, Title: "a", URL: "u1", State: "QUEUED"}, {Number: 2, Title: "b", URL: "u2", State: "QUEUED", GroupHead: "g2"}}}
	if err != nil || !reflect.DeepEqual(q, want) || !reflect.DeepEqual(afters, []any{nil, "c1"}) {
		t.Fatalf("readQueue = %+v, %v after %v; want %+v", q, err, afters, want)
	}
}

func TestReadQueueAnswersNoMergeQueueWithTheBranchResolved(t *testing.T) {
	// Arrange
	gh := newGH()
	g := newGQL()
	gh.graphql = g.answer
	g.defaultBranch = "trunk"

	// Act
	q, err := readQueue(context.Background(), gh, repoOR, "")

	// Assert
	if !errors.Is(err, ErrNoMergeQueue) || q.Branch != "trunk" {
		t.Fatalf("readQueue = %+v, %v; want ErrNoMergeQueue on trunk", q, err)
	}
}

func TestReadDeparturesSaysWhatBecameOfEachEntry(t *testing.T) {
	// Arrange
	gh := newGH()
	g := newGQL()
	gh.graphql = g.answer
	g.setDeparture(1, departureJSON("a", true, "sha1", nil))
	g.setDeparture(2, departureJSON("b", false, "", ptr("conflicts")))

	// Act
	got, failures, err := readDepartures(context.Background(), gh, repoOR, []uint32{1, 2})

	// Assert
	want := map[uint32]remoteDeparture{1: {Title: "a", Merged: true, MergeSHA: "sha1"}, 2: {Title: "b", Reason: ptr("conflicts")}}
	if err != nil || len(failures) != 0 || !reflect.DeepEqual(got, want) {
		t.Fatalf("readDepartures = %+v, %v, %v; want %+v", got, failures, err, want)
	}
}

func TestATicksQueueReadsReadEachQueueOnce(t *testing.T) {
	// Arrange
	gh := newGH()
	g := newGQL()
	gh.graphql = g.answer
	g.setQueue("main", queueJSON(entry(1, "QUEUED")))
	reads := newQueueReads(gh)
	key := wsm.GithubMergeQueueKey{Owner: "o", Repo: "r", Branch: "main"}

	// Act
	first, errFirst := reads.get(context.Background(), key)
	second, errSecond := reads.get(context.Background(), key)

	// Assert
	calls := 0
	for _, c := range gh.calls {
		if c == "GRAPHQL MergeQueue" {
			calls++
		}
	}
	if errFirst != nil || errSecond != nil || !reflect.DeepEqual(first, second) || calls != 1 {
		t.Fatalf("reads = %+v / %+v (%v, %v) in %d calls; want one call", first, second, errFirst, errSecond, calls)
	}
}

func TestPositionOf(t *testing.T) {
	q := remoteQueue{Entries: []remoteQueueEntry{entry(1, "QUEUED"), entry(2, "QUEUED"), entry(3, "QUEUED")}}
	tests := []struct {
		name   string
		number uint32
		want   position
		ok     bool
	}{
		{name: "the head", number: 1, want: position{Ahead: 0, Total: 3}, ok: true},
		{name: "the tail", number: 3, want: position{Ahead: 2, Total: 3}, ok: true},
		{name: "not held", number: 9, ok: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := positionOf(q, tt.number)

			// Assert
			if ok != tt.ok || got != tt.want {
				t.Fatalf("positionOf(%d) = %+v, %v; want %+v, %v", tt.number, got, ok, tt.want, tt.ok)
			}
		})
	}
}
