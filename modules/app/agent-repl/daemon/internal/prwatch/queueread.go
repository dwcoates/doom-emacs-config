package prwatch

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"sort"
	"strings"
	"time"

	"agent-repld/internal/ghclient"
	"agent-repld/internal/wsm"
)

// The GitHub merge queue reads. Only GraphQL exposes a merge queue, so each
// is one GraphQL query; a pull request's own lists stay conditional REST
// reads (read.go).
//
// ONE QUERY PER REPOSITORY, NOT PER PULL REQUEST. Unlike a REST read, which
// a 304 makes free, every GraphQL query spends the `gh` login's rate limit,
// so the queue facts of every followed pull request in one repository are
// asked in one query, each under its own alias. A queue's entries are read
// once a tick however many pull requests and queue subscriptions need them
// (queueReads).

// timelineWindow is how many of a pull request's latest merge-queue timeline
// events each read asks for. Each joining or removal takes a person's or the
// queue's action and GitHub's processing, so a window this wide holds every
// event since a read one cadence earlier.
const timelineWindow = 100

// ErrNoMergeQueue is a branch GitHub runs no merge queue for. It is an
// answer about the branch, never a failure to reach GitHub.
var ErrNoMergeQueue = errors.New("prwatch: GitHub runs no merge queue for the branch")

// remoteQueueEvent is one of a pull request's merge-queue timeline events.
type remoteQueueEvent struct {
	// ID is GitHub's node id, which the event is told once by.
	ID string
	// Removed is true for a removal, false for a joining.
	Removed bool
	// Actor is who acted; empty when GitHub names no one.
	Actor string
	// Reason is a removal's reason; nil when GitHub gives none.
	Reason *string
	At     time.Time
}

// remotePRQueue is a pull request's merge-queue facts.
type remotePRQueue struct {
	// Base is the branch the pull request merges into, whose queue it joins.
	Base string
	// Entry is its queue entry; nil when it is not in the queue.
	Entry *remoteEntryFacts
	// Events is its latest merge-queue timeline events, oldest first.
	Events []remoteQueueEvent
}

// remoteEntryFacts is what a pull request's own read says of its entry.
type remoteEntryFacts struct {
	State string
	// GroupHead is the merge group commit the queue tests; empty before the
	// queue made one.
	GroupHead string
}

// remoteQueue is a branch's merge queue.
type remoteQueue struct {
	URL string
	// Branch is the branch it merges into, as GitHub resolved it: the
	// default branch's name when the read named none.
	Branch string
	// Entries is every entry, head first.
	Entries []remoteQueueEntry
}

// remoteQueueEntry is one entry of a queue's read.
type remoteQueueEntry struct {
	Number    uint32
	Title     string
	URL       string
	State     string
	GroupHead string
}

// position is how far an entry is from merging.
type position struct {
	Ahead uint32
	Total uint32
}

// positionOf is pull request NUMBER's place in Q; false when Q does not hold
// it, which a pull request that left between two reads shows.
func positionOf(q remoteQueue, number uint32) (position, bool) {
	for i, e := range q.Entries {
		if e.Number == number {
			return position{Ahead: uint32(i), Total: uint32(len(q.Entries))}, true
		}
	}
	return position{}, false
}

// remoteDeparture is what became of a pull request that left a queue.
type remoteDeparture struct {
	Title    string
	Merged   bool
	MergeSHA string
	// Reason is its latest removal's reason; nil when GitHub gives none.
	Reason *string
}

// repository names one repository on github.com.
type repository struct{ Owner, Repo string }

// aliasOf is pull request NUMBER's alias in a batched query.
func aliasOf(number uint32) string { return fmt.Sprintf("pr%d", number) }

// batchedPullRequests is a query asking FRAGMENT of every pull request in
// NUMBERS, each under its alias, in one repository.
func batchedPullRequests(name string, numbers []uint32, fragment string) string {
	var b strings.Builder
	b.WriteString(fmt.Sprintf("query %s($owner: String!, $repo: String!) {\n  repository(owner: $owner, name: $repo) {\n", name))
	for _, n := range numbers {
		b.WriteString(fmt.Sprintf("    %s: pullRequest(number: %d) { ...facts }\n", aliasOf(n), n))
	}
	b.WriteString("  }\n}\n")
	b.WriteString(fragment)
	return b.String()
}

// prQueueFragment is what a pull request's queue read asks.
var prQueueFragment = fmt.Sprintf(`fragment facts on PullRequest {
  baseRefName
  mergeQueueEntry { state headCommit { oid } }
  timelineItems(last: %d, itemTypes: [ADDED_TO_MERGE_QUEUE_EVENT, REMOVED_FROM_MERGE_QUEUE_EVENT]) {
    nodes {
      __typename
      ... on AddedToMergeQueueEvent { id createdAt actor { login } }
      ... on RemovedFromMergeQueueEvent { id createdAt actor { login } reason }
    }
  }
}
`, timelineWindow)

// departureFragment is what a departed entry's read asks.
const departureFragment = `fragment facts on PullRequest {
  title
  merged
  mergeCommit { oid }
  timelineItems(last: 1, itemTypes: [REMOVED_FROM_MERGE_QUEUE_EVENT]) {
    nodes { ... on RemovedFromMergeQueueEvent { reason } }
  }
}
`

// queueQuery reads one page of a branch's queue.
const queueQuery = `query MergeQueue($owner: String!, $repo: String!, $branch: String, $after: String) {
  repository(owner: $owner, name: $repo) {
    defaultBranchRef { name }
    mergeQueue(branch: $branch) {
      url
      entries(first: 100, after: $after) {
        pageInfo { hasNextPage endCursor }
        nodes { state headCommit { oid } pullRequest { number title url } }
      }
    }
  }
}
`

// batchAnswer is one batched query's answer: each pull request's raw facts,
// or the failure GitHub reported for it alone.
type batchAnswer struct {
	facts    map[uint32]json.RawMessage
	failures map[uint32]error
}

// readBatch runs a batched query for NUMBERS in REPO. A failure of the
// whole query, or of the repository itself, is the error; a pull request
// GitHub could not resolve is its own entry in failures.
func readBatch(ctx context.Context, gh ghclient.Client, name string, repo repository, numbers []uint32, fragment string) (batchAnswer, error) {
	resp, err := gh.GraphQL(ctx, batchedPullRequests(name, numbers, fragment), map[string]any{"owner": repo.Owner, "repo": repo.Repo})
	if err != nil {
		return batchAnswer{}, err
	}
	var shaped struct {
		Repository map[string]json.RawMessage `json:"repository"`
	}
	if err := json.Unmarshal(resp.Data, &shaped); err != nil {
		return batchAnswer{}, fmt.Errorf("prwatch: decode the %s answer: %w", name, err)
	}
	if shaped.Repository == nil {
		return batchAnswer{}, nulledFailure(resp, "repository")
	}
	out := batchAnswer{facts: map[uint32]json.RawMessage{}, failures: map[uint32]error{}}
	for _, n := range numbers {
		raw, ok := shaped.Repository[aliasOf(n)]
		if !ok || string(raw) == "null" {
			out.failures[n] = nulledFailure(resp, "repository", aliasOf(n))
			continue
		}
		out.facts[n] = raw
	}
	return out, nil
}

// nulledFailure is the failure GitHub reported for the field at PATH, which
// it nulled; a nulled field it reported nothing for is a defect of the
// answer, reported as one.
func nulledFailure(resp ghclient.GraphQLResponse, path ...string) error {
	want := strings.Join(path, ".")
	for _, e := range resp.Errors {
		if strings.Join(e.Path, ".") == want {
			return e.Failure()
		}
	}
	return fmt.Errorf("prwatch: GitHub nulled %s and reported no error for it", want)
}

// readPRQueues reads the queue facts of every pull request in NUMBERS of
// REPO in one query.
func readPRQueues(ctx context.Context, gh ghclient.Client, repo repository, numbers []uint32) (map[uint32]remotePRQueue, map[uint32]error, error) {
	answer, err := readBatch(ctx, gh, "PullRequestQueues", repo, numbers, prQueueFragment)
	if err != nil {
		return nil, nil, err
	}
	out := map[uint32]remotePRQueue{}
	for n, raw := range answer.facts {
		q, err := decodePRQueue(raw)
		if err != nil {
			answer.failures[n] = err
			continue
		}
		out[n] = q
	}
	return out, answer.failures, nil
}

type loginJSON struct {
	Login string `json:"login"`
}

func decodePRQueue(raw json.RawMessage) (remotePRQueue, error) {
	var shaped struct {
		BaseRefName     string `json:"baseRefName"`
		MergeQueueEntry *struct {
			State      string `json:"state"`
			HeadCommit *struct {
				OID string `json:"oid"`
			} `json:"headCommit"`
		} `json:"mergeQueueEntry"`
		TimelineItems struct {
			Nodes []struct {
				Typename  string     `json:"__typename"`
				ID        string     `json:"id"`
				CreatedAt time.Time  `json:"createdAt"`
				Actor     *loginJSON `json:"actor"`
				Reason    *string    `json:"reason"`
			} `json:"nodes"`
		} `json:"timelineItems"`
	}
	if err := json.Unmarshal(raw, &shaped); err != nil {
		return remotePRQueue{}, fmt.Errorf("prwatch: decode a pull request's merge queue facts: %w", err)
	}
	q := remotePRQueue{Base: shaped.BaseRefName}
	if e := shaped.MergeQueueEntry; e != nil {
		q.Entry = &remoteEntryFacts{State: e.State}
		if e.HeadCommit != nil {
			q.Entry.GroupHead = e.HeadCommit.OID
		}
	}
	for _, n := range shaped.TimelineItems.Nodes {
		ev := remoteQueueEvent{ID: n.ID, At: n.CreatedAt, Reason: n.Reason}
		switch n.Typename {
		case "AddedToMergeQueueEvent":
		case "RemovedFromMergeQueueEvent":
			ev.Removed = true
		default:
			return remotePRQueue{}, fmt.Errorf("prwatch: a merge queue timeline event of type %q", n.Typename)
		}
		if n.Actor != nil {
			ev.Actor = n.Actor.Login
		}
		q.Events = append(q.Events, ev)
	}
	sort.SliceStable(q.Events, func(i, j int) bool { return q.Events[i].At.Before(q.Events[j].At) })
	return q, nil
}

// readQueue reads every entry of REPO's queue for BRANCH, head first; an
// empty BRANCH is the repository's default branch. A branch with no queue
// is ErrNoMergeQueue, answered with the branch resolved.
func readQueue(ctx context.Context, gh ghclient.Client, repo repository, branch string) (remoteQueue, error) {
	var (
		out   remoteQueue
		after any
	)
	vars := map[string]any{"owner": repo.Owner, "repo": repo.Repo, "branch": nil}
	if branch != "" {
		vars["branch"] = branch
	}
	for {
		vars["after"] = after
		resp, err := gh.GraphQL(ctx, queueQuery, vars)
		if err != nil {
			return remoteQueue{}, err
		}
		var shaped struct {
			Repository *struct {
				DefaultBranchRef *struct {
					Name string `json:"name"`
				} `json:"defaultBranchRef"`
				MergeQueue *struct {
					URL     string `json:"url"`
					Entries struct {
						PageInfo struct {
							HasNextPage bool   `json:"hasNextPage"`
							EndCursor   string `json:"endCursor"`
						} `json:"pageInfo"`
						Nodes []struct {
							State      string `json:"state"`
							HeadCommit *struct {
								OID string `json:"oid"`
							} `json:"headCommit"`
							PullRequest struct {
								Number uint32 `json:"number"`
								Title  string `json:"title"`
								URL    string `json:"url"`
							} `json:"pullRequest"`
						} `json:"nodes"`
					} `json:"entries"`
				} `json:"mergeQueue"`
			} `json:"repository"`
		}
		if err := json.Unmarshal(resp.Data, &shaped); err != nil {
			return remoteQueue{}, fmt.Errorf("prwatch: decode the merge queue answer: %w", err)
		}
		if shaped.Repository == nil {
			return remoteQueue{}, nulledFailure(resp, "repository")
		}
		out.Branch = branch
		if branch == "" && shaped.Repository.DefaultBranchRef != nil {
			out.Branch = shaped.Repository.DefaultBranchRef.Name
		}
		if shaped.Repository.MergeQueue == nil {
			// The branch is resolved even so, so the caller names it.
			return remoteQueue{Branch: out.Branch}, ErrNoMergeQueue
		}
		out.URL = shaped.Repository.MergeQueue.URL
		for _, n := range shaped.Repository.MergeQueue.Entries.Nodes {
			e := remoteQueueEntry{Number: n.PullRequest.Number, Title: n.PullRequest.Title, URL: n.PullRequest.URL, State: n.State}
			if n.HeadCommit != nil {
				e.GroupHead = n.HeadCommit.OID
			}
			out.Entries = append(out.Entries, e)
		}
		page := shaped.Repository.MergeQueue.Entries.PageInfo
		if !page.HasNextPage {
			return out, nil
		}
		after = page.EndCursor
	}
}

// readDepartures reads what became of every pull request in NUMBERS of REPO,
// each of which left a queue, in one query.
func readDepartures(ctx context.Context, gh ghclient.Client, repo repository, numbers []uint32) (map[uint32]remoteDeparture, map[uint32]error, error) {
	answer, err := readBatch(ctx, gh, "MergeQueueDepartures", repo, numbers, departureFragment)
	if err != nil {
		return nil, nil, err
	}
	out := map[uint32]remoteDeparture{}
	for n, raw := range answer.facts {
		var shaped struct {
			Title       string `json:"title"`
			Merged      bool   `json:"merged"`
			MergeCommit *struct {
				OID string `json:"oid"`
			} `json:"mergeCommit"`
			TimelineItems struct {
				Nodes []struct {
					Reason *string `json:"reason"`
				} `json:"nodes"`
			} `json:"timelineItems"`
		}
		if err := json.Unmarshal(raw, &shaped); err != nil {
			answer.failures[n] = fmt.Errorf("prwatch: decode a departed entry: %w", err)
			continue
		}
		d := remoteDeparture{Title: shaped.Title, Merged: shaped.Merged}
		if shaped.MergeCommit != nil {
			d.MergeSHA = shaped.MergeCommit.OID
		}
		if nodes := shaped.TimelineItems.Nodes; len(nodes) > 0 {
			d.Reason = nodes[len(nodes)-1].Reason
		}
		out[n] = d
	}
	return out, answer.failures, nil
}

// queueReads is one tick's reads of merge queues, so a queue every
// pull-request subscription and queue subscription needs is read once.
type queueReads struct {
	gh   ghclient.Client
	read map[wsm.GithubMergeQueueKey]queueRead
}

type queueRead struct {
	q   remoteQueue
	err error
}

func newQueueReads(gh ghclient.Client) *queueReads {
	return &queueReads{gh: gh, read: map[wsm.GithubMergeQueueKey]queueRead{}}
}

// get answers KEY's queue, reading it on the tick's first ask.
func (r *queueReads) get(ctx context.Context, key wsm.GithubMergeQueueKey) (remoteQueue, error) {
	if got, ok := r.read[key]; ok {
		return got.q, got.err
	}
	q, err := readQueue(ctx, r.gh, repository{key.Owner, key.Repo}, key.Branch)
	r.read[key] = queueRead{q: q, err: err}
	return q, err
}
