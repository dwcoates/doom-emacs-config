package prwatch

import (
	"context"
	"encoding/json"
	"fmt"
	"sync"
	"time"

	"agent-repld/internal/ghclient"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/wsm"
)

// perPage is the page size every list is read in: GitHub's largest.
const perPage = 100

// author is who wrote an item, as GitHub names them.
type author struct {
	Login       string
	Association string
}

// remotePR is the pull request itself.
type remotePR struct {
	Title    string
	URL      string
	Open     bool
	Draft    bool
	Merged   bool
	MergeSHA string
	Head     string
	MergedBy string
}

// remoteComment is a conversation comment.
type remoteComment struct {
	ID      uint64
	Author  author
	Body    string
	URL     string
	Created time.Time
	Updated time.Time
}

// remoteReviewComment is a line comment.
type remoteReviewComment struct {
	remoteComment
	Path      string
	Line      *uint32
	InReplyTo *uint64
	ReviewID  *uint64
}

// remoteReview is a submitted review.
type remoteReview struct {
	ID        uint64
	Author    author
	Body      string
	State     string
	URL       string
	Submitted time.Time
}

// remoteCheck is one check run on the head commit.
type remoteCheck struct {
	ID         uint64
	Name       string
	Completed  bool
	Conclusion string
}

// snapshot is everything one read of a pull request found.
type snapshot struct {
	PR             remotePR
	Comments       []remoteComment
	ReviewComments []remoteReviewComment
	Reviews        []remoteReview
	// Checks is read only when a subscriber asks for checks; ChecksRead says
	// whether it was.
	Checks     []remoteCheck
	ChecksRead bool
	// Gates is the repository's gate checks at the head commit, left out of
	// its CI outcome; read with Checks.
	Gates map[string]bool
	// Queue is read only when a subscriber asks for the merge queue; nil
	// when it was not.
	Queue *prQueueSnapshot
}

// cachedPage is one page as GitHub last answered it, so an unchanged page's
// 304 is read from here.
type cachedPage struct {
	etag string
	body []byte
}

// reader reads pull requests, conditionally. Its page cache lives as long as
// the daemon: a restart reads every page whole once, which costs only the
// requests a 304 would have saved.
type reader struct {
	gh    ghclient.Client
	mu    sync.Mutex
	pages map[string]cachedPage
	// gates holds each commit's gate checks as its lifecycle config names
	// them, by gatesKey: a commit's file never changes, so each commit's is
	// read once.
	gates map[string]gatesRead
}

// gatesRead is one commit's gate checks, or why its config is unusable.
type gatesRead struct {
	names map[string]bool
	err   error
}

func newReader(gh ghclient.Client) *reader {
	return &reader{gh: gh, pages: map[string]cachedPage{}, gates: map[string]gatesRead{}}
}

// read reads PR's lists, and its head commit's check runs when CHECKS. Any
// failure is the leaf's *ghclient.Failure (recorded there) or a decode error.
func (r *reader) read(ctx context.Context, pr wsm.PullRequestKey, checks bool) (snapshot, error) {
	base := fmt.Sprintf("repos/%s/%s", pr.Owner, pr.Repo)
	var snap snapshot
	body, err := r.get(ctx, fmt.Sprintf("%s/pulls/%d", base, pr.Number))
	if err != nil {
		return snap, err
	}
	if snap.PR, err = decodePR(body); err != nil {
		return snap, err
	}
	if snap.Comments, err = readList(ctx, r, fmt.Sprintf("%s/issues/%d/comments", base, pr.Number), decodeComment); err != nil {
		return snap, err
	}
	if snap.ReviewComments, err = readList(ctx, r, fmt.Sprintf("%s/pulls/%d/comments", base, pr.Number), decodeReviewComment); err != nil {
		return snap, err
	}
	if snap.Reviews, err = readList(ctx, r, fmt.Sprintf("%s/pulls/%d/reviews", base, pr.Number), decodeReview); err != nil {
		return snap, err
	}
	if checks && snap.PR.Head != "" {
		if snap.Checks, err = r.readChecks(ctx, fmt.Sprintf("%s/commits/%s/check-runs", base, snap.PR.Head)); err != nil {
			return snap, err
		}
		if snap.Gates, err = r.gateChecks(ctx, pr.Owner, pr.Repo, snap.PR.Head); err != nil {
			return snap, err
		}
		snap.ChecksRead = true
	}
	return snap, nil
}

// readState reads PR itself and nothing of its lists: the state of a linked
// pull request no subscription reads (Watcher.pollLinked). It shares read's
// conditional page, so a pull request both kinds read costs one request.
func (r *reader) readState(ctx context.Context, pr wsm.PullRequestKey) (wsm.PullRequestState, error) {
	body, err := r.get(ctx, fmt.Sprintf("repos/%s/%s/pulls/%d", pr.Owner, pr.Repo, pr.Number))
	if err != nil {
		return "", err
	}
	remote, err := decodePR(body)
	if err != nil {
		return "", err
	}
	return stateOf(remote), nil
}

// get reads one resource conditionally, answering its body: the fresh one,
// or the cached one on a 304.
func (r *reader) get(ctx context.Context, path string) ([]byte, error) {
	r.mu.Lock()
	cached, ok := r.pages[path]
	r.mu.Unlock()
	etag := ""
	if ok {
		etag = cached.etag
	}
	resp, err := r.gh.Get(ctx, path, etag)
	if err != nil {
		return nil, err
	}
	if resp.NotModified() {
		if !ok {
			return nil, fmt.Errorf("prwatch: GitHub answered %s unchanged to a read that sent no entity tag", path)
		}
		return cached.body, nil
	}
	r.mu.Lock()
	r.pages[path] = cachedPage{etag: resp.ETag(), body: resp.Body}
	r.mu.Unlock()
	return resp.Body, nil
}

// readList reads every page of a list, each conditionally. A list ends at
// the first page shorter than perPage, so an item added past a full last page
// is read from the page that then exists.
func readList[T any](ctx context.Context, r *reader, path string, decode func(json.RawMessage) (T, error)) ([]T, error) {
	var out []T
	for page := 1; ; page++ {
		body, err := r.get(ctx, fmt.Sprintf("%s?per_page=%d&page=%d", path, perPage, page))
		if err != nil {
			return nil, err
		}
		var raw []json.RawMessage
		if err := json.Unmarshal(body, &raw); err != nil {
			return nil, fmt.Errorf("prwatch: decode %s page %d: %w", path, page, err)
		}
		for _, item := range raw {
			v, err := decode(item)
			if err != nil {
				return nil, fmt.Errorf("prwatch: decode an item of %s: %w", path, err)
			}
			out = append(out, v)
		}
		if len(raw) < perPage {
			return out, nil
		}
	}
}

// readChecks reads every page of a commit's check runs.
func (r *reader) readChecks(ctx context.Context, path string) ([]remoteCheck, error) {
	var out []remoteCheck
	for page := 1; ; page++ {
		body, err := r.get(ctx, fmt.Sprintf("%s?per_page=%d&page=%d", path, perPage, page))
		if err != nil {
			return nil, err
		}
		var shaped struct {
			CheckRuns []struct {
				ID         uint64  `json:"id"`
				Name       string  `json:"name"`
				Status     string  `json:"status"`
				Conclusion *string `json:"conclusion"`
			} `json:"check_runs"`
		}
		if err := json.Unmarshal(body, &shaped); err != nil {
			return nil, fmt.Errorf("prwatch: decode %s page %d: %w", path, page, err)
		}
		for _, run := range shaped.CheckRuns {
			c := remoteCheck{ID: run.ID, Name: run.Name, Completed: run.Status == "completed"}
			if run.Conclusion != nil {
				c.Conclusion = *run.Conclusion
			}
			out = append(out, c)
		}
		if len(shaped.CheckRuns) < perPage {
			return out, nil
		}
	}
}

// lifecycleConfigQuery reads one file as it stands at one commit. It is
// GraphQL because a REST read of a file the commit does not hold is a 404,
// which every repository without the file would answer on every new head.
const lifecycleConfigQuery = `query LifecycleConfigAt($owner: String!, $repo: String!, $expression: String!) {
  repository(owner: $owner, name: $repo) {
    object(expression: $expression) {
      __typename
      ... on Blob { text isTruncated }
    }
  }
}`

// gateChecks answers the gate checks the repository's lifecycle config names
// at commit SHA (repoconfig.v1.PullRequestFollowing.gate_checks): none when
// the commit holds no config, and an error naming the file when it holds one
// that is unusable. Each commit is read once, whatever it answered.
func (r *reader) gateChecks(ctx context.Context, owner, repo, sha string) (map[string]bool, error) {
	key := owner + "/" + repo + "@" + sha
	r.mu.Lock()
	known, ok := r.gates[key]
	r.mu.Unlock()
	if ok {
		return known.names, known.err
	}
	resp, err := r.gh.GraphQL(ctx, lifecycleConfigQuery, map[string]any{"owner": owner, "repo": repo, "expression": sha + ":" + lifecycleconfig.Rel})
	if err != nil {
		// A failed call is no fact about the commit, so it is asked again.
		return nil, err
	}
	read := decodeGates(resp, key+":"+lifecycleconfig.Rel)
	r.mu.Lock()
	r.gates[key] = read
	r.mu.Unlock()
	return read.names, read.err
}

// decodeGates reads a LifecycleConfigAt answer for the file at PATH.
func decodeGates(resp ghclient.GraphQLResponse, path string) gatesRead {
	var shaped struct {
		Repository *struct {
			Object *struct {
				Typename    string  `json:"__typename"`
				Text        *string `json:"text"`
				IsTruncated bool    `json:"isTruncated"`
			} `json:"object"`
		} `json:"repository"`
	}
	if err := json.Unmarshal(resp.Data, &shaped); err != nil {
		return gatesRead{err: fmt.Errorf("prwatch: decode the LifecycleConfigAt answer for %s: %w", path, err)}
	}
	switch o := shaped.Repository; {
	case o == nil:
		return gatesRead{err: nulledFailure(resp, "repository")}
	case o.Object == nil:
		return gatesRead{names: map[string]bool{}}
	case o.Object.Typename != "Blob" || o.Object.Text == nil:
		return gatesRead{err: fmt.Errorf("prwatch: %s is a %s, not a readable file", path, o.Object.Typename)}
	case o.Object.IsTruncated:
		return gatesRead{err: fmt.Errorf("prwatch: GitHub truncated %s, so it cannot be read whole", path)}
	}
	cfg, err := lifecycleconfig.Parse(path, []byte(*shaped.Repository.Object.Text))
	if err != nil {
		return gatesRead{err: err}
	}
	names := map[string]bool{}
	for _, name := range cfg.GetPullRequests().GetGateChecks() {
		names[name] = true
	}
	return gatesRead{names: names}
}

type userJSON struct {
	Login string `json:"login"`
}

func decodePR(body []byte) (remotePR, error) {
	var shaped struct {
		Title          string    `json:"title"`
		HTMLURL        string    `json:"html_url"`
		State          string    `json:"state"`
		Draft          bool      `json:"draft"`
		MergedAt       *string   `json:"merged_at"`
		MergeCommitSHA string    `json:"merge_commit_sha"`
		MergedBy       *userJSON `json:"merged_by"`
		Head           struct {
			SHA string `json:"sha"`
		} `json:"head"`
	}
	if err := json.Unmarshal(body, &shaped); err != nil {
		return remotePR{}, fmt.Errorf("prwatch: decode a pull request: %w", err)
	}
	if shaped.State != "open" && shaped.State != "closed" {
		return remotePR{}, fmt.Errorf("prwatch: a pull request in state %q", shaped.State)
	}
	pr := remotePR{Title: shaped.Title, URL: shaped.HTMLURL, Open: shaped.State == "open", Draft: shaped.Draft,
		Merged: shaped.MergedAt != nil, Head: shaped.Head.SHA}
	if pr.Merged {
		pr.MergeSHA = shaped.MergeCommitSHA
		if shaped.MergedBy != nil {
			pr.MergedBy = shaped.MergedBy.Login
		}
	}
	return pr, nil
}

type commentJSON struct {
	ID                uint64    `json:"id"`
	User              *userJSON `json:"user"`
	AuthorAssociation string    `json:"author_association"`
	Body              string    `json:"body"`
	HTMLURL           string    `json:"html_url"`
	CreatedAt         time.Time `json:"created_at"`
	UpdatedAt         time.Time `json:"updated_at"`
}

func (c commentJSON) comment() remoteComment {
	return remoteComment{ID: c.ID, Author: authorOf(c.User, c.AuthorAssociation), Body: c.Body, URL: c.HTMLURL, Created: c.CreatedAt, Updated: c.UpdatedAt}
}

func authorOf(u *userJSON, association string) author {
	a := author{Association: association}
	if u != nil {
		a.Login = u.Login
	}
	return a
}

func decodeComment(raw json.RawMessage) (remoteComment, error) {
	var c commentJSON
	err := json.Unmarshal(raw, &c)
	return c.comment(), err
}

func decodeReviewComment(raw json.RawMessage) (remoteReviewComment, error) {
	var c struct {
		commentJSON
		Path        string  `json:"path"`
		Line        *uint32 `json:"line"`
		InReplyToID *uint64 `json:"in_reply_to_id"`
		ReviewID    *uint64 `json:"pull_request_review_id"`
	}
	err := json.Unmarshal(raw, &c)
	return remoteReviewComment{remoteComment: c.comment(), Path: c.Path, Line: c.Line, InReplyTo: c.InReplyToID, ReviewID: c.ReviewID}, err
}

func decodeReview(raw json.RawMessage) (remoteReview, error) {
	var r struct {
		ID                uint64     `json:"id"`
		User              *userJSON  `json:"user"`
		AuthorAssociation string     `json:"author_association"`
		Body              string     `json:"body"`
		State             string     `json:"state"`
		HTMLURL           string     `json:"html_url"`
		SubmittedAt       *time.Time `json:"submitted_at"`
	}
	if err := json.Unmarshal(raw, &r); err != nil {
		return remoteReview{}, err
	}
	out := remoteReview{ID: r.ID, Author: authorOf(r.User, r.AuthorAssociation), Body: r.Body, State: r.State, URL: r.HTMLURL}
	if r.SubmittedAt != nil {
		out.Submitted = *r.SubmittedAt
	}
	return out, nil
}
