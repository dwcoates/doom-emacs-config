package workspace

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/slackfmt"
	"agent-repld/internal/slackpost"
	"agent-repld/internal/wsm"
)

const (
	originLink = "https://w.slack.com/archives/C1/p1730000099000001?thread_ts=1730000000.123456"
	pullLink   = "https://github.com/a/b/pull/7"
	designLink = "https://docs.example.com/d"
	theTask    = "Fix the footer's stale activity line."
)

var (
	originThread = slackpost.Thread{URL: originLink, Channel: "C1", TS: "1730000000.123456"}
	pullKey      = wsm.PullRequestKey{Owner: "a", Repo: "b", Number: 7}
)

// slackRecord answers the slack-origin record at LEVEL.
func slackRecord(t *testing.T, f *fixture, level string) (dlog.Record, bool) {
	t.Helper()
	return dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: level, Operation: opSlackOrigin})
}

// assertSlackFailed asserts the one ERROR record naming WHAT with the
// workspace, the cause and every field of CONTEXT, and the transient footer
// line on WS.
func assertSlackFailed(t *testing.T, f *fixture, ws ids.WorkspaceID, what string, context dlog.Context) {
	t.Helper()
	r, ok := slackRecord(t, f, dlog.LevelError)
	if !ok || r.Message != what || r.Context["workspace"] != string(ws) || r.Context["cause"] == nil {
		t.Errorf("record = %+v, want an ERROR %s saying %q with the workspace and the cause", r, opSlackOrigin, what)
	}
	for k, want := range context {
		if got := r.Context[k]; got != want {
			t.Errorf("record context[%q] = %v, want %v", k, got, want)
		}
	}
	if len(f.footer.faults) != 1 || f.footer.faults[0].Kind != FaultKindSlackPostFailed || f.footer.faults[0].Status != "" || f.footer.faults[0].Detail != what {
		t.Errorf("footer faults = %+v, want one transient %s saying %q", f.footer.faults, FaultKindSlackPostFailed, what)
	}
}

// originFixture is a fixture holding workspace w1, created for originThread.
func originFixture(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.slackOrigins = map[ids.WorkspaceID]string{"w1": originLink}
	return f
}

// --- the opening ---------------------------------------------------------

func TestAdoptSlackOriginRecordsAndLinksTheThread(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())

	// Act.
	f.verbs.(*verbs).adoptSlackOrigin(context.Background(), ws, originLink)

	// Assert.
	if got := f.db.slackOrigins["w1"]; got != originLink {
		t.Errorf("recorded origin = %q, want %q", got, originLink)
	}
	want := []wsm.WorkspaceLink{{URL: originLink, Label: SlackOriginLabel, Shown: true}}
	if got := f.db.links["w1"]; len(got) != 1 || got[0] != want[0] {
		t.Errorf("links = %+v, want the thread stored as a reference %+v", got, want)
	}
}

func TestAdoptSlackOriginWithNoSummaryYetPostsNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())

	// Act.
	f.verbs.(*verbs).adoptSlackOrigin(context.Background(), ws, originLink)

	// Assert.
	if len(f.slack.replies) != 0 || f.db.slackOpened["w1"] {
		t.Fatalf("replies %+v, opened %v; want no post and the opening unclaimed", f.slack.replies, f.db.slackOpened)
	}
	if r, ok := slackRecord(t, f, dlog.LevelInfo); !ok || !strings.Contains(r.Message, "waits for the workspace's first summary") {
		t.Errorf("record = %+v, want an INFO saying the opening waits", r)
	}
}

func TestAdoptSlackOriginPostsAStandingSummaryAsTheOpening(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	f.db.summaries = map[ids.WorkspaceID]wsm.WorkspaceSummary{"w1": {Title: theTask}}

	// Act.
	f.verbs.(*verbs).adoptSlackOrigin(context.Background(), ws, originLink)

	// Assert.
	if len(f.slack.replies) != 1 || f.slack.replies[0].thread != originThread || f.slack.replies[0].body != theTask+"\n" {
		t.Fatalf("replies = %+v, want the summary posted into the thread's root", f.slack.replies)
	}
	r, ok := slackRecord(t, f, dlog.LevelInfo)
	if !ok || r.Context["post"] != "opening" || r.Context["message_ts"] != "ts-1" || r.Context["thread_url"] != originLink || r.Context["elapsed_ms"] == nil {
		t.Errorf("record = %+v, want an INFO naming the post, its ts, the thread and its elapsed time", r)
	}
}

func TestTheFirstSummaryIsPostedAsTheOpening(t *testing.T) {
	// Arrange.
	f := originFixture(t)

	// Act.
	f.verbs.SummaryInstalled(context.Background(), "w1", theTask)

	// Assert.
	if len(f.slack.replies) != 1 || f.slack.replies[0].thread != originThread || f.slack.replies[0].body != theTask+"\n" {
		t.Fatalf("replies = %+v, want the summary posted into the thread", f.slack.replies)
	}
}

func TestTheOpeningIsPostedOnce(t *testing.T) {
	tests := []struct {
		name  string
		first func(f *fixture)
	}{
		{name: "a later summary posts nothing", first: func(f *fixture) {
			f.verbs.SummaryInstalled(context.Background(), "w1", theTask)
		}},
		{name: "a summary after the create posted one posts nothing", first: func(f *fixture) {
			delete(f.db.slackOrigins, "w1")
			f.db.summaries = map[ids.WorkspaceID]wsm.WorkspaceSummary{"w1": {Title: theTask}}
			ws, _ := f.db.Workspace(context.Background(), "w1")
			f.verbs.(*verbs).adoptSlackOrigin(context.Background(), ws, originLink)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := originFixture(t)
			tt.first(f)

			// Act.
			f.verbs.SummaryInstalled(context.Background(), "w1", "Fix the footer and its tests.")

			// Assert.
			if len(f.slack.replies) != 1 || f.slack.replies[0].body != theTask+"\n" {
				t.Fatalf("replies = %+v, want only the first summary posted", f.slack.replies)
			}
		})
	}
}

func TestASummaryOfAWorkspaceCreatedForNoThreadPostsNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	f.verbs.SummaryInstalled(context.Background(), "w1", theTask)

	// Assert.
	if len(f.slack.replies) != 0 || len(f.footer.faults) != 0 {
		t.Fatalf("replies %+v, faults %+v; want none", f.slack.replies, f.footer.faults)
	}
}

func TestAdoptSlackOriginOfACreateNoMessageStartedDoesNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())

	// Act.
	f.verbs.(*verbs).adoptSlackOrigin(context.Background(), ws, "")

	// Assert.
	if len(f.db.slackOrigins) != 0 || len(f.db.links["w1"]) != 0 || len(f.slack.replies) != 0 {
		t.Fatalf("origins %+v, links %+v, replies %+v; want nothing", f.db.slackOrigins, f.db.links, f.slack.replies)
	}
}

func TestAdoptSlackOriginFailures(t *testing.T) {
	tests := []struct {
		name       string
		link       string
		arrange    func(f *fixture)
		what       string
		context    dlog.Context
		wantOrigin bool
		wantLinks  int
		wantPosts  int
	}{
		{name: "a link that does not parse records nothing", link: "https://example.com/x",
			what: "the originating thread's link does not parse", context: dlog.Context{"thread_url": "https://example.com/x"}},
		{name: "an origin that will not record stores and posts nothing", link: originLink,
			arrange: func(f *fixture) { f.db.slackOriginErr = errors.New("disk full") },
			what:    "could not record the originating thread", context: dlog.Context{"thread_url": originLink}},
		{name: "a link that will not store posts nothing", link: originLink,
			arrange:    func(f *fixture) { f.db.linksErr = errors.New("disk full") },
			what:       "could not store the originating thread among the links",
			context:    dlog.Context{"thread_url": originLink},
			wantOrigin: true},
		{name: "a summary that will not read posts nothing", link: originLink,
			arrange:    func(f *fixture) { f.db.summaryErr = errors.New("disk gone") },
			what:       "could not read the summary the opening post tells the thread",
			context:    dlog.Context{"thread_url": originLink},
			wantOrigin: true, wantLinks: 1},
		{name: "an opening that will not claim posts nothing", link: originLink,
			arrange: func(f *fixture) {
				f.db.summaries = map[ids.WorkspaceID]wsm.WorkspaceSummary{"w1": {Title: theTask}}
				f.db.slackOpeningErr = errors.New("disk full")
			},
			what:       "could not claim the opening post",
			context:    dlog.Context{"thread_url": originLink},
			wantOrigin: true, wantLinks: 1},
		{name: "an opening Slack rejects leaves the origin and its link standing", link: originLink,
			arrange: func(f *fixture) {
				f.db.summaries = map[ids.WorkspaceID]wsm.WorkspaceSummary{"w1": {Title: theTask}}
				f.slack.err = errors.New("not_in_channel")
			},
			what:       "the originating thread was not posted to",
			context:    dlog.Context{"thread_url": originLink, "post": "opening"},
			wantOrigin: true, wantLinks: 1, wantPosts: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			ws := f.workspace("w1", t.TempDir())
			if tt.arrange != nil {
				tt.arrange(f)
			}

			// Act.
			f.verbs.(*verbs).adoptSlackOrigin(context.Background(), ws, tt.link)

			// Assert.
			assertSlackFailed(t, f, "w1", tt.what, tt.context)
			if _, found := f.db.slackOrigins["w1"]; found != tt.wantOrigin {
				t.Errorf("origin recorded = %v, want %v", found, tt.wantOrigin)
			}
			if len(f.db.links["w1"]) != tt.wantLinks || len(f.slack.replies) != tt.wantPosts {
				t.Errorf("links %+v, replies %+v; want %d and %d", f.db.links["w1"], f.slack.replies, tt.wantLinks, tt.wantPosts)
			}
		})
	}
}

func TestSummaryInstalledFailures(t *testing.T) {
	tests := []struct {
		name    string
		origin  string
		arrange func(f *fixture)
		what    string
		context dlog.Context
	}{
		{name: "an opening that will not claim", origin: originLink,
			arrange: func(f *fixture) { f.db.slackOpeningErr = errors.New("disk full") },
			what:    "could not claim the opening post"},
		{name: "a recorded origin that does not parse", origin: "https://example.com/x",
			what: "the recorded originating thread's link does not parse", context: dlog.Context{"thread_url": "https://example.com/x"}},
		{name: "an opening Slack rejects", origin: originLink,
			arrange: func(f *fixture) { f.slack.err = errors.New("invalid_auth") },
			what:    "the originating thread was not posted to", context: dlog.Context{"thread_url": originLink, "post": "opening"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.slackOrigins = map[ids.WorkspaceID]string{"w1": tt.origin}
			if tt.arrange != nil {
				tt.arrange(f)
			}

			// Act.
			f.verbs.SummaryInstalled(context.Background(), "w1", theTask)

			// Assert.
			assertSlackFailed(t, f, "w1", tt.what, tt.context)
		})
	}
}

func TestCreateWithASlackOriginTakesTheThreadUp(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.SlackThread = originLink

	// Act.
	created, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if err != nil {
		t.Fatalf("Create: %v", err)
	}
	if f.db.slackOrigins[created.ID] != originLink || len(f.slack.replies) != 0 {
		t.Fatalf("origins %+v, replies %+v; want the thread recorded and nothing posted before a summary", f.db.slackOrigins, f.slack.replies)
	}
}

func TestCreateWithoutASlackOriginPostsNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if len(f.slack.replies) != 0 || len(f.db.slackOrigins) != 0 {
		t.Fatalf("origins %+v, replies %+v; want none", f.db.slackOrigins, f.slack.replies)
	}
}

// --- the owned links -----------------------------------------------------

func TestAnOwnedLinkIsPostedAndItsTSRecorded(t *testing.T) {
	tests := []struct {
		name  string
		url   string
		label string
		want  string
	}{
		{name: "a pull request by its label and number", url: pullLink, label: "The fix itself",
			want: "The fix itself [#7](" + pullLink + ")\n"},
		{name: "anything else by its label", url: designLink, label: "The design write-up",
			want: "[The design write-up](" + designLink + ")\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := originFixture(t)

			// Act.
			_, err := f.verbs.SetLink(context.Background(), "w1", tt.url, tt.label, true)

			// Assert.
			if err != nil {
				t.Fatalf("SetLink: %v", err)
			}
			if len(f.slack.replies) != 1 || f.slack.replies[0].thread != originThread || f.slack.replies[0].body != tt.want {
				t.Fatalf("replies = %+v, want %q posted into the thread", f.slack.replies, tt.want)
			}
			want := []wsm.SlackLinkPost{{URL: tt.url, MessageTS: "ts-1"}}
			if got := f.db.slackLinkPosts["w1"]; !reflect.DeepEqual(got, want) {
				t.Errorf("posts = %+v, want %+v", got, want)
			}
			r, ok := slackRecord(t, f, dlog.LevelInfo)
			if !ok || r.Context["post"] != "link" || r.Context["url"] != tt.url || r.Context["message_ts"] != "ts-1" || r.Context["thread_url"] != originLink {
				t.Errorf("record = %+v, want an INFO naming the link post, the link, its ts and the thread", r)
			}
		})
	}
}

func TestARelabelledLinkIsNotPostedAgain(t *testing.T) {
	// Arrange.
	f := originFixture(t)
	if _, err := f.verbs.SetLink(context.Background(), "w1", pullLink, "The fix itself", true); err != nil {
		t.Fatalf("SetLink: %v", err)
	}

	// Act.
	_, err := f.verbs.SetLink(context.Background(), "w1", pullLink, "The fix, reworked", true)

	// Assert.
	if err != nil || len(f.slack.replies) != 1 {
		t.Fatalf("SetLink = %v after replies %+v, want the relabel stored and nothing posted again", err, f.slack.replies)
	}
}

func TestALinkThatIsNotPostedAtAll(t *testing.T) {
	tests := []struct {
		name   string
		origin bool
		owned  bool
	}{
		{name: "a reference link", origin: true},
		{name: "an owned link of a workspace created for no thread", owned: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			if tt.origin {
				f.db.slackOrigins = map[ids.WorkspaceID]string{"w1": originLink}
			}

			// Act.
			_, err := f.verbs.SetLink(context.Background(), "w1", pullLink, "The fix itself", tt.owned)

			// Assert.
			if err != nil || len(f.slack.replies) != 0 || len(f.footer.faults) != 0 {
				t.Fatalf("SetLink = %v after replies %+v, faults %+v; want nothing posted", err, f.slack.replies, f.footer.faults)
			}
		})
	}
}

func TestOwnedLinkPostFailures(t *testing.T) {
	tests := []struct {
		name      string
		origin    string
		arrange   func(f *fixture)
		what      string
		context   dlog.Context
		wantPosts int
	}{
		{name: "a link that will not claim", origin: originLink,
			arrange: func(f *fixture) { f.db.slackClaimErr = errors.New("disk full") },
			what:    "could not claim the owned link's post", context: dlog.Context{"url": pullLink}},
		{name: "a recorded origin that does not parse", origin: "https://example.com/x",
			what: "the recorded originating thread's link does not parse", context: dlog.Context{"url": pullLink, "thread_url": "https://example.com/x"}},
		{name: "a post Slack rejects", origin: originLink,
			arrange: func(f *fixture) { f.slack.err = errors.New("not_in_channel") },
			what:    "the originating thread was not posted to", context: dlog.Context{"url": pullLink, "thread_url": originLink, "post": "link"},
			wantPosts: 1},
		{name: "a ts that will not record", origin: originLink,
			arrange: func(f *fixture) { f.db.slackRecordErr = errors.New("disk full") },
			what:    "could not record the owned link's posted message; its merge is not reacted to",
			context: dlog.Context{"url": pullLink, "thread_url": originLink, "message_ts": "ts-1"}, wantPosts: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.slackOrigins = map[ids.WorkspaceID]string{"w1": tt.origin}
			if tt.arrange != nil {
				tt.arrange(f)
			}

			// Act.
			_, err := f.verbs.SetLink(context.Background(), "w1", pullLink, "The fix itself", true)

			// Assert.
			if err != nil {
				t.Fatalf("SetLink = %v, want the link stored whatever the post did", err)
			}
			assertSlackFailed(t, f, "w1", tt.what, tt.context)
			if len(f.slack.replies) != tt.wantPosts {
				t.Errorf("replies = %+v, want %d", f.slack.replies, tt.wantPosts)
			}
		})
	}
}

// --- the merged reaction -------------------------------------------------

func TestAMergedPullRequestsPostIsReactedTo(t *testing.T) {
	// Arrange.
	f := originFixture(t)
	f.db.slackLinkPosts = map[ids.WorkspaceID][]wsm.SlackLinkPost{"w1": {
		{URL: designLink, MessageTS: "ts-doc"},
		{URL: pullLink, MessageTS: "ts-pr"},
	}}

	// Act.
	f.verbs.PullRequestMerged(context.Background(), "w1", pullKey)

	// Assert.
	want := []slackReaction{{channel: "C1", ts: "ts-pr", emoji: MergedReaction}}
	if !reflect.DeepEqual(f.slack.reactions, want) {
		t.Fatalf("reactions = %+v, want %+v", f.slack.reactions, want)
	}
	r, ok := slackRecord(t, f, dlog.LevelInfo)
	if !ok || r.Context["message_ts"] != "ts-pr" || r.Context["url"] != pullLink || r.Context["thread_url"] != originLink || r.Context["reaction"] != MergedReaction {
		t.Errorf("record = %+v, want an INFO naming the reaction, the message, the link and the thread", r)
	}
}

func TestAMergeIsReactedToOnce(t *testing.T) {
	// Arrange.
	f := originFixture(t)
	f.db.slackLinkPosts = map[ids.WorkspaceID][]wsm.SlackLinkPost{"w1": {{URL: pullLink, MessageTS: "ts-pr"}}}
	f.verbs.PullRequestMerged(context.Background(), "w1", pullKey)

	// Act.
	f.verbs.PullRequestMerged(context.Background(), "w1", pullKey)

	// Assert.
	if len(f.slack.reactions) != 1 {
		t.Fatalf("reactions = %+v, want one", f.slack.reactions)
	}
}

func TestAMergeWhilePostingIsReactedToOnceTheTSIsRecorded(t *testing.T) {
	// Arrange — the pull request merges while its post is claimed and
	// unanswered, so the merge finds no message to react to.
	f := originFixture(t)
	f.slack.onReply = func() { f.verbs.PullRequestMerged(context.Background(), "w1", pullKey) }

	// Act.
	_, err := f.verbs.SetLink(context.Background(), "w1", pullLink, "The fix itself", true)

	// Assert — the ts's record found the merge marked, and reacted.
	if err != nil {
		t.Fatalf("SetLink: %v", err)
	}
	want := []slackReaction{{channel: "C1", ts: "ts-1", emoji: MergedReaction}}
	if !reflect.DeepEqual(f.slack.reactions, want) {
		t.Fatalf("reactions = %+v, want %+v", f.slack.reactions, want)
	}
}

func TestAMergeOfAPullRequestNeverPostedReactsToNothing(t *testing.T) {
	// Arrange.
	f := originFixture(t)
	f.db.slackLinkPosts = map[ids.WorkspaceID][]wsm.SlackLinkPost{"w1": {{URL: "https://github.com/a/b/pull/8", MessageTS: "ts-8"}}}

	// Act.
	f.verbs.PullRequestMerged(context.Background(), "w1", pullKey)

	// Assert.
	if len(f.slack.reactions) != 0 || len(f.footer.faults) != 0 {
		t.Fatalf("reactions %+v, faults %+v; want none", f.slack.reactions, f.footer.faults)
	}
}

func TestPullRequestMergedFailures(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		what    string
		context dlog.Context
	}{
		{name: "posts that will not read",
			arrange: func(f *fixture) { f.db.slackPostsReadErr = errors.New("disk gone") },
			what:    "could not read the posted links of a workspace whose pull request merged",
			context: dlog.Context{"pull_request": pullKey.String()}},
		{name: "a merge that will not mark",
			arrange: func(f *fixture) { f.db.slackMarkErr = errors.New("disk full") },
			what:    "could not mark the posted pull request merged; its merge is not reacted to",
			context: dlog.Context{"pull_request": pullKey.String(), "url": pullLink}},
		{name: "an origin that will not read",
			arrange: func(f *fixture) { f.db.slackOriginReadErr = errors.New("disk gone") },
			what:    "could not read the originating thread of a merged pull request's post",
			context: dlog.Context{"url": pullLink}},
		{name: "a reaction Slack rejects",
			arrange: func(f *fixture) { f.slack.reactErr = errors.New("message_not_found") },
			what:    "the merged pull request's post was not reacted to",
			context: dlog.Context{"url": pullLink, "thread_url": originLink, "message_ts": "ts-pr", "reaction": MergedReaction}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := originFixture(t)
			f.db.slackLinkPosts = map[ids.WorkspaceID][]wsm.SlackLinkPost{"w1": {{URL: pullLink, MessageTS: "ts-pr"}}}
			tt.arrange(f)

			// Act.
			f.verbs.PullRequestMerged(context.Background(), "w1", pullKey)

			// Assert.
			assertSlackFailed(t, f, "w1", tt.what, tt.context)
		})
	}
}

// --- the merge's end -----------------------------------------------------

func TestAFailedDeployIsPostedWithWhereToLook(t *testing.T) {
	// Arrange.
	f := originFixture(t)
	f.db.links = map[ids.WorkspaceID][]wsm.WorkspaceLink{"w1": {
		{URL: pullLink, Label: "The fix itself", Shown: true, Owned: true},
		{URL: originLink, Label: SlackOriginLabel, Shown: true},
	}}

	// Act.
	f.verbs.MergeEnded(context.Background(), "w1", merge.DetachedOutcome{Kind: merge.DetachedFailed, Summary: "the tests failed"})

	// Assert.
	want := slackFailedLead + " " + slackWhereToLook + "\n\n- The `sample` " + slackWorkspaceLeft + "\n- The fix itself [#7](" + pullLink + ")\n"
	if len(f.slack.replies) != 1 || f.slack.replies[0].thread != originThread || f.slack.replies[0].body != want {
		t.Fatalf("replies = %+v, want one reply %q into the thread", f.slack.replies, want)
	}
	r, ok := slackRecord(t, f, dlog.LevelInfo)
	if !ok || r.Context["post"] != "failed" || r.Context["workspace"] != "w1" {
		t.Errorf("record = %+v, want an INFO naming the failed post and the workspace", r)
	}
}

func TestMergeEndedPostsNothing(t *testing.T) {
	tests := []struct {
		name     string
		origin   bool
		out      merge.DetachedOutcome
		wantInfo bool
	}{
		{name: "for a workspace created for no thread", out: merge.DetachedOutcome{Kind: merge.DetachedFailed}},
		{name: "for a landing", origin: true, out: merge.DetachedOutcome{Kind: merge.DetachedLanded}, wantInfo: true},
		{name: "for a merge taken off the queue", origin: true, out: merge.DetachedOutcome{Kind: merge.DetachedAbandoned}, wantInfo: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			if tt.origin {
				f.db.slackOrigins = map[ids.WorkspaceID]string{"w1": originLink}
			}

			// Act.
			f.verbs.MergeEnded(context.Background(), "w1", tt.out)

			// Assert.
			if len(f.slack.replies) != 0 || len(f.footer.faults) != 0 {
				t.Fatalf("replies %+v, faults %+v; want none", f.slack.replies, f.footer.faults)
			}
			if _, ok := slackRecord(t, f, dlog.LevelInfo); ok != tt.wantInfo {
				t.Errorf("INFO record present = %v, want %v", ok, tt.wantInfo)
			}
		})
	}
}

func TestMergeEndedFailures(t *testing.T) {
	tests := []struct {
		name    string
		origin  string
		arrange func(f *fixture)
		what    string
		context dlog.Context
	}{
		{name: "an origin that will not read", origin: originLink,
			arrange: func(f *fixture) { f.db.slackOriginReadErr = errors.New("disk gone") },
			what:    "could not read the originating thread of a workspace whose merge ended"},
		{name: "a recorded origin that does not parse", origin: "https://example.com/x",
			what: "the recorded originating thread's link does not parse", context: dlog.Context{"thread_url": "https://example.com/x"}},
		{name: "a workspace that will not read", origin: originLink,
			arrange: func(f *fixture) { f.db.workspaceErr = errors.New("disk gone") },
			what:    "could not read the workspace to tell the originating thread where to look", context: dlog.Context{"thread_url": originLink}},
		{name: "links that will not read", origin: originLink,
			arrange: func(f *fixture) { f.db.linksReadErr = errors.New("disk gone") },
			what:    "could not read the links to tell the originating thread", context: dlog.Context{"thread_url": originLink}},
		{name: "a post Slack rejects", origin: originLink,
			arrange: func(f *fixture) { f.slack.err = errors.New("invalid_auth") },
			what:    "the originating thread was not posted to", context: dlog.Context{"thread_url": originLink, "post": "failed"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.slackOrigins = map[ids.WorkspaceID]string{"w1": tt.origin}
			if tt.arrange != nil {
				tt.arrange(f)
			}

			// Act.
			f.verbs.MergeEnded(context.Background(), "w1", merge.DetachedOutcome{Kind: merge.DetachedFailed})

			// Assert.
			assertSlackFailed(t, f, "w1", tt.what, tt.context)
		})
	}
}

// --- the bounded call ----------------------------------------------------

func TestTimedSlackBoundsTheCallAndAnswersItsError(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	failure := errors.New("rate_limited")
	var deadline time.Time
	var bounded bool

	// Act.
	_, err := f.verbs.(*verbs).timedSlack(context.Background(), func(ctx context.Context) error {
		deadline, bounded = ctx.Deadline()
		return failure
	})

	// Assert.
	if !errors.Is(err, failure) {
		t.Errorf("err = %v, want the call's own error", err)
	}
	if !bounded || time.Until(deadline) > slackPostTimeout {
		t.Errorf("deadline = %v (set %v), want one within %v", deadline, bounded, slackPostTimeout)
	}
}

// TestEveryPostAndReactionIsBounded pins that the post and the reaction both
// go through timedSlack, so neither can hang the verb that made it.
func TestEveryPostAndReactionIsBounded(t *testing.T) {
	// Arrange.
	f := originFixture(t)
	f.slack.onReply = func() { f.verbs.PullRequestMerged(context.Background(), "w1", pullKey) }

	// Act — a post whose merge lands meanwhile, so it is reacted to too.
	if _, err := f.verbs.SetLink(context.Background(), "w1", pullLink, "The fix itself", true); err != nil {
		t.Fatalf("SetLink: %v", err)
	}

	// Assert.
	if want := []bool{true, true}; !reflect.DeepEqual(f.slack.bounded, want) {
		t.Fatalf("bounded = %v, want the post and the reaction bounded", f.slack.bounded)
	}
}

// --- the bodies ----------------------------------------------------------

func TestFailedBody(t *testing.T) {
	tests := []struct {
		name  string
		links []wsm.WorkspaceLink
		want  string
	}{
		{name: "with nothing owned it names only the workspace",
			links: []wsm.WorkspaceLink{{URL: originLink, Label: SlackOriginLabel}},
			want:  slackFailedLead + " " + slackWhereToLook + "\n\n- The `w` " + slackWorkspaceLeft + "\n"},
		{name: "owned links follow the workspace in order, a pull request by its number",
			links: []wsm.WorkspaceLink{
				{URL: designLink, Label: "The design write-up", Owned: true},
				{URL: originLink, Label: SlackOriginLabel},
				{URL: pullLink, Label: "The fix itself", Owned: true},
			},
			want: slackFailedLead + " " + slackWhereToLook + "\n\n- The `w` " + slackWorkspaceLeft + "\n- [The design write-up](" + designLink + ")\n- The fix itself [#7](" + pullLink + ")\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := FailedBody("w", tt.links)

			// Assert.
			if got != tt.want {
				t.Fatalf("FailedBody = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestEveryComposedPostIsSlackNative pins that the daemon's own bodies pass
// the formatter the poster renders them with, so no composed post is refused.
func TestEveryComposedPostIsSlackNative(t *testing.T) {
	links := []wsm.WorkspaceLink{
		{URL: pullLink, Label: "The fix itself", Owned: true},
		{URL: designLink, Label: "The [draft] write-up", Owned: true},
	}
	for name, body := range map[string]string{
		"pull request": LinkMarkdown("The fix itself", pullLink) + "\n",
		"other link":   LinkMarkdown("The [draft] write-up", designLink) + "\n",
		"failed":       FailedBody("slack-origin-pr-posts", links),
	} {
		t.Run(name, func(t *testing.T) {
			// Act.
			blocks, err := slackfmt.Renderer{}.RenderBlocks(body)

			// Assert.
			if err != nil || len(blocks) == 0 || strings.Contains(body, "agent-repl") {
				t.Fatalf("RenderBlocks(%q) = %v, %v; want blocks and no mention of agent-repl", body, blocks, err)
			}
		})
	}
}
