package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

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
)

var originThread = slackpost.Thread{URL: originLink, Channel: "C1", TS: "1730000000.123456"}

// slackRecord answers the slack-origin record at LEVEL.
func slackRecord(t *testing.T, f *fixture, level string) (dlog.Record, bool) {
	t.Helper()
	return dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: level, Operation: opSlackOrigin})
}

// assertSlackFailed asserts the one ERROR record naming WHAT with a cause, and
// the transient footer line on WS.
func assertSlackFailed(t *testing.T, f *fixture, ws ids.WorkspaceID, what string) {
	t.Helper()
	r, ok := slackRecord(t, f, dlog.LevelError)
	if !ok || r.Message != what || r.Context["workspace"] != string(ws) || r.Context["cause"] == nil {
		t.Errorf("record = %+v, want an ERROR %s saying %q with the workspace and the cause", r, opSlackOrigin, what)
	}
	if len(f.footer.faults) != 1 || f.footer.faults[0].Kind != FaultKindSlackPostFailed || f.footer.faults[0].Status != "" || f.footer.faults[0].Detail != what {
		t.Errorf("footer faults = %+v, want one transient %s saying %q", f.footer.faults, FaultKindSlackPostFailed, what)
	}
}

func TestAdoptSlackOriginRecordsLinksAndPostsTheBeginning(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	f.slack.permalink = "https://w.slack.com/archives/C1/p1730000100000000"

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
	if len(f.slack.replies) != 1 || f.slack.replies[0].thread != originThread || f.slack.replies[0].body != slackBegunBody {
		t.Errorf("replies = %+v, want the beginning posted into the thread's root", f.slack.replies)
	}
	r, ok := slackRecord(t, f, dlog.LevelInfo)
	if !ok || r.Context["post"] != "begun" || r.Context["permalink"] != f.slack.permalink || r.Context["elapsed_ms"] == nil {
		t.Errorf("record = %+v, want an INFO naming the post, its permalink and its elapsed time", r)
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
		wantOrigin bool
		wantLinks  int
		wantPosts  int
	}{
		{name: "a link that does not parse records nothing", link: "https://example.com/x",
			what: "the originating thread's link does not parse"},
		{name: "an origin that will not record stores and posts nothing", link: originLink,
			arrange: func(f *fixture) { f.db.slackOriginErr = errors.New("disk full") },
			what:    "could not record the originating thread"},
		{name: "a link that will not store posts nothing", link: originLink,
			arrange:    func(f *fixture) { f.db.linksErr = errors.New("disk full") },
			what:       "could not store the originating thread among the links",
			wantOrigin: true},
		{name: "a post Slack rejects leaves the origin and its link standing", link: originLink,
			arrange:    func(f *fixture) { f.slack.err = errors.New("not_in_channel") },
			what:       "the originating thread was not posted to",
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
			assertSlackFailed(t, f, "w1", tt.what)
			if _, found := f.db.slackOrigins["w1"]; found != tt.wantOrigin {
				t.Errorf("origin recorded = %v, want %v", found, tt.wantOrigin)
			}
			if len(f.db.links["w1"]) != tt.wantLinks || len(f.slack.replies) != tt.wantPosts {
				t.Errorf("links %+v, replies %+v; want %d and %d", f.db.links["w1"], f.slack.replies, tt.wantLinks, tt.wantPosts)
			}
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
	if f.db.slackOrigins[created.ID] != originLink || len(f.slack.replies) != 1 || f.slack.replies[0].body != slackBegunBody {
		t.Fatalf("origins %+v, replies %+v; want the thread recorded and told work began", f.db.slackOrigins, f.slack.replies)
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

func TestMergeEndedPostsTheEndWithTheOwnedLinks(t *testing.T) {
	tests := []struct {
		name string
		out  merge.DetachedOutcome
		want string
	}{
		{name: "a landing", out: merge.DetachedOutcome{Kind: merge.DetachedLanded},
			want: slackLandedLead + " " + slackOwnedHeading + "\n\n- The fix itself [#7](" + pullLink + ")\n"},
		{name: "a failure", out: merge.DetachedOutcome{Kind: merge.DetachedFailed, Summary: "the tests failed"},
			want: slackFailedLead + " " + slackOwnedHeading + "\n\n- The fix itself [#7](" + pullLink + ")\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.slackOrigins = map[ids.WorkspaceID]string{"w1": originLink}
			f.db.links = map[ids.WorkspaceID][]wsm.WorkspaceLink{"w1": {
				{URL: pullLink, Label: "The fix itself", Shown: true, Owned: true},
				{URL: originLink, Label: SlackOriginLabel, Shown: true},
			}}

			// Act.
			f.verbs.MergeEnded(context.Background(), "w1", tt.out)

			// Assert.
			if len(f.slack.replies) != 1 || f.slack.replies[0].thread != originThread || f.slack.replies[0].body != tt.want {
				t.Fatalf("replies = %+v, want one reply %q into the thread", f.slack.replies, tt.want)
			}
			r, ok := slackRecord(t, f, dlog.LevelInfo)
			if !ok || r.Context["post"] != "ended" || r.Context["workspace"] != "w1" {
				t.Errorf("record = %+v, want an INFO naming the ended post and the workspace", r)
			}
		})
	}
}

func TestMergeEndedPostsNothing(t *testing.T) {
	tests := []struct {
		name     string
		origin   bool
		out      merge.DetachedOutcome
		wantInfo bool
	}{
		{name: "for a workspace created for no thread", out: merge.DetachedOutcome{Kind: merge.DetachedLanded}},
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
	}{
		{name: "an origin that will not read", origin: originLink,
			arrange: func(f *fixture) { f.db.slackOriginReadErr = errors.New("disk gone") },
			what:    "could not read the originating thread of a workspace whose merge ended"},
		{name: "a recorded origin that does not parse", origin: "https://example.com/x",
			what: "the recorded originating thread's link does not parse"},
		{name: "links that will not read", origin: originLink,
			arrange: func(f *fixture) { f.db.linksReadErr = errors.New("disk gone") },
			what:    "could not read the links to tell the originating thread"},
		{name: "a post Slack rejects", origin: originLink,
			arrange: func(f *fixture) { f.slack.err = errors.New("invalid_auth") },
			what:    "the originating thread was not posted to"},
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
			f.verbs.MergeEnded(context.Background(), "w1", merge.DetachedOutcome{Kind: merge.DetachedLanded})

			// Assert.
			assertSlackFailed(t, f, "w1", tt.what)
		})
	}
}

func TestEndBody(t *testing.T) {
	tests := []struct {
		name  string
		out   merge.DetachedOutcome
		links []wsm.WorkspaceLink
		want  string
	}{
		{name: "a landing with nothing owned says only that it is done",
			out:   merge.DetachedOutcome{Kind: merge.DetachedLanded},
			links: []wsm.WorkspaceLink{{URL: originLink, Label: SlackOriginLabel}},
			want:  slackLandedLead + "\n"},
		{name: "a failure with nothing owned says only that it stopped",
			out:  merge.DetachedOutcome{Kind: merge.DetachedFailed},
			want: slackFailedLead + "\n"},
		{name: "owned links are bulleted in order, a pull request by its number",
			out: merge.DetachedOutcome{Kind: merge.DetachedLanded},
			links: []wsm.WorkspaceLink{
				{URL: "https://docs.example.com/d", Label: "The design write-up", Owned: true},
				{URL: originLink, Label: SlackOriginLabel},
				{URL: pullLink, Label: "The fix itself", Owned: true},
			},
			want: slackLandedLead + " " + slackOwnedHeading + "\n\n- [The design write-up](https://docs.example.com/d)\n- The fix itself [#7](" + pullLink + ")\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := EndBody(tt.out, tt.links)

			// Assert.
			if got != tt.want {
				t.Fatalf("EndBody = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestEveryComposedPostIsSlackNative pins that the daemon's own bodies pass
// the formatter the poster renders them with, so no composed post is refused.
func TestEveryComposedPostIsSlackNative(t *testing.T) {
	links := []wsm.WorkspaceLink{
		{URL: pullLink, Label: "The fix itself", Owned: true},
		{URL: "https://docs.example.com/d", Label: "The [draft] write-up", Owned: true},
	}
	for name, body := range map[string]string{
		"begun":  slackBegunBody,
		"landed": EndBody(merge.DetachedOutcome{Kind: merge.DetachedLanded}, links),
		"failed": EndBody(merge.DetachedOutcome{Kind: merge.DetachedFailed}, links),
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
