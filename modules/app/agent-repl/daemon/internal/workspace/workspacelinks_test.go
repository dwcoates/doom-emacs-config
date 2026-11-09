package workspace

import (
	"context"
	"errors"
	"reflect"
	"testing"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/topbar"
	"agent-repld/internal/textline"
	"agent-repld/internal/wsm"
)

var (
	docLink    = wsm.WorkspaceLink{URL: "https://example.com/doc", Label: "The design doc", Shown: true}
	ticketLink = wsm.WorkspaceLink{URL: "https://example.com/ticket", Label: "The tracking ticket", Shown: true}
)

func TestLinkVerbsRecordAndRepublish(t *testing.T) {
	tests := []struct {
		name       string
		standing   []wsm.WorkspaceLink
		act        func(v Verbs) ([]wsm.WorkspaceLink, error)
		want       []wsm.WorkspaceLink
		wantTopbar []topbar.Link
		wantLine   string
	}{
		{name: "a set stores the link and hands the title it",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", docLink.URL, docLink.Label, false, "")
			},
			want: []wsm.WorkspaceLink{docLink}, wantTopbar: []topbar.Link{{Label: docLink.Label, URL: docLink.URL}}, wantLine: "set a link"},
		{name: "an owned set stores the link owned",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", docLink.URL, docLink.Label, true, "")
			},
			want: []wsm.WorkspaceLink{{URL: docLink.URL, Label: docLink.Label, Shown: true, Owned: true}}, wantTopbar: []topbar.Link{{Label: docLink.Label, URL: docLink.URL}}, wantLine: "set a link"},
		{name: "a removal hands the title the rest", standing: []wsm.WorkspaceLink{ticketLink, docLink},
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.RemoveLink(context.Background(), "w1", ticketLink.URL)
			},
			want: []wsm.WorkspaceLink{docLink}, wantTopbar: []topbar.Link{{Label: docLink.Label, URL: docLink.URL}}, wantLine: "removed a link"},
		{name: "a show hands the title only the shown links", standing: []wsm.WorkspaceLink{ticketLink, docLink},
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.ShowLinks(context.Background(), "w1", []string{docLink.URL})
			},
			want:       []wsm.WorkspaceLink{{URL: ticketLink.URL, Label: ticketLink.Label}, docLink},
			wantTopbar: []topbar.Link{{Label: docLink.Label, URL: docLink.URL}}, wantLine: "chose the shown links"},
		{name: "a clear hands the title none", standing: []wsm.WorkspaceLink{docLink},
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.ClearLinks(context.Background(), "w1")
			},
			want: nil, wantTopbar: nil, wantLine: "cleared the links"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.links = map[ids.WorkspaceID][]wsm.WorkspaceLink{"w1": tt.standing}

			// Act.
			got, err := tt.act(f.verbs)

			// Assert.
			if err != nil {
				t.Fatalf("verb: %v", err)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("answered links = %+v, want %+v", got, tt.want)
			}
			if !reflect.DeepEqual(f.topbarLinks, [][]topbar.Link{tt.wantTopbar}) {
				t.Errorf("topbar links = %+v, want [%+v]", f.topbarLinks, tt.wantTopbar)
			}
			r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opChangeLinks})
			if !ok || r.Message != tt.wantLine || r.Context["workspace"] != "w1" {
				t.Errorf("record = %+v, want an INFO %s saying %q and naming the workspace", r, opChangeLinks, tt.wantLine)
			}
		})
	}
}

func TestLinkVerbsRefuse(t *testing.T) {
	tests := []struct {
		name       string
		act        func(v Verbs) ([]wsm.WorkspaceLink, error)
		arm        string
		wantFields map[string]any
	}{
		{name: "a url that is not http or https",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", "ftp://example.com/x", "An ftp link", false, "")
			}, arm: ArmInvalidLink, wantFields: map[string]any{"url": "ftp://example.com/x"}},
		{name: "a url carrying whitespace",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", "https://example.com/a b", "A spaced link", false, "")
			}, arm: ArmInvalidLink, wantFields: map[string]any{"url": "https://example.com/a b"}},
		{name: "a one-word label",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", "https://example.com/x", "Doc", false, "")
			}, arm: ArmInvalidLabel, wantFields: map[string]any{"label": "Doc"}},
		{name: "a nine-word label",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", "https://example.com/x", "one two three four five six seven eight nine", false, "")
			}, arm: ArmInvalidLabel, wantFields: map[string]any{"label": "one two three four five six seven eight nine"}},
		{name: "a label of two sentences",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", "https://example.com/x", "The doc. Read it", false, "")
			}, arm: ArmInvalidLabel, wantFields: map[string]any{"label": "The doc. Read it"}},
		{name: "a label on two lines",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", "https://example.com/x", "The design\ndoc", false, "")
			}, arm: ArmInvalidLabel, wantFields: map[string]any{"label": "The design\ndoc"}},
		{name: "a label with whitespace at its end",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", "https://example.com/x", "The design doc ", false, "")
			}, arm: ArmInvalidLabel, wantFields: map[string]any{"label": "The design doc "}},
		{name: "removing a link the workspace does not hold",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.RemoveLink(context.Background(), "w1", "https://example.com/absent")
			}, arm: ArmUnknownLink, wantFields: map[string]any{"url": "https://example.com/absent"}},
		{name: "showing a link the workspace does not hold",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.ShowLinks(context.Background(), "w1", []string{"https://example.com/absent"})
			}, arm: ArmUnknownLink, wantFields: map[string]any{"url": "https://example.com/absent"}},
		{name: "showing more links than the menu shows",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.ShowLinks(context.Background(), "w1", []string{"a", "b", "c", "d", "e", "f"})
			}, arm: ArmTooManyShown, wantFields: map[string]any{"named": uint32(6), "limit": uint32(5)}},
		{name: "a workspace that is not registered",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "absent", docLink.URL, docLink.Label, false, "")
			}, arm: ArmUnknownWorkspace, wantFields: map[string]any{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.links = map[ids.WorkspaceID][]wsm.WorkspaceLink{"w1": {docLink}}

			// Act.
			got, err := tt.act(f.verbs)

			// Assert: a typed refusal carrying its fields, nothing written, and
			// nothing published.
			refusal := asRefusal(t, err, tt.arm)
			for name, want := range tt.wantFields {
				if refusal.Fields[name] != want {
					t.Errorf("refusal field %s = %v, want %v", name, refusal.Fields[name], want)
				}
			}
			if got != nil {
				t.Errorf("answered links = %+v, want none", got)
			}
			if !reflect.DeepEqual(f.db.links["w1"], []wsm.WorkspaceLink{docLink}) || len(f.topbarLinks) != 0 {
				t.Errorf("links = %+v and published %+v, want the standing link untouched and nothing published", f.db.links["w1"], f.topbarLinks)
			}
			if tt.arm == ArmUnknownWorkspace {
				return
			}
			r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opRefusal})
			if !ok || r.Context["arm"] != tt.arm || r.Context["rpc"] != rpcChangeWorkspaceLinks {
				t.Errorf("refusal record = %+v, want an INFO refusal of %s naming %s", r, rpcChangeWorkspaceLinks, tt.arm)
			}
		})
	}
}

func TestOwningAPullRequestAnotherWorkspaceOwnsIsRefusedNamingTheOwner(t *testing.T) {
	const url = "https://github.com/o/r/pull/7"
	tests := []struct {
		name string
		// ownerRegistered is whether the owner's row can be read.
		ownerRegistered bool
	}{
		{name: "the refusal names the owner", ownerRegistered: true},
		{name: "an owner whose row cannot be read fails loudly"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			if tt.ownerRegistered {
				owner := f.workspace("w2", t.TempDir())
				owner.Name = "owner"
				f.db.with(owner)
			}
			f.db.linksErr = &wsm.PullRequestOwnedElsewhereError{PullRequest: wsm.PullRequestKey{Owner: "o", Repo: "r", Number: 7}, Owner: "w2", OwnerName: "owner"}

			// Act
			_, err := f.verbs.SetLink(context.Background(), "w1", url, "The pull request", false, "")

			// Assert
			if !tt.ownerRegistered {
				if err == nil || !errors.Is(err, wsm.ErrNotFound) {
					t.Fatalf("SetLink = %v, want the owner's read failure", err)
				}
				r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opChangeLinks})
				if !ok || r.Context["owner"] != "w2" || r.Context["pull_request"] != "o/r#7" || r.Context["cause"] == nil {
					t.Fatalf("record = %+v, want an ERROR naming the owner, the pull request and the cause", r)
				}
				return
			}
			refusal := asRefusal(t, err, ArmPullRequestOwnedElsewhere)
			owner, _ := refusal.Fields["owner"].(*workspacev1.WorkspaceRef)
			if refusal.Fields["url"] != url || refusal.Fields["owner_name"] != "owner" || owner.GetId() != "w2" {
				t.Fatalf("refusal fields = %+v, want the url, and the owner by ref and name", refusal.Fields)
			}
			if len(f.topbarLinks) != 0 {
				t.Fatalf("topbar links = %+v, want nothing published", f.topbarLinks)
			}
		})
	}
}

func TestLinkVerbsFailLoudlyWhenTheStoreWillNotWrite(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.linksErr = errors.New("disk full")

	// Act.
	_, err := f.verbs.SetLink(context.Background(), "w1", docLink.URL, docLink.Label, false, "")

	// Assert: the error surfaces, is recorded with its context, and the title
	// is not handed links the store does not hold.
	if !errors.Is(err, f.db.linksErr) {
		t.Fatalf("SetLink = %v, want the store's error", err)
	}
	if len(f.topbarLinks) != 0 {
		t.Errorf("topbar links = %+v, want none", f.topbarLinks)
	}
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opChangeLinks})
	if !ok || r.Context["url"] != docLink.URL || r.Context["workspace"] != "w1" || r.Context["cause"] == nil {
		t.Errorf("record = %+v, want an ERROR %s naming the workspace, the url and the cause", r, opChangeLinks)
	}
}

func TestValidLinkLabel(t *testing.T) {
	tests := []struct {
		name  string
		label string
		ok    bool
	}{
		{name: "two words", label: "Design doc", ok: true},
		{name: "eight words", label: "one two three four five six seven eight", ok: true},
		{name: "a closing period", label: "The design doc.", ok: true},
		{name: "a dotted word inside", label: "The v1.2 release notes", ok: true},
		{name: "one word", label: "Doc"},
		{name: "nine words", label: "one two three four five six seven eight nine"},
		{name: "a second sentence after a question", label: "Why? Because of this"},
		{name: "a second sentence after an exclamation", label: "Read this! Then that"},
		{name: "a line break", label: "Design\ndoc"},
		{name: "leading whitespace", label: " Design doc"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			err := validLinkLabel(tt.label)

			// Assert.
			if (err == nil) != tt.ok {
				t.Fatalf("validLinkLabel(%q) = %v, want ok=%v", tt.label, err, tt.ok)
			}
		})
	}
}

func TestValidLinkLabelIsTheSharedOneSentenceRule(t *testing.T) {
	// Arrange — a label breaking the shared rule.
	label := "Read this! Then that"

	// Act
	got := validLinkLabel(label)

	// Assert — the refusal is textline.OneSentence's own, never a second
	// spelling of the rule.
	want := textline.OneSentence("label", label, minLinkLabelWords, maxLinkLabelWords)
	if got == nil || want == nil || got.Error() != want.Error() {
		t.Fatalf("validLinkLabel = %v, want textline.OneSentence's %v", got, want)
	}
}

func TestShownLinksKeepsTheShownInOrder(t *testing.T) {
	// Arrange.
	unshown := wsm.WorkspaceLink{URL: "https://example.com/old", Label: "An older draft"}

	// Act.
	got := ShownLinks([]wsm.WorkspaceLink{ticketLink, unshown, docLink})

	// Assert.
	want := []topbar.Link{{Label: ticketLink.Label, URL: ticketLink.URL}, {Label: docLink.Label, URL: docLink.URL}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("ShownLinks = %+v, want %+v", got, want)
	}
}

func TestLinksView(t *testing.T) {
	unshown := wsm.WorkspaceLink{URL: "https://example.com/old", Label: "An older draft"}
	shownArm := &agentreplv1.WorkspaceLinkPlacement{Placement: &agentreplv1.WorkspaceLinkPlacement_Shown{Shown: &agentreplv1.WorkspaceLinkPlacementShown{}}}
	unshownArm := &agentreplv1.WorkspaceLinkPlacement{Placement: &agentreplv1.WorkspaceLinkPlacement_Unshown{Unshown: &agentreplv1.WorkspaceLinkPlacementUnshown{}}}
	referenceArm := &agentreplv1.WorkspaceLinkOwnership{Ownership: &agentreplv1.WorkspaceLinkOwnership_Reference{Reference: &agentreplv1.WorkspaceLinkReference{}}}
	// An owned link carries how the daemon follows it: this pull request is
	// owned and holds no subscription.
	ownedArm := &agentreplv1.WorkspaceLinkOwnership{Ownership: &agentreplv1.WorkspaceLinkOwnership_Owned{Owned: &agentreplv1.WorkspaceLinkOwned{
		Follow: &agentreplv1.WorkspaceLinkOwnedFollow{Standing: &agentreplv1.WorkspaceLinkOwnedFollow_Unsubscribed{Unsubscribed: &agentreplv1.WorkspaceLinkOwnedFollowUnsubscribed{}}}}}}
	pull := wsm.WorkspaceLink{URL: "https://github.com/a/b/pull/7", Label: "The fix itself", Shown: true, Owned: true,
		PullRequest: &wsm.PullRequestKey{Owner: "a", Repo: "b", Number: 7}}
	five := []wsm.WorkspaceLink{docLink, docLink, docLink, docLink, docLink}
	tests := []struct {
		name  string
		links []wsm.WorkspaceLink
		want  *agentreplv1.WorkspaceLinks
	}{
		{name: "no links leave the whole menu free", links: nil,
			want: &agentreplv1.WorkspaceLinks{Links: []*agentreplv1.WorkspaceLink{},
				Menu: &agentreplv1.WorkspaceLinksMenu{Standing: &agentreplv1.WorkspaceLinksMenu_Room{Room: &agentreplv1.WorkspaceLinksMenuRoom{Free: 5}}}}},
		{name: "each link carries its placement, and room counts only the shown", links: []wsm.WorkspaceLink{docLink, unshown},
			want: &agentreplv1.WorkspaceLinks{Links: []*agentreplv1.WorkspaceLink{
				{Url: docLink.URL, Label: docLink.Label, Placement: shownArm, Ownership: referenceArm},
				{Url: unshown.URL, Label: unshown.Label, Placement: unshownArm, Ownership: referenceArm}},
				Menu: &agentreplv1.WorkspaceLinksMenu{Standing: &agentreplv1.WorkspaceLinksMenu_Room{Room: &agentreplv1.WorkspaceLinksMenuRoom{Free: 4}}}}},
		{name: "five shown links fill the menu", links: five,
			want: func() *agentreplv1.WorkspaceLinks {
				out := LinksView(five[:4])
				out.Links = append(out.Links, &agentreplv1.WorkspaceLink{Url: docLink.URL, Label: docLink.Label, Placement: shownArm, Ownership: referenceArm})
				out.Menu = &agentreplv1.WorkspaceLinksMenu{Standing: &agentreplv1.WorkspaceLinksMenu_Full{Full: &agentreplv1.WorkspaceLinksMenuFull{Limit: 5}}}
				return out
			}()},
		{name: "an owned link carries the owned arm", links: []wsm.WorkspaceLink{pull},
			want: &agentreplv1.WorkspaceLinks{Links: []*agentreplv1.WorkspaceLink{
				{Url: pull.URL, Label: pull.Label, Placement: shownArm, Ownership: ownedArm}},
				Menu: &agentreplv1.WorkspaceLinksMenu{Standing: &agentreplv1.WorkspaceLinksMenu_Room{Room: &agentreplv1.WorkspaceLinksMenuRoom{Free: 4}}}}},
		{name: "a full menu counts the unshown", links: append([]wsm.WorkspaceLink{unshown}, five...),
			want: func() *agentreplv1.WorkspaceLinks {
				out := LinksView(append([]wsm.WorkspaceLink{unshown}, five...))
				out.Menu = &agentreplv1.WorkspaceLinksMenu{Standing: &agentreplv1.WorkspaceLinksMenu_Full{Full: &agentreplv1.WorkspaceLinksMenuFull{Limit: 5, Unshown: 1}}}
				return out
			}()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := LinksView(tt.links)

			// Assert.
			if !proto.Equal(got, tt.want) {
				t.Fatalf("LinksView = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestSetLinkFollowsAnOwnedPullRequest(t *testing.T) {
	prURL := "https://github.com/ChessCom/explanation-engine/pull/42"
	pr := wsm.PullRequestKey{Owner: "ChessCom", Repo: "explanation-engine", Number: 42}
	tests := []struct {
		name             string
		url              string
		owned            bool
		held             bool
		anchorErr        error
		wantPR           bool
		wantAnchor       bool
		wantSub          bool
		wantTold         string
		wantUnreadRecord bool
	}{
		{name: "an owned pull request is anchored and subscribed", url: prURL, owned: true, wantPR: true, wantAnchor: true, wantSub: true, wantTold: "anchored"},
		{name: "an owned pull request already followed reads nothing", url: prURL, owned: true, held: true, wantPR: true},
		{name: "an owned pull request that cannot be read is subscribed unanchored", url: prURL, owned: true, anchorErr: errors.New("down"),
			wantPR: true, wantAnchor: true, wantSub: true, wantUnreadRecord: true},
		{name: "a referenced pull request names it and subscribes nothing", url: prURL, wantPR: true},
		{name: "an owned link that is no pull request subscribes nothing", url: docLink.URL, owned: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.anchors.err = tt.anchorErr
			if tt.held {
				f.db.prSubs = map[ids.WorkspaceID][]wsm.PullRequestSubscription{"w1": {{Subscriber: "w1", PullRequest: pr}}}
			}

			// Act.
			_, err := f.verbs.SetLink(context.Background(), "w1", tt.url, "The pull request", tt.owned, "")

			// Assert.
			if err != nil || len(f.db.linkSets) != 1 {
				t.Fatalf("SetLink = %v with %d sets, want one set written", err, len(f.db.linkSets))
			}
			set := f.db.linkSets[0]
			if set.Owned != tt.owned || (set.PullRequest != nil) != tt.wantPR || (set.PullRequest != nil && *set.PullRequest != pr) {
				t.Errorf("set = %+v, want owned %v naming the pull request %v", set, tt.owned, tt.wantPR)
			}
			if (len(f.anchors.asked) == 1) != tt.wantAnchor || (set.Subscribe != nil) != tt.wantSub {
				t.Errorf("anchored %v, subscribe %+v; want anchored %v subscribing %v", f.anchors.asked, set.Subscribe, tt.wantAnchor, tt.wantSub)
			}
			if tt.wantSub && (set.Subscribe.Told != tt.wantTold || set.Subscribe.Origin != wsm.PullRequestOriginOwnedLink || set.Subscribe.Events != wsm.AllPullRequestEvents()) {
				t.Errorf("subscription = %+v, want every event for the owned link, told %q", set.Subscribe, tt.wantTold)
			}
			_, recorded := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opChangeLinks,
				Message: "the owned pull request could not be read, so it is followed unanchored: its first successful read tells what is already on it"})
			if recorded != tt.wantUnreadRecord {
				t.Errorf("unread record %v, want %v", recorded, tt.wantUnreadRecord)
			}
		})
	}
}

func TestLinksViewSaysHowAnOwnedLinkIsFollowed(t *testing.T) {
	pr := &wsm.PullRequestKey{Owner: "o", Repo: "r", Number: 1}
	tests := []struct {
		name string
		link wsm.WorkspaceLink
		arm  string
	}{
		{name: "a reference", link: wsm.WorkspaceLink{URL: "u", PullRequest: pr}, arm: "reference"},
		{name: "owned and followed", link: wsm.WorkspaceLink{URL: "u", Owned: true, PullRequest: pr,
			Follow: &wsm.PullRequestSubscription{PullRequest: *pr, Events: wsm.AllPullRequestEvents(), Origin: wsm.PullRequestOriginOwnedLink}}, arm: "following"},
		{name: "owned and ended", link: wsm.WorkspaceLink{URL: "u", Owned: true, PullRequest: pr,
			Follow: &wsm.PullRequestSubscription{PullRequest: *pr, Ended: &wsm.PullRequestEnd{Merged: true}}}, arm: "ended"},
		{name: "owned and unsubscribed", link: wsm.WorkspaceLink{URL: "u", Owned: true, PullRequest: pr}, arm: "unsubscribed"},
		{name: "owned and no pull request", link: wsm.WorkspaceLink{URL: "u", Owned: true}, arm: "not_followable"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			rel := LinksView([]wsm.WorkspaceLink{tt.link}).GetLinks()[0].GetOwnership()

			// Assert.
			got := "reference"
			if owned := rel.GetOwned(); owned != nil {
				got = string(owned.GetFollow().ProtoReflect().WhichOneof(owned.GetFollow().ProtoReflect().Descriptor().Oneofs().ByName("standing")).Name())
			}
			if got != tt.arm {
				t.Fatalf("ownership = %v, want %s", rel, tt.arm)
			}
		})
	}
}

// slackPost is a Slack post announcing docLink, as a finish stage stores it.
const slackPost = "https://acme.slack.com/archives/C1/p1730000000123456"

func TestAnOwnedLinkAnnouncesAStoredLink(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.links = map[ids.WorkspaceID][]wsm.WorkspaceLink{"w1": {docLink}}

	// Act
	links, err := f.verbs.SetLink(context.Background(), "w1", slackPost, "The pull request's announcement", true, docLink.URL)

	// Assert
	if err != nil {
		t.Fatalf("SetLink: %v", err)
	}
	if links[0].URL != slackPost || links[0].Announces != docLink.URL {
		t.Fatalf("links = %+v, want the post announcing the doc", links)
	}
	view := LinksView(links)
	if got := view.GetLinks()[0].GetOwnership().GetOwned().GetAnnounces().GetUrl(); got != docLink.URL {
		t.Fatalf("view = %v, want the announcement read back as owned.announces", view.GetLinks()[0])
	}
	if view.GetLinks()[1].GetOwnership().GetOwned().GetAnnounces() != nil {
		t.Fatalf("view = %v, want the announced link to announce nothing", view.GetLinks()[1])
	}
}

func TestAnAnnouncementOfAnUnheldLinkIsRefused(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	_, err := f.verbs.SetLink(context.Background(), "w1", slackPost, "The pull request's announcement", true, docLink.URL)

	// Assert
	refusal, refused := AsRefusal(err)
	if !refused || refusal.Arm != ArmUnknownAnnouncedLink || refusal.Fields["url"] != docLink.URL {
		t.Fatalf("SetLink = %v, want unknown_announced_link naming the url", err)
	}
	if len(f.db.links["w1"]) != 0 {
		t.Fatalf("links = %+v, want nothing stored", f.db.links["w1"])
	}
}
