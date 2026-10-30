package workspace

import (
	"context"
	"errors"
	"reflect"
	"testing"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/topbar"
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
				return v.SetLink(context.Background(), "w1", docLink.URL, docLink.Label, false)
			},
			want: []wsm.WorkspaceLink{docLink}, wantTopbar: []topbar.Link{{Label: docLink.Label, URL: docLink.URL}}, wantLine: "set a link"},
		{name: "an owned set stores the link owned",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", docLink.URL, docLink.Label, true)
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
				return v.SetLink(context.Background(), "w1", "ftp://example.com/x", "An ftp link", false)
			}, arm: ArmInvalidLink, wantFields: map[string]any{"url": "ftp://example.com/x"}},
		{name: "a url carrying whitespace",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", "https://example.com/a b", "A spaced link", false)
			}, arm: ArmInvalidLink, wantFields: map[string]any{"url": "https://example.com/a b"}},
		{name: "a one-word label",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", "https://example.com/x", "Doc", false)
			}, arm: ArmInvalidLabel, wantFields: map[string]any{"label": "Doc"}},
		{name: "a nine-word label",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", "https://example.com/x", "one two three four five six seven eight nine", false)
			}, arm: ArmInvalidLabel, wantFields: map[string]any{"label": "one two three four five six seven eight nine"}},
		{name: "a label of two sentences",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", "https://example.com/x", "The doc. Read it", false)
			}, arm: ArmInvalidLabel, wantFields: map[string]any{"label": "The doc. Read it"}},
		{name: "a label on two lines",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", "https://example.com/x", "The design\ndoc", false)
			}, arm: ArmInvalidLabel, wantFields: map[string]any{"label": "The design\ndoc"}},
		{name: "a label with whitespace at its end",
			act: func(v Verbs) ([]wsm.WorkspaceLink, error) {
				return v.SetLink(context.Background(), "w1", "https://example.com/x", "The design doc ", false)
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
				return v.SetLink(context.Background(), "absent", docLink.URL, docLink.Label, false)
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

func TestLinkVerbsFailLoudlyWhenTheStoreWillNotWrite(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.linksErr = errors.New("disk full")

	// Act.
	_, err := f.verbs.SetLink(context.Background(), "w1", docLink.URL, docLink.Label, false)

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
	ownedArm := &agentreplv1.WorkspaceLinkOwnership{Ownership: &agentreplv1.WorkspaceLinkOwnership_Owned{Owned: &agentreplv1.WorkspaceLinkOwned{}}}
	pull := wsm.WorkspaceLink{URL: "https://github.com/a/b/pull/7", Label: "The fix itself", Shown: true, Owned: true}
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
