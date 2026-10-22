package workspace

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

func TestSetPrincipalLinkRecordsAndRepublishes(t *testing.T) {
	link := "https://github.com/o/r/pull/9"
	tests := []struct {
		name       string
		link       *string
		wantTopbar string
		wantLine   string
	}{
		{name: "a set records the url and hands the title it", link: &link, wantTopbar: link, wantLine: "set the principal link"},
		{name: "a clear records none and hands the title none", link: nil, wantTopbar: "", wantLine: "cleared the principal link"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())

			// Act.
			err := f.verbs.SetPrincipalLink(context.Background(), "w1", tt.link)

			// Assert.
			if err != nil {
				t.Fatalf("SetPrincipalLink: %v", err)
			}
			if got := f.db.workspaces["w1"].PrincipalLink; got != tt.link {
				t.Errorf("recorded link = %v, want %v", got, tt.link)
			}
			if !slices.Equal(f.topbarPrincipalLinks, []string{tt.wantTopbar}) {
				t.Errorf("topbar links = %q, want [%q]", f.topbarPrincipalLinks, tt.wantTopbar)
			}
			r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opSetPrincipalLink})
			if !ok || r.Message != tt.wantLine {
				t.Errorf("record = %+v, want an INFO %s saying %q", r, opSetPrincipalLink, tt.wantLine)
			}
		})
	}
}

func TestSetPrincipalLinkRefusesALinkTheBrowserWouldNotOpen(t *testing.T) {
	tests := []struct {
		name string
		url  string
	}{
		{name: "not http or https", url: "ftp://example.com/x"},
		{name: "carrying whitespace", url: "https://example.com/a b"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())

			// Act.
			err := f.verbs.SetPrincipalLink(context.Background(), "w1", &tt.url)

			// Assert: a typed refusal carrying the url and why, and nothing written.
			refusal := asRefusal(t, err, ArmInvalidLink)
			if refusal.Fields["url"] != tt.url || refusal.Fields["reason"] == "" {
				t.Errorf("refusal fields = %v, want the url %q and a reason", refusal.Fields, tt.url)
			}
			if f.db.workspaces["w1"].PrincipalLink != nil || len(f.topbarPrincipalLinks) != 0 {
				t.Errorf("recorded %v and published %q, want nothing", f.db.workspaces["w1"].PrincipalLink, f.topbarPrincipalLinks)
			}
			r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opRefusal})
			if !ok || r.Context["arm"] != ArmInvalidLink || r.Context["arm_url"] != tt.url {
				t.Errorf("refusal record = %+v, want an INFO refusal naming %s and the url", r, ArmInvalidLink)
			}
		})
	}
}

func TestSetPrincipalLinkFailsLoudlyWhenTheStoreWillNotWrite(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.setPrincipalLinkErr = errors.New("disk full")
	link := "https://example.com/doc"

	// Act.
	err := f.verbs.SetPrincipalLink(context.Background(), "w1", &link)

	// Assert: the error surfaces, is recorded with its context, and the title
	// is not handed a link the store does not hold.
	if err == nil || !errors.Is(err, f.db.setPrincipalLinkErr) {
		t.Fatalf("SetPrincipalLink = %v, want the store's error", err)
	}
	if len(f.topbarPrincipalLinks) != 0 {
		t.Errorf("topbar links = %q, want none", f.topbarPrincipalLinks)
	}
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opSetPrincipalLink})
	if !ok || r.Context["url"] != link || r.Context["cause"] == nil {
		t.Errorf("record = %+v, want an ERROR %s naming the url and the cause", r, opSetPrincipalLink)
	}
}

func TestSetPrincipalLinkOfAnUnknownWorkspaceIsRefused(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	link := "https://example.com"

	// Act.
	err := f.verbs.SetPrincipalLink(context.Background(), ids.WorkspaceID("absent"), &link)

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
	if len(f.topbarPrincipalLinks) != 0 {
		t.Errorf("topbar links = %q, want none", f.topbarPrincipalLinks)
	}
}
