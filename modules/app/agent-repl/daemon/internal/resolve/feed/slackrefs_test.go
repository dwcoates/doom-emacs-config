package feed

import (
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/prose"
)

// fakeSlackChannels answers a fixed set of channel ids, counting asks.
type fakeSlackChannels struct {
	ids  map[string]string
	asks int
}

func (f *fakeSlackChannels) ChannelIDs() map[string]string {
	f.asks++
	return f.ids
}

const linkedChannel = "[#cee_and_ceac_integration](https://acme.slack.com/archives/C0123ABCD)"

func TestDrawLinksLinksSlackChannels(t *testing.T) {
	known := map[string]string{"cee_and_ceac_integration": "C0123ABCD"}
	cases := []struct {
		name      string
		source    *fakeSlackChannels
		subdomain string
		in        string
		arriving  bool
		want      string
	}{
		{name: "a known channel is linked", source: &fakeSlackChannels{ids: known}, subdomain: "acme",
			in: "post in #cee_and_ceac_integration please", want: "post in " + linkedChannel + " please"},
		{name: "a pull request beside a channel is linked too", source: &fakeSlackChannels{ids: known}, subdomain: "acme",
			in: "#12 in #cee_and_ceac_integration", want: linked12 + " in " + linkedChannel},
		{name: "an arriving text leaves its last channel", source: &fakeSlackChannels{ids: known}, subdomain: "acme",
			in: "post in #cee_and_ceac_integration", arriving: true, want: "post in #cee_and_ceac_integration"},
		{name: "no channels read yet leaves channels plain", source: &fakeSlackChannels{}, subdomain: "acme",
			in: "post in #cee_and_ceac_integration please", want: "post in #cee_and_ceac_integration please"},
		{name: "no subdomain leaves channels plain", source: &fakeSlackChannels{ids: known},
			in: "post in #cee_and_ceac_integration please", want: "post in #cee_and_ceac_integration please"},
		{name: "no source leaves channels plain", subdomain: "acme",
			in: "post in #cee_and_ceac_integration please", want: "post in #cee_and_ceac_integration please"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.resolver.SetPullTarget(testWorkspace, chessComTarget)
			if tc.source != nil {
				h.resolver.deps.SlackChannels = tc.source
			}
			h.resolver.deps.SlackSubdomain = tc.subdomain
			parsed := prose.ParsePrompt(dlog.NewTestLogger(), tc.in)

			// Act.
			h.resolver.drawLinks(testWorkspace, parsed, tc.arriving)

			// Assert.
			if got := plainMarkdown(t, parsed); got != tc.want {
				t.Fatalf("drawn markdown = %q, want %q", got, tc.want)
			}
			if got := parsed.GetSource().GetMarkdown(); got != tc.in {
				t.Fatalf("source = %q, want the text as written", got)
			}
		})
	}
}

func TestDrawLinksAsksForTheChannelsOncePerText(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	source := &fakeSlackChannels{ids: map[string]string{"cee_and_ceac_integration": "C0123ABCD"}}
	h.resolver.deps.SlackChannels = source
	h.resolver.deps.SlackSubdomain = "acme"
	parsed := prose.ParsePrompt(dlog.NewTestLogger(), "#cee_and_ceac_integration\n\n1 - #cee_and_ceac_integration")

	// Act.
	h.resolver.drawLinks(testWorkspace, parsed, false)

	// Assert: every drawn markdown of the text is linked from one ask.
	eachDrawnMarkdown(parsed, false, func(markdown string, _ bool) string {
		if strings.Contains(strings.ReplaceAll(markdown, linkedChannel, ""), "#cee_and_ceac_integration") {
			t.Fatalf("drawn markdown = %q, want every channel linked", markdown)
		}
		return markdown
	})
	if source.asks != 1 {
		t.Fatalf("asks = %d, want 1 per text", source.asks)
	}
}
