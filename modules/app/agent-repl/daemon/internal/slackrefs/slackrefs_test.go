package slackrefs

import (
	"testing"

	"agent-repld/internal/externalbrowser"
	"agent-repld/internal/mdinline/mdinlinetest"
)

var channels = Channels{Subdomain: "acme", IDs: map[string]string{
	"cee_and_ceac_integration": "C0123ABCD",
	"eng-help":                 "C0456EFGH",
	"2024":                     "C0789IJKL",
}}

const linked = "[#cee_and_ceac_integration](https://acme.slack.com/archives/C0123ABCD)"

const pull = "[#12](https://github.com/ChessCom/explanation-engine/pull/12)"

func TestLinkify(t *testing.T) {
	cases := []struct {
		name     string
		in       string
		channels Channels
		arriving bool
		want     string
	}{
		{name: "a known channel is linked", in: "post in #cee_and_ceac_integration now", want: "post in " + linked + " now"},
		{name: "a channel opening the text is linked", in: "#eng-help asked", want: "[#eng-help](https://acme.slack.com/archives/C0456EFGH) asked"},
		{name: "trailing punctuation is not part of the name", in: "see #cee_and_ceac_integration.", want: "see " + linked + "."},
		{name: "a parenthesized channel is linked", in: "(#cee_and_ceac_integration)", want: "(" + linked + ")"},
		{name: "an unknown name stays plain", in: "see #nowhere", want: "see #nowhere"},
		{name: "a code span is left alone", in: "run `#cee_and_ceac_integration` here", want: "run `#cee_and_ceac_integration` here"},
		{name: "a fenced block is left alone", in: "```\n#cee_and_ceac_integration\n```\n", want: "```\n#cee_and_ceac_integration\n```\n"},
		{name: "a link's text is left alone", in: "[#eng-help](https://x.dev)", want: "[#eng-help](https://x.dev)"},
		{name: "a link's destination is left alone", in: "[top](#eng-help)", want: "[top](#eng-help)"},
		{name: "a url fragment is left alone", in: "https://x.dev/page#eng-help", want: "https://x.dev/page#eng-help"},
		{name: "a channel glued after a word is left alone", in: "a#eng-help", want: "a#eng-help"},
		{name: "a name glued to an uppercase letter is left alone", in: "#eng-helpX", want: "#eng-helpX"},
		{name: "a name glued to a non-ASCII rune is left alone", in: "#eng-helpé", want: "#eng-helpé"},
		{name: "a settled text links its last channel", in: "see #eng-help", want: "see [#eng-help](https://acme.slack.com/archives/C0456EFGH)"},
		{name: "an arriving text leaves its last channel", in: "see #eng-help", arriving: true, want: "see #eng-help"},
		{name: "an arriving text links an earlier channel", in: "#cee_and_ceac_integration and #eng", arriving: true, want: linked + " and #eng"},
		{name: "an all-digit name is a pull request reference", in: "see #2024", want: "see #2024"},
		{name: "a linked pull request beside a channel stays", in: pull + " in #cee_and_ceac_integration", want: pull + " in " + linked},
		{name: "a bare pull request beside a channel stays", in: "#12 #cee_and_ceac_integration", want: "#12 " + linked},
		{name: "no channels leaves the text", in: "see #eng-help", channels: Channels{Subdomain: "acme"}, want: "see #eng-help"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			ch := channels
			if tc.channels.Subdomain != "" {
				ch = tc.channels
			}

			// Act
			got := Linkify(tc.in, ch, tc.arriving)

			// Assert
			if got != tc.want {
				t.Errorf("Linkify(%q) = %q, want %q", tc.in, got, tc.want)
			}
		})
	}
}

func TestChannelName(t *testing.T) {
	long := "#"
	for range maxName + 1 {
		long += "a"
	}
	cases := []struct {
		name   string
		line   string
		atEnd  bool
		want   string
		wantOK bool
	}{
		{name: "a name is read", line: "#a-b_c d", want: "a-b_c", wantOK: true},
		{name: "a bare hash is no name", line: "# x"},
		{name: "an all-digit name is no name", line: "#123 x"},
		{name: "digits beside letters are a name", line: "#q4-plans x", want: "q4-plans", wantOK: true},
		{name: "a name longer than Slack allows is no name", line: long + " x"},
		{name: "a name at an arriving end is no name", line: "#abc", atEnd: true},
		{name: "a name at a settled end is a name", line: "#abc", want: "abc", wantOK: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got, ok := channelName(tc.line, 0, tc.atEnd)
			if got != tc.want || ok != tc.wantOK {
				t.Errorf("channelName(%q) = (%q, %v), want (%q, %v)", tc.line, got, ok, tc.want, tc.wantOK)
			}
		})
	}
}

func TestPermalink(t *testing.T) {
	if got, want := Permalink("acme", "C0123ABCD"), "https://acme.slack.com/archives/C0123ABCD"; got != want {
		t.Errorf("Permalink() = %q, want %q", got, want)
	}
}

// Linkify walks prose through the one shared walk (mdinline.RewriteProse).
func TestLinkifyRewritesThroughTheSharedWalk(t *testing.T) {
	mdinlinetest.AssertRewriteShape(t, "slackrefs.go", "Linkify")
}

// A permalink is the shape the Slack-opening path hands to the desktop app.
func TestPermalinkOpensInTheSlackApp(t *testing.T) {
	// Arrange
	teams := externalbrowser.SlackTeams{"acme": "T0123ABCD"}

	// Act
	deep, ok, why := externalbrowser.SlackDeepLink(Permalink("acme", "C0123ABCD"), teams)

	// Assert
	if !ok || deep != "slack://channel?team=T0123ABCD&id=C0123ABCD" {
		t.Fatalf("SlackDeepLink(Permalink()) = (%q, %v, %q), want the channel's deep link", deep, ok, why)
	}
}

// Every link the rewrite adds is written through mdinline.WriteLink.
func TestLinksAreWrittenThroughWriteLink(t *testing.T) {
	mdinlinetest.AssertNoHandWrittenLinks(t, "slackrefs.go")
}
