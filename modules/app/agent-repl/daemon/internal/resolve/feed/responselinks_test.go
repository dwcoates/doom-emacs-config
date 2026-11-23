package feed

import (
	"context"
	"errors"
	"reflect"
	"testing"

	"agent-repld/internal/ids"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// fakeResponseLinks is an in-memory ResponseLinkSink.
type fakeResponseLinks struct {
	calls [][]string
	err   error
}

func (f *fakeResponseLinks) AddResponseLinks(_ context.Context, _ ids.WorkspaceID, urls []string) error {
	f.calls = append(f.calls, urls)
	return f.err
}

func TestResponseURLs(t *testing.T) {
	cases := []struct {
		name string
		text string
		want []string
	}{
		{name: "a bare url", text: "see https://example.com/a for it", want: []string{"https://example.com/a"}},
		{name: "an inline link's destination", text: "[the doc](https://example.com/doc)", want: []string{"https://example.com/doc"}},
		{name: "an autolink", text: "<https://example.com/auto>", want: []string{"https://example.com/auto"}},
		{name: "a repeated url is named once, in first-seen order", text: "https://b.example/x then https://a.example/y then https://b.example/x",
			want: []string{"https://b.example/x", "https://a.example/y"}},
		{name: "a url in a code span is no link", text: "run `curl https://example.com/in-code`"},
		{name: "a url in a fenced block is no link", text: "```\nhttps://example.com/fenced\n```\n"},
		{name: "a non-http scheme is no link", text: "[mail](mailto:a@example.com) and <ftp://example.com/f>"},
		{name: "a url with inline code markers in it is unmarked", text: "https://example.com/`x`", want: []string{"https://example.com/x"}},
		{name: "text with no link names none", text: "nothing to see"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := responseURLs(tc.text)

			// Assert.
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("responseURLs(%q) = %q, want %q", tc.text, got, tc.want)
			}
		})
	}
}

func TestASettledResponseHandsItsLinksToTheSink(t *testing.T) {
	// Arrange.
	sink := &fakeResponseLinks{}
	h := newDurableHarness(t, durableStores{responseLinks: sink})

	// Act.
	settleResponseAs(h, "unit-1", "see https://example.com/a")

	// Assert.
	if want := [][]string{{"https://example.com/a"}}; !reflect.DeepEqual(sink.calls, want) {
		t.Fatalf("sink calls = %q, want %q", sink.calls, want)
	}
}

func TestAnArrivingResponseHandsTheSinkNothing(t *testing.T) {
	// Arrange.
	sink := &fakeResponseLinks{}
	h := newDurableHarness(t, durableStores{responseLinks: sink})

	// Act: the text so far may end mid-url.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "see https://example.com/a"}, nil), nil, nil)

	// Assert.
	if len(sink.calls) != 0 {
		t.Fatalf("sink calls = %q, want none", sink.calls)
	}
}

func TestAUrlHandedOverIsNotHandedOverAgain(t *testing.T) {
	// Arrange.
	sink := &fakeResponseLinks{}
	h := newDurableHarness(t, durableStores{responseLinks: sink})
	settleResponseAs(h, "unit-1", "see https://example.com/a")

	// Act: another response names it again, and one more.
	settleResponseAs(h, "unit-2", "again https://example.com/a and https://example.com/b")

	// Assert.
	want := [][]string{{"https://example.com/a"}, {"https://example.com/b"}}
	if !reflect.DeepEqual(sink.calls, want) {
		t.Fatalf("sink calls = %q, want %q", sink.calls, want)
	}
}

func TestASinkFailureIsLoggedRaisedAndRetriedOnTheNextDraw(t *testing.T) {
	// Arrange.
	sink := &fakeResponseLinks{err: errors.New("disk on fire")}
	h := newDurableHarness(t, durableStores{responseLinks: sink})

	// Act.
	settleResponseAs(h, "unit-1", "see https://example.com/a")
	sink.err = nil
	settleResponseAs(h, "unit-2", "see https://example.com/a")

	// Assert.
	if !h.hasRecordWith("error", opResponseLinks, "error", "disk on fire") {
		t.Fatalf("records = %+v, want the failed store at error", h.records())
	}
	if keys := h.warnings.keys(); len(keys) != 1 || keys[0] != opResponseLinks {
		t.Fatalf("warnings = %v, want the failure raised once", keys)
	}
	if len(sink.calls) != 2 {
		t.Fatalf("sink calls = %q, want the url offered again after the failure", sink.calls)
	}
}

func TestAResponseStillDrawsWithNoSink(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	parsed := settleResponse(h, "see https://example.com/a")

	// Assert.
	if parsed.GetSource().GetMarkdown() != "see https://example.com/a" {
		t.Fatalf("source = %q", parsed.GetSource().GetMarkdown())
	}
}
