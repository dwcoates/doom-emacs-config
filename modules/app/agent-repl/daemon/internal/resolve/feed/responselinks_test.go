package feed

import (
	"context"
	"errors"
	"reflect"
	"sync"
	"testing"

	"agent-repld/internal/ids"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// fakeResponseLinks is an in-memory ResponseLinkSink. The drainer calls it
// off the resolver's lock, so what it holds is guarded.
type fakeResponseLinks struct {
	mu    sync.Mutex
	calls [][]string
	err   error
	// entered, when set, is told each call as it begins.
	entered chan struct{}
	// release, when set, holds each call until it is closed.
	release chan struct{}
}

func (f *fakeResponseLinks) AddResponseLinks(_ context.Context, _ ids.WorkspaceID, urls []string) error {
	if f.entered != nil {
		f.entered <- struct{}{}
	}
	if f.release != nil {
		<-f.release
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, urls)
	return f.err
}

// taken is every url batch the sink was handed, in order.
func (f *fakeResponseLinks) taken() [][]string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([][]string(nil), f.calls...)
}

// setErr makes every later call fail with ERR (nil succeeds).
func (f *fakeResponseLinks) setErr(err error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.err = err
}

// hasRecordWithURLs reports whether a LEVEL record of opResponseLinks
// carries URLS.
func (h *harness) hasRecordWithURLs(level string, urls []string) bool {
	h.t.Helper()
	for _, record := range h.records() {
		if record.Level == level && record.Operation == opResponseLinks && reflect.DeepEqual(record.Context["urls"], urls) {
			return true
		}
	}
	return false
}

// startSpawned runs the work the resolver started off its lock on a
// goroutine of its own, answering a channel closed once it has all returned.
func (h *harness) startSpawned() <-chan struct{} {
	h.t.Helper()
	spawned := h.spawned
	h.spawned = nil
	done := make(chan struct{})
	go func() {
		defer close(done)
		for _, f := range spawned {
			f()
		}
	}()
	return done
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
	before := sink.taken()
	h.runSpawned()

	// Assert: the draw only handed the links over; the drainer stored them.
	if len(before) != 0 {
		t.Fatalf("sink calls during the draw = %q, want none", before)
	}
	if want := [][]string{{"https://example.com/a"}}; !reflect.DeepEqual(sink.taken(), want) {
		t.Fatalf("sink calls = %q, want %q", sink.taken(), want)
	}
	if !h.hasRecordWithURLs("debug", []string{"https://example.com/a"}) {
		t.Fatalf("records = %+v, want the stored links at debug", h.records())
	}
}

func TestAnArrivingResponseHandsTheSinkNothing(t *testing.T) {
	// Arrange.
	sink := &fakeResponseLinks{}
	h := newDurableHarness(t, durableStores{responseLinks: sink})

	// Act: the text so far may end mid-url.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "see https://example.com/a"}, nil), nil, nil)
	spawned := len(h.spawned)
	h.runSpawned()

	// Assert.
	if spawned != 0 {
		t.Fatalf("spawned %d drainer(s), want none", spawned)
	}
	if calls := sink.taken(); len(calls) != 0 {
		t.Fatalf("sink calls = %q, want none", calls)
	}
}

func TestAUrlHandedOverIsNotHandedOverAgain(t *testing.T) {
	cases := []struct {
		name string
		// drainBetween stores the first response's links before the second
		// is drawn; unset, the second is drawn while the first store is
		// still pending.
		drainBetween bool
	}{
		{name: "after the first store landed", drainBetween: true},
		{name: "while the first store is still pending"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			sink := &fakeResponseLinks{}
			h := newDurableHarness(t, durableStores{responseLinks: sink})
			settleResponseAs(h, "unit-1", "see https://example.com/a")
			if tc.drainBetween {
				h.runSpawned()
			}

			// Act: another response names it again, and one more.
			settleResponseAs(h, "unit-2", "again https://example.com/a and https://example.com/b")
			h.runSpawned()

			// Assert.
			want := [][]string{{"https://example.com/a"}, {"https://example.com/b"}}
			if !reflect.DeepEqual(sink.taken(), want) {
				t.Fatalf("sink calls = %q, want %q", sink.taken(), want)
			}
		})
	}
}

func TestResponsesReachTheSinkInTheOrderTheyWereDrawn(t *testing.T) {
	// Arrange.
	sink := &fakeResponseLinks{}
	h := newDurableHarness(t, durableStores{responseLinks: sink})

	// Act.
	settleResponseAs(h, "unit-1", "first https://example.com/1")
	settleResponseAs(h, "unit-2", "second https://example.com/2")
	settleResponseAs(h, "unit-3", "third https://example.com/3")
	spawned := len(h.spawned)
	h.runSpawned()

	// Assert: one drainer took all three, in draw order.
	if spawned != 1 {
		t.Fatalf("spawned %d drainers, want one", spawned)
	}
	want := [][]string{{"https://example.com/1"}, {"https://example.com/2"}, {"https://example.com/3"}}
	if !reflect.DeepEqual(sink.taken(), want) {
		t.Fatalf("sink calls = %q, want %q", sink.taken(), want)
	}
}

func TestABlockedSinkDoesNotHoldTheResolver(t *testing.T) {
	// Arrange: the sink blocks inside its first call.
	sink := &fakeResponseLinks{entered: make(chan struct{}, 2), release: make(chan struct{})}
	h := newDurableHarness(t, durableStores{responseLinks: sink})
	settleResponseAs(h, "unit-1", "see https://example.com/a")
	done := h.startSpawned()
	<-sink.entered

	// Act: entry points that take the resolver's lock run while the store is
	// blocked; the second names a fresh url.
	h.resolver.OnMainAgent(testWorkspace, mainAgent())
	settleResponseAs(h, "unit-2", "then https://example.com/b")
	spawnedWhileBlocked := len(h.spawned)
	close(sink.release)
	<-done

	// Assert: the running drainer took the later batch too, after the first.
	if spawnedWhileBlocked != 0 {
		t.Fatalf("spawned %d drainer(s) while one ran, want none", spawnedWhileBlocked)
	}
	want := [][]string{{"https://example.com/a"}, {"https://example.com/b"}}
	if !reflect.DeepEqual(sink.taken(), want) {
		t.Fatalf("sink calls = %q, want %q", sink.taken(), want)
	}
}

func TestASinkFailureIsLoggedRaisedAndRetriedOnTheNextDraw(t *testing.T) {
	// Arrange.
	sink := &fakeResponseLinks{err: errors.New("disk on fire")}
	h := newDurableHarness(t, durableStores{responseLinks: sink})

	// Act.
	settleResponseAs(h, "unit-1", "see https://example.com/a")
	h.runSpawned()
	sink.setErr(nil)
	settleResponseAs(h, "unit-2", "see https://example.com/a")
	h.runSpawned()

	// Assert.
	if !h.hasRecordWith("error", opResponseLinks, "error", "disk on fire") {
		t.Fatalf("records = %+v, want the failed store at error", h.records())
	}
	if !h.hasRecordWithURLs("error", []string{"https://example.com/a"}) {
		t.Fatalf("records = %+v, want the failed store's urls at error", h.records())
	}
	if keys := h.warnings.keys(); len(keys) != 1 || keys[0] != opResponseLinks {
		t.Fatalf("warnings = %v, want the failure raised once", keys)
	}
	want := [][]string{{"https://example.com/a"}, {"https://example.com/a"}}
	if !reflect.DeepEqual(sink.taken(), want) {
		t.Fatalf("sink calls = %q, want the url offered again after the failure", sink.taken())
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
