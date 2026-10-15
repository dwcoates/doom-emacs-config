package feed

import (
	"context"
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// loadThrough walks reader-1 from above its oldest held row (from the newest
// page when it holds none) to the prompt row of TURN, and answers the pages
// handed over, oldest-so-far last, with the outcome. Reader-1 then holds them.
func (h *harness) loadThrough(turn string) ([]*frontendv1.FeedPage, *frontendv1.FeedId, error) {
	h.t.Helper()
	var pages []*frontendv1.FeedPage
	reached, err := h.resolver.LoadThrough(context.Background(), testWorkspace, h.heldKey("reader-1"),
		&frontendv1.FeedId{Value: h.promptRowID(turn)},
		func(page *frontendv1.FeedPage) error {
			pages = append(pages, page)
			if h.held == nil {
				h.held = map[testReader]string{}
			}
			h.hold("reader-1", page)
			return nil
		})
	return pages, reached, err
}

// heldKey is READER's oldest held row's order key, nil when it holds none.
func (h *harness) heldKey(reader testReader) *string {
	key, ok := h.held[reader]
	if !ok {
		return nil
	}
	return &key
}

func TestLoadThroughATargetAlreadyLoadedHandsOverNoPage(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.mainBook(3, promptsBook(5))
	h.openPage(rootFeed(), "reader-1")

	// Act.
	pages, reached, err := h.loadThrough("turn-3")

	// Assert.
	if err != nil || reached.GetValue() != h.promptRowID("turn-3") {
		t.Fatalf("LoadThrough = %v, %v; want reached", reached, err)
	}
	if len(pages) != 0 {
		t.Fatalf("pages = %d, want none for a target already loaded", len(pages))
	}
}

func TestLoadThroughHandsOverEveryPageDownToTheTarget(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.mainBook(2, promptsBook(7))
	h.openPage(rootFeed(), "reader-1")

	// Act.
	pages, _, err := h.loadThrough("turn-1")

	// Assert: every page between, in walk order.
	if err != nil {
		t.Fatalf("LoadThrough: %v", err)
	}
	var got []string
	for _, page := range pages {
		got = append(got, strings.Join(rowIDs(pageRows(t, page)), ","))
	}
	want := []string{h.promptRowIDs("turn-3", "turn-4"), h.promptRowIDs("turn-1", "turn-2")}
	if strings.Join(got, "|") != strings.Join(want, "|") {
		t.Fatalf("pages = %v, want %v", got, want)
	}
}

func TestANextAfterLoadThroughContinuesBelowItsOldestPage(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.mainBook(2, promptsBook(7))
	h.openPage(rootFeed(), "reader-1")
	if _, _, err := h.loadThrough("turn-2"); err != nil {
		t.Fatalf("LoadThrough: %v", err)
	}

	// Act.
	page := h.nextPage("reader-1")

	// Assert.
	if got, want := strings.Join(rowIDs(pageRows(t, page)), ","), h.promptRowIDs("turn-0"); got != want {
		t.Fatalf("next page = %v, want %v", got, want)
	}
}

func TestLoadThroughATargetTheConversationNeverHadIsNotFound(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.mainBook(2, promptsBook(5))
	h.openPage(rootFeed(), "reader-1")

	// Act.
	pages, _, err := h.loadThrough("turn-absent")

	// Assert: the walk reached the start, every page of it handed over.
	if !errors.Is(err, ErrTargetNotFound) {
		t.Fatalf("LoadThrough err = %v, want ErrTargetNotFound", err)
	}
	if len(pages) != 2 {
		t.Fatalf("pages = %d, want the two older pages walked", len(pages))
	}
}

func TestLoadThroughWhoseReadFailsMidWalkKeepsWhatItHandedOver(t *testing.T) {
	// Arrange: the second older read fails.
	h := newHarness(t)
	store := h.mainBook(2, promptsBook(7))
	h.openPage(rootFeed(), "reader-1")
	store.failFrom = 3

	// Act.
	pages, _, err := h.loadThrough("turn-0")

	// Assert.
	if !errors.Is(err, ErrHistoryUnavailable) {
		t.Fatalf("LoadThrough err = %v, want ErrHistoryUnavailable", err)
	}
	if len(pages) != 1 {
		t.Fatalf("pages = %d, want the one page read before the failure", len(pages))
	}
	if got, want := strings.Join(rowIDs(pageRows(t, h.nextPageAfterRecovery(store))), ","), h.promptRowIDs("turn-1", "turn-2"); got != want {
		t.Fatalf("next page after recovery = %v, want %v: the walk stays where the handed-over page left it", got, want)
	}
}

// nextPageAfterRecovery heals the store and asks reader-1's next page.
func (h *harness) nextPageAfterRecovery(store *fakeHistory) *frontendv1.FeedPage {
	h.t.Helper()
	store.mu.Lock()
	store.failFrom = 0
	store.mu.Unlock()
	return h.nextPage("reader-1")
}

func TestLoadThroughWithNoWalkStandingBeginsAtTheNewestPage(t *testing.T) {
	// Arrange: a reader that never opened.
	h := newHarness(t)
	h.mainBook(3, promptsBook(5))

	// Act.
	pages, _, err := h.loadThrough("turn-3")

	// Assert.
	if err != nil {
		t.Fatalf("LoadThrough: %v", err)
	}
	if len(pages) != 1 || strings.Join(rowIDs(pageRows(t, pages[0])), ",") != h.promptRowIDs("turn-2", "turn-3", "turn-4") {
		t.Fatalf("pages = %v, want the newest page alone", pages)
	}
}

func TestLoadThroughWhosePageCannotBeHandedOverFails(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.mainBook(2, promptsBook(5))
	h.openPage(rootFeed(), "reader-1")
	gone := errors.New("the reader went away")

	// Act.
	_, err := h.resolver.LoadThrough(context.Background(), testWorkspace, h.heldKey("reader-1"),
		&frontendv1.FeedId{Value: h.promptRowID("turn-0")},
		func(*frontendv1.FeedPage) error { return gone })

	// Assert.
	if !errors.Is(err, gone) {
		t.Fatalf("LoadThrough err = %v, want the hand-over's own error", err)
	}
}

// ---- LoadOlder: a feature reading past the oldest loaded row ----

func TestLoadOlderPushesTheOlderPagesRows(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.mainBook(3, promptsBook(5))
	rows := h.follow(rootFeed(), "reader-1")

	// Act.
	if _, err := h.resolver.LoadOlder(context.Background(), testWorkspace); err != nil {
		t.Fatalf("LoadOlder: %v", err)
	}
	sentinel := h.sendSentinel()

	// Assert.
	if got, want := strings.Join(pushedBefore(t, rows, sentinel), ","), h.promptRowIDs("turn-0", "turn-1"); got != want {
		t.Fatalf("pushed %v, want %v", got, want)
	}
}

func TestLoadOlderAtTheConversationsStartLoadsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(2))
	h.openPage(rootFeed(), "reader-1")

	// Act.
	loaded, err := h.resolver.LoadOlder(context.Background(), testWorkspace)

	// Assert.
	if err != nil || loaded {
		t.Fatalf("LoadOlder = %v, %v; want nothing loaded", loaded, err)
	}
	if got := store.readCount(); got != 1 {
		t.Fatalf("reads = %d, want no read past the start", got)
	}
}

func TestLoadThroughWithNoSourceIsHistoryUnavailableNotNotFound(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.mainBook(2, promptsBook(4)).noSource = true
	h.openPage(rootFeed(), "reader-1")

	// Act.
	_, _, err := h.loadThrough("turn-0")

	// Assert: no shim to read from says nothing about where the target is.
	if !errors.Is(err, ErrHistoryUnavailable) || !errors.Is(err, ErrNoHistorySource) {
		t.Fatalf("LoadThrough err = %v, want ErrHistoryUnavailable wrapping ErrNoHistorySource", err)
	}
}
