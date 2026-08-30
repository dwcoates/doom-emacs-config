package shimclient

import "testing"

// TestLinkFeedDeliversInOrder asserts every transition arrives, in order.
func TestLinkFeedDeliversInOrder(t *testing.T) {
	// Arrange.
	f := newLinkFeed()
	t.Cleanup(f.close)

	// Act: published before anything reads, so nothing may be dropped.
	f.publish(LinkDialing)
	f.publish(LinkConnected)
	f.publish(LinkRedialing)

	// Assert.
	want := []LinkState{LinkDialing, LinkConnected, LinkRedialing}
	for i, expected := range want {
		if got := <-f.states(); got != expected {
			t.Fatalf("state %d = %v, want %v", i, got, expected)
		}
	}
}

// TestLinkFeedCollapsesARepeat asserts a transition to the state already
// published is not a transition.
func TestLinkFeedCollapsesARepeat(t *testing.T) {
	// Arrange.
	f := newLinkFeed()
	t.Cleanup(f.close)

	// Act.
	f.publish(LinkConnected)
	f.publish(LinkConnected)
	f.publish(LinkDead)

	// Assert.
	if got := <-f.states(); got != LinkConnected {
		t.Fatalf("first state = %v, want connected", got)
	}
	if got := <-f.states(); got != LinkDead {
		t.Fatalf("second state = %v, want dead (the repeat must collapse)", got)
	}
}

// TestLinkFeedClosesAfterDrainingWhatWasPublished asserts a close never
// discards a transition already published.
func TestLinkFeedClosesAfterDrainingWhatWasPublished(t *testing.T) {
	// Arrange.
	f := newLinkFeed()
	f.publish(LinkDialing)
	f.publish(LinkDead)

	// Act.
	f.close()

	// Assert.
	if got := <-f.states(); got != LinkDialing {
		t.Fatalf("first state = %v, want dialing", got)
	}
	if got := <-f.states(); got != LinkDead {
		t.Fatalf("second state = %v, want dead", got)
	}
	if _, open := <-f.states(); open {
		t.Fatal("the feed did not close after its pending states")
	}
}

// TestLinkFeedIgnoresPublishAfterClose asserts a closed feed never yields
// another state.
func TestLinkFeedIgnoresPublishAfterClose(t *testing.T) {
	// Arrange.
	f := newLinkFeed()
	f.close()

	// Act.
	f.publish(LinkConnected)

	// Assert.
	if _, open := <-f.states(); open {
		t.Fatal("a state arrived after close")
	}
}
