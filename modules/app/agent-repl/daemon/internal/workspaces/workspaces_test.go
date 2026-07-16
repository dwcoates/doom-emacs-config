package workspaces

import (
	"bytes"
	"testing"
)

// recv pops the client's pending snapshot without blocking; delivery is
// synchronous under the hub's lock, so anything sent is already buffered.
func recv(t *testing.T, c *Client) []byte {
	t.Helper()
	select {
	case data, ok := <-c.Send:
		if !ok {
			t.Fatal("Send is closed, want a snapshot")
		}
		return data
	default:
		t.Fatal("Send is empty, want a snapshot")
		return nil
	}
}

func TestLatestIsNilBeforeAnyIngest(t *testing.T) {
	// Arrange.
	h := NewHub()

	// Act.
	got := h.Latest()

	// Assert.
	if got != nil {
		t.Errorf("Latest() = %q, want nil before any ingest", got)
	}
}

func TestSetLatestStoresTheSnapshot(t *testing.T) {
	// Arrange.
	h := NewHub()
	want := []byte(`{"type":"workspace-snapshot"}`)

	// Act.
	h.SetLatest(want)

	// Assert.
	if got := h.Latest(); !bytes.Equal(got, want) {
		t.Errorf("Latest() = %s, want %s", got, want)
	}
}

func TestSetLatestReplacesThePreviousSnapshot(t *testing.T) {
	// Arrange.
	h := NewHub()
	h.SetLatest([]byte(`{"n":1}`))

	// Act.
	h.SetLatest([]byte(`{"n":2}`))

	// Assert.
	if got, want := h.Latest(), []byte(`{"n":2}`); !bytes.Equal(got, want) {
		t.Errorf("Latest() = %s, want %s", got, want)
	}
}

func TestBroadcastReachesEveryAttachedClient(t *testing.T) {
	// Arrange.
	h := NewHub()
	c1, c2 := NewClient(), NewClient()
	h.Attach(c1)
	h.Attach(c2)
	want := []byte(`{"n":1}`)

	// Act.
	h.SetLatest(want)

	// Assert.
	if got := recv(t, c1); !bytes.Equal(got, want) {
		t.Errorf("client 1 received %s, want %s", got, want)
	}
	if got := recv(t, c2); !bytes.Equal(got, want) {
		t.Errorf("client 2 received %s, want %s", got, want)
	}
}

func TestAttachPrimesWithTheLatestSnapshot(t *testing.T) {
	// Arrange.
	h := NewHub()
	want := []byte(`{"n":1}`)
	h.SetLatest(want)
	c := NewClient()

	// Act.
	h.Attach(c)

	// Assert.
	if got := recv(t, c); !bytes.Equal(got, want) {
		t.Errorf("primed snapshot = %s, want %s", got, want)
	}
}

func TestAttachBeforeAnyIngestDeliversNothing(t *testing.T) {
	// Arrange.
	h := NewHub()
	c := NewClient()

	// Act.
	h.Attach(c)

	// Assert.
	select {
	case data := <-c.Send:
		t.Errorf("received %s, want nothing before any ingest", data)
	default:
	}
}

func TestSlowClientReceivesOnlyTheNewestSnapshot(t *testing.T) {
	// Arrange — the client never drains between ingests.
	h := NewHub()
	c := NewClient()
	h.Attach(c)

	// Act — if delivery blocked on the full buffer, this would hang.
	h.SetLatest([]byte(`{"n":1}`))
	h.SetLatest([]byte(`{"n":2}`))

	// Assert — the stale snapshot was dropped in favor of the newest.
	if got, want := recv(t, c), []byte(`{"n":2}`); !bytes.Equal(got, want) {
		t.Errorf("received %s, want %s", got, want)
	}
	select {
	case data := <-c.Send:
		t.Errorf("received a second snapshot %s, want the stale one dropped", data)
	default:
	}
}

func TestDetachClosesTheSendChannel(t *testing.T) {
	// Arrange.
	h := NewHub()
	c := NewClient()
	h.Attach(c)

	// Act.
	h.Detach(c)

	// Assert.
	if _, ok := <-c.Send; ok {
		t.Error("Send delivered a value after Detach, want it closed")
	}
}

func TestDetachIsIdempotent(t *testing.T) {
	// Arrange.
	h := NewHub()
	c := NewClient()
	h.Attach(c)
	h.Detach(c)

	// Act & Assert — a second Detach must not close Send again (panic).
	h.Detach(c)
}

func TestDetachedClientMissesLaterBroadcasts(t *testing.T) {
	// Arrange.
	h := NewHub()
	c := NewClient()
	h.Attach(c)
	h.Detach(c)

	// Act — must not send on (or panic over) the closed channel.
	h.SetLatest([]byte(`{"n":1}`))

	// Assert.
	if data, ok := <-c.Send; ok {
		t.Errorf("detached client received %s, want nothing", data)
	}
}
