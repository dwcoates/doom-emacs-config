package harness

import (
	"testing"
	"time"
)

// TestStreamDrainKeepsAPumpFromWedging pins the property a PARTICIPANT HOLD
// depends on: a stream opened for its server-side effect, whose pushes nobody
// reads, must keep being read anyway.
//
// runStream pumps onto a buffered channel. Left unread, that buffer fills and
// the pump blocks on the send, which blocks the daemon's own writer behind it
// — so a hold that looks open stops behaving like one. Drain is what keeps the
// reading going for the rest of the stream's life.
//
// The undrained arm is a NEGATIVE assertion (nothing more gets through), so it
// necessarily waits out ProbeWindow rather than synchronizing on an event.
func TestStreamDrainKeepsAPumpFromWedging(t *testing.T) {
	// The buffer under test is deliberately tiny: the property is "more pushes
	// than the buffer holds", not the production capacity.
	const buffer = 4
	const pushes = buffer * 8

	tests := []struct {
		name     string
		drain    bool
		want     int
		wantMore bool
	}{
		{
			name:     "a drained stream takes every push past its buffer",
			drain:    true,
			want:     pushes,
			wantMore: true,
		},
		{
			name:     "an undrained stream stops taking pushes at its buffer",
			drain:    false,
			want:     buffer,
			wantMore: false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a stream whose channel is the pump's own buffered one,
			// and a pusher that reports how far it got.
			ch := make(chan int, buffer)
			s := &Stream[int]{C: ch, t: t, cancel: func() {}}
			if tc.drain {
				s.Drain()
			}
			moved := make(chan int, 1)
			go func() {
				sent := 0
				for ; sent < tc.want; sent++ {
					ch <- sent
				}
				moved <- sent
			}()

			// Act: wait for the pusher to place everything the arm expects,
			// then probe whether ONE more would go through.
			var sent int
			select {
			case sent = <-moved:
			case <-time.After(DefaultTimeout):
				t.Fatalf("the pusher wedged before %d pushes with drain=%v", tc.want, tc.drain)
			}
			more := false
			select {
			case ch <- sent:
				more = true
			case <-time.After(ProbeWindow):
			}

			// Assert.
			if sent != tc.want {
				t.Errorf("pushes accepted = %d, want %d", sent, tc.want)
			}
			if more != tc.wantMore {
				t.Errorf("one more push went through = %v, want %v", more, tc.wantMore)
			}
		})
	}
}

func TestFirstPushMatching(t *testing.T) {
	tests := []struct {
		name   string
		pushes []int
		close  bool
		want   int
		wantOK bool
	}{
		{name: "passes over pushes it rejects and answers the first it accepts", pushes: []int{1, 3, 4, 6}, want: 4, wantOK: true},
		{name: "answers nothing when every push is rejected", pushes: []int{1, 3}, wantOK: false},
		{name: "answers nothing when the stream closes first", pushes: []int{1}, close: true, wantOK: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			ch := make(chan int, len(tc.pushes))
			for _, v := range tc.pushes {
				ch <- v
			}
			if tc.close {
				close(ch)
			}
			s := &Stream[int]{C: ch, t: t, cancel: func() {}}

			// Act
			got, ok := firstPushMatching(s, 50*time.Millisecond, func(v int) bool { return v%2 == 0 })

			// Assert
			if got != tc.want || ok != tc.wantOK {
				t.Fatalf("firstPushMatching = (%d, %t), want (%d, %t)", got, ok, tc.want, tc.wantOK)
			}
		})
	}
}
