package harness

import (
	"context"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
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
// The undrained arm's "nothing more gets through" is read off the full buffer
// at once: with no reader, a send onto a full channel cannot be taken.
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
			if tc.wantMore {
				// The drain takes it; the bound only fails a drain that stopped.
				select {
				case ch <- sent:
					more = true
				case <-time.After(DefaultTimeout):
				}
			} else {
				select {
				case ch <- sent:
					more = true
				default:
				}
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

func TestPendingPushMatching(t *testing.T) {
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
			got, ok := pendingPushMatching(s, func(v int) bool { return v%2 == 0 })

			// Assert
			if got != tc.want || ok != tc.wantOK {
				t.Fatalf("pendingPushMatching = (%d, %t), want (%d, %t)", got, ok, tc.want, tc.wantOK)
			}
		})
	}
}

func TestBootWifiStanding(t *testing.T) {
	tests := []struct {
		name string
		push *agentreplv1.WatchDaemonResponse
		want bool
	}{
		{name: "a push that is no standing is not it", push: &agentreplv1.WatchDaemonResponse{}, want: false},
		{name: "a standing with the lid unheard is not it", push: wifiPush(&agentreplv1.PersistentWifiState{}), want: false},
		{name: "a standing with the lid heard closed is not it", push: wifiPush(&agentreplv1.PersistentWifiState{
			Lid: &agentreplv1.PersistentWifiState_LidClosed{LidClosed: &agentreplv1.PersistentWifiLidClosed{}},
		}), want: false},
		{name: "a standing with the lid heard open is it", push: wifiPush(lidOpenStanding()), want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := BootWifiStanding(tc.push)

			// Assert
			if got != tc.want {
				t.Fatalf("BootWifiStanding(%v) = %t, want %t", tc.push, got, tc.want)
			}
		})
	}
}

func TestTopbarBootWifiStanding(t *testing.T) {
	tests := []struct {
		name string
		view *frontendv1.TopbarView
		want bool
	}{
		{name: "a strip with no standing is not it", view: &frontendv1.TopbarView{}, want: false},
		{name: "a strip with the lid unheard is not it", view: &frontendv1.TopbarView{PersistentWifi: &frontendv1.TopbarPersistentWifi{}}, want: false},
		{name: "a strip with the lid heard closed is not it", view: &frontendv1.TopbarView{PersistentWifi: &frontendv1.TopbarPersistentWifi{
			Lid: &frontendv1.TopbarPersistentWifi_LidClosed{LidClosed: &frontendv1.TopbarPersistentWifiLidClosed{}},
		}}, want: false},
		{name: "a strip with the lid heard open is it", view: &frontendv1.TopbarView{PersistentWifi: &frontendv1.TopbarPersistentWifi{
			Lid: &frontendv1.TopbarPersistentWifi_LidOpen{LidOpen: &frontendv1.TopbarPersistentWifiLidOpen{}},
		}}, want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := TopbarBootWifiStanding(tc.view)

			// Assert
			if got != tc.want {
				t.Fatalf("TopbarBootWifiStanding(%v) = %t, want %t", tc.view, got, tc.want)
			}
		})
	}
}

func TestAwaitBootWifiStandingPassesOverTheStandingWithTheLidUnheard(t *testing.T) {
	// Arrange: the stream is told the boot's first read, then the lid's.
	ch := make(chan *agentreplv1.WatchDaemonResponse, 2)
	ch <- wifiPush(&agentreplv1.PersistentWifiState{})
	ch <- wifiPush(lidOpenStanding())
	s := &Stream[*agentreplv1.WatchDaemonResponse]{C: ch, t: t, cancel: func() {}}

	// Act
	got := AwaitBootWifiStanding(t, context.Background(), s)

	// Assert
	if got.GetLidOpen() == nil {
		t.Fatalf("AwaitBootWifiStanding = %v, want the standing with the lid heard open", got)
	}
	if len(ch) != 0 {
		t.Fatalf("pushes left unread = %d, want none: the wait took the settled standing", len(ch))
	}
}

func wifiPush(s *agentreplv1.PersistentWifiState) *agentreplv1.WatchDaemonResponse {
	return &agentreplv1.WatchDaemonResponse{Push: &agentreplv1.WatchDaemonResponse_PersistentWifi{PersistentWifi: s}}
}

func lidOpenStanding() *agentreplv1.PersistentWifiState {
	return &agentreplv1.PersistentWifiState{
		Lid: &agentreplv1.PersistentWifiState_LidOpen{LidOpen: &agentreplv1.PersistentWifiLidOpen{}},
	}
}
