package changesignal

import "testing"

func isClosed(ch <-chan struct{}) bool {
	select {
	case <-ch:
		return true
	default:
		return false
	}
}

func TestSignal(t *testing.T) {
	cases := []struct {
		name string
		act  func(s *Signal) (taken <-chan struct{}, after <-chan struct{})
		// wantTaken and wantAfter are whether each channel is closed.
		wantTaken, wantAfter bool
	}{
		{name: "a taken channel stays open until a change", act: func(s *Signal) (<-chan struct{}, <-chan struct{}) {
			return s.Changed(), s.Changed()
		}},
		{name: "a change closes the taken channel and stands a fresh one", wantTaken: true, act: func(s *Signal) (<-chan struct{}, <-chan struct{}) {
			taken := s.Changed()
			s.Notify()
			return taken, s.Changed()
		}},
		{name: "a change nobody took is not seen by a later taker", act: func(s *Signal) (<-chan struct{}, <-chan struct{}) {
			s.Notify()
			taken := s.Changed()
			return taken, s.Changed()
		}},
		{name: "every waiter that took the channel is woken", wantTaken: true, wantAfter: true, act: func(s *Signal) (<-chan struct{}, <-chan struct{}) {
			first, second := s.Changed(), s.Changed()
			s.Notify()
			return first, second
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var s Signal

			// Act
			taken, after := tc.act(&s)

			// Assert
			if isClosed(taken) != tc.wantTaken || isClosed(after) != tc.wantAfter {
				t.Fatalf("closed = (%v, %v), want (%v, %v)", isClosed(taken), isClosed(after), tc.wantTaken, tc.wantAfter)
			}
		})
	}
}
