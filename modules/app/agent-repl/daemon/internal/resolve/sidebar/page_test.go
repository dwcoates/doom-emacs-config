package sidebar

import (
	"testing"
	"time"
)

func TestPageVerdict(t *testing.T) {
	now := time.UnixMilli(1_700_000_000_000)
	ago := func(d time.Duration) *time.Time { at := now.Add(-d); return &at }
	tests := []struct {
		name       string
		in         pageInputs
		wantLoaded bool
		wantFlips  time.Time
	}{
		{name: "a workspace that is not visible holds no page, whatever else holds",
			in: pageInputs{selected: true, working: true, unread: true}, wantLoaded: false},
		{name: "the selection holds its page",
			in: pageInputs{visible: true, selected: true, awayUnloaded: true}, wantLoaded: true},
		{name: "a one-shot or sync merge off screen holds no page, even working",
			in: pageInputs{visible: true, awayUnloaded: true, working: true}, wantLoaded: false},
		{name: "a working workspace holds its page",
			in: pageInputs{visible: true, working: true, leftAt: ago(time.Hour)}, wantLoaded: true},
		{name: "an unread result holds its page inside the hour after its turn ended",
			in: pageInputs{visible: true, unread: true, turnEndedAt: ago(59 * time.Minute)}, wantLoaded: true, wantFlips: now.Add(time.Minute)},
		{name: "an unread result lets its page go an hour after its turn ended",
			in: pageInputs{visible: true, unread: true, turnEndedAt: ago(PageUnreadHold)}, wantLoaded: false},
		{name: "a viewed workspace holds its page for the leave grace",
			in: pageInputs{visible: true, leftAt: ago(5 * time.Second)}, wantLoaded: true, wantFlips: now.Add(5 * time.Second)},
		{name: "a viewed workspace lets its page go after the leave grace",
			in: pageInputs{visible: true, leftAt: ago(PageLeaveGrace)}, wantLoaded: false},
		{name: "a visit that first viewed it holds the page for the first-view grace",
			in: pageInputs{visible: true, leftAt: ago(14 * time.Minute), leftFirstViewed: true}, wantLoaded: true, wantFlips: now.Add(time.Minute)},
		{name: "a visit that first viewed it lets the page go after the first-view grace",
			in: pageInputs{visible: true, leftAt: ago(PageFirstViewGrace), leftFirstViewed: true}, wantLoaded: false},
		{name: "a workspace never left and idle holds no page",
			in: pageInputs{visible: true}, wantLoaded: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			loaded, flips := pageVerdict(tt.in, now)

			// Assert
			if loaded != tt.wantLoaded || !flips.Equal(tt.wantFlips) {
				t.Fatalf("pageVerdict = (%v, %v), want (%v, %v)", loaded, flips, tt.wantLoaded, tt.wantFlips)
			}
		})
	}
}

func TestADepartureRecordsWhetherTheVisitFirstViewedTheWorkspace(t *testing.T) {
	tests := []struct {
		name              string
		viewedAtArrival   bool
		viewedAtDeparture bool
		want              bool
	}{
		{name: "a visit that viewed an unread workspace first viewed it", viewedAtArrival: false, viewedAtDeparture: true, want: true},
		{name: "a visit to an already viewed workspace did not", viewedAtArrival: true, viewedAtDeparture: true, want: false},
		{name: "a visit that viewed nothing did not", viewedAtArrival: false, viewedAtDeparture: false, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := &wsState{}
			if tt.viewedAtArrival {
				s.viewedArm = "done"
			}
			noteArrival(s)
			s.viewedArm = ""
			if tt.viewedAtDeparture {
				s.viewedArm = "done"
			}
			left := time.UnixMilli(1_700_000_000_000)

			// Act
			noteDeparture(s, left)

			// Assert
			if s.leftFirstViewed != tt.want || s.leftAt == nil || !s.leftAt.Equal(left) {
				t.Fatalf("leftFirstViewed = %v, leftAt = %v; want %v at %v", s.leftFirstViewed, s.leftAt, tt.want, left)
			}
		})
	}
}
