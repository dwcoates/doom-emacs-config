package grantedclock

import (
	"errors"
	"strings"
	"testing"
	"time"
)

func TestSpanStringNamesAllThreeTimes(t *testing.T) {
	// Arrange
	span := Span{Wall: 3196 * time.Millisecond, Stalled: 2900 * time.Millisecond, Granted: 296 * time.Millisecond}

	// Act
	got := span.String()

	// Assert
	want := "296ms granted of 3.196s wall, the processes it covers waiting 2.9s for a CPU"
	if got != want {
		t.Fatalf("String() = %q, want %q", got, want)
	}
}

// fakeClockSource is a wall clock and a stall counter a test moves by hand.
type fakeClockSource struct {
	wall, stall time.Duration
	err         error
}

func (f *fakeClockSource) now() time.Time { return time.Unix(0, 0).Add(f.wall) }
func (f *fakeClockSource) stalled() (time.Duration, error) {
	return f.stall, f.err
}

// reading is one step a test moves a fakeClockSource by before reading the
// clock.
type reading struct {
	wall, stall time.Duration
}

func TestClockAccruesGrantedTimeReadingByReading(t *testing.T) {
	tests := []struct {
		name     string
		readings []reading
		want     Span
	}{
		{name: "an unstalled interval is granted its wall time",
			readings: []reading{{wall: 3 * time.Second}},
			want:     Span{Wall: 3 * time.Second, Granted: 3 * time.Second}},
		{name: "a stalled interval is granted only the rest",
			readings: []reading{{wall: 3196 * time.Millisecond, stall: 2900 * time.Millisecond}},
			want:     Span{Wall: 3196 * time.Millisecond, Stalled: 2900 * time.Millisecond, Granted: 296 * time.Millisecond}},
		{name: "a fully stalled interval is granted nothing",
			readings: []reading{{wall: time.Second, stall: time.Second}},
			want:     Span{Wall: time.Second, Stalled: time.Second}},
		{name: "clock skew past a full stall reads as nothing",
			readings: []reading{{wall: time.Second, stall: time.Second + time.Microsecond}},
			want:     Span{Wall: time.Second, Stalled: time.Second + time.Microsecond}},
		{name: "a summed stall past wall grants nothing without cancelling the granted time after it",
			readings: []reading{{wall: time.Second, stall: 4 * time.Second}, {wall: 2 * time.Second}},
			want:     Span{Wall: 3 * time.Second, Stalled: 4 * time.Second, Granted: 2 * time.Second}},
		{name: "a summed stall past wall grants nothing without cancelling the granted time before it",
			readings: []reading{{wall: 2 * time.Second}, {wall: time.Second, stall: 4 * time.Second}},
			want:     Span{Wall: 3 * time.Second, Stalled: 4 * time.Second, Granted: 2 * time.Second}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a clock started with history already on both counters.
			src := &fakeClockSource{wall: time.Hour, stall: 40 * time.Minute}
			clock, err := Start(src.now, src.stalled)
			if err != nil {
				t.Fatalf("Start() = %v", err)
			}

			// Act
			var span Span
			for _, r := range tt.readings {
				src.wall += r.wall
				src.stall += r.stall
				if span, err = clock.Span(); err != nil {
					t.Fatalf("Span() = %v", err)
				}
			}

			// Assert
			if span != tt.want {
				t.Fatalf("Span() = %+v, want %+v", span, tt.want)
			}
		})
	}
}

func TestClockRefusesAReadingItCannotTrust(t *testing.T) {
	tests := []struct {
		name          string
		advanceStall  time.Duration
		failLater     error
		wantErrSubstr string
	}{
		{name: "a stall that cannot be read later fails the reading", failLater: errors.New("EIO"),
			wantErrSubstr: "read the CPU stall: EIO"},
		{name: "a stall that went backwards fails the reading", advanceStall: -time.Millisecond,
			wantErrSubstr: "the CPU stall went backwards, from 40m0s to 39m59.999s"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: one good reading, then the bad one.
			src := &fakeClockSource{wall: time.Hour, stall: 40 * time.Minute}
			clock, err := Start(src.now, src.stalled)
			if err != nil {
				t.Fatalf("Start() = %v", err)
			}
			src.wall += time.Second
			good, err := clock.Span()
			if err != nil {
				t.Fatalf("Span() = %v", err)
			}
			src.wall += time.Second
			src.stall += tt.advanceStall
			src.err = tt.failLater

			// Act
			span, err := clock.Span()

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.wantErrSubstr) {
				t.Fatalf("Span() = %v, %v, want an error naming %q", span, err, tt.wantErrSubstr)
			}
			if span != good {
				t.Fatalf("Span() = %+v after a refused reading, want the last good one %+v", span, good)
			}
		})
	}
}

func TestStartRefusesAnUnreadableStall(t *testing.T) {
	// Arrange
	src := &fakeClockSource{err: errors.New("no such file")}

	// Act
	clock, err := Start(src.now, src.stalled)

	// Assert
	if clock != nil || err == nil || !strings.Contains(err.Error(), "to start a granted clock: no such file") {
		t.Fatalf("Start() = %v, %v, want a refusal naming the read", clock, err)
	}
}

// scriptedClock answers one span per reading from a script, and repeats the
// last entry once the script runs out.
type scriptedClock struct {
	spans []Span
	errs  []error
	reads int
	// onRead, when set, runs after each reading with the count so far.
	onRead func(reads int)
}

func (c *scriptedClock) Span() (Span, error) {
	i := min(c.reads, len(c.spans)-1)
	c.reads++
	if c.onRead != nil {
		c.onRead(c.reads)
	}
	var err error
	if i < len(c.errs) {
		err = c.errs[i]
	}
	return c.spans[i], err
}

func TestAwaitGivesUpOnlyOnGrantedTime(t *testing.T) {
	over := Span{Wall: 4 * time.Second, Granted: 4 * time.Second}
	starved := Span{Wall: 30 * time.Second, Stalled: 29 * time.Second, Granted: time.Second}
	under := Span{Wall: time.Second, Granted: time.Second}
	tests := []struct {
		name         string
		clock        *scriptedClock
		polls        int
		answerReady  bool
		wantAnswered bool
		wantErr      error
		wantSpan     Span
	}{
		{name: "an answer before any poll is taken", clock: &scriptedClock{spans: []Span{under}},
			answerReady: true, wantAnswered: true, wantSpan: under},
		{name: "a wait past its bound in granted time gives up", clock: &scriptedClock{spans: []Span{under, over}},
			polls: 2, wantErr: ErrBoundRunOut, wantSpan: over},
		{name: "a wait past its bound only in wall time keeps waiting", clock: &scriptedClock{spans: []Span{starved}},
			polls: 3, wantAnswered: true, wantSpan: starved},
		{name: "a clock that cannot be read ends the wait", clock: &scriptedClock{spans: []Span{{}}, errs: []error{errors.New("EIO")}},
			polls: 1, wantErr: errors.New("EIO")},
		{name: "an answer that arrived with the tick beats a run-out bound", clock: &scriptedClock{spans: []Span{over}},
			polls: 1, answerReady: true, wantAnswered: true, wantSpan: over},
		{name: "an answer whose span cannot be read is still answered", clock: &scriptedClock{spans: []Span{{}}, errs: []error{errors.New("EIO")}},
			answerReady: true, wantAnswered: true, wantErr: errors.New("EIO")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: POLLS ticks queued; the answer is either queued with
			// them or sent once the clock has been read on every one of them
			// (a wait that stayed starved throughout).
			answer := make(chan string, 1)
			poll := make(chan time.Time, tt.polls)
			for range tt.polls {
				poll <- time.Time{}
			}
			if tt.answerReady {
				answer <- "pid"
			} else if tt.wantAnswered {
				tt.clock.onRead = func(reads int) {
					if reads == tt.polls {
						answer <- "pid"
					}
				}
			}

			// Act
			got, answered, span, err := Await(answer, poll, tt.clock, 3*time.Second)

			// Assert
			if answered != tt.wantAnswered {
				t.Fatalf("Await() answered = %v, want %v (err %v)", answered, tt.wantAnswered, err)
			}
			if answered && got != "pid" {
				t.Fatalf("Await() answer = %q, want pid", got)
			}
			if (err == nil) != (tt.wantErr == nil) || (err != nil && err.Error() != tt.wantErr.Error()) {
				t.Fatalf("Await() err = %v, want %v", err, tt.wantErr)
			}
			if span != tt.wantSpan {
				t.Fatalf("Await() span = %+v, want %+v", span, tt.wantSpan)
			}
		})
	}
}

func TestPollUntilGivesUpOnlyOnGrantedTime(t *testing.T) {
	over := Span{Wall: 6 * time.Second, Granted: 6 * time.Second}
	starved := Span{Wall: 60 * time.Second, Stalled: 58 * time.Second, Granted: 2 * time.Second}
	under := Span{Wall: time.Second, Granted: time.Second}
	tests := []struct {
		name         string
		clock        *scriptedClock
		doneOn       int // the attempt that succeeds, or 0 for none
		wantAttempts int
		wantErr      error
		wantSpan     Span
	}{
		{name: "a first attempt that succeeds is measured", clock: &scriptedClock{spans: []Span{under}},
			doneOn: 1, wantAttempts: 1, wantSpan: under},
		{name: "a wait past its bound in granted time gives up", clock: &scriptedClock{spans: []Span{under, over}},
			wantAttempts: 2, wantErr: ErrBoundRunOut, wantSpan: over},
		{name: "a wait past its bound only in wall time keeps polling", clock: &scriptedClock{spans: []Span{starved}},
			doneOn: 4, wantAttempts: 4, wantSpan: starved},
		{name: "a last attempt that succeeds is never failed for its timing", clock: &scriptedClock{spans: []Span{under, over}},
			doneOn: 2, wantAttempts: 2, wantSpan: over},
		{name: "a clock that cannot be read ends the wait", clock: &scriptedClock{spans: []Span{{}}, errs: []error{errors.New("EIO")}},
			wantAttempts: 1, wantErr: errors.New("EIO")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a poll that is always ready, so only the clock and
			// the attempts decide when the wait ends.
			poll := make(chan time.Time)
			close(poll)
			attempts := 0

			// Act
			span, err := PollUntil(poll, tt.clock, 5*time.Second, func() bool {
				attempts++
				return attempts == tt.doneOn
			})

			// Assert
			if attempts != tt.wantAttempts {
				t.Fatalf("PollUntil() made %d attempts, want %d", attempts, tt.wantAttempts)
			}
			if (err == nil) != (tt.wantErr == nil) || (err != nil && err.Error() != tt.wantErr.Error()) {
				t.Fatalf("PollUntil() err = %v, want %v", err, tt.wantErr)
			}
			if span != tt.wantSpan {
				t.Fatalf("PollUntil() span = %+v, want %+v", span, tt.wantSpan)
			}
		})
	}
}
