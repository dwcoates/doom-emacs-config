package e2e

import (
	"errors"
	"strings"
	"testing"
	"time"
)

func TestParseCPUPressureSomeReadsTheSomeTotal(t *testing.T) {
	tests := []struct {
		name    string
		body    string
		want    time.Duration
		wantErr string
	}{
		{name: "the some total is read in microseconds",
			body: "some avg10=14.31 avg60=2.58 avg300=0.54 total=2690646\nfull avg10=14.31 avg60=2.58 avg300=0.54 total=2690645\n",
			want: 2690646 * time.Microsecond},
		{name: "the full line is never read for it",
			body: "full avg10=0.00 avg60=0.00 avg300=0.00 total=9\nsome avg10=0.00 avg60=0.00 avg300=0.00 total=66\n",
			want: 66 * time.Microsecond},
		{name: "a file with no some line is refused", body: "full avg10=0.00 total=9\n",
			wantErr: "no some line"},
		{name: "a some line with no total is refused", body: "some avg10=0.00 avg60=0.00\n",
			wantErr: "carries no total"},
		{name: "a total that is not a number is refused", body: "some avg10=0.00 total=12x\n",
			wantErr: `the some line's total "12x"`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: tt.body.

			// Act
			got, err := parseCPUPressureSome(tt.body)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("parseCPUPressureSome() = %s, %v, want an error naming %q", got, err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("parseCPUPressureSome() = %s, %v, want %s", got, err, tt.want)
			}
		})
	}
}

func TestGrantedSpanChargesOnlyUnstalledTime(t *testing.T) {
	tests := []struct {
		name string
		span grantedSpan
		want time.Duration
	}{
		{name: "an unstalled interval is granted its wall time", span: grantedSpan{Wall: 3 * time.Second}, want: 3 * time.Second},
		{name: "a stalled interval is granted only the rest", span: grantedSpan{Wall: 3196 * time.Millisecond, Stalled: 2900 * time.Millisecond}, want: 296 * time.Millisecond},
		{name: "a fully stalled interval is granted nothing", span: grantedSpan{Wall: time.Second, Stalled: time.Second}, want: 0},
		{name: "clock skew past a full stall reads as nothing", span: grantedSpan{Wall: time.Second, Stalled: time.Second + time.Microsecond}, want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: tt.span.

			// Act
			got := tt.span.Granted()

			// Assert
			if got != tt.want {
				t.Fatalf("Granted() = %s, want %s", got, tt.want)
			}
		})
	}
}

func TestGrantedSpanStringNamesAllThreeTimes(t *testing.T) {
	// Arrange
	span := grantedSpan{Wall: 3196 * time.Millisecond, Stalled: 2900 * time.Millisecond}

	// Act
	got := span.String()

	// Assert
	want := "296ms granted of 3.196s wall, 2.9s of it with a container task waiting for a CPU"
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

func TestGrantedClockMeasuresFromItsStart(t *testing.T) {
	tests := []struct {
		name          string
		advanceWall   time.Duration
		advanceStall  time.Duration
		failLater     error
		wantSpan      grantedSpan
		wantErrSubstr string
	}{
		{name: "the span counts only what happened after the start", advanceWall: 2 * time.Second, advanceStall: 500 * time.Millisecond,
			wantSpan: grantedSpan{Wall: 2 * time.Second, Stalled: 500 * time.Millisecond}},
		{name: "a stall that cannot be read later fails the reading", advanceWall: time.Second, failLater: errors.New("EIO"),
			wantErrSubstr: "read the container's CPU stall: EIO"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a clock started with history already on both counters.
			src := &fakeClockSource{wall: time.Hour, stall: 40 * time.Minute}
			clock, err := startGrantedClock(src.now, src.stalled)
			if err != nil {
				t.Fatalf("startGrantedClock() = %v", err)
			}
			src.wall += tt.advanceWall
			src.stall += tt.advanceStall
			src.err = tt.failLater

			// Act
			span, err := clock.span()

			// Assert
			if tt.wantErrSubstr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErrSubstr) {
					t.Fatalf("span() = %v, %v, want an error naming %q", span, err, tt.wantErrSubstr)
				}
				return
			}
			if err != nil || span != tt.wantSpan {
				t.Fatalf("span() = %+v, %v, want %+v", span, err, tt.wantSpan)
			}
		})
	}
}

func TestStartGrantedClockRefusesAnUnreadableStall(t *testing.T) {
	// Arrange
	src := &fakeClockSource{err: errors.New("no such file")}

	// Act
	clock, err := startGrantedClock(src.now, src.stalled)

	// Assert
	if clock != nil || err == nil || !strings.Contains(err.Error(), "to start a granted clock: no such file") {
		t.Fatalf("startGrantedClock() = %v, %v, want a refusal naming the read", clock, err)
	}
}

// scriptedClock answers one span per reading from a script, and repeats the
// last entry once the script runs out.
type scriptedClock struct {
	spans []grantedSpan
	errs  []error
	reads int
	// onRead, when set, runs after each reading with the count so far.
	onRead func(reads int)
}

func (c *scriptedClock) span() (grantedSpan, error) {
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

func TestAwaitGrantedGivesUpOnlyOnGrantedTime(t *testing.T) {
	over := grantedSpan{Wall: 4 * time.Second}
	starved := grantedSpan{Wall: 30 * time.Second, Stalled: 29 * time.Second}
	under := grantedSpan{Wall: time.Second}
	tests := []struct {
		name         string
		clock        *scriptedClock
		polls        int
		answerReady  bool
		wantAnswered bool
		wantErr      error
		wantSpan     grantedSpan
	}{
		{name: "an answer before any poll is taken", clock: &scriptedClock{spans: []grantedSpan{under}},
			answerReady: true, wantAnswered: true, wantSpan: under},
		{name: "a wait past its bound in granted time gives up", clock: &scriptedClock{spans: []grantedSpan{under, over}},
			polls: 2, wantErr: errGrantedBoundRunOut, wantSpan: over},
		{name: "a wait past its bound only in wall time keeps waiting", clock: &scriptedClock{spans: []grantedSpan{starved}},
			polls: 3, wantAnswered: true, wantSpan: starved},
		{name: "a clock that cannot be read ends the wait", clock: &scriptedClock{spans: []grantedSpan{{}}, errs: []error{errors.New("EIO")}},
			polls: 1, wantErr: errors.New("EIO")},
		{name: "an answer that arrived with the tick beats a run-out bound", clock: &scriptedClock{spans: []grantedSpan{over}},
			polls: 1, answerReady: true, wantAnswered: true, wantSpan: over},
		{name: "an answer whose span cannot be read is still answered", clock: &scriptedClock{spans: []grantedSpan{{}}, errs: []error{errors.New("EIO")}},
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
			got, answered, span, err := awaitGranted(answer, poll, tt.clock, 3*time.Second)

			// Assert
			if answered != tt.wantAnswered {
				t.Fatalf("awaitGranted() answered = %v, want %v (err %v)", answered, tt.wantAnswered, err)
			}
			if answered && got != "pid" {
				t.Fatalf("awaitGranted() answer = %q, want pid", got)
			}
			if (err == nil) != (tt.wantErr == nil) || (err != nil && err.Error() != tt.wantErr.Error()) {
				t.Fatalf("awaitGranted() err = %v, want %v", err, tt.wantErr)
			}
			if span != tt.wantSpan {
				t.Fatalf("awaitGranted() span = %+v, want %+v", span, tt.wantSpan)
			}
		})
	}
}

func TestPollUntilGrantedGivesUpOnlyOnGrantedTime(t *testing.T) {
	over := grantedSpan{Wall: 6 * time.Second}
	starved := grantedSpan{Wall: 60 * time.Second, Stalled: 58 * time.Second}
	under := grantedSpan{Wall: time.Second}
	tests := []struct {
		name         string
		clock        *scriptedClock
		doneOn       int // the attempt that succeeds, or 0 for none
		wantAttempts int
		wantErr      error
		wantSpan     grantedSpan
	}{
		{name: "a first attempt that succeeds is measured", clock: &scriptedClock{spans: []grantedSpan{under}},
			doneOn: 1, wantAttempts: 1, wantSpan: under},
		{name: "a wait past its bound in granted time gives up", clock: &scriptedClock{spans: []grantedSpan{under, over}},
			wantAttempts: 2, wantErr: errGrantedBoundRunOut, wantSpan: over},
		{name: "a wait past its bound only in wall time keeps polling", clock: &scriptedClock{spans: []grantedSpan{starved}},
			doneOn: 4, wantAttempts: 4, wantSpan: starved},
		{name: "a last attempt that succeeds is never failed for its timing", clock: &scriptedClock{spans: []grantedSpan{under, over}},
			doneOn: 2, wantAttempts: 2, wantSpan: over},
		{name: "a clock that cannot be read ends the wait", clock: &scriptedClock{spans: []grantedSpan{{}}, errs: []error{errors.New("EIO")}},
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
			span, err := pollUntilGranted(poll, tt.clock, 5*time.Second, func() bool {
				attempts++
				return attempts == tt.doneOn
			})

			// Assert
			if attempts != tt.wantAttempts {
				t.Fatalf("pollUntilGranted() made %d attempts, want %d", attempts, tt.wantAttempts)
			}
			if (err == nil) != (tt.wantErr == nil) || (err != nil && err.Error() != tt.wantErr.Error()) {
				t.Fatalf("pollUntilGranted() err = %v, want %v", err, tt.wantErr)
			}
			if span != tt.wantSpan {
				t.Fatalf("pollUntilGranted() span = %+v, want %+v", span, tt.wantSpan)
			}
		})
	}
}
