package seats

import (
	"errors"
	"os"
	"reflect"
	"strings"
	"syscall"
	"testing"
	"time"
)

// admitAll is an admission that always admits and must never wait.
func admitAll(t *testing.T, me Holder) Admission {
	return Admission{
		Me:      me,
		Check:   func([]Holder) (bool, error) { return true, nil },
		Waiting: func(w Wait) error { t.Fatalf("an admitting taker waited: %+v", w); return nil },
		Recheck: func() <-chan time.Time { return nil },
	}
}

// admitted takes a seat of p for me at once, as another run would.
func admitted(t *testing.T, p Pool, me Holder) *Seat {
	t.Helper()
	s, err := p.Admit(admitAll(t, me))
	if err != nil {
		t.Fatalf("admit %+v: %v", me, err)
	}
	return s
}

// taken is one Admit's answer.
type taken struct {
	s   *Seat
	err error
}

// admitAsync runs Admit in a goroutine, answering on the channel returned.
func admitAsync(p Pool, a Admission) <-chan taken {
	done := make(chan taken, 1)
	go func() {
		s, err := p.Admit(a)
		done <- taken{s, err}
	}()
	return done
}

var (
	gate   = Holder{Pid: 101, Workspace: "gate-ws", Rank: 10, Size: 4096}
	runA   = Holder{Pid: 202, Workspace: "fix-footer", Rank: 19, Size: 4096}
	byHand = Holder{Pid: 303, Rank: 19, Size: 4096}
)

func TestAdmitTakesAFreeSeatAtOnceAndNamesItsHolder(t *testing.T) {
	// Arrange
	p := Pool{Dir: t.TempDir(), Prefix: "ram", N: 2, What: "test"}
	first := admitted(t, p, runA)
	defer first.Release()
	var checked []Holder

	// Act: the next taker's check sees the first holder, named.
	a := admitAll(t, byHand)
	a.Check = func(held []Holder) (bool, error) { checked = held; return true, nil }
	second, err := p.Admit(a)

	// Assert
	if err != nil || second.Index != 1 {
		t.Fatalf("Admit = %+v, %v; want seat 1", second, err)
	}
	defer second.Release()
	if !reflect.DeepEqual(checked, []Holder{runA}) || !reflect.DeepEqual(second.Held, []Holder{runA}) {
		t.Fatalf("the check saw %+v and the seat names %+v; want the first holder %+v", checked, second.Held, runA)
	}
}

func TestAdmitRefusesWhatItCannotRun(t *testing.T) {
	cases := []struct {
		name string
		n    int
		edit func(*Admission)
		want string
	}{
		{name: "an empty pool", n: 0, edit: func(*Admission) {}, want: "has 0 seats"},
		{name: "a negative rank", n: 1, edit: func(a *Admission) { a.Me.Rank = -1 }, want: "a rank is zero or more"},
		{name: "no check", n: 1, edit: func(a *Admission) { a.Check = nil }, want: "needs its Check, Waiting and Recheck"},
		{name: "no recheck", n: 1, edit: func(a *Admission) { a.Recheck = nil }, want: "needs its Check, Waiting and Recheck"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			p := Pool{Dir: t.TempDir(), Prefix: "ram", N: tc.n, What: "test"}
			a := admitAll(t, runA)
			tc.edit(&a)

			// Act
			s, err := p.Admit(a)

			// Assert
			if err == nil || s != nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Admit = %v, %v; want it refused with %q", s, err, tc.want)
			}
		})
	}
}

func TestAdmitSurfacesTheChecksFailure(t *testing.T) {
	// Arrange
	p := Pool{Dir: t.TempDir(), Prefix: "ram", N: 1, What: "test"}
	a := admitAll(t, runA)
	a.Check = func([]Holder) (bool, error) { return false, errors.New("the memory read failed") }
	a.Waiting = func(Wait) error { return nil }

	// Act
	s, err := p.Admit(a)

	// Assert
	if err == nil || s != nil || !strings.Contains(err.Error(), "the memory read failed") {
		t.Fatalf("Admit = %v, %v; want the check's failure", s, err)
	}
	if held, _ := p.Held(0); held {
		t.Fatal("the refused taker kept the seat")
	}
}

func TestAdmitEndsTheWaitOnWaitingsFailure(t *testing.T) {
	// Arrange: the only seat is held, and the wait cannot be stated.
	p := Pool{Dir: t.TempDir(), Prefix: "ram", N: 1, What: "test"}
	other := admitted(t, p, runA)
	defer other.Release()
	a := admitAll(t, gate)
	a.Waiting = func(Wait) error { return errors.New("the memory read failed") }

	// Act
	s, err := p.Admit(a)

	// Assert
	if err == nil || s != nil || !strings.Contains(err.Error(), "the memory read failed") {
		t.Fatalf("Admit = %v, %v; want the wait's failure", s, err)
	}
}

func TestAdmitRefusesAHeldSeatWhoseRecordDoesNotRead(t *testing.T) {
	// Arrange: a seat held by a taker that never named itself.
	p := Pool{Dir: t.TempDir(), Prefix: "ram", N: 1, What: "test"}
	f, err := p.TryTake(0)
	if err != nil || f == nil {
		t.Fatalf("hold seat 0: %v", err)
	}
	defer f.Close()

	// Act
	s, err := p.Admit(admitAll(t, runA))

	// Assert
	if err == nil || s != nil || !strings.Contains(err.Error(), "its holder's record") {
		t.Fatalf("Admit = %v, %v; want the unreadable record refused", s, err)
	}
}

func TestAdmitWaitsAndWakes(t *testing.T) {
	cases := []struct {
		name string
		// n is the pool's size; the other run holds seat 0.
		n int
		// refuseFirst makes the check refuse until it is asked again.
		refuseFirst bool
		wantWhy     Why
		// wake is how the wait ends: the other run lets its seat go, or the
		// recheck fires.
		wake string
	}{
		{name: "a full pool wakes when the holder lets go", n: 1, wantWhy: Full, wake: "release"},
		{name: "a refused taker wakes when a holder lets go", n: 2, refuseFirst: true, wantWhy: Refused, wake: "release"},
		{name: "a refused taker wakes on the recheck", n: 2, refuseFirst: true, wantWhy: Refused, wake: "recheck"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			p := Pool{Dir: t.TempDir(), Prefix: "ram", N: tc.n, What: "test"}
			other := admitted(t, p, runA)
			defer other.Release()
			waits := make(chan Wait, 4)
			recheck := make(chan time.Time, 1)
			asked := 0
			a := Admission{
				Me: gate,
				Check: func([]Holder) (bool, error) {
					asked++
					return !tc.refuseFirst || asked > 1, nil
				},
				Waiting: func(w Wait) error { waits <- w; return nil },
				Recheck: func() <-chan time.Time { return recheck },
			}

			// Act
			done := admitAsync(p, a)
			w := <-waits
			if tc.wake == "release" {
				if err := other.Release(); err != nil {
					t.Fatal(err)
				}
			} else {
				recheck <- time.Time{}
			}
			got := <-done

			// Assert
			if w.Why != tc.wantWhy || !reflect.DeepEqual(w.Held, []Holder{runA}) {
				t.Fatalf("the wait was %+v; want %v behind %+v", w, tc.wantWhy, runA)
			}
			if got.err != nil {
				t.Fatalf("Admit after the wake = %v", got.err)
			}
			if err := got.s.Release(); err != nil {
				t.Fatal(err)
			}
		})
	}
}

func TestAdmitWaitsForABetterRankWaiting(t *testing.T) {
	// Arrange: a better-ranked taker (rank 10) waits, holding its lane.
	p := Pool{Dir: t.TempDir(), Prefix: "ram", N: 1, What: "test"}
	if err := os.MkdirAll(p.Dir, 0o755); err != nil {
		t.Fatal(err)
	}
	better, err := p.lock(p.lanePath(10), syscall.LOCK_SH)
	if err != nil {
		t.Fatal(err)
	}
	waits := make(chan Wait, 1)
	a := admitAll(t, runA)
	a.Waiting = func(w Wait) error { waits <- w; return nil }

	// Act: the rank-19 taker finds a free seat, and still waits.
	done := admitAsync(p, a)
	w := <-waits
	stillFree := true
	if held, _ := p.Held(0); held {
		stillFree = false
	}
	if err := better.Close(); err != nil {
		t.Fatal(err)
	}
	got := <-done

	// Assert
	if w.Why != Outranked || !stillFree {
		t.Fatalf("the wait was %+v with the seat free %v; want outranked, the seat left free", w, stillFree)
	}
	if got.err != nil {
		t.Fatalf("Admit after the better rank left = %v", got.err)
	}
	if err := got.s.Release(); err != nil {
		t.Fatal(err)
	}
}

func TestAdmitIgnoresAWorseRankWaiting(t *testing.T) {
	// Arrange: a worse-ranked taker (rank 19) waits.
	p := Pool{Dir: t.TempDir(), Prefix: "ram", N: 1, What: "test"}
	if err := os.MkdirAll(p.Dir, 0o755); err != nil {
		t.Fatal(err)
	}
	worse, err := p.lock(p.lanePath(19), syscall.LOCK_SH)
	if err != nil {
		t.Fatal(err)
	}
	defer worse.Close()

	// Act: the gate (rank 10) takes the seat at once.
	s, err := p.Admit(admitAll(t, gate))

	// Assert
	if err != nil {
		t.Fatalf("Admit = %v; want the seat at once", err)
	}
	if err := s.Release(); err != nil {
		t.Fatal(err)
	}
}

func TestReleaseFreesTheSeatAndItsWatch(t *testing.T) {
	// Arrange
	p := Pool{Dir: t.TempDir(), Prefix: "ram", N: 1, What: "test"}
	s := admitted(t, p, runA)

	// Act
	err := s.Release()

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if held, _ := p.Held(0); held {
		t.Fatal("the seat is still held after Release")
	}
	watch, err := p.lock(p.watchPath(0), syscall.LOCK_EX|syscall.LOCK_NB)
	if err != nil {
		t.Fatalf("the watch is still held after Release: %v", err)
	}
	watch.Close()
}

func TestParseHolder(t *testing.T) {
	cases := []struct {
		name    string
		raw     string
		want    Holder
		wantErr bool
	}{
		{name: "a workspace's run", raw: "pid=7\nworkspace=\"a b\"\nrank=10\nsize=4096\n", want: Holder{Pid: 7, Workspace: "a b", Rank: 10, Size: 4096}},
		{name: "a run started by hand", raw: "pid=7\nworkspace=\"\"\nrank=19\nsize=4096\n", want: Holder{Pid: 7, Rank: 19, Size: 4096}},
		{name: "an empty record", raw: "", wantErr: true},
		{name: "a missing field", raw: "pid=7\nrank=10\nsize=4096\n", wantErr: true},
		{name: "an unknown line", raw: "pid=7\nworkspace=\"a\"\nrank=10\nsize=4096\ncolor=red\n", wantErr: true},
		{name: "a field that does not read", raw: "pid=x\nworkspace=\"a\"\nrank=10\nsize=4096\n", wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := parseHolder(tc.raw)

			// Assert
			if (err != nil) != tc.wantErr || got != tc.want {
				t.Fatalf("parseHolder(%q) = %+v, %v; want %+v, error %v", tc.raw, got, err, tc.want, tc.wantErr)
			}
		})
	}
}
