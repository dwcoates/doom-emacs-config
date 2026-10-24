package merge

import (
	"context"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/scriptrunner"
)

// This file holds the synchronization every fake in the package shares: the
// holds a test arms to park one call where it stands, and the bounded waits
// a test makes on what it caused.

// holdSet parks the calls a test names. Each hold armed for a key parks
// exactly one call made with that key, in the order the holds were armed. The
// zero value is ready to use.
type holdSet struct {
	mu    sync.Mutex
	armed map[string][]callHold
}

// callHold is one parked call: entered closes as the call parks, and the call
// goes on once release closes.
type callHold struct{ entered, release chan struct{} }

// arm parks the next call made with KEY that no earlier hold parks, answering
// when it parked and the release that lets it go on.
func (s *holdSet) arm(key string) (entered, release chan struct{}) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.armed == nil {
		s.armed = map[string][]callHold{}
	}
	h := callHold{entered: make(chan struct{}), release: make(chan struct{})}
	s.armed[key] = append(s.armed[key], h)
	return h.entered, h.release
}

// claim takes the first hold armed for KEY, if any, so it parks exactly one
// call.
func (s *holdSet) claim(key string) (callHold, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	hs := s.armed[key]
	if len(hs) == 0 {
		return callHold{}, false
	}
	if len(hs) == 1 {
		delete(s.armed, key)
	} else {
		s.armed[key] = hs[1:]
	}
	return hs[0], true
}

// pending answers how many holds stand armed for KEY.
func (s *holdSet) pending(key string) int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return len(s.armed[key])
}

// park stands the call until its release, or until CTX ends, which it reports
// as false.
func (h callHold) park(ctx context.Context) bool {
	close(h.entered)
	select {
	case <-h.release:
		return true
	case <-ctx.Done():
		return false
	}
}

// parkUncancelled stands the call until its release alone, as a git command
// mid-flight ignores its caller's cancellation.
func (h callHold) parkUncancelled() {
	close(h.entered)
	<-h.release
}

// waitBound is how long a test waits for something it caused before it fails.
const waitBound = 5 * time.Second

// receiveWithin answers the next value CH delivers, or the zero value once CH
// closes, failing the test when WHAT has not happened within waitBound.
func receiveWithin[T any](t *testing.T, ch <-chan T, what string) T {
	t.Helper()
	select {
	case v := <-ch:
		return v
	case <-time.After(waitBound):
		t.Fatalf("%s never happened", what)
		var zero T
		return zero
	}
}

// receiveUntil receives from CH until a value MATCH accepts, failing the test
// when WHAT has not happened within waitBound of the first receive.
func receiveUntil[T any](t *testing.T, ch <-chan T, what string, match func(T) bool) {
	t.Helper()
	bound := time.NewTimer(waitBound)
	defer bound.Stop()
	for {
		select {
		case v := <-ch:
			if match(v) {
				return
			}
		case <-bound.C:
			t.Fatalf("%s never happened", what)
		}
	}
}

// liveGone closes once every goroutine O counted has ended.
func liveGone(o *orchestrator) <-chan struct{} {
	gone := make(chan struct{})
	go func() { o.live.Wait(); close(gone) }()
	return gone
}

func TestAHoldSetParksOneCallPerArmedHoldInItsKeyInArmingOrder(t *testing.T) {
	// Arrange
	var s holdSet
	firstIn, firstOut := s.arm("dir-a")
	secondIn, secondOut := s.arm("dir-a")

	// Act
	other, otherHeld := s.claim("dir-b")
	first, firstHeld := s.claim("dir-a")
	second, secondHeld := s.claim("dir-a")
	_, againHeld := s.claim("dir-a")

	// Assert
	if otherHeld || other.entered != nil {
		t.Fatal("a hold armed in dir-a parked a call in dir-b")
	}
	if !firstHeld || first.entered != firstIn || first.release != firstOut {
		t.Fatal("the first call in dir-a did not claim the first hold armed there")
	}
	if !secondHeld || second.entered != secondIn || second.release != secondOut {
		t.Fatal("the second call in dir-a did not claim the second hold armed there")
	}
	if againHeld || s.pending("dir-a") != 0 {
		t.Fatal("a hold parked a call beyond the one it was armed for")
	}
}

func TestAParkedCallGoesOnAtItsRelease(t *testing.T) {
	// Arrange
	var s holdSet
	entered, release := s.arm("k")
	h, _ := s.claim("k")
	parked := make(chan bool, 1)

	// Act
	go func() { parked <- h.park(context.Background()) }()
	receiveWithin(t, entered, "the call parking")
	close(release)

	// Assert
	if !receiveWithin(t, parked, "the parked call going on") {
		t.Fatal("park = false at its release, want true")
	}
}

func TestAParkedCallEndsWithItsCaller(t *testing.T) {
	// Arrange
	var s holdSet
	s.arm("k")
	h, _ := s.claim("k")
	ctx, cancel := context.WithCancel(context.Background())
	parked := make(chan bool, 1)

	// Act
	go func() { parked <- h.park(ctx) }()
	receiveWithin(t, h.entered, "the call parking")
	cancel()

	// Assert
	if receiveWithin(t, parked, "the parked call ending") {
		t.Fatal("park = true at its caller's end, want false")
	}
}

func TestAnUncancelledParkWaitsForItsReleaseAlone(t *testing.T) {
	// Arrange
	var s holdSet
	entered, release := s.arm("k")
	h, _ := s.claim("k")
	done := make(chan struct{})

	// Act
	go func() { defer close(done); h.parkUncancelled() }()
	receiveWithin(t, entered, "the call parking")
	select {
	case <-done:
		t.Fatal("the uncancelled park went on before its release")
	default:
	}
	close(release)

	// Assert
	receiveWithin(t, done, "the parked call going on")
}

func TestReceiveWithinAnswersTheValueDelivered(t *testing.T) {
	// Arrange
	ch := make(chan int, 1)
	ch <- 7

	// Act
	got := receiveWithin(t, ch, "the value")

	// Assert
	if got != 7 {
		t.Fatalf("receiveWithin = %d, want 7", got)
	}
}

func TestReceiveUntilPassesOverValuesItsMatchRefuses(t *testing.T) {
	// Arrange
	ch := make(chan int, 3)
	ch <- 1
	ch <- 2
	ch <- 3
	var seen []int

	// Act
	receiveUntil(t, ch, "the value 2", func(v int) bool { seen = append(seen, v); return v == 2 })

	// Assert
	if len(seen) != 2 || len(ch) != 1 {
		t.Fatalf("receiveUntil saw %v leaving %d, want it to stop at 2 leaving 3 unread", seen, len(ch))
	}
}

func TestLiveGoneClosesOnceEveryCountedGoroutineEnds(t *testing.T) {
	// Arrange
	o := &orchestrator{}
	o.live.Add(1)
	gone := liveGone(o)
	select {
	case <-gone:
		t.Fatal("liveGone closed while a counted goroutine runs")
	default:
	}

	// Act
	o.live.Done()

	// Assert
	receiveWithin(t, gone, "the counted goroutines ending")
}

// holdCase is one fake's hold: arm arms it for the call, answering its
// channels and the holdSet and key it must stand in, and call makes the call.
type holdCase struct {
	name string
	arm  func() (entered, release chan struct{}, set *holdSet, key string)
	call func()
}

// holdCases answers a holdCase for every fake in the package with a hold.
func holdCases() []holdCase {
	g := newFakeGit(func() int { return 0 })
	cg := newChainGit(newFakeGit(func() int { return 0 }))
	runner := newChainRunner()
	hooks := &fakeHookRunner{}
	const hook, worktree = "/x/hook.sh", "/w"
	return []holdCase{
		{"fakeGit", func() (chan struct{}, chan struct{}, *holdSet, string) {
			in, out := g.hold("default_branch")
			return in, out, &g.holds, "default_branch"
		}, func() { _, _ = g.DefaultBranch(context.Background(), "") }},
		{"chainGit rebase", func() (chan struct{}, chan struct{}, *holdSet, string) {
			in, out := cg.holdRebase("dir-a")
			return in, out, &cg.rebaseHolds, "dir-a"
		}, func() {
			cg.branch("feature", "a1")
			_, _ = cg.StartRebase(context.Background(), "dir-a", "master", []string{cg.ref("feature")})
		}},
		{"chainGit clean", func() (chan struct{}, chan struct{}, *holdSet, string) {
			in, out := cg.holdClean("dir-a")
			return in, out, &cg.cleanHolds, "dir-a"
		}, func() { _, _ = cg.IsClean(context.Background(), "dir-a") }},
		{"chainRunner", func() (chan struct{}, chan struct{}, *holdSet, string) {
			in, out := runner.hold("dir-a")
			return in, out, &runner.holds, "dir-a"
		}, func() {
			_, _ = runner.RunLinesMeasured(context.Background(), "dir-a", []string{"t.sh"}, func(string) {})
		}},
		{"fakeHookRunner", func() (chan struct{}, chan struct{}, *holdSet, string) {
			in, out := hooks.hold(hook, worktree)
			return in, out, &hooks.holds, holdKey(hook, worktree)
		}, func() {
			_, _ = hooks.RunLinesWith(context.Background(), "/x", []string{"bash", hook},
				scriptrunner.Options{Env: []string{EnvCallingWorkspace + "=" + worktree}}, func(string) {})
		}},
	}
}

func TestEveryFakesHoldParksItsCallThroughTheOneHoldSet(t *testing.T) {
	// Each fake's hold parks exactly the call it names through a holdSet: a
	// hold hand-rolled beside it fails here.
	for _, tc := range holdCases() {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			entered, release, set, key := tc.arm()
			if set.pending(key) != 1 {
				t.Fatal("the fake's hold was not armed in its holdSet")
			}
			ended := make(chan struct{})

			// Act
			go func() { defer close(ended); tc.call() }()
			receiveWithin(t, entered, "the held call parking")
			select {
			case <-ended:
				t.Fatal("the held call ended before its release")
			default:
			}
			close(release)
			receiveWithin(t, ended, "the released call ending")

			// Assert
			if set.pending(key) != 0 {
				t.Fatal("the hold outlived the one call it parks")
			}
		})
	}
}
