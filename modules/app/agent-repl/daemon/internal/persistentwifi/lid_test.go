package persistentwifi

import (
	"context"
	"errors"
	"net"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"

	"agent-repld/internal/appfreeze"
	"agent-repld/internal/dlog"
)

// hear hands the controller one lid state, as WatchLid's heard does.
func (h *harness) hear(l Lid) {
	h.c.lidHeard(context.Background(), heardLid(l))
}

// lastStanding answers the standing last handed to OnChange.
func (h *harness) lastStanding(t *testing.T) *harness {
	t.Helper()
	if len(h.changes) == 0 {
		t.Fatal("no standing was published")
	}
	return h
}

func TestALidCloseWithTheModeOnEntersTheQuietState(t *testing.T) {
	// Arrange
	h := newHarness(t).standing(pmsetOn, summary(true, "Home"))
	h.freezer.frozen = []appfreeze.Frozen{{Name: "Slack", Processes: 4}}
	h.r.on(argPowerSource, answer{out: onBattery})
	h.r.on(argDisplaySleep, answer{})

	// Act
	h.hear(LidClosed)

	// Assert
	if !slices.Equal(h.viewMoves, []bool{true}) || h.freezer.count("freeze") != 1 || h.r.count(argDisplaySleep) != 1 {
		t.Fatalf("views %v, freezer %v, calls %v: want views stopped, one freeze, one display sleep", h.viewMoves, h.freezer.calls, h.r.ran())
	}
	last := h.lastStanding(t).changes[len(h.changes)-1]
	q := last.GetQuiet()
	if last.GetLidClosed() == nil || q == nil || q.GetDisplayAsleep() == nil ||
		len(q.GetFrozenApps()) != 1 || q.GetFrozenApps()[0].GetName() != "Slack" || q.GetFrozenApps()[0].GetProcesses() != 4 {
		t.Fatalf("standing = %v, want the lid closed and quiet with Slack's 4 processes frozen and the display asleep", last)
	}
	if !slices.Equal(h.telemetry.set, []bool{true}) {
		t.Fatalf("gauge set %v, want [true]", h.telemetry.set)
	}
	infos := h.records(dlog.LevelInfo, opQuiet)
	if len(infos) != 1 || infos[0].Context["trigger"] != "lid_closed" {
		t.Fatalf("INFO records = %+v, want the one entering record", infos)
	}
}

func TestALidOpenLeavesTheQuietStateInReverse(t *testing.T) {
	// Arrange
	h := newHarness(t).standing(pmsetOn, summary(true, "Home"))
	h.freezer.frozen = []appfreeze.Frozen{{Name: "Slack", Processes: 4}}
	h.r.on(argPowerSource, answer{out: onBattery})
	h.r.on(argDisplaySleep, answer{})
	h.r.on(argDisplayWake, answer{})
	h.hear(LidClosed)

	// Act
	h.hear(LidOpen)

	// Assert
	calls := h.r.ran()
	if h.r.count(argDisplayWake) != 1 || !slices.Equal(h.freezer.calls, []string{"freeze:lid_closed", "thaw:lid_open"}) {
		t.Fatalf("calls %v, freezer %v: want one wake and one thaw", calls, h.freezer.calls)
	}
	if !slices.Equal(h.viewMoves, []bool{true, false}) {
		t.Fatalf("views %v, want stopped then resumed", h.viewMoves)
	}
	if last := h.changes[len(h.changes)-1]; last.GetLidOpen() == nil || last.GetQuiet() != nil {
		t.Fatalf("standing = %v, want the lid open and no quiet state", last)
	}
	if !slices.Equal(h.telemetry.set, []bool{true, false}) {
		t.Fatalf("gauge set %v, want [true false]", h.telemetry.set)
	}
}

func TestALidCloseWithTheModeOffStaysAwake(t *testing.T) {
	// Arrange
	h := newHarness(t).standing(pmsetOff, summary(true, "Home"))

	// Act
	h.hear(LidClosed)

	// Assert
	if h.freezer.count("freeze") != 0 || h.r.count(argDisplaySleep) != 0 || len(h.viewMoves) != 0 {
		t.Fatalf("freezer %v, calls %v, views %v: want nothing quieted", h.freezer.calls, h.r.ran(), h.viewMoves)
	}
	if last := h.changes[len(h.changes)-1]; last.GetLidClosed() == nil || last.GetQuiet() != nil {
		t.Fatalf("standing = %v, want the lid closed and no quiet state", last)
	}
}

func TestADaemonHearingAnOpenLidThawsAKeptFrozenSet(t *testing.T) {
	// Arrange: nothing stands in this daemon; a kept set from an earlier one
	// is the freezer's to thaw.
	h := newHarness(t).standing(pmsetOn, summary(true, "Home"))

	// Act
	h.hear(LidOpen)

	// Assert
	if !slices.Equal(h.freezer.calls, []string{"thaw:lid_open"}) || len(h.viewMoves) != 0 {
		t.Fatalf("freezer %v, views %v: want one thaw and no view move", h.freezer.calls, h.viewMoves)
	}
}

func TestARepeatedClosedLidNeverFreezesTwice(t *testing.T) {
	// Arrange
	h := newHarness(t).standing(pmsetOn, summary(true, "Home"))
	h.r.on(argPowerSource, answer{out: onBattery})
	h.r.on(argDisplaySleep, answer{})
	h.hear(LidClosed)

	// Act
	h.hear(LidClosed)

	// Assert
	if h.freezer.count("freeze") != 1 || h.r.count(argDisplaySleep) != 1 {
		t.Fatalf("freezer %v, calls %v: want one freeze and one sleep", h.freezer.calls, h.r.ran())
	}
}

func TestAFailedDisplaySleepIsTriedAgainOnTheNextLidReport(t *testing.T) {
	// Arrange
	h := newHarness(t).standing(pmsetOn, summary(true, "Home"))
	h.r.on(argPowerSource, answer{out: onBattery})
	h.r.on(argDisplaySleep, answer{out: "no display", code: 1}, answer{})
	h.hear(LidClosed)
	awake := h.changes[len(h.changes)-1].GetQuiet().GetDisplayAwake()

	// Act
	h.hear(LidClosed)

	// Assert
	if awake == nil || !strings.Contains(awake.GetDetail(), "no display") {
		t.Fatalf("first standing's display = %v, want awake with the cause", awake)
	}
	if h.changes[len(h.changes)-1].GetQuiet().GetDisplayAsleep() == nil || h.freezer.count("freeze") != 1 {
		t.Fatalf("standing %v, freezer %v: want the display asleep after the retry and still one freeze", h.changes[len(h.changes)-1], h.freezer.calls)
	}
	if errs := h.records(dlog.LevelError, opDisplay); len(errs) != 1 {
		t.Fatalf("ERROR records = %+v, want the one failed sleep", errs)
	}
}

func TestWatchLidHandsEveryHeardLidToTheController(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOff, summary(true, "Home"))
	done := make(chan error, 1)
	go func() { done <- h.c.WatchLid(context.Background()) }()

	// Act.
	h.lid.lids <- LidOpen
	h.lid.lids <- LidClosed
	h.lid.end = nil
	close(h.lid.lids)
	<-done

	// Assert.
	var lids []string
	for _, s := range h.changes {
		switch {
		case s.GetLidOpen() != nil:
			lids = append(lids, "open")
		case s.GetLidClosed() != nil:
			lids = append(lids, "closed")
		case s.GetLidUnwatched() != nil:
			lids = append(lids, "unwatched")
		}
	}
	if strings.Join(lids, ",") != "open,closed,unwatched" {
		t.Fatalf("published lids = %v, want open, closed, then unwatched once the watch ended", lids)
	}
}

func TestWatchLidStandsAFailedWatchAsUnwatched(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(true, hotspot))
	h.c.Refresh(context.Background())
	h.lid.end = errors.New("IOServiceAddInterestNotification answered 0xe00002c2")
	close(h.lid.lids)

	// Act.
	err := h.c.WatchLid(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("WatchLid() = %v, want nil: the daemon runs on without the lid", err)
	}
	last := h.changes[len(h.changes)-1]
	if !strings.Contains(last.GetLidUnwatched().GetDetail(), "0xe00002c2") {
		t.Fatalf("standing = %v, want unwatched naming the cause", last)
	}
	errs := h.records(dlog.LevelError, opLid)
	if len(errs) != 1 || !strings.Contains(errs[0].Context["cause"].(string), "0xe00002c2") {
		t.Fatalf("lid ERROR records = %+v, want one carrying the cause", errs)
	}
}

func TestWatchLidEndedByItsContextRecordsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	err := h.c.WatchLid(ctx)

	// Assert.
	if err != nil || len(h.records(dlog.LevelError, opLid)) != 0 || h.c.lid.kind != lidUnheard {
		t.Fatalf("WatchLid() = %v with lid %v and records %+v, want a quiet end", err, h.c.lid, h.log.Records())
	}
}

// lidServer serves a Unix socket in a short directory, writing each
// connection the lines given and then holding it open until hold is closed
// (or closing it at once when end is set). It answers the socket's path.
func lidServer(t *testing.T, lines string, end bool) (path string, hold chan struct{}) {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "pwl")
	if err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	path = filepath.Join(dir, "lid")
	ln, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	hold = make(chan struct{})
	var held sync.WaitGroup
	t.Cleanup(func() {
		_ = ln.Close()
		select {
		case <-hold:
		default:
			close(hold)
		}
		held.Wait()
	})
	held.Add(1)
	go func() {
		defer held.Done()
		for {
			conn, err := ln.Accept()
			if err != nil {
				return
			}
			held.Add(1)
			go func() {
				defer held.Done()
				defer conn.Close()
				_, _ = conn.Write([]byte(lines))
				if !end {
					<-hold
				}
			}()
		}
	}()
	return path, hold
}

func TestSocketLidHearsEachLine(t *testing.T) {
	// Arrange.
	path, _ := lidServer(t, "open\nclosed\nnone\nbogus\n", false)
	var heard []Lid

	// Act.
	err := SocketLid(path).Watch(context.Background(), func(l Lid) { heard = append(heard, l) })

	// Assert.
	if len(heard) != 3 || heard[0] != LidOpen || heard[1] != LidClosed || heard[2] != LidNone {
		t.Fatalf("heard = %v, want open, closed, none", heard)
	}
	if err == nil || !strings.Contains(err.Error(), "bogus") {
		t.Fatalf("Watch() = %v, want the unnamed line refused", err)
	}
}

func TestSocketLidEndsQuietlyWithItsContext(t *testing.T) {
	// Arrange.
	path, _ := lidServer(t, "open\n", false)
	ctx, cancel := context.WithCancel(context.Background())

	// Act: the first line cancels, which closes the connection under the
	// blocked read of the next.
	err := SocketLid(path).Watch(ctx, func(Lid) { cancel() })

	// Assert.
	if err != nil {
		t.Fatalf("Watch() = %v, want nil once ctx ended", err)
	}
}

// TestSocketLidGivesEachWatchItsOwnStream pins what a handover needs: a
// watch whose ctx ends stops at once while another watch of the same socket
// still hears the lid.
func TestSocketLidGivesEachWatchItsOwnStream(t *testing.T) {
	// Arrange.
	path, _ := lidServer(t, "open\n", false)
	ctxA, cancelA := context.WithCancel(context.Background())
	ctxB, cancelB := context.WithCancel(context.Background())
	heardB := make(chan Lid, 1)
	doneB := make(chan error, 1)
	go func() { doneB <- SocketLid(path).Watch(ctxB, func(l Lid) { heardB <- l }) }()

	// Act.
	errA := SocketLid(path).Watch(ctxA, func(Lid) { cancelA() })

	// Assert.
	if errA != nil {
		t.Fatalf("first Watch() = %v, want nil once its ctx ended", errA)
	}
	if l := <-heardB; l != LidOpen {
		t.Fatalf("second watch heard %v, want open", l)
	}
	cancelB()
	if err := <-doneB; err != nil {
		t.Fatalf("second Watch() = %v, want nil once its ctx ended", err)
	}
}

func TestSocketLidReportsAStreamThatEnded(t *testing.T) {
	// Arrange.
	path, _ := lidServer(t, "open\n", true)

	// Act.
	err := SocketLid(path).Watch(context.Background(), func(Lid) {})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "ended") {
		t.Fatalf("Watch() = %v, want the ended stream reported", err)
	}
}

func TestSocketLidRefusesAMissingSocket(t *testing.T) {
	// Act.
	err := SocketLid(filepath.Join("/tmp", "pwl-absent-lid")).Watch(context.Background(), func(Lid) {})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "connect to the lid socket") {
		t.Fatalf("Watch() = %v, want the connection refused", err)
	}
}

func TestFixedLidIsHeardOnceUntilItsContextEnds(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	var heard []Lid

	// Act.
	err := FixedLid(LidClosed).Watch(ctx, func(l Lid) { heard = append(heard, l); cancel() })

	// Assert.
	if err != nil || len(heard) != 1 || heard[0] != LidClosed {
		t.Fatalf("Watch() = %v having heard %v, want closed once and nil", err, heard)
	}
}

func TestConfigLidPicksTheWatcherItsValueNames(t *testing.T) {
	cases := []struct {
		fake string
		want LidWatcher
	}{
		{fake: "", want: SystemLid()},
		{fake: "none", want: FixedLid(LidNone)},
		{fake: "/tmp/lid", want: SocketLid("/tmp/lid")},
	}
	for _, tc := range cases {
		t.Run(tc.fake, func(t *testing.T) {
			// Act, Assert.
			if got := (Config{FakeLid: tc.fake}).Lid(); got != tc.want {
				t.Fatalf("Lid() = %#v, want %#v", got, tc.want)
			}
		})
	}
}

func TestLidFactNamesAndStampsEveryKind(t *testing.T) {
	cases := []struct {
		fact lidFact
		name string
	}{
		{lidFact{kind: lidOpen}, "open"},
		{lidFact{kind: lidClosed}, "closed"},
		{lidFact{kind: lidNone}, "none"},
		{lidFact{kind: lidUnwatched, cause: "gone"}, "unwatched"},
		{lidFact{}, "unheard"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t).standing(pmsetOff, summary(true, "Home"))
			h.c.lid = tc.fact

			// Act.
			h.c.Refresh(context.Background())

			// Assert.
			s := h.changes[0]
			got := map[bool]string{true: tc.name}[s.GetLidOpen() != nil && tc.name == "open" ||
				s.GetLidClosed() != nil && tc.name == "closed" ||
				s.GetNoLid() != nil && tc.name == "none" ||
				s.GetLidUnwatched().GetDetail() == "gone" && tc.name == "unwatched" ||
				s.GetLid() == nil && tc.name == "unheard"]
			if got != tc.name || tc.fact.name() != tc.name {
				t.Fatalf("standing lid = %v, name = %q, want %q", s.GetLid(), tc.fact.name(), tc.name)
			}
		})
	}
}
