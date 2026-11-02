package persistentwifi

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"

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

func TestALidCloseWithTheModeOnDims(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(true, hotspot))
	h.r.on(argBrightnessRead, answer{out: "0.81"})
	h.r.on(argLidDim, answer{})

	// Act.
	h.hear(LidClosed)

	// Assert.
	if h.r.count(argLidDim) != 1 || h.dims.kept() == nil || *h.dims.kept() != 0.81 {
		t.Fatalf("calls = %v, kept = %v, want a dim keeping 0.81", h.r.ran(), h.dims.kept())
	}
	last := h.lastStanding(t).changes[len(h.changes)-1]
	if last.GetLidClosed() == nil || last.GetLidDim().GetRestoreLevel() != 0.81 {
		t.Fatalf("standing = %v, want the lid closed and dimmed from 0.81", last)
	}
	infos := h.records(dlog.LevelInfo, opDisplay)
	if len(infos) != 1 || infos[0].Context["trigger"] != "lid_closed" {
		t.Fatalf("display INFO records = %+v, want one for the closed lid", infos)
	}
}

func TestALidOpenRestoresThePreCloseLevel(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(true, hotspot))
	h.r.on(argBrightnessRead, answer{out: "0.81"})
	h.r.on(argLidDim, answer{})
	h.r.on(argBrightness("0.81"), answer{})
	h.hear(LidClosed)

	// Act.
	h.hear(LidOpen)

	// Assert.
	if h.r.count(argBrightness("0.81")) != 1 || h.dims.kept() != nil || h.dims.clears != 1 {
		t.Fatalf("calls = %v, kept = %v, want a restore to 0.81 that cleared the level", h.r.ran(), h.dims.kept())
	}
	last := h.changes[len(h.changes)-1]
	if last.GetLidOpen() == nil || last.GetLidDim() != nil {
		t.Fatalf("standing = %v, want the lid open and no dim", last)
	}
}

func TestALidCloseWithTheModeOffLeavesTheBrightnessAlone(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOff, summary(true, "Home"))

	// Act.
	h.hear(LidClosed)

	// Assert.
	for _, c := range h.r.ran() {
		if strings.HasPrefix(c, tools.Brightness) {
			t.Fatalf("ran %q with the mode off", c)
		}
	}
	if last := h.changes[len(h.changes)-1]; last.GetLidClosed() == nil || last.GetLidDim() != nil {
		t.Fatalf("standing = %v, want the lid closed and no dim", last)
	}
}

func TestALidCloseWhoseBrightnessCannotBeReadIsNotDimmed(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(true, hotspot))
	h.r.on(argBrightnessRead, answer{out: "IOKit error", code: 1})

	// Act.
	h.hear(LidClosed)

	// Assert.
	if h.r.count(argLidDim) != 0 || h.dims.kept() != nil {
		t.Fatalf("calls = %v, kept = %v, want no dim and nothing kept", h.r.ran(), h.dims.kept())
	}
	errs := h.records(dlog.LevelError, opDisplay)
	if len(errs) != 1 || errs[0].Context["trigger"] != "lid_closed" || !strings.Contains(errs[0].Context["cause"].(string), "IOKit error") {
		t.Fatalf("display ERROR records = %+v, want one carrying the tool's cause", errs)
	}
	if last := h.changes[len(h.changes)-1]; last.GetLidDim() != nil {
		t.Fatalf("standing = %v, want no dim", last)
	}
}

func TestARestartWithAKeptLevelStandsDimmedAndRestoresOnOpen(t *testing.T) {
	// Arrange: the previous daemon dimmed the closed lid from 0.6.
	h := newHarness(t).standing(pmsetOn, summary(true, hotspot)).keep(0.6)
	h.r.on(argBrightness("0.6"), answer{})

	// Act.
	h.c.Refresh(context.Background())
	h.hear(LidClosed)
	closed := h.changes[len(h.changes)-1]
	h.hear(LidOpen)

	// Assert.
	if h.changes[0].GetLidDim().GetRestoreLevel() != 0.6 {
		t.Fatalf("boot standing = %v, want the kept dim of 0.6", h.changes[0])
	}
	if closed.GetLidClosed() == nil || closed.GetLidDim().GetRestoreLevel() != 0.6 {
		t.Fatalf("closed standing = %v, want closed and still dimmed from 0.6", closed)
	}
	if h.r.count(argBrightnessRead) != 0 || h.r.count(argBrightness("0.6")) != 1 || h.dims.kept() != nil {
		t.Fatalf("calls = %v, kept = %v, want no re-dim and one restore to 0.6", h.r.ran(), h.dims.kept())
	}
}

func TestUpdateOnWithTheLidClosedAfterARestartKeepsTheKeptLevel(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOff, summary(true, hotspot)).keep(0.6).withLid(lidClosed)
	scriptPower(h.r, "1")

	// Act.
	resp := h.c.Update(context.Background(), update("on"))

	// Assert.
	if resp.GetSuccess().GetDisplay().GetUnchanged() == nil || h.r.count(argBrightnessRead) != 0 {
		t.Fatalf("display = %v with calls %v, want unchanged and no read", resp.GetSuccess().GetDisplay(), h.r.ran())
	}
	if resp.GetSuccess().GetState().GetLidDim().GetRestoreLevel() != 0.6 {
		t.Fatalf("state = %v, want the kept dim of 0.6", resp.GetSuccess().GetState())
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

// fifo makes a named pipe holding lines, read-write so the writes buffer
// before any reader opens it.
func fifo(t *testing.T, lines string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "lid")
	if err := syscall.Mkfifo(path, 0o600); err != nil {
		t.Fatalf("mkfifo: %v", err)
	}
	w, err := os.OpenFile(path, os.O_RDWR, 0)
	if err != nil {
		t.Fatalf("open the fifo: %v", err)
	}
	t.Cleanup(func() { _ = w.Close() })
	if _, err := w.WriteString(lines); err != nil {
		t.Fatalf("write the fifo: %v", err)
	}
	return path
}

func TestPipeLidHearsEachLine(t *testing.T) {
	// Arrange.
	path := fifo(t, "open\nclosed\nnone\nbogus\n")
	var heard []Lid

	// Act.
	err := PipeLid(path).Watch(context.Background(), func(l Lid) { heard = append(heard, l) })

	// Assert.
	if len(heard) != 3 || heard[0] != LidOpen || heard[1] != LidClosed || heard[2] != LidNone {
		t.Fatalf("heard = %v, want open, closed, none", heard)
	}
	if err == nil || !strings.Contains(err.Error(), "bogus") {
		t.Fatalf("Watch() = %v, want the unnamed line refused", err)
	}
}

func TestPipeLidEndsQuietlyWithItsContext(t *testing.T) {
	// Arrange.
	path := fifo(t, "open\n")
	ctx, cancel := context.WithCancel(context.Background())

	// Act: the first line cancels, which closes the pipe under the blocked
	// read of the second.
	err := PipeLid(path).Watch(ctx, func(Lid) { cancel() })

	// Assert.
	if err != nil {
		t.Fatalf("Watch() = %v, want nil once ctx ended", err)
	}
}

func TestPipeLidRefusesAMissingPipe(t *testing.T) {
	// Act.
	err := PipeLid(filepath.Join(t.TempDir(), "absent")).Watch(context.Background(), func(Lid) {})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "open the lid pipe") {
		t.Fatalf("Watch() = %v, want the open refused", err)
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
