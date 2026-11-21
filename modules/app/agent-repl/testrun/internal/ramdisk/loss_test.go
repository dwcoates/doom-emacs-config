package ramdisk

import (
	"errors"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/internal/proctable"
)

// fakeExit is an ExitWatch the test ends: exit closes Exited, as the
// helper's exit would.
type fakeExit struct {
	exited   chan struct{}
	closed   bool
	closeErr error
}

func newFakeExit() *fakeExit { return &fakeExit{exited: make(chan struct{})} }

func (f *fakeExit) Exited() <-chan struct{} { return f.exited }
func (f *fakeExit) Close() error            { f.closed = true; return f.closeErr }
func (f *fakeExit) exit()                   { close(f.exited) }

var lossHelperStart = time.Date(2026, 11, 20, 18, 8, 17, 597326000, time.UTC)

// lossVolume is a volume served by helper 78555, whose WatchExit answers
// watch and err, and whose process table answers procs and procsErr.
func lossVolume(watch ExitWatch, watchErr error, procs []proctable.Entry, procsErr error) (*Volume, *fakeLog, *[]int) {
	log := &fakeLog{}
	watched := &[]int{}
	m := Manager{
		Log: log,
		WatchExit: func(pid int) (ExitWatch, error) {
			*watched = append(*watched, pid)
			return watch, watchErr
		},
		Procs: func() ([]proctable.Entry, error) { return procs, procsErr },
	}
	return &Volume{m: m, Device: "/dev/disk8", Mount: "/tmp/artr-77421", helper: helper{Pid: 78555, Start: lossHelperStart}}, log, watched
}

// theHelper is the process table naming helper 78555 as it started.
func theHelper() []proctable.Entry { return []proctable.Entry{helperEntry(78555, lossHelperStart)} }

// lostSignal is a lost callback that counts its calls and signals each.
func lostSignal() (func(), chan struct{}) {
	ch := make(chan struct{}, 2)
	return func() { ch <- struct{}{} }, ch
}

func TestWatchLossCutsTheRunWhenTheHelperExits(t *testing.T) {
	// Arrange.
	watch := newFakeExit()
	v, log, watched := lossVolume(watch, nil, theHelper(), nil)
	lost, calls := lostSignal()
	if err := v.WatchLoss(lost); err != nil {
		t.Fatalf("WatchLoss: %v", err)
	}

	// Act.
	watch.exit()

	// Assert: the record is written before lost is called.
	<-calls
	if len(*watched) != 1 || (*watched)[0] != 78555 {
		t.Fatalf("watched %v, want the helper 78555", *watched)
	}
	if !log.has("ERROR RAM DISK LOST: /dev/disk8, mounted at /tmp/artr-77421, is gone: its helper, process 78555 (started ") {
		t.Fatalf("log = %q, want the loss at ERROR", log.lines)
	}
	if err := v.stopLoss(); err != nil || len(calls) != 0 {
		t.Fatalf("stopLoss = %v, %d more lost calls; want one call", err, len(calls))
	}
}

func TestWatchLossStoppedBeforeTheExitNeverCutsTheRun(t *testing.T) {
	// Arrange.
	watch := newFakeExit()
	v, log, _ := lossVolume(watch, nil, theHelper(), nil)
	lost, calls := lostSignal()
	if err := v.WatchLoss(lost); err != nil {
		t.Fatalf("WatchLoss: %v", err)
	}

	// Act: the release stops the watch, then the detach ends the helper.
	err := v.stopLoss()
	watch.exit()

	// Assert: the watch's goroutine ended inside stopLoss, so nothing is
	// left to read the exit.
	if err != nil || !watch.closed {
		t.Fatalf("stopLoss = %v, closed %v; want the watch closed", err, watch.closed)
	}
	if len(calls) != 0 || log.has("ERROR") {
		t.Fatalf("%d lost calls, log %q; want none", len(calls), log.lines)
	}
}

func TestWatchLossOfAHelperAlreadyGone(t *testing.T) {
	recycled := []proctable.Entry{helperEntry(78555, lossHelperStart.Add(time.Minute))}
	tests := []struct {
		name     string
		watchErr error
		procs    []proctable.Entry
	}{
		{name: "exited before its watch", watchErr: errExitedBeforeWatch, procs: nil},
		{name: "its pid recycled before its watch", procs: recycled},
		{name: "absent from the process table", procs: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			var watch ExitWatch
			var fake *fakeExit
			if tt.watchErr == nil {
				fake = newFakeExit()
				watch = fake
			}
			v, log, _ := lossVolume(watch, tt.watchErr, tt.procs, nil)
			lost, calls := lostSignal()

			// Act.
			err := v.WatchLoss(lost)

			// Assert.
			if err != nil {
				t.Fatalf("WatchLoss: %v", err)
			}
			<-calls
			if fake != nil && !fake.closed {
				t.Fatal("the watch on the pid that is no longer the helper was left open")
			}
			if !log.has("ERROR RAM DISK LOST: /dev/disk8") {
				t.Fatalf("log = %q, want the loss at ERROR", log.lines)
			}
		})
	}
}

func TestWatchLossFailures(t *testing.T) {
	boom := errors.New("boom")
	tests := []struct {
		name     string
		helper   helper
		watchErr error
		procsErr error
		closeErr error
		want     string
	}{
		{name: "no helper was identified", want: "the RAM disk /dev/disk8 has no helper to watch"},
		{name: "the watch cannot be taken", helper: helper{Pid: 78555, Start: lossHelperStart}, watchErr: boom, want: "watch the RAM disk /dev/disk8's helper, process 78555: boom"},
		{name: "the process table cannot be read", helper: helper{Pid: 78555, Start: lossHelperStart}, procsErr: boom, want: "read the process table to identify the RAM disks' helpers: boom"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			watch := newFakeExit()
			v, log, watched := lossVolume(watch, tt.watchErr, theHelper(), tt.procsErr)
			v.helper = tt.helper
			lost, calls := lostSignal()

			// Act.
			err := v.WatchLoss(lost)

			// Assert: nothing watches on, and the run is not cut.
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("WatchLoss = %v, want %q", err, tt.want)
			}
			if tt.helper.zero() && len(*watched) != 0 {
				t.Fatalf("watched %v for a disk with no helper", *watched)
			}
			if tt.procsErr != nil && !watch.closed {
				t.Fatal("the watch was left open after the failed identification")
			}
			if v.loss != nil || len(calls) != 0 || log.has("ERROR") {
				t.Fatalf("loss %v, %d lost calls, log %q; want nothing", v.loss, len(calls), log.lines)
			}
		})
	}
}

func TestStopLossAnswersAWatchThatCannotBeClosed(t *testing.T) {
	// Arrange.
	watch := newFakeExit()
	watch.closeErr = errors.New("boom")
	v, _, _ := lossVolume(watch, nil, theHelper(), nil)
	lost, _ := lostSignal()
	if err := v.WatchLoss(lost); err != nil {
		t.Fatalf("WatchLoss: %v", err)
	}

	// Act.
	err := v.stopLoss()

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "stop watching the RAM disk /dev/disk8's helper, process 78555: boom") {
		t.Fatalf("stopLoss = %v, want the failed close", err)
	}
}

func TestStopLossWithoutAWatchDoesNothing(t *testing.T) {
	// Arrange.
	v, _, _ := lossVolume(nil, nil, nil, nil)

	// Act.
	err := v.stopLoss()

	// Assert.
	if err != nil {
		t.Fatalf("stopLoss = %v", err)
	}
}

func TestReleaseStopsTheLossWatchBeforeTheDetach(t *testing.T) {
	// Arrange.
	h := &fakeHost{}
	m := newManager(t, h)
	watch := newFakeExit()
	m.WatchExit = func(int) (ExitWatch, error) { return watch, nil }
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	lost, calls := lostSignal()
	if err := v.WatchLoss(lost); err != nil {
		t.Fatalf("WatchLoss: %v", err)
	}
	var closedAtDetach bool
	run := m.Run
	v.m.Run = func(name string, args ...string) (string, error) {
		if name == "hdiutil" && len(args) > 0 && args[0] == "detach" {
			closedAtDetach = watch.closed
			watch.exit()
		}
		return run(name, args...)
	}

	// Act.
	err = v.Release()

	// Assert.
	if err != nil {
		t.Fatalf("Release: %v", err)
	}
	if !closedAtDetach || len(calls) != 0 {
		t.Fatalf("watch closed at the detach %v, %d lost calls; want closed first and no loss", closedAtDetach, len(calls))
	}
}

func TestReleaseAnswersALossWatchThatCannotBeClosed(t *testing.T) {
	// Arrange.
	h := &fakeHost{}
	m := newManager(t, h)
	watch := newFakeExit()
	watch.closeErr = errors.New("boom")
	m.WatchExit = func(int) (ExitWatch, error) { return watch, nil }
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	lost, _ := lostSignal()
	if err := v.WatchLoss(lost); err != nil {
		t.Fatalf("WatchLoss: %v", err)
	}

	// Act.
	err = v.Release()

	// Assert: the failure is answered and the disk is still released.
	if err == nil || !strings.Contains(err.Error(), "stop watching the RAM disk") {
		t.Fatalf("Release = %v, want the failed close", err)
	}
	if !h.detached {
		t.Fatal("the disk was not detached")
	}
}
