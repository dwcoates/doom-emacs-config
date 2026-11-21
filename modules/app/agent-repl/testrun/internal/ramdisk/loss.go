package ramdisk

import (
	"errors"
	"fmt"
)

// ExitWatch is the kernel's watch on one process's exit (WatchExit).
type ExitWatch interface {
	// Exited closes once the process has exited.
	Exited() <-chan struct{}
	// Close ends the watch; Exited never closes after it.
	Close() error
}

// errExitedBeforeWatch is a WatchExit whose process had already exited.
var errExitedBeforeWatch = errors.New("the process had exited before its exit was watched")

// loss is the watch on a RAM disk's helper for the run's lifetime
// (WatchLoss).
type loss struct {
	watch ExitWatch
	stop  chan struct{}
	done  chan struct{}
}

// WatchLoss calls lost, once, the moment the disk's helper exits while the
// run still holds the disk. It is watched until Release, which stops it
// before the detach ends the helper on purpose.
//
// A RAM DISK THAT DIES UNDER ITS RUN TAKES EVERY UNIT WITH IT, AND NOTHING
// ELSE SAYS SO. The helper serves the disk's image; with it gone the volume's
// media is gone, the kernel force-unmounts it, and every process running a
// binary from it faults on its next page of text, endlessly: runnable, deaf
// to every signal it would handle, each at a third to half a core (2026-11-20
// 18:14, run 77421's /dev/disk8, helper 78555: 36 processes of its e2e unit
// spun for 54 minutes, until the unit's bound, at load 80 to 200). No record
// or exit said so; only the bound ended the unit, and the processes outlived
// it, since its kill reached only the unit's own group. The helper's exit is
// the kernel's own word that the disk is gone, so the run is cut on it.
//
// THE HELPER IS IDENTIFIED AFTER ITS WATCH IS TAKEN, so a pid that was
// recycled before the watch is never mistaken for the helper: the watch
// names whichever process holds the pid, and the process table then says
// whether that is the helper, by its start (helper). A helper already gone is
// a loss at once.
func (v *Volume) WatchLoss(lost func()) error {
	if v.helper.zero() {
		return fmt.Errorf("the RAM disk %s has no helper to watch: its attach never identified the process serving it", v.Device)
	}
	watch, err := v.m.WatchExit(v.helper.Pid)
	if errors.Is(err, errExitedBeforeWatch) {
		watch = exitedWatch()
	} else if err != nil {
		return fmt.Errorf("watch the RAM disk %s's helper, process %d: %w", v.Device, v.helper.Pid, err)
	}
	starts, err := v.m.helperStarts()
	if err != nil {
		return errors.Join(err, watch.Close())
	}
	if start, ok := starts[v.helper.Pid]; !ok || !(helper{Pid: v.helper.Pid, Start: start}).equal(v.helper) {
		if err := watch.Close(); err != nil {
			return fmt.Errorf("close the watch on process %d, which is no longer the RAM disk %s's helper: %w", v.helper.Pid, v.Device, err)
		}
		watch = exitedWatch()
	}
	v.loss = &loss{watch: watch, stop: make(chan struct{}), done: make(chan struct{})}
	go func() {
		defer close(v.loss.done)
		select {
		case <-v.loss.stop:
		case <-watch.Exited():
			v.m.Log.Errorf("RAM DISK LOST: %s, mounted at %s, is gone: its helper, process %d (started %s), exited while the run still held it, so every process running out of it can only fault from now on; the run is cut", v.Device, v.Mount, v.helper.Pid, stamp(v.helper.Start))
			lost()
		}
	}()
	return nil
}

// stopLoss ends the watch on the helper, before the release detaches the
// disk. A watch whose Close fails is answered, never dropped.
func (v *Volume) stopLoss() error {
	if v.loss == nil {
		return nil
	}
	close(v.loss.stop)
	<-v.loss.done
	err := v.loss.watch.Close()
	v.loss = nil
	if err != nil {
		return fmt.Errorf("stop watching the RAM disk %s's helper, process %d: %w", v.Device, v.helper.Pid, err)
	}
	return nil
}

// exited is an ExitWatch of a process that has already exited.
type exited chan struct{}

func exitedWatch() ExitWatch {
	ch := make(exited)
	close(ch)
	return ch
}

func (e exited) Exited() <-chan struct{} { return e }
func (e exited) Close() error            { return nil }
