package life

import (
	"errors"
	"fmt"
	"io"
	"os"
	"time"

	"agentrepl/testrun/reap"
)

// Warden is a warden's work once its runner is gone.
type Warden struct {
	// Runner and Since name the runner: its pid (its session id) and when it
	// started.
	Runner int
	Since  time.Time
	// Sentinel is the runner's sentinel, never reaped.
	Sentinel int
	// Reap is the one reap.
	Reap func(reap.Members) (reap.Result, error)
	// Release releases the dead runner's disk (ramdisk.Manager.ReclaimRun),
	// answering whether it had one left.
	Release func(runner int) (bool, error)
	Log     Logger
}

// WatchLifeline is Watch over the warden's lifeline descriptor.
func (w Warden) WatchLifeline() int {
	f := os.NewFile(lifelineFD, "lifeline")
	if f == nil {
		w.Log.Errorf("the warden of run %d has no lifeline descriptor", w.Runner)
		return 1
	}
	defer f.Close()
	return w.Watch(f)
}

// Watch reads the runner's lifeline in until it ends. A runner that said
// "done" released everything, and the warden leaves. A runner that died
// without it is reaped after: its whole session, and whatever runs out of
// the roots it named, are reaped; its disk is released; its roots are
// removed. Watch answers the warden's exit status.
func (w Warden) Watch(in io.Reader) int {
	roots, done, err := wardenMessages(in)
	if err != nil {
		w.Log.Errorf("the warden of run %d could not read its runner's lifeline: %v; it reaps the run as dead", w.Runner, err)
	}
	if done && err == nil {
		return 0
	}
	w.Log.Errorf("the runner pid %d died without releasing its run; its warden reaps session %d and releases what it held", w.Runner, w.Runner)
	var errs []error
	m := reap.Members{What: fmt.Sprintf("dead run %d", w.Runner), Session: w.Runner, Since: w.Since, Except: []int{os.Getpid(), w.Sentinel}}
	for _, root := range roots {
		dirs, err := reap.Spellings(root)
		if err != nil {
			errs = append(errs, err)
			w.Log.Errorf("the warden of run %d could not resolve the run root %s: %v", w.Runner, root, err)
			continue
		}
		m.Dirs = append(m.Dirs, dirs...)
	}
	if _, err := w.Reap(m); err != nil {
		errs = append(errs, err)
	}
	// A FAILED REAP KEEPS EVERYTHING: Release reaps again before it detaches
	// and keeps the disk and its record when anything remains, and no root is
	// removed from under a process still running out of it.
	if _, err := w.Release(w.Runner); err != nil {
		errs = append(errs, err)
	}
	if len(errs) == 0 {
		for _, root := range roots {
			if err := os.RemoveAll(root); err != nil {
				errs = append(errs, err)
				w.Log.Errorf("the warden of run %d could not remove the run root %s: %v", w.Runner, root, err)
			}
		}
	}
	if err := errors.Join(errs...); err != nil {
		return 1
	}
	w.Log.Infof("the warden of run %d reaped it and released what it held", w.Runner)
	return 0
}
