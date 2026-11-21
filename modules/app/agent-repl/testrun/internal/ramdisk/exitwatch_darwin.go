package ramdisk

import (
	"errors"
	"fmt"
	"sync"
	"syscall"
)

// closeIdent is the EVFILT_USER event Close triggers to end the wait.
const closeIdent = 1

// kqueueExit is an ExitWatch on one kqueue: the process's NOTE_EXIT and the
// user event Close triggers, so the wait ends on one or the other, never on a
// clock.
type kqueueExit struct {
	kq     int
	exited chan struct{}
	done   chan struct{}
	err    error
	once   sync.Once
}

// WatchProcessExit is WatchExit through kqueue: EVFILT_PROC NOTE_EXIT, the
// kernel's own word that pid has exited, waited on until Close. A pid that has
// already exited answers errExitedBeforeWatch.
func WatchProcessExit(pid int) (ExitWatch, error) {
	kq, err := syscall.Kqueue()
	if err != nil {
		return nil, fmt.Errorf("kqueue: %w", err)
	}
	changes := []syscall.Kevent_t{
		{Ident: closeIdent, Filter: syscall.EVFILT_USER, Flags: syscall.EV_ADD | syscall.EV_CLEAR},
		{Ident: uint64(pid), Filter: syscall.EVFILT_PROC, Flags: syscall.EV_ADD | syscall.EV_ONESHOT, Fflags: syscall.NOTE_EXIT},
	}
	if _, err := syscall.Kevent(kq, changes, nil, nil); err != nil {
		closeErr := syscall.Close(kq)
		if errors.Is(err, syscall.ESRCH) {
			return nil, errors.Join(errExitedBeforeWatch, closeErr)
		}
		return nil, errors.Join(fmt.Errorf("watch process %d's exit: %w", pid, err), closeErr)
	}
	w := &kqueueExit{kq: kq, exited: make(chan struct{}), done: make(chan struct{})}
	go w.wait(pid)
	return w, nil
}

// wait blocks on the kqueue until the process exits or Close triggers the
// user event. A failed wait is kept for Close to answer.
func (w *kqueueExit) wait(pid int) {
	defer close(w.done)
	events := make([]syscall.Kevent_t, 2)
	for {
		n, err := syscall.Kevent(w.kq, nil, events, nil)
		if errors.Is(err, syscall.EINTR) {
			continue
		}
		if err != nil {
			w.err = fmt.Errorf("wait for process %d's exit: %w", pid, err)
			return
		}
		for _, ev := range events[:n] {
			switch ev.Filter {
			case syscall.EVFILT_USER:
				return
			case syscall.EVFILT_PROC:
				close(w.exited)
				return
			}
		}
	}
}

func (w *kqueueExit) Exited() <-chan struct{} { return w.exited }

// Close ends the wait and closes the kqueue; it answers a wait that failed.
func (w *kqueueExit) Close() error {
	var err error
	w.once.Do(func() {
		trigger := []syscall.Kevent_t{{Ident: closeIdent, Filter: syscall.EVFILT_USER, Fflags: syscall.NOTE_TRIGGER}}
		if _, kerr := syscall.Kevent(w.kq, trigger, nil, nil); kerr != nil {
			err = fmt.Errorf("end the wait for the exit: %w", kerr)
		}
		<-w.done
		err = errors.Join(err, w.err)
		if cerr := syscall.Close(w.kq); cerr != nil {
			err = errors.Join(err, fmt.Errorf("close the exit watch's kqueue: %w", cerr))
		}
	})
	return err
}
