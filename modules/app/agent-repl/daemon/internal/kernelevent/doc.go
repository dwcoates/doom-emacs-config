// Package kernelevent waits on what the kernel itself reports -- a process's
// exit, a change under a path -- so no caller ever polls for either.
//
// EVERY WAIT HERE IS AN EVENT, NEVER A TIMER. A caller that wants "has pid
// exited" or "did this file appear, grow or go" registers for the kernel's
// report and blocks on it: kqueue's EVFILT_PROC and EVFILT_VNODE on Darwin,
// pidfd and inotify on Linux. A wait ends when the event arrives or its
// context ends, and nothing in between reads a clock.
//
// A registration is taken BEFORE the caller's own check, so an event that
// lands between the check and the wait is still pending on the queue and
// wakes the wait at once (Follow).
package kernelevent
