// Package causelog decides when a STANDING failure is worth a record: once
// when its cause first appears or changes, once when it clears, and never
// again while it merely persists.
//
// It exists for work that repeats on a schedule (a metrics scrape every 15
// seconds, a resolution after every deploy): recording a failure on every
// repetition buries the log, and recording it never hides it. The caller
// still records through its own canonical logger; this only says when.
package causelog

import "sync"

// Tracker remembers the standing cause of each keyed step. The zero value is
// ready to use and safe for concurrent use.
type Tracker struct {
	mu   sync.Mutex
	last map[string]string
}

// Failed notes that step failed with cause, answering whether the cause is
// new for step and so should be recorded.
func (t *Tracker) Failed(step, cause string) bool {
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.last == nil {
		t.last = map[string]string{}
	}
	if t.last[step] == cause {
		return false
	}
	t.last[step] = cause
	return true
}

// Succeeded notes that step succeeded, answering whether it had a standing
// cause that has now cleared and so should be recorded.
func (t *Tracker) Succeeded(step string) bool {
	t.mu.Lock()
	defer t.mu.Unlock()
	if _, standing := t.last[step]; !standing {
		return false
	}
	delete(t.last, step)
	return true
}
