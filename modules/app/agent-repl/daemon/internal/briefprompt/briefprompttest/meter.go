// Package briefprompttest is the test double of briefprompt.Meter.
package briefprompttest

import "sync"

// Meter records every notice it is told of, in order.
type Meter struct {
	mu         sync.Mutex
	observed   []Observed
	suppressed []string
}

// Observed is one notice ObserveNotice recorded.
type Observed struct {
	Notice string
	Bytes  int
}

// ObserveNotice implements briefprompt.Meter.
func (m *Meter) ObserveNotice(notice string, bytes int) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.observed = append(m.observed, Observed{Notice: notice, Bytes: bytes})
}

// CountSuppressedNotice implements briefprompt.Meter.
func (m *Meter) CountSuppressedNotice(notice string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.suppressed = append(m.suppressed, notice)
}

// ObservedNotices answers the notices observed so far.
func (m *Meter) ObservedNotices() []Observed {
	m.mu.Lock()
	defer m.mu.Unlock()
	return append([]Observed(nil), m.observed...)
}

// Suppressed answers the suppressed notices counted so far.
func (m *Meter) Suppressed() []string {
	m.mu.Lock()
	defer m.mu.Unlock()
	return append([]string(nil), m.suppressed...)
}
