// Package hostcpu reads the time every processor of the host has spent in
// each CPU state since boot: the one reader of the host's CPU load for every
// agent-repl program that needs it (the daemon's runtime metrics, testrun's
// CPU governor).
//
// The counters are cumulative. What a reader wants is almost always the share
// of the host that was busy between two reads, which Busy answers.
package hostcpu

import "fmt"

// Times is the time, in seconds summed over every processor, the host has
// spent in each state since boot.
type Times struct {
	User   float64
	System float64
	Idle   float64
	Nice   float64
}

// Total is every state's time together.
func (t Times) Total() float64 { return t.User + t.System + t.Idle + t.Nice }

// busy is the time spent doing anything but idling.
func (t Times) busy() float64 { return t.User + t.System + t.Nice }

// Busy answers the share of the host, from 0 to 1, that was busy between
// prev and t. Counters that did not move forward between the two reads
// cannot answer, and are refused: they mean the reads were taken out of order
// or the counters were reset, never that the host was idle.
func (t Times) Busy(prev Times) (float64, error) {
	total := t.Total() - prev.Total()
	busy := t.busy() - prev.busy()
	if total <= 0 || busy < 0 || busy > total {
		return 0, fmt.Errorf("hostcpu: the CPU counters did not move forward between two reads (busy %.2fs of %.2fs)", busy, total)
	}
	return busy / total, nil
}
