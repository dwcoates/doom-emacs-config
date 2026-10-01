package sched

import (
	"container/heap"
	"fmt"
)

// Simulate runs the Queue's policy on n slots with every unit taking exactly
// its estimate, and returns the predicted makespan. It assumes every unit
// succeeds.
func Simulate(units []Unit, n int) (float64, error) {
	if n < 1 {
		return 0, fmt.Errorf("sched: simulate needs at least one slot, got %d", n)
	}
	q, err := NewQueue(units)
	if err != nil {
		return 0, err
	}
	running := &endHeap{}
	now := 0.0
	for {
		for running.Len() < n {
			u, ok := q.Next()
			if !ok {
				break
			}
			heap.Push(running, ending{at: now + u.Est, id: u.ID})
		}
		if running.Len() == 0 {
			break
		}
		e := heap.Pop(running).(ending)
		now = e.at
		q.Done(e.id, true)
	}
	if p := q.Pending(); p != 0 {
		return 0, fmt.Errorf("sched: simulation stalled with %d units never ready", p)
	}
	return now, nil
}

type ending struct {
	at float64
	id string
}

type endHeap []ending

func (h endHeap) Len() int { return len(h) }
func (h endHeap) Less(i, j int) bool {
	if h[i].at != h[j].at {
		return h[i].at < h[j].at
	}
	return h[i].id < h[j].id
}
func (h endHeap) Swap(i, j int) { h[i], h[j] = h[j], h[i] }
func (h *endHeap) Push(x any)   { *h = append(*h, x.(ending)) }
func (h *endHeap) Pop() any {
	old := *h
	e := old[len(old)-1]
	*h = old[:len(old)-1]
	return e
}
