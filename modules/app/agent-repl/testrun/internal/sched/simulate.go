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
	if err := CheckWidths(units, n); err != nil {
		return 0, err
	}
	q, err := NewQueue(units)
	if err != nil {
		return 0, err
	}
	running := &endHeap{}
	now := 0.0
	used := 0
	for {
		for {
			u, ok := q.Next(n - used)
			if !ok {
				break
			}
			used += u.Width()
			heap.Push(running, ending{at: now + u.Est, id: u.ID, width: u.Width()})
		}
		if running.Len() == 0 {
			break
		}
		e := heap.Pop(running).(ending)
		now = e.at
		used -= e.width
		q.Done(e.id, true)
	}
	if p := q.Pending(); p != 0 {
		return 0, fmt.Errorf("sched: simulation stalled with %d units never ready", p)
	}
	return now, nil
}

type ending struct {
	at    float64
	id    string
	width int
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
