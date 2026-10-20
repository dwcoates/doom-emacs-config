package grantedclock

import (
	"errors"
	"strings"
	"testing"
	"time"
)

// scriptedTree is a process table a test rewrites between readings.
type scriptedTree struct {
	kids  map[int][]int
	usage map[int]processUsage
	// failUsage and failKids fail that pid's read.
	failUsage, failKids map[int]error
}

func (s *scriptedTree) tree(root int) *processTree {
	return &processTree{
		root: root,
		children: func(pid int) ([]int, error) {
			if err := s.failKids[pid]; err != nil {
				return nil, err
			}
			return s.kids[pid], nil
		},
		usage: func(pid int) (processUsage, bool, error) {
			if err := s.failUsage[pid]; err != nil {
				return processUsage{}, false, err
			}
			u, ok := s.usage[pid]
			return u, !ok, nil
		},
	}
}

func TestProcessTreeSumsEveryDescendantsStallSinceItsFirstReading(t *testing.T) {
	tests := []struct {
		name  string
		later func(s *scriptedTree)
		want  time.Duration
	}{
		{name: "nothing accrued since the first reading reads as nothing",
			later: func(*scriptedTree) {}, want: 0},
		{name: "the root's own stall is counted",
			later: func(s *scriptedTree) { s.usage[1] = processUsage{start: 10, runnable: 5 * time.Second} }, want: 4 * time.Second},
		{name: "a grandchild's stall is counted",
			later: func(s *scriptedTree) { s.usage[3] = processUsage{start: 30, runnable: 3500 * time.Millisecond} }, want: 500 * time.Millisecond},
		{name: "a process born since the last reading is counted whole",
			later: func(s *scriptedTree) {
				s.kids[2] = append(s.kids[2], 4)
				s.usage[4] = processUsage{start: 40, runnable: 700 * time.Millisecond}
			}, want: 700 * time.Millisecond},
		{name: "a pid the kernel reused is a new process, counted whole",
			later: func(s *scriptedTree) { s.usage[3] = processUsage{start: 31, runnable: 200 * time.Millisecond} }, want: 200 * time.Millisecond},
		{name: "a process that exited takes nothing back",
			later: func(s *scriptedTree) { delete(s.usage, 3) }, want: 0},
		{name: "a process outside the tree is never counted",
			later: func(s *scriptedTree) { s.usage[99] = processUsage{start: 990, runnable: time.Hour} }, want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: root 1 -> 2 -> 3, each with history before the first
			// reading.
			s := &scriptedTree{
				kids: map[int][]int{1: {2}, 2: {3}},
				usage: map[int]processUsage{
					1: {start: 10, runnable: time.Second},
					2: {start: 20, runnable: 2 * time.Second},
					3: {start: 30, runnable: 3 * time.Second},
				},
			}
			tree := s.tree(1)
			if got, err := tree.read(); err != nil || got != 0 {
				t.Fatalf("first read() = %s, %v, want 0", got, err)
			}
			tt.later(s)

			// Act
			got, err := tree.read()

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("read() = %s, %v, want %s", got, err, tt.want)
			}
		})
	}
}

func TestProcessTreeAccumulatesAcrossReadings(t *testing.T) {
	// Arrange: a child that accrues, exits, and a successor that accrues.
	s := &scriptedTree{
		kids:  map[int][]int{1: {2}},
		usage: map[int]processUsage{1: {start: 10}, 2: {start: 20, runnable: time.Second}},
	}
	tree := s.tree(1)
	if _, err := tree.read(); err != nil {
		t.Fatalf("first read() = %v", err)
	}
	s.usage[2] = processUsage{start: 20, runnable: 3 * time.Second}
	if _, err := tree.read(); err != nil {
		t.Fatalf("second read() = %v", err)
	}
	delete(s.usage, 2)
	s.kids[1] = []int{5}
	s.usage[5] = processUsage{start: 50, runnable: time.Second}

	// Act
	got, err := tree.read()

	// Assert: 2s from the first child, 1s from its successor.
	if err != nil || got != 3*time.Second {
		t.Fatalf("read() = %s, %v, want 3s", got, err)
	}
}

func TestProcessTreeRefusesAReadingItCannotTrust(t *testing.T) {
	tests := []struct {
		name    string
		later   func(s *scriptedTree)
		wantErr string
	}{
		{name: "a runnable time that went backwards is refused",
			later:   func(s *scriptedTree) { s.usage[2] = processUsage{start: 20, runnable: time.Second} },
			wantErr: "process 2's runnable time went backwards, from 2s to 1s"},
		{name: "a usage the kernel refuses is refused",
			later:   func(s *scriptedTree) { s.failUsage = map[int]error{2: errors.New("EPERM")} },
			wantErr: "read process 2's runnable time: EPERM"},
		{name: "a child listing the kernel refuses is refused",
			later:   func(s *scriptedTree) { s.failKids = map[int]error{1: errors.New("EINVAL")} },
			wantErr: "list process 1's children: EINVAL"},
		{name: "a tree whose root is gone is refused",
			later:   func(s *scriptedTree) { delete(s.usage, 1) },
			wantErr: "the tree's root, process 1, is gone"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := &scriptedTree{
				kids:  map[int][]int{1: {2}},
				usage: map[int]processUsage{1: {start: 10}, 2: {start: 20, runnable: 2 * time.Second}},
			}
			tree := s.tree(1)
			if _, err := tree.read(); err != nil {
				t.Fatalf("first read() = %v", err)
			}
			tt.later(s)

			// Act
			_, err := tree.read()

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("read() = %v, want an error naming %q", err, tt.wantErr)
			}
		})
	}
}
