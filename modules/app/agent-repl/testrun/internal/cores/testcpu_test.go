package cores

import (
	"errors"
	"math"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/internal/proctable"
)

// proc is a marked process's scan entry: pid, parent, own and reaped
// children's CPU seconds; every one started at the same instant.
func proc(pid, ppid int, own, children float64) proctable.Usage {
	return proctable.Usage{Pid: pid, Ppid: ppid, Start: time.Unix(1000, 0), Own: own, Children: children}
}

func TestTestsCPUCountsEveryCPUSecondOnce(t *testing.T) {
	cases := []struct {
		name  string
		scans [][]proctable.Usage
		want  float64
	}{
		{
			name:  "the first read takes the baseline",
			scans: [][]proctable.Usage{{proc(10, 1, 50, 20)}},
			want:  0,
		},
		{
			name:  "a process seen at both reads adds what it used between them",
			scans: [][]proctable.Usage{{proc(10, 1, 5, 1)}, {proc(10, 1, 7, 2.5)}},
			want:  3.5,
		},
		{
			name:  "a process new since the last read adds all it used",
			scans: [][]proctable.Usage{{proc(10, 1, 5, 0)}, {proc(10, 1, 5, 0), proc(11, 10, 0.75, 0.25)}},
			want:  1,
		},
		{
			name: "a child reaped by a marked parent is counted once, its last interval through its parent",
			scans: [][]proctable.Usage{
				{proc(10, 1, 1, 0)},
				{proc(10, 1, 1, 0), proc(11, 10, 2, 0)},
				// 11 used 0.5s more, then 10 reaped it: 10's children hold 2.5.
				{proc(10, 1, 1, 2.5)},
			},
			want: 2.5,
		},
		{
			name: "a child that lived and was reaped between two reads is counted in its parent",
			scans: [][]proctable.Usage{
				{proc(10, 1, 1, 0)},
				{proc(10, 1, 1, 0.4)},
			},
			want: 0.4,
		},
		{
			name: "a process whose parent is not marked loses only its last interval",
			scans: [][]proctable.Usage{
				{proc(11, 1, 1, 0)},
				{proc(11, 1, 3, 0)},
				{},
			},
			want: 2,
		},
		{
			name: "a pid reused by a new process is a new process",
			scans: [][]proctable.Usage{
				{proc(10, 1, 9, 0)},
				{{Pid: 10, Ppid: 1, Start: time.Unix(2000, 0), Own: 0.5}},
			},
			want: 0.5,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			i := 0
			a := &TestsCPU{Scan: func() ([]proctable.Usage, error) {
				s := tc.scans[i]
				i++
				return s, nil
			}}

			// Act
			var got float64
			for range tc.scans {
				var err error
				if got, err = a.Read(); err != nil {
					t.Fatal(err)
				}
			}

			// Assert
			if math.Abs(got-tc.want) > 1e-9 {
				t.Fatalf("Read() = %v after %d reads; want %v", got, len(tc.scans), tc.want)
			}
		})
	}
}

func TestTestsCPUSurfacesAFailedScan(t *testing.T) {
	// Arrange
	a := &TestsCPU{Scan: func() ([]proctable.Usage, error) { return nil, errors.New("proc_info answered EIO") }}

	// Act
	_, err := a.Read()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "proc_info answered EIO") {
		t.Fatalf("Read() = %v; want the scan's failure", err)
	}
}
