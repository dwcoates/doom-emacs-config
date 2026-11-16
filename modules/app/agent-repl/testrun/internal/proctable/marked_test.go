package proctable

import (
	"errors"
	"reflect"
	"strings"
	"syscall"
	"testing"
	"time"
)

// fakeMarked scripts a scan's kernel; argsRead counts each environment read.
type fakeMarked struct {
	pids     []int
	listErr  error
	records  map[int]procRecord
	recErr   map[int]error
	args     map[int][]byte
	argsErr  map[int]error
	cpu      map[int][2]float64
	cpuErr   map[int]error
	argsRead map[int]int
}

func (f *fakeMarked) scanner() *Marked {
	f.argsRead = map[int]int{}
	return &Marked{Name: "MARK", UID: 501, k: markedKernel{
		listPids: func() ([]int, error) { return f.pids, f.listErr },
		record: func(pid int) (procRecord, error) {
			if err := f.recErr[pid]; err != nil {
				return procRecord{}, err
			}
			r, ok := f.records[pid]
			if !ok {
				return procRecord{}, syscall.ESRCH
			}
			return r, nil
		},
		args: procargs2Args(func(pid int) ([]byte, error) {
			f.argsRead[pid]++
			if err := f.argsErr[pid]; err != nil {
				return nil, err
			}
			return f.args[pid], nil
		}),
		rusage: func(pid int) (float64, float64, error) {
			if err := f.cpuErr[pid]; err != nil {
				return 0, 0, err
			}
			c := f.cpu[pid]
			return c[0], c[1], nil
		},
	}}
}

func TestMarkedScanAnswersThisUsersMarkedProcessesAndTheirCPU(t *testing.T) {
	// Arrange
	start := time.Date(2026, 11, 13, 9, 0, 0, 0, time.UTC)
	f := &fakeMarked{
		pids: []int{0, 10, 11, 12, 13, 14, 15, 16},
		records: map[int]procRecord{
			10: {Ppid: 1, UID: 501, Start: start},
			11: {Ppid: 10, UID: 501, Start: start},
			12: {Ppid: 10, UID: 0, Start: start},
			14: {Ppid: 10, UID: 501, Start: start},
			15: {Ppid: 10, UID: 501, Start: start},
			16: {Ppid: 10, UID: 501, Start: start},
		},
		recErr: map[int]error{13: syscall.EPERM},
		args: map[int][]byte{
			10: procArgsBuf(1, "/bin/sh", "sh", "MARK=7"),
			11: procArgsBuf(1, "/bin/sh", "sh", "MARKED=1"),
			12: procArgsBuf(1, "/bin/sh", "sh", "MARK=7"),
			15: procArgsBuf(1, "/bin/sh", "sh", "MARK=7"),
			16: procArgsBuf(1, "/bin/sh", "sh", "MARK="),
		},
		argsErr: map[int]error{14: syscall.EIO},
		cpu:     map[int][2]float64{10: {1.5, 2}, 16: {0.25, 0}},
		cpuErr:  map[int]error{15: syscall.ESRCH},
	}

	// Act
	got, err := f.scanner().Scan()

	// Assert: 0 skipped, 11 named another variable, 12 another user's, 13
	// unreadable, 14 mid-exec, 15 gone before its CPU was read.
	if err != nil {
		t.Fatal(err)
	}
	want := []Usage{
		{Pid: 10, Ppid: 1, Start: start, Own: 1.5, Children: 2},
		{Pid: 16, Ppid: 10, Start: start, Own: 0.25},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Scan() =\n%+v\nwant\n%+v", got, want)
	}
}

func TestMarkedScanReadsAProcessesEnvironmentOnce(t *testing.T) {
	// Arrange
	start := time.Date(2026, 11, 13, 9, 0, 0, 0, time.UTC)
	f := &fakeMarked{
		pids:    []int{10},
		records: map[int]procRecord{10: {UID: 501, Start: start}},
		args:    map[int][]byte{10: procArgsBuf(1, "/bin/sh", "sh", "MARK=1")},
		cpu:     map[int][2]float64{10: {1, 0}},
	}
	m := f.scanner()

	// Act
	for range 3 {
		if _, err := m.Scan(); err != nil {
			t.Fatal(err)
		}
	}

	// Assert
	if f.argsRead[10] != 1 {
		t.Fatalf("the environment was read %d times; want once", f.argsRead[10])
	}
}

func TestMarkedScanReadsAgainAProcessThatReusedAPid(t *testing.T) {
	// Arrange: pid 10 is marked, then a new process takes its pid unmarked.
	f := &fakeMarked{
		pids:    []int{10},
		records: map[int]procRecord{10: {UID: 501, Start: time.Unix(100, 0)}},
		args:    map[int][]byte{10: procArgsBuf(1, "/bin/sh", "sh", "MARK=1")},
		cpu:     map[int][2]float64{10: {1, 0}},
	}
	m := f.scanner()
	if got, err := m.Scan(); err != nil || len(got) != 1 {
		t.Fatalf("first Scan() = %v, %v; want pid 10", got, err)
	}
	f.records[10] = procRecord{UID: 501, Start: time.Unix(200, 0)}
	f.args[10] = procArgsBuf(1, "/bin/sh", "sh", "OTHER=1")

	// Act
	got, err := m.Scan()

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("Scan() = %v, %v; want the new, unmarked process skipped", got, err)
	}
}

func TestMarkedScanReadsAgainAnEnvironmentThatWasUnreadable(t *testing.T) {
	// Arrange: mid-exec on the first scan, readable on the second.
	start := time.Unix(100, 0)
	f := &fakeMarked{
		pids:    []int{10},
		records: map[int]procRecord{10: {UID: 501, Start: start}},
		argsErr: map[int]error{10: syscall.EIO},
		cpu:     map[int][2]float64{10: {1, 0}},
	}
	m := f.scanner()
	if _, err := m.Scan(); err != nil {
		t.Fatal(err)
	}
	f.argsErr = nil
	f.args = map[int][]byte{10: procArgsBuf(1, "/bin/sh", "sh", "MARK=1")}

	// Act
	got, err := m.Scan()

	// Assert
	if err != nil || len(got) != 1 || f.argsRead[10] != 2 {
		t.Fatalf("Scan() = %v, %v after %d reads; want pid 10 marked on the second read", got, err, f.argsRead[10])
	}
}

func TestMarkedScanFailures(t *testing.T) {
	boom := errors.New("boom")
	marked := map[int][]byte{7: procArgsBuf(1, "/bin/sh", "sh", "MARK=1")}
	mine := map[int]procRecord{7: {UID: 501}}
	cases := []struct {
		name string
		f    *fakeMarked
		want string
	}{
		{name: "the pid list fails", f: &fakeMarked{listErr: boom}, want: "list the processes: boom"},
		{name: "a record fails", f: &fakeMarked{pids: []int{7}, recErr: map[int]error{7: boom}}, want: "read pid 7's record: boom"},
		{name: "an environment fails", f: &fakeMarked{pids: []int{7}, records: mine, argsErr: map[int]error{7: boom}}, want: "read pid 7's environment: sysctl kern.procargs2: boom"},
		{name: "an environment is malformed", f: &fakeMarked{pids: []int{7}, records: mine, args: map[int][]byte{7: {1}}}, want: "read pid 7's environment: kern.procargs2: 1 bytes"},
		{name: "a CPU read fails", f: &fakeMarked{pids: []int{7}, records: mine, args: marked, cpuErr: map[int]error{7: boom}}, want: "read pid 7's CPU: boom"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := tc.f.scanner().Scan()

			// Assert
			if err == nil || got != nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Scan() = %v, %v; want error %q and nothing scanned", got, err, tc.want)
			}
		})
	}
}
