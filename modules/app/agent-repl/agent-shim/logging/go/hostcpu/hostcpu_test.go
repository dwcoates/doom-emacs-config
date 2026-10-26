package hostcpu

import (
	"math"
	"runtime"
	"strings"
	"testing"
)

func TestBusy(t *testing.T) {
	prev := Times{User: 10, System: 5, Idle: 80, Nice: 5}
	cases := []struct {
		name    string
		now     Times
		want    float64
		wantErr string
	}{
		{name: "a quarter busy", now: Times{User: 15, System: 10, Idle: 110, Nice: 5}, want: 0.25},
		{name: "nice time is busy time", now: Times{User: 10, System: 5, Idle: 90, Nice: 15}, want: 0.5},
		{name: "a wholly idle interval", now: Times{User: 10, System: 5, Idle: 100, Nice: 5}, want: 0},
		{name: "a wholly busy interval", now: Times{User: 30, System: 5, Idle: 80, Nice: 5}, want: 1},
		{name: "counters that did not move are refused", now: prev, wantErr: "did not move forward"},
		{name: "counters that went backwards are refused", now: Times{User: 1, System: 1, Idle: 1, Nice: 1}, wantErr: "did not move forward"},
		{name: "busy time that went backwards is refused", now: Times{User: 5, System: 5, Idle: 120, Nice: 5}, wantErr: "did not move forward"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := tc.now.Busy(prev)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("Busy = %v, %v; want an error containing %q", got, err, tc.wantErr)
				}
				return
			}
			if err != nil || math.Abs(got-tc.want) > 1e-9 {
				t.Fatalf("Busy = %v, %v; want %v", got, err, tc.want)
			}
		})
	}
}

func TestParseProcStat(t *testing.T) {
	cases := []struct {
		name    string
		data    string
		want    Times
		wantErr string
	}{
		{
			name: "interrupts and steal are system time, iowait is idle",
			data: "cpu  100 20 300 4000 50 6 7 8 0 0\ncpu0 1 2 3 4 5 6 7 8 0 0\n",
			want: Times{User: 1, Nice: 0.2, System: 3 + 0.06 + 0.07 + 0.08, Idle: 40 + 0.5},
		},
		{name: "a first line that is not the aggregate", data: "cpu0 1 2 3 4 5 6 7 8\n", wantErr: "not the aggregate cpu line"},
		{name: "too few times", data: "cpu 1 2 3 4\n", wantErr: "not the aggregate cpu line"},
		{name: "a time that is not a number", data: "cpu 1 2 x 4 5 6 7 8\n", wantErr: "cpu time 3"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := parseProcStat([]byte(tc.data))

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("parseProcStat = %+v, %v; want an error containing %q", got, err, tc.wantErr)
				}
				return
			}
			if err != nil || !near(got, tc.want) {
				t.Fatalf("parseProcStat = %+v, %v; want %+v", got, err, tc.want)
			}
		})
	}
}

func TestReadAnswersTheHostsCountersInOrder(t *testing.T) {
	if runtime.GOOS != "darwin" && runtime.GOOS != "linux" {
		t.Skipf("no reader on %s", runtime.GOOS)
	}
	// Act
	first, err := Read()
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	second, err := Read()
	if err != nil {
		t.Fatalf("Read: %v", err)
	}

	// Assert: a booted host has spent time, and no counter runs backwards.
	if first.Total() <= 0 || first.Idle <= 0 {
		t.Fatalf("Read = %+v; want a host that has spent time, some of it idle", first)
	}
	if second.User < first.User || second.System < first.System || second.Idle < first.Idle || second.Nice < first.Nice {
		t.Fatalf("Read went backwards: %+v then %+v", first, second)
	}
}

func near(a, b Times) bool {
	const eps = 1e-9
	return math.Abs(a.User-b.User) < eps && math.Abs(a.System-b.System) < eps && math.Abs(a.Idle-b.Idle) < eps && math.Abs(a.Nice-b.Nice) < eps
}
