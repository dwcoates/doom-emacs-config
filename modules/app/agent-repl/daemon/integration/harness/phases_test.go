package harness

import (
	"errors"
	"reflect"
	"strings"
	"testing"

	"agentrepl/testrun/testenv"
)

// phaseLog records each phase handed to it, or answers err.
type phaseLog struct {
	got []string
	cpu []float64
	err error
}

func (l *phaseLog) record(phase string, cpu float64) error {
	if l.err != nil {
		return l.err
	}
	l.got, l.cpu = append(l.got, phase), append(l.cpu, cpu)
	return nil
}

func TestRecordDaemonBootRecordsTheCPUUntilServingUnderItsPhase(t *testing.T) {
	cases := []struct {
		name   string
		shared bool
		want   string
	}{
		{"a suite-owned daemon's boot is shared", true, testenv.PhaseDaemonBootShared},
		{"a test's own daemon's boot is fresh", false, testenv.PhaseDaemonBootFresh},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var log phaseLog
			cpu := func(pid int) (float64, error) {
				if pid != 41 {
					t.Fatalf("read pid %d's CPU; want the daemon's, 41", pid)
				}
				return 1.25, nil
			}

			// Act
			err := recordDaemonBoot(41, tc.shared, cpu, log.record)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			if !reflect.DeepEqual(log.got, []string{tc.want}) || !reflect.DeepEqual(log.cpu, []float64{1.25}) {
				t.Fatalf("recorded %v %v; want %s 1.25", log.got, log.cpu, tc.want)
			}
		})
	}
}

func TestRecordDaemonBootFailures(t *testing.T) {
	cases := []struct {
		name      string
		cpuErr    error
		recordErr error
		want      []string
	}{
		{"a failed CPU read names the daemon and its phase", errors.New("ESRCH"), nil,
			[]string{"read the CPU daemon 41 used until it served", testenv.PhaseDaemonBootFresh, "ESRCH"}},
		{"a failed record names the daemon, its phase and its CPU", nil, errors.New("disk full"),
			[]string{"record daemon 41's " + testenv.PhaseDaemonBootFresh + " of 1.250s", "disk full"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			log := phaseLog{err: tc.recordErr}
			cpu := func(int) (float64, error) { return 1.25, tc.cpuErr }

			// Act
			err := recordDaemonBoot(41, false, cpu, log.record)

			// Assert: the failure surfaces, and nothing was recorded.
			if err == nil {
				t.Fatal("recordDaemonBoot succeeded; want the failure")
			}
			for _, part := range tc.want {
				if !strings.Contains(err.Error(), part) {
					t.Fatalf("err = %v; want it to say %q", err, part)
				}
			}
			if len(log.got) != 0 {
				t.Fatalf("recorded %v after the failure", log.got)
			}
		})
	}
}

// selfReads answers each read of the binary's CPU in turn.
func selfReads(reads ...float64) func() (float64, error) {
	i := 0
	return func() (float64, error) {
		v := reads[i]
		i++
		return v, nil
	}
}

func TestPhaseMeterRecordsTheSetupBeforeTheFirstTestAndTheTeardownAfterTheLast(t *testing.T) {
	// Arrange: 2s before the tests, 10s at their end, 10.5s after teardown.
	var log phaseLog
	p := &phaseMeter{self: selfReads(2, 10, 10.5), record: log.record}

	// Act
	errStart := p.start()
	p.ran()
	errFinish := p.finish()

	// Assert
	if errStart != nil || errFinish != nil {
		t.Fatal(errStart, errFinish)
	}
	if want := []string{testenv.PhaseHarnessSetup, testenv.PhaseHarnessTeardown}; !reflect.DeepEqual(log.got, want) || !reflect.DeepEqual(log.cpu, []float64{2, 0.5}) {
		t.Fatalf("recorded %v %v; want %v [2 0.5]", log.got, log.cpu, want)
	}
}

func TestAPhaseMeterOfARunThatDidNotAskDoesNothing(t *testing.T) {
	// Arrange
	t.Setenv(testenv.PhasesOut, "")

	// Act
	p := newPhaseMeter()
	p.ran()

	// Assert
	if p != nil || p.start() != nil || p.finish() != nil {
		t.Fatalf("newPhaseMeter() = %+v; want none, doing nothing", p)
	}
}

func TestPhaseMeterFailures(t *testing.T) {
	failing := errors.New("getrusage: EFAULT")
	cases := []struct {
		name  string
		meter func(*phaseLog) *phaseMeter
		act   func(*phaseMeter) error
		want  string
	}{
		{
			name: "a failed read before the first test",
			meter: func(l *phaseLog) *phaseMeter {
				return &phaseMeter{self: func() (float64, error) { return 0, failing }, record: l.record}
			},
			act:  func(p *phaseMeter) error { return p.start() },
			want: "read the test binary's CPU before its first test: getrusage: EFAULT",
		},
		{
			name: "a failed read after the last test",
			meter: func(l *phaseLog) *phaseMeter {
				return &phaseMeter{self: func() (float64, error) { return 0, failing }, record: l.record}
			},
			act:  func(p *phaseMeter) error { p.ran(); return p.finish() },
			want: "read the test binary's CPU after its last test: getrusage: EFAULT",
		},
		{
			name: "a failed read after the teardown",
			meter: func(l *phaseLog) *phaseMeter {
				reads := 0
				return &phaseMeter{self: func() (float64, error) {
					reads++
					if reads == 2 {
						return 0, failing
					}
					return 1, nil
				}, record: l.record}
			},
			act:  func(p *phaseMeter) error { p.ran(); return p.finish() },
			want: "read the test binary's CPU after the suite's teardown: getrusage: EFAULT",
		},
		{
			name: "a failed record of the setup",
			meter: func(l *phaseLog) *phaseMeter {
				l.err = errors.New("disk full")
				return &phaseMeter{self: selfReads(2), record: l.record}
			},
			act:  func(p *phaseMeter) error { return p.start() },
			want: "record the test binary's setup of 2.000s: disk full",
		},
		{
			name: "a failed record of the teardown",
			meter: func(l *phaseLog) *phaseMeter {
				l.err = errors.New("disk full")
				return &phaseMeter{self: selfReads(10, 10.5), record: l.record}
			},
			act:  func(p *phaseMeter) error { p.ran(); return p.finish() },
			want: "record the test binary's teardown of 0.500s: disk full",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var log phaseLog
			p := tc.meter(&log)

			// Act
			err := tc.act(p)

			// Assert: the failure surfaces, and nothing was recorded.
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("err = %v; want %q", err, tc.want)
			}
			if len(log.got) != 0 {
				t.Fatalf("recorded %v after the failure", log.got)
			}
		})
	}
}

func TestSelfCPUReadsThisProcessesOwnCPU(t *testing.T) {
	// Act
	first, err := selfCPU()
	if err != nil {
		t.Fatal(err)
	}
	second, err := selfCPU()

	// Assert: a running process has used some CPU, and never less later.
	if err != nil || first <= 0 || second < first {
		t.Fatalf("selfCPU = %v then %v, %v; want a positive, nondecreasing CPU", first, second, err)
	}
}
