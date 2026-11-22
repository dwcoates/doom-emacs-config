package restarttiming_test

import (
	"slices"
	"testing"
	"time"

	"agent-repld/internal/restarttiming"
	"agent-repld/internal/restarttiming/restarttimingtest"
)

func TestAnExpectedReconnectIsTimedOnceFromItsRestart(t *testing.T) {
	// Arrange
	began := time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)
	now := began.Add(3 * time.Second)
	var rec restarttimingtest.Recorder
	r := restarttiming.NewReconnects(&rec, restarttiming.NoWindows{}, func() time.Time { return now })
	r.Expect(restarttiming.ReconnectCold, began, restarttiming.ReconnectWeb, "w1", "w2")

	// Act
	r.Connected("w1", restarttiming.ReconnectWeb)
	r.Connected("w1", restarttiming.ReconnectWeb)

	// Assert: the second open of the same stream is no reconnect.
	got := rec.Of(restarttiming.WorkspaceReconnect)
	if len(got) != 1 || got[0].Workspace() != "w1" || got[0].Phase() != "web" || got[0].Cause() != "cold" || got[0].Seconds() != 3 {
		t.Fatalf("reconnects = %+v, want w1's web stream once, cold, 3s", got)
	}
}

func TestAConnectionNothingExpectedIsNotTimed(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(r *restarttiming.Reconnects, began time.Time)
	}{
		{"no restart expected anything", func(*restarttiming.Reconnects, time.Time) {}},
		{"another workspace was expected", func(r *restarttiming.Reconnects, began time.Time) {
			r.Expect(restarttiming.ReconnectCold, began, restarttiming.ReconnectWeb, "w2")
		}},
		{"another phase was expected", func(r *restarttiming.Reconnects, began time.Time) {
			r.Expect(restarttiming.ReconnectCold, began, restarttiming.ReconnectHost, "w1")
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			began := time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)
			var rec restarttimingtest.Recorder
			r := restarttiming.NewReconnects(&rec, restarttiming.NoWindows{}, func() time.Time { return began.Add(time.Second) })
			tt.arrange(r, began)

			// Act
			r.Connected("w1", restarttiming.ReconnectWeb)

			// Assert
			if got := rec.Observations(); len(got) != 0 {
				t.Fatalf("observations = %+v, want none", got)
			}
		})
	}
}

func TestALaterExpectationReplacesAnEarlierOne(t *testing.T) {
	// Arrange
	boot := time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)
	reload := boot.Add(time.Minute)
	var rec restarttimingtest.Recorder
	r := restarttiming.NewReconnects(&rec, restarttiming.NoWindows{}, func() time.Time { return reload.Add(2 * time.Second) })
	r.Expect(restarttiming.ReconnectCold, boot, restarttiming.ReconnectWeb, "w1")
	r.Expect(restarttiming.ReconnectWebappReload, reload, restarttiming.ReconnectWeb, "w1")

	// Act
	r.Connected("w1", restarttiming.ReconnectWeb)

	// Assert
	got := rec.Of(restarttiming.WorkspaceReconnect)
	if len(got) != 1 || got[0].Cause() != "webapp_reload" || got[0].Seconds() != 2 {
		t.Fatalf("reconnects = %+v, want the reload's, 2s", got)
	}
}

func TestAStartAfterNowIsStatedAsZero(t *testing.T) {
	// Arrange: a manifest stamped after this daemon's clock reads now.
	now := time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)
	var rec restarttimingtest.Recorder
	r := restarttiming.NewReconnects(&rec, restarttiming.NoWindows{}, func() time.Time { return now })
	r.Expect(restarttiming.ReconnectSuccessor, now.Add(time.Second), restarttiming.ReconnectHost, "w1")

	// Act
	r.Connected("w1", restarttiming.ReconnectHost)

	// Assert
	if got := rec.Of(restarttiming.WorkspaceReconnect); len(got) != 1 || got[0].Seconds() != 0 {
		t.Fatalf("reconnects = %+v, want one at zero", got)
	}
}

func TestAWaveIsTimedAtItsFirstAndLastConnectionAndItsSpread(t *testing.T) {
	// Arrange
	began := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	now := began
	var rec restarttimingtest.Recorder
	r := restarttiming.NewReconnects(&rec, restarttiming.NoWindows{}, func() time.Time { return now })
	r.Expect(restarttiming.ReconnectSuccessor, began, restarttiming.ReconnectHost, "w1", "w2", "w3")

	// Act
	now = began.Add(2 * time.Second)
	r.Connected("w2", restarttiming.ReconnectHost)
	now = began.Add(5 * time.Second)
	r.Connected("w1", restarttiming.ReconnectHost)
	now = began.Add(9 * time.Second)
	r.Connected("w3", restarttiming.ReconnectHost)

	// Assert
	want := []string{"host_first successor 2", "host_last successor 9", "host_spread successor 7"}
	if got := rec.WaveMarks(); !slices.Equal(got, want) {
		t.Fatalf("wave = %v, want %v", got, want)
	}
}

func TestAWaveWithAMemberStillOutTimesOnlyItsFirst(t *testing.T) {
	// Arrange
	began := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	var rec restarttimingtest.Recorder
	r := restarttiming.NewReconnects(&rec, restarttiming.NoWindows{}, func() time.Time { return began.Add(time.Second) })
	r.Expect(restarttiming.ReconnectCold, began, restarttiming.ReconnectWeb, "w1", "w2")

	// Act
	r.Connected("w1", restarttiming.ReconnectWeb)

	// Assert
	if got := rec.WaveMarks(); !slices.Equal(got, []string{"web_first cold 1"}) {
		t.Fatalf("wave = %v, want only its first", got)
	}
}

func TestAWaveAMemberOfWhichALaterRestartExpectedNeverTimesItsLast(t *testing.T) {
	// Arrange: w2 of the boot's wave is expected again by a reload.
	boot := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	reload := boot.Add(time.Minute)
	now := boot.Add(time.Second)
	var rec restarttimingtest.Recorder
	r := restarttiming.NewReconnects(&rec, restarttiming.NoWindows{}, func() time.Time { return now })
	r.Expect(restarttiming.ReconnectCold, boot, restarttiming.ReconnectWeb, "w1", "w2")
	r.Expect(restarttiming.ReconnectWebappReload, reload, restarttiming.ReconnectWeb, "w2")

	// Act
	r.Connected("w1", restarttiming.ReconnectWeb)
	now = reload.Add(3 * time.Second)
	r.Connected("w2", restarttiming.ReconnectWeb)

	// Assert: the boot's wave lost w2, so only its first is timed; the
	// reload's wave of one is whole.
	want := []string{"web_first cold 1", "web_first webapp_reload 3", "web_last webapp_reload 3", "web_spread webapp_reload 0"}
	if got := rec.WaveMarks(); !slices.Equal(got, want) {
		t.Fatalf("wave = %v, want %v", got, want)
	}
}

func TestAWorkspaceNamedTwiceInOneExpectationIsOneMemberOfItsWave(t *testing.T) {
	// Arrange
	began := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	var rec restarttimingtest.Recorder
	r := restarttiming.NewReconnects(&rec, restarttiming.NoWindows{}, func() time.Time { return began.Add(4 * time.Second) })
	r.Expect(restarttiming.ReconnectCold, began, restarttiming.ReconnectHost, "w1", "w1")

	// Act
	r.Connected("w1", restarttiming.ReconnectHost)

	// Assert
	want := []string{"host_first cold 4", "host_last cold 4", "host_spread cold 0"}
	if got := rec.WaveMarks(); !slices.Equal(got, want) {
		t.Fatalf("wave = %v, want %v", got, want)
	}
}

func TestAWaveMeasuresTheProcessesItsPhaseBringsBackUntilItsLastReturns(t *testing.T) {
	tests := []struct {
		name      string
		phase     restarttiming.ReconnectPhase
		wantWatch []restarttiming.Watched
		wantBound time.Duration
	}{
		{name: "Emacs's hosts", phase: restarttiming.ReconnectHost, wantBound: 180 * time.Second,
			wantWatch: []restarttiming.Watched{{Process: restarttiming.ProcessEmacs}}},
		{name: "the webviews", phase: restarttiming.ReconnectWeb, wantBound: 600 * time.Second,
			wantWatch: []restarttiming.Watched{{Process: restarttiming.ProcessWebKitWebContent}, {Process: restarttiming.ProcessWebKitNetworking}, {Process: restarttiming.ProcessWebKitGPU}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			began := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
			var windows restarttimingtest.Windows
			r := restarttiming.NewReconnects(&restarttimingtest.Recorder{}, &windows, func() time.Time { return began })
			r.Expect(restarttiming.ReconnectSuccessor, began, tt.phase, "w1", "w2")
			r.Connected("w1", tt.phase)
			stillOpen := !windows.Windows()[0].Closed

			// Act
			r.Connected("w2", tt.phase)

			// Assert
			got := windows.Windows()
			if !stillOpen || len(got) != 1 || got[0].Restart != restarttiming.ReconnectWave || got[0].Phase != string(tt.phase) ||
				!slices.Equal(got[0].Watch, tt.wantWatch) || got[0].Bound != tt.wantBound || !got[0].Closed {
				t.Fatalf("windows = %+v (open after the first: %v), want one %s window over %v, closed at the last", got, stillOpen, tt.phase, tt.wantWatch)
			}
		})
	}
}

func TestABrokenWaveClosesItsWindow(t *testing.T) {
	// Arrange
	boot := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	var windows restarttimingtest.Windows
	r := restarttiming.NewReconnects(&restarttimingtest.Recorder{}, &windows, func() time.Time { return boot })
	r.Expect(restarttiming.ReconnectCold, boot, restarttiming.ReconnectWeb, "w1", "w2")

	// Act
	r.Expect(restarttiming.ReconnectWebappReload, boot.Add(time.Minute), restarttiming.ReconnectWeb, "w2")

	// Assert
	got := windows.Windows()
	if len(got) != 2 || !got[0].Closed || got[1].Closed {
		t.Fatalf("windows = %+v, want the boot's wave's closed and the reload's open", got)
	}
}

func TestAnExpectationOfNoWorkspaceOpensNoWindow(t *testing.T) {
	// Arrange
	var windows restarttimingtest.Windows
	r := restarttiming.NewReconnects(&restarttimingtest.Recorder{}, &windows, time.Now)

	// Act
	r.Expect(restarttiming.ReconnectCold, time.Now(), restarttiming.ReconnectHost)

	// Assert
	if got := windows.Windows(); len(got) != 0 {
		t.Fatalf("windows = %+v, want none for a wave of nobody", got)
	}
}

func TestAnExpectationRecordsItsWaveSizeOncePerPhaseAndCause(t *testing.T) {
	tests := []struct {
		name   string
		expect func(r *restarttiming.Reconnects, began time.Time)
		want   []string
	}{
		{"a cold boot states host then web", func(r *restarttiming.Reconnects, began time.Time) {
			r.Expect(restarttiming.ReconnectCold, began, restarttiming.ReconnectHost, "a", "b", "c")
			r.Expect(restarttiming.ReconnectCold, began, restarttiming.ReconnectWeb, "a")
		}, []string{"host cold 3", "web cold 1"}},
		{"a successor counts each phase's own members", func(r *restarttiming.Reconnects, began time.Time) {
			r.Expect(restarttiming.ReconnectSuccessor, began, restarttiming.ReconnectHost, "a", "b")
			r.Expect(restarttiming.ReconnectSuccessor, began, restarttiming.ReconnectWeb, "a", "b", "c", "d")
		}, []string{"host successor 2", "web successor 4"}},
		{"a webapp reload counts its webviews", func(r *restarttiming.Reconnects, began time.Time) {
			r.Expect(restarttiming.ReconnectWebappReload, began, restarttiming.ReconnectWeb, "a", "b")
		}, []string{"web webapp_reload 2"}},
		{"a workspace named twice counts once", func(r *restarttiming.Reconnects, began time.Time) {
			r.Expect(restarttiming.ReconnectCold, began, restarttiming.ReconnectHost, "a", "a")
		}, []string{"host cold 1"}},
		{"an empty expectation counts nothing", func(r *restarttiming.Reconnects, began time.Time) {
			r.Expect(restarttiming.ReconnectCold, began, restarttiming.ReconnectHost)
		}, nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			began := time.Date(2026, 11, 22, 12, 0, 0, 0, time.UTC)
			var rec restarttimingtest.Recorder
			r := restarttiming.NewReconnects(&rec, restarttiming.NoWindows{}, func() time.Time { return began })

			// Act
			tt.expect(r, began)

			// Assert
			if got := rec.Affecteds(); !slices.Equal(got, tt.want) {
				t.Fatalf("affected = %v, want %v", got, tt.want)
			}
		})
	}
}
