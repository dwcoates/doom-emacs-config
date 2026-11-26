package persistentwifi

import (
	"context"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

func TestHotspotJoinFailed(t *testing.T) {
	cases := []struct {
		name   string
		answer hotspotAnswer
		want   string
	}{
		{name: "sharingd could not be asked", answer: hotspotAnswer{Authorization: "authorized",
			Hotspot: &helperHotspotJoin{Stage: "browse_failed", Error: "Sharing.framework has no SFRemoteHotspotSession"}},
			want: "the hotspot join stopped at browse_failed: sharingd could not be asked which phones it hears: Sharing.framework has no SFRemoteHotspotSession"},
		{name: "no phone of the name, naming the phones heard", answer: hotspotAnswer{Authorization: "authorized",
			Hotspot: &helperHotspotJoin{Stage: "no_phone", PhonesHeard: []string{"An iPad", "A Watch"}}},
			want: "the hotspot join stopped at no_phone: sharingd heard no phone named Test Hotspot over Bluetooth (heard: An iPad, A Watch)"},
		{name: "no phone at all", answer: hotspotAnswer{Authorization: "authorized", Hotspot: &helperHotspotJoin{Stage: "no_phone"}},
			want: "the hotspot join stopped at no_phone: sharingd heard no phone named Test Hotspot over Bluetooth (heard: none)"},
		{name: "the phone did not start its hotspot", answer: hotspotAnswer{Authorization: "authorized",
			Hotspot: &helperHotspotJoin{Stage: "enable_failed", Error: "the phone did not start its hotspot: kNetworkErr"}},
			want: "the hotspot join stopped at enable_failed: the phone did not start its hotspot: kNetworkErr"},
		{name: "the hotspot out of range", answer: hotspotAnswer{Authorization: "authorized",
			Hotspot: &helperHotspotJoin{Stage: "not_visible", Network: "Test’s Phone", Error: "no network named Test’s Phone is in range"}},
			want: "the hotspot join stopped at not_visible: the phone started its hotspot, but Test’s Phone was not in range: no network named Test’s Phone is in range"},
		{name: "the join refused", answer: hotspotAnswer{Authorization: "authorized",
			Hotspot: &helperHotspotJoin{Stage: "join_failed", Network: "Test’s Phone", Error: "the join failed: tmpErr"}},
			want: "the hotspot join stopped at join_failed: Test’s Phone could not be joined: the join failed: tmpErr"},
		{name: "a stage this daemon does not know is named", answer: hotspotAnswer{Authorization: "authorized",
			Hotspot: &helperHotspotJoin{Stage: "paused", Error: "why"}},
			want: `the hotspot join stopped at paused: the Wi-Fi helper named a stage this daemon does not know ("paused"): why`},
		{name: "a helper without Location Services says so", answer: hotspotAnswer{Authorization: "denied",
			Hotspot: &helperHotspotJoin{Stage: "not_visible", Network: "P", Error: "no network named P is in range"}},
			want: "the hotspot join stopped at not_visible: the phone started its hotspot, but P was not in range: no network named P is in range; the Wi-Fi helper is not allowed Location Services (denied), so it names no network"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := hotspotJoinFailed(hotspot, tc.answer)

			// Assert.
			if got.Error() != tc.want {
				t.Fatalf("hotspotJoinFailed = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestDarwinJoinHotspot(t *testing.T) {
	cases := []struct {
		name        string
		said        answer
		wantNetwork string
		wantSteps   dlog.Context
		wantErr     string
	}{
		{name: "a join answers the network the helper joined and each step's time",
			said:        answer{file: `{"authorization":"authorized","hotspot":{"stage":"joined","network":"Test’s Phone","browse_ms":3,"enable_ms":1300,"scan_ms":4100,"join_ms":5000}}`},
			wantNetwork: "Test’s Phone",
			wantSteps:   dlog.Context{"stage": "joined", "browse_ms": int64(3), "enable_ms": int64(1300), "scan_ms": int64(4100), "join_ms": int64(5000)}},
		{name: "a join that stopped is its stage's error, with the steps it took",
			said:        answer{file: `{"authorization":"authorized","hotspot":{"stage":"no_phone","browse_ms":6000}}`},
			wantNetwork: hotspot, wantSteps: dlog.Context{"stage": "no_phone", "browse_ms": int64(6000)},
			wantErr: "the hotspot join stopped at no_phone: sharingd heard no phone named Test Hotspot over Bluetooth (heard: none)"},
		{name: "a helper that could not try is its error",
			said:        answer{file: `{"authorization":"authorized","error":"no Wi-Fi interface"}`},
			wantNetwork: hotspot, wantSteps: dlog.Context{}, wantErr: "the Wi-Fi helper's no Wi-Fi interface"},
		{name: "an answer naming no join is an error",
			said:        answer{file: `{"authorization":"authorized"}`},
			wantNetwork: hotspot, wantSteps: dlog.Context{}, wantErr: "the Wi-Fi helper's hotspot answer named no join"},
		{name: "a helper that could not be run is the launch's error",
			said:        answer{out: "LSOpenURLsWithRole() failed", code: 1},
			wantNetwork: hotspot, wantErr: "the Wi-Fi helper could not be run"},
		{name: "a helper that answered before open waited keeps the lost wait",
			said:        answer{out: openWaitLostSaid, code: 1, file: `{"authorization":"authorized","hotspot":{"stage":"joined","network":"P"}}`},
			wantNetwork: "P", wantSteps: dlog.Context{"stage": "joined", "launch_wait_lost": "/t/open exited 1: " + openWaitLostSaid}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.r.on(argJoin, tc.said)

			// Act.
			join, err := h.c.host.JoinHotspot(context.Background(), "en0", hotspot)

			// Assert.
			if join.network != tc.wantNetwork || len(join.steps) != len(tc.wantSteps) {
				t.Fatalf("JoinHotspot = %+v, want network %q and steps %+v", join, tc.wantNetwork, tc.wantSteps)
			}
			for k, v := range tc.wantSteps {
				if join.steps[k] != v {
					t.Fatalf("JoinHotspot steps[%s] = %v, want %v", k, join.steps[k], v)
				}
			}
			if (tc.wantErr == "") != (err == nil) || (err != nil && !strings.Contains(err.Error(), tc.wantErr)) {
				t.Fatalf("JoinHotspot error = %v, want %q", err, tc.wantErr)
			}
		})
	}
}

func TestAHotspotJoinPastItsBoundIsAFailedOutcomeNamingTheBound(t *testing.T) {
	// Arrange: the helper never answers.
	h := newHarness(t).standing(pmsetOff, summary(true, "Home"))
	h.c.hotspotJoinBound = 10 * time.Millisecond
	h.r.hangPrefix(argJoin)
	before := h.c.host.Read(context.Background())

	// Act.
	step := h.c.joinHotspot(context.Background(), before)

	// Assert.
	failed := step.outcome.GetFailed()
	if !step.cut || failed == nil || !strings.Contains(failed.GetDetail(), "the hotspot join did not answer within 10ms (measured 14s)") {
		t.Fatalf("joinHotspot = %+v, want a failed outcome cut by its bound", step)
	}
}
