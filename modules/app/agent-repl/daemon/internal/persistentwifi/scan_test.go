package persistentwifi

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"strings"
	"testing"
	"time"
)

func TestScanWifi(t *testing.T) {
	cases := []struct {
		name      string
		answer    func(argv []string) answer
		wantErr   string
		wantNames []string
	}{
		{name: "an authorized scan names what it saw, joined and in sight",
			answer:    writing(`{"authorization":"authorized","current":"Home","visible":[{"ssid":"Home","rssi":-40},{"ssid":"Cafe","rssi":-70}]}`),
			wantNames: []string{"Home", "Cafe"}},
		{name: "a helper denied Location Services names nothing and points at System Settings",
			answer:  writing(`{"authorization":"denied","visible":[]}`),
			wantErr: "allow it in System Settings > Privacy & Security > Location Services (its standing: denied)"},
		{name: "a helper macOS never asked about names nothing and points at the prompt",
			answer:  writing(`{"authorization":"not_determined","visible":[]}`),
			wantErr: "run `bin/requirements.sh install` to ask again (its standing: not_determined)"},
		{name: "a scan the helper could not make says why",
			answer:  writing(`{"authorization":"authorized","error":"the scan failed: busy","visible":[]}`),
			wantErr: "the Wi-Fi helper's the scan failed: busy"},
		{name: "a helper that wrote no answer is an error",
			answer:  func([]string) answer { return answer{} },
			wantErr: "wrote no answer"},
		{name: "an answer that is not the helper's is an error",
			answer:  writing(`not json`),
			wantErr: "could not be read"},
		{name: "a helper that could not be launched is an error",
			answer:  func([]string) answer { return answer{out: "LSOpenURLsWithRole() failed", code: 1} },
			wantErr: "could not be run"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a not_determined standing also launches the ask.
			h := newHarness(t).launchesAuthorize()
			h.r.onPrefix(argScan, tc.answer)

			// Act
			scan, err := h.c.scanWifi(context.Background())

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("scanWifi error = %v, want one saying %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("scanWifi error = %v", err)
			}
			var names []string
			for _, v := range scan.Visible {
				names = append(names, v.SSID)
			}
			if strings.Join(names, ",") != strings.Join(tc.wantNames, ",") || scan.Current != "Home" {
				t.Fatalf("scan = %+v, want %v in sight and Home joined", scan, tc.wantNames)
			}
		})
	}
}

func TestUnauthorizedScan(t *testing.T) {
	const systemSettings = "allow it in System Settings > Privacy & Security > Location Services"
	cases := []struct {
		standing   string
		wantRemedy string
	}{
		{standing: "not_determined", wantRemedy: "macOS never asked about it, so it is not yet listed in System Settings; allow it in the Location Services prompt agent-repl asks macOS to show, or run `bin/requirements.sh install` to ask again"},
		{standing: "denied", wantRemedy: systemSettings},
		{standing: "restricted", wantRemedy: systemSettings},
		{standing: "unknown", wantRemedy: systemSettings},
	}
	for _, tc := range cases {
		t.Run(tc.standing, func(t *testing.T) {
			// Act
			err := unauthorizedScan(tc.standing)

			// Assert
			if !errors.Is(err, errScanUnauthorized) {
				t.Fatalf("unauthorizedScan(%q) = %v, want errScanUnauthorized", tc.standing, err)
			}
			want := errScanUnauthorized.Error() + "; " + tc.wantRemedy + " (its standing: " + tc.standing + ")"
			if err.Error() != want {
				t.Fatalf("unauthorizedScan(%q) = %q, want %q", tc.standing, err.Error(), want)
			}
		})
	}
}

// writing answers a scan by writing RAW to the file its argv names.
func writing(raw string) func(argv []string) answer {
	return func(argv []string) answer {
		if err := os.WriteFile(argv[len(argv)-1], []byte(raw), 0o600); err != nil {
			return answer{err: err}
		}
		return answer{}
	}
}

func TestAScanThatOutlivesItsBoundIsCutAndSaysSo(t *testing.T) {
	// Arrange: a helper that never answers, under a bound short enough to run.
	h := newHarness(t)
	h.c.scanBound = time.Millisecond
	h.r.hangPrefix(argScan)

	// Act
	_, err := h.c.scanWifi(context.Background())

	// Assert
	if err == nil || !errors.Is(err, context.DeadlineExceeded) || !strings.Contains(err.Error(), "did not answer within 1ms (measured 7.8s)") {
		t.Fatalf("scanWifi error = %v, want the bound and its measurement named", err)
	}
}

func TestAScanLeavesNoAnswerFileBehind(t *testing.T) {
	// Arrange
	h := newHarness(t)
	var written string
	h.r.onPrefix(argScan, func(argv []string) answer {
		written = argv[len(argv)-1]
		return writing(`{"authorization":"authorized","visible":[]}`)(argv)
	})

	// Act
	if _, err := h.c.scanWifi(context.Background()); err != nil {
		t.Fatalf("scanWifi: %v", err)
	}

	// Assert
	if _, err := os.Stat(written); !os.IsNotExist(err) {
		t.Fatalf("the answer file %s is still there (stat %v)", written, err)
	}
}

func TestVisibleKnown(t *testing.T) {
	// Arrange
	scan := inSight("", "Cafe", "Stranger", "Home’s")

	// Act
	got := visibleKnown([]string{"Home's", "Office", "Cafe"}, scan)

	// Assert: preference order, apostrophes matched as macOS names them.
	if strings.Join(got, ",") != "Home's,Cafe" {
		t.Fatalf("visibleKnown = %v, want the known networks in sight in preference order", got)
	}
}

// authorizeCalls answers every authorize launch, in order.
func (h *harness) authorizeCalls() []string {
	var out []string
	for _, c := range h.r.ran() {
		if strings.HasPrefix(c, argAuthorize) {
			out = append(out, c)
		}
	}
	return out
}

func TestAScanOfAStandingNeverAskedForAsksOnce(t *testing.T) {
	cases := []struct {
		name      string
		standing  string
		scans     int
		wantAsked int
	}{
		{name: "a standing never asked for is asked for", standing: "not_determined", scans: 1, wantAsked: 1},
		{name: "a second scan asks no more", standing: "not_determined", scans: 2, wantAsked: 1},
		{name: "a denied standing is the owner's answer and is not asked again", standing: "denied", scans: 1, wantAsked: 0},
		{name: "a standing whose ask waits on the owner is not asked again", standing: "awaiting_owner", scans: 1, wantAsked: 0},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t).sight(wifiScan{Authorization: tc.standing}).launchesAuthorize()

			// Act
			for range tc.scans {
				if _, err := h.c.scanWifi(context.Background()); !errors.Is(err, errScanUnauthorized) {
					t.Fatalf("scanWifi error = %v, want the unauthorized standing", err)
				}
			}

			// Assert
			if got := h.authorizeCalls(); len(got) != tc.wantAsked {
				t.Fatalf("authorize launches = %v, want %d", got, tc.wantAsked)
			}
			asked := h.failoverRecords("info", "asked macOS for the Wi-Fi helper's Location Services permission; its prompt waits for the owner")
			if len(asked) != tc.wantAsked {
				t.Fatalf("asked records = %+v, want %d", asked, tc.wantAsked)
			}
			if tc.wantAsked > 0 && asked[0]["app"] != "/t/AgentReplWifi.app" {
				t.Fatalf("asked record = %+v, want the helper named", asked[0])
			}
		})
	}
}

func TestAnAuthorizeLaunchThatFailsIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t).sight(wifiScan{Authorization: "not_determined"})
	h.r.onPrefix(argAuthorize, func([]string) answer { return answer{out: "LSOpenURLsWithRole() failed", code: 1} })

	// Act
	_, err := h.c.scanWifi(context.Background())

	// Assert
	if !errors.Is(err, errScanUnauthorized) {
		t.Fatalf("scanWifi error = %v, want the unauthorized standing still named", err)
	}
	errs := h.failoverRecords("error", "could not ask macOS for the Wi-Fi helper's Location Services permission")
	if len(errs) != 1 || errs[0]["app"] != "/t/AgentReplWifi.app" || errs[0]["cause"] == nil {
		t.Fatalf("error records = %+v, want one naming the helper and the cause", errs)
	}
	if asked := h.failoverRecords("info", "asked macOS for the Wi-Fi helper's Location Services permission; its prompt waits for the owner"); len(asked) != 0 {
		t.Fatalf("asked records = %+v, want none for a launch that failed", asked)
	}
}

func TestHelperLaunch(t *testing.T) {
	cases := []struct {
		name string
		wait helperWait
		verb string
		want string
	}{
		{name: "a scan waits for its answer", wait: helperWaits, verb: "scan", want: argScan + "/f"},
		{name: "an authorize is launched and left", wait: helperLeftRunning, verb: "authorize", want: argAuthorize + "/f"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			tools := Tools{Open: "/t/open", WifiApp: "/t/AgentReplWifi.app"}

			// Act
			got := strings.Join(helperLaunch(tools, tc.wait, tc.verb, "/f"), " ")

			// Assert: the very argv every controller launch is scripted against.
			if got != tc.want {
				t.Fatalf("helperLaunch = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestAStandingForgottenAfterAnAllowedScanIsAskedForAgain(t *testing.T) {
	// Arrange: asked, then allowed, then forgotten by a re-signed helper.
	h := newHarness(t).sight(
		wifiScan{Authorization: "not_determined"},
		wifiScan{Authorization: "authorized"},
		wifiScan{Authorization: "not_determined"},
	).launchesAuthorize()

	// Act
	for range 3 {
		_, _ = h.c.scanWifi(context.Background())
	}

	// Assert
	if got := h.authorizeCalls(); len(got) != 2 {
		t.Fatalf("authorize launches = %v, want one per standing never asked for", got)
	}
}

func TestStopAsks(t *testing.T) {
	// argv is the one pkill StopAsks runs against the helper at /t.
	const argv = `/t/pkill -f ^/t/AgentReplWifi\.app/Contents/MacOS/agent-repl-wifi authorize( |$)`
	cases := []struct {
		name        string
		answer      answer
		wantStopped bool
		wantErr     string
	}{
		{name: "an ask that was waiting is stopped", answer: answer{code: 0}, wantStopped: true},
		{name: "no ask waiting stops nothing", answer: answer{code: 1}},
		{name: "a pkill that failed says how", answer: answer{out: "pkill: bad pattern\n", code: 2}, wantErr: "pkill exited 2: pkill: bad pattern"},
		{name: "a pkill that could not run says why", answer: answer{err: errors.New("no such file")}, wantErr: "pkill could not be run: no such file"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r := newFakeRunner(t).on(argv, tc.answer)

			// Act
			stopped, err := StopAsks(context.Background(), r, Tools{Pkill: "/t/pkill", WifiApp: "/t/AgentReplWifi.app"})

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("StopAsks error = %v, want one saying %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || stopped != tc.wantStopped {
				t.Fatalf("StopAsks = %v, %v, want %v", stopped, err, tc.wantStopped)
			}
			if got := r.ran(); len(got) != 1 || got[0] != argv {
				t.Fatalf("ran %q, want the one pkill %q", got, argv)
			}
		})
	}
}

func TestAStopAsksThatOutlivesItsBoundSaysSo(t *testing.T) {
	// Arrange
	r := newFakeRunner(t).hangPrefix("/t/pkill")

	// Act
	_, err := StopAsks(context.Background(), r, Tools{Pkill: "/t/pkill", WifiApp: "/t/AgentReplWifi.app"})

	// Assert
	if err == nil || !errors.Is(err, context.DeadlineExceeded) || !strings.Contains(err.Error(), "did not end within 500ms (expected 200ms)") {
		t.Fatalf("StopAsks error = %v, want the bound and its measurement named", err)
	}
}

func TestScanContextCarriesTheHelpersStandingTime(t *testing.T) {
	standing := int64(23)
	cases := []struct {
		name string
		raw  string
		want any
	}{
		{name: "a helper that heard its standing states its time", raw: `{"authorization":"authorized","standing_ms":23}`, want: standing},
		{name: "a helper that never heard it states none", raw: `{"authorization":"authorized"}`, want: nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var scan wifiScan
			if err := json.Unmarshal([]byte(tc.raw), &scan); err != nil {
				t.Fatal(err)
			}

			// Act
			ctx := scanContext(scan, 300*time.Millisecond)

			// Assert
			if got := ctx["standing_ms"]; got != tc.want {
				t.Fatalf("standing_ms = %v, want %v", got, tc.want)
			}
			if ctx["took_ms"] != int64(300) {
				t.Fatalf("took_ms = %v, want 300", ctx["took_ms"])
			}
		})
	}
}
