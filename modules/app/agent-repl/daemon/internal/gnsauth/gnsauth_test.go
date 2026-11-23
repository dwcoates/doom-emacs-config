package gnsauth

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
)

const (
	workEmail = "dodge@chess.com"
	signInURL = "https://gns.example/auth/device?user_code=ABCD-EFGH"
	startOut  = "Visit: " + signInURL + "\nCode: ABCD-EFGH\n"
	rejection = "Error: authentication failed for production: credential is invalid or expired (permanently rejected by the server). Run: gns auth login --env production"
	// awaitBound bounds a test's wait for a record. Everything the fakes
	// answer is instant, so a healthy wait is under a millisecond.
	awaitBound = 5 * time.Second
)

// fakeRunner answers each gns call from handler and records every call.
type fakeRunner struct {
	mu      sync.Mutex
	calls   []string
	handler func(args string) (string, int, error)
}

func (f *fakeRunner) Run(ctx context.Context, dir string, argv []string) (string, int, error) {
	args := strings.Join(argv, " ")
	f.mu.Lock()
	f.calls = append(f.calls, args)
	handler := f.handler
	f.mu.Unlock()
	return handler(args)
}

func (f *fakeRunner) count(args string) int {
	f.mu.Lock()
	defer f.mu.Unlock()
	n := 0
	for _, c := range f.calls {
		if c == args {
			n++
		}
	}
	return n
}

type openCall struct{ url, email string }

type fakeOpener struct {
	mu    sync.Mutex
	calls []openCall
	err   error
}

func (f *fakeOpener) Open(ctx context.Context, log dlog.Logger, url, email string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, openCall{url, email})
	return f.err
}

func (f *fakeOpener) opened() []openCall {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]openCall(nil), f.calls...)
}

type fixture struct {
	r      *Renewer
	run    *fakeRunner
	open   *fakeOpener
	clock  *clocktest.Scripted
	log    *dlog.TestLogger
	email  string
	emailE error
}

// healthyRenewal answers a sign-in that completes after one pending poll.
func healthyRenewal() func(string) (string, int, error) {
	polls := 0
	return func(args string) (string, int, error) {
		switch args {
		case "gns whoami":
			return rejection, 1, nil
		case "gns auth login --env production --device":
			return startOut, 0, nil
		case "gns auth login --env production --device-poll":
			polls++
			if polls == 1 {
				return "authorization_pending", 1, nil
			}
			return "", 0, nil
		case "gns auth status":
			return "User: dodge (" + workEmail + ")", 0, nil
		}
		return "", 0, errors.New("unexpected call " + args)
	}
}

func newFixture(t *testing.T, handler func(string) (string, int, error)) *fixture {
	t.Helper()
	f := &fixture{
		run:   &fakeRunner{handler: handler},
		open:  &fakeOpener{},
		clock: clocktest.New(time.Date(2026, 11, 23, 12, 0, 0, 0, time.UTC)),
		log:   dlog.NewTestLogger(),
		email: workEmail,
	}
	r, err := New(Deps{
		Run: f.run, Dir: "/tmp", Open: f.open,
		WorkEmail: func(context.Context) (string, error) { return f.email, f.emailE },
		Clock:     f.clock, Life: context.Background(), Log: f.log, Env: "production",
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	f.r = r
	return f
}

// awaitOutcome waits for the n-th renewal outcome record (renewed or failed).
func (f *fixture) awaitOutcome(t *testing.T, n int) dlog.Record {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), awaitBound)
	defer cancel()
	records, err := f.log.AwaitCount(ctx, dlog.RecordQuery{Operation: opRenew, Match: func(r dlog.Record) bool {
		return r.Message == "the GNS credential is renewed" || r.Message == "the GNS sign-in did not complete"
	}}, n)
	if err != nil {
		t.Fatalf("awaiting outcome %d: %v (records %v)", n, err, f.log.Records())
	}
	return records[n-1]
}

func TestIsRejection(t *testing.T) {
	cases := []struct {
		name   string
		output string
		want   bool
	}{
		{"the gns-sockets log's rejection", rejection, true},
		{"the gateway's invalid jwt", "HTTP 401: Invalid or expired JWT", true},
		{"a missing login", "Error: not logged in", true},
		{"an unauthenticated status", "Not authenticated. Run: gns auth login", true},
		{"a name that does not resolve", `dial tcp: lookup gns.int.sgns.chess-platform.com: no such host`, false},
		{"a request that timed out", "context deadline exceeded", false},
		{"empty output", "", false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the case's output.

			// Act.
			got := IsRejection(tc.output)

			// Assert.
			if got != tc.want {
				t.Fatalf("IsRejection(%q) = %v, want %v", tc.output, got, tc.want)
			}
		})
	}
}

func TestSignInURL(t *testing.T) {
	// Arrange: the output of a real `gns auth login --device`.
	out := "🔐 GNS requires sign-in.\n\n   Visit: " + signInURL + "\n   Code:  ABCD-EFGH\n"

	// Act.
	got, err := SignInURL(out)

	// Assert.
	if err != nil || got != signInURL {
		t.Fatalf("SignInURL() = (%q, %v), want (%q, nil)", got, err, signInURL)
	}
}

func TestSignInURLRefusesOutputWithoutAPage(t *testing.T) {
	// Arrange: a start that printed no page.
	out := "Error: something else\n"

	// Act.
	_, err := SignInURL(out)

	// Assert.
	if err == nil {
		t.Fatal("SignInURL() = nil error, want a refusal for the missing page")
	}
}

func TestNewRefusesMissingDeps(t *testing.T) {
	full := Deps{
		Run: &fakeRunner{}, Open: &fakeOpener{},
		WorkEmail: func(context.Context) (string, error) { return "", nil },
		Clock:     clocktest.New(time.Now()), Life: context.Background(), Log: dlog.NewTestLogger(), Env: "production",
	}
	cases := []struct {
		name  string
		strip func(*Deps)
	}{
		{"runner", func(d *Deps) { d.Run = nil }},
		{"opener", func(d *Deps) { d.Open = nil }},
		{"work email source", func(d *Deps) { d.WorkEmail = nil }},
		{"clock", func(d *Deps) { d.Clock = nil }},
		{"lifetime", func(d *Deps) { d.Life = nil }},
		{"logger", func(d *Deps) { d.Log = nil }},
		{"environment", func(d *Deps) { d.Env = "" }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			deps := full
			tc.strip(&deps)

			// Act.
			_, err := New(deps)

			// Assert.
			if err == nil {
				t.Fatalf("New without a %s succeeded", tc.name)
			}
		})
	}
}

func TestARefusedProbeRenewsInTheWorkAccountsProfile(t *testing.T) {
	// Arrange.
	f := newFixture(t, healthyRenewal())

	// Act.
	f.r.probe(context.Background())
	outcome := f.awaitOutcome(t, 1)

	// Assert.
	if outcome.Message != "the GNS credential is renewed" {
		t.Fatalf("outcome = %q, want the renewal to succeed (records %v)", outcome.Message, f.log.Records())
	}
	if got := f.open.opened(); len(got) != 1 || got[0] != (openCall{signInURL, workEmail}) {
		t.Fatalf("opened = %v, want the sign-in page once, in the work account's profile", got)
	}
	if got := f.run.count("gns auth login --env production --device-poll"); got != 2 {
		t.Fatalf("polls = %d, want 2 (one pending, one done)", got)
	}
}

func TestAProbeThatIsNotARefusalStartsNoSignIn(t *testing.T) {
	cases := []struct {
		name    string
		handler func(string) (string, int, error)
		level   string
		branch  string
	}{
		{"an accepted credential", func(string) (string, int, error) { return "ok", 0, nil }, "debug", "accepted"},
		{"a name that does not resolve", func(string) (string, int, error) { return "lookup gns: no such host", 1, nil }, "warn", "not-a-rejection"},
		{"a gns that cannot run", func(string) (string, int, error) { return "", 0, errors.New("exec: gns: not found") }, "error", "unrunnable"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t, tc.handler)

			// Act.
			f.r.probe(context.Background())

			// Assert.
			rec, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Operation: opProbe, Level: tc.level})
			if !ok || rec.Context["branch"] != tc.branch {
				t.Fatalf("probe record = (%v, %v), want a %s record on branch %q", rec, ok, tc.level, tc.branch)
			}
			if got := f.run.count("gns auth login --env production --device"); got != 0 {
				t.Fatalf("sign-ins started = %d, want 0", got)
			}
			if got := f.open.opened(); len(got) != 0 {
				t.Fatalf("pages opened = %v, want none", got)
			}
		})
	}
}

func TestAProbePastItsBoundStartsNoSignIn(t *testing.T) {
	// Arrange: a probe whose context has already run out.
	f := newFixture(t, func(string) (string, int, error) { return "", 0, context.DeadlineExceeded })
	ctx, cancel := context.WithDeadline(context.Background(), time.Now().Add(-time.Second))
	defer cancel()

	// Act.
	f.r.probe(ctx)

	// Assert.
	rec, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Operation: opProbe, Level: "warn"})
	if !ok || rec.Context["branch"] != "bound" {
		t.Fatalf("probe record = (%v, %v), want the bound warning", rec, ok)
	}
	if got := f.run.count("gns auth login --env production --device"); got != 0 {
		t.Fatalf("sign-ins started = %d, want 0", got)
	}
}

func TestARenewalThatCannotCompleteIsLoggedOnceAndOpensNoPersonalPage(t *testing.T) {
	cases := []struct {
		name      string
		email     string
		emailErr  error
		openErr   error
		handler   func(string) (string, int, error)
		wantOpens int
		wantError string
	}{
		{
			name: "no work account is signed in", email: "", handler: healthyRenewal(),
			wantOpens: 0, wantError: "no work account",
		},
		{
			name: "the work account cannot be read", email: workEmail, emailErr: errors.New("malformed .claude.json"), handler: healthyRenewal(),
			wantOpens: 0, wantError: "reading the work account",
		},
		{
			name: "no profile signs in as the work account", email: workEmail, openErr: errors.New("no Chrome profile is signed in as " + workEmail), handler: healthyRenewal(),
			wantOpens: 1, wantError: "no Chrome profile",
		},
		{
			name: "the sign-in cannot start", email: workEmail, wantOpens: 0, wantError: "starting the device-code sign-in",
			handler: func(args string) (string, int, error) {
				if args == "gns whoami" {
					return rejection, 1, nil
				}
				return "boom", 3, nil
			},
		},
		{
			name: "the start prints no page", email: workEmail, wantOpens: 0, wantError: "printed no sign-in page",
			handler: func(args string) (string, int, error) {
				if args == "gns whoami" {
					return rejection, 1, nil
				}
				return "nothing here", 0, nil
			},
		},
		{
			name: "the sign-in is refused", email: workEmail, wantOpens: 1, wantError: "was refused (device-poll exit 2)",
			handler: func(args string) (string, int, error) {
				switch args {
				case "gns whoami":
					return rejection, 1, nil
				case "gns auth login --env production --device":
					return startOut, 0, nil
				}
				return "access_denied", 2, nil
			},
		},
		{
			name: "the environment reads as another account", email: workEmail, wantOpens: 1, wantError: "does not read as " + workEmail,
			handler: func(args string) (string, int, error) {
				switch args {
				case "gns whoami":
					return rejection, 1, nil
				case "gns auth login --env production --device":
					return startOut, 0, nil
				case "gns auth status":
					return "User: someone (other@example.com)", 0, nil
				}
				return "", 0, nil
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t, tc.handler)
			f.email, f.emailE, f.open.err = tc.email, tc.emailErr, tc.openErr

			// Act.
			f.r.probe(context.Background())
			outcome := f.awaitOutcome(t, 1)

			// Assert.
			if outcome.Level != "error" || !strings.Contains(outcome.Context["error"].(string), tc.wantError) {
				t.Fatalf("outcome = %v, want one error record naming %q", outcome, tc.wantError)
			}
			if got := f.open.opened(); len(got) != tc.wantOpens {
				t.Fatalf("pages opened = %v, want %d", got, tc.wantOpens)
			}
			for _, call := range f.open.opened() {
				if call.email != workEmail {
					t.Fatalf("a page was opened for %q, want only the work account", call.email)
				}
			}
			if got := f.run.count("gns auth login --env production"); got != 0 {
				t.Fatalf("the default browser sign-in ran %d times, want never", got)
			}
		})
	}
}

func TestTwoRefusalsStartOneSignIn(t *testing.T) {
	// Arrange: a sign-in held open until the test releases it.
	release := make(chan struct{})
	started := make(chan struct{}, 4)
	handler := healthyRenewal()
	f := newFixture(t, func(args string) (string, int, error) {
		if args == "gns auth login --env production --device" {
			started <- struct{}{}
			<-release
		}
		return handler(args)
	})

	// Act.
	f.r.Observe(rejection)
	<-started
	f.r.Observe(rejection)
	close(release)
	f.awaitOutcome(t, 1)

	// Assert.
	if got := f.run.count("gns auth login --env production --device"); got != 1 {
		t.Fatalf("sign-ins started = %d, want 1", got)
	}
}

func TestAFailedRenewalHoldsOffTheNextUntilItsCooldownEnds(t *testing.T) {
	// Arrange: a renewal that fails at the start.
	f := newFixture(t, func(args string) (string, int, error) { return "boom", 3, nil })
	f.r.Observe(rejection)
	f.awaitOutcome(t, 1)

	// Act and Assert: inside the cooldown a refusal starts nothing.
	f.clock.Set(f.clock.Now().Add(Cooldown - time.Second))
	f.r.Observe(rejection)
	if got := f.run.count("gns auth login --env production --device"); got != 1 {
		t.Fatalf("sign-ins started inside the cooldown = %d, want 1", got)
	}

	// Act and Assert: past the cooldown one starts.
	f.clock.Set(f.clock.Now().Add(2 * time.Second))
	f.r.Observe(rejection)
	f.awaitOutcome(t, 2)
	if got := f.run.count("gns auth login --env production --device"); got != 2 {
		t.Fatalf("sign-ins started past the cooldown = %d, want 2", got)
	}
}

func TestObserveIgnoresOutputThatIsNotARefusal(t *testing.T) {
	// Arrange.
	f := newFixture(t, healthyRenewal())

	// Act.
	got := f.r.Observe("lookup gns: no such host")

	// Assert.
	if got || f.run.count("gns auth login --env production --device") != 0 {
		t.Fatalf("Observe() = %v with %v calls, want false and no sign-in", got, f.run.calls)
	}
}

func TestObserveRunnerRaisesOnlyAGnsCallGnsRefused(t *testing.T) {
	cases := []struct {
		name string
		argv []string
		out  string
		code int
		err  error
		want int
	}{
		{"a refused gns call", []string{"gns", "slack", "channel"}, rejection, 1, nil, 1},
		{"a gns call that worked", []string{"gns", "slack", "channel"}, rejection, 0, nil, 0},
		{"a gns call that failed otherwise", []string{"gns", "slack"}, "no such host", 1, nil, 0},
		{"another program saying the same words", []string{"other", "x"}, rejection, 1, nil, 0},
		{"a gns call that could not run", []string{"gns", "slack"}, rejection, 1, errors.New("spawn"), 0},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: renewals fail at the start so each is one counted start.
			f := newFixture(t, func(string) (string, int, error) { return "boom", 3, nil })
			inner := &fakeRunner{handler: func(string) (string, int, error) { return tc.out, tc.code, tc.err }}
			wrapped := ObserveRunner(inner, f.r)

			// Act.
			out, code, err := wrapped.Run(context.Background(), "/tmp", tc.argv)
			if tc.want == 1 {
				f.awaitOutcome(t, 1)
			}

			// Assert: the answer is passed through, and the renewal ran or did not.
			if out != tc.out || code != tc.code || !errors.Is(err, tc.err) {
				t.Fatalf("Run() = (%q, %d, %v), want the inner answer unchanged", out, code, err)
			}
			if got := f.run.count("gns auth login --env production --device"); got != tc.want {
				t.Fatalf("sign-ins started = %d, want %d", got, tc.want)
			}
		})
	}
}

func TestWatchProbesAtOnceThenOnEveryBeat(t *testing.T) {
	// Arrange.
	f := newFixture(t, func(string) (string, int, error) { return "ok", 0, nil })
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- f.r.Watch(ctx) }()

	// Act: the first wait is none at all, the next is the probe's beat.
	if got := <-f.clock.Asked; got != 0 {
		t.Fatalf("first wait = %v, want none", got)
	}
	f.clock.Fire <- time.Time{}
	if got := <-f.clock.Asked; got != ProbeEvery {
		t.Fatalf("second wait = %v, want %v", got, ProbeEvery)
	}
	cancel()

	// Assert.
	if err := <-done; !errors.Is(err, context.Canceled) {
		t.Fatalf("Watch() = %v, want the context's end", err)
	}
	if got := f.run.count("gns whoami"); got != 1 {
		t.Fatalf("probes = %d, want 1", got)
	}
}
