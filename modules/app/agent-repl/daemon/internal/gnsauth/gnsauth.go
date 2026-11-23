// Package gnsauth keeps the daemon's GNS sign-in alive: when GNS rejects the
// stored credential, it starts the device-code sign-in and opens its page in
// the WORK account's Chrome profile, never the personal one.
//
// WHAT IT NOTICES. A GNS call answering that its credential is invalid or
// expired (IsRejection) is the signal. Two things raise it: the daemon's own
// gns calls, observed through ObserveRunner, and a standing probe (Watch) that
// asks the server whether the credential still works, because the plugins that
// poll GNS in the background (gns-sockets) log the rejection where no agent
// reads it. A failure that is not a rejection, such as the network being
// down, is an expected condition and starts no sign-in.
//
// WHAT IT DOES. One renewal runs at a time (Renewer.trigger): it starts
// `gns auth login --device`, opens the printed page in the Chrome profile the
// work account signs in as, polls `--device-poll` until the person completes
// the sign-in, and confirms `gns auth status` reads as the work account. A
// renewal that fails is logged once, here, and waits Cooldown before another
// may open a page, so an ignored sign-in is not a tab every probe.
//
// THE BROWSER IS NEVER A GUESS. The work account's address is required and
// the opener refuses an address no Chrome profile signs in as, so a missing
// setup fails loudly and no page opens in a personal profile. The default
// browser flow (`gns auth login` without `--device`) is never run.
package gnsauth

import (
	"context"
	"errors"
	"fmt"
	"regexp"
	"strings"
	"sync"
	"time"

	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
	"agent-repld/internal/outputtext"
	"agent-repld/internal/usersetup"
)

const (
	opProbe = "daemon.gnsauth.probe"
	opRenew = "daemon.gnsauth.renew"
)

// Bounds. Each is two to three times what was measured on 2026-11-23 on a
// healthy host, except where the wait is a person's.
const (
	// ProbeBound bounds one `gns whoami`: measured 0.44s, 0.86s and 0.93s.
	ProbeBound = 3 * time.Second
	// StartBound bounds `gns auth login --device`: measured 0.29s. It sits
	// just above three times the measure because the network call is all it
	// does and a tighter bound would fail a healthy start.
	StartBound = time.Second
	// PollBound bounds one `gns auth login --device-poll`, which holds the
	// call open while the sign-in is pending: measured 32.2s.
	PollBound = 90 * time.Second
	// StatusBound bounds `gns auth status`: measured 0.10s.
	StatusBound = time.Second
	// SignInBound bounds the whole wait for the person to complete the
	// sign-in page: a device code lasts minutes, so a sign-in nobody is
	// completing is given up on after fifteen.
	SignInBound = 15 * time.Minute
	// ProbeEvery is how often the standing probe asks the server. The
	// gns-sockets plugin polls every three minutes, so a rejection is noticed
	// as soon as the plugin would hit it.
	ProbeEvery = 3 * time.Minute
	// Cooldown is how long a failed renewal waits before another may open a
	// page.
	Cooldown = 30 * time.Minute
)

// rejectionPhrases are what GNS and its gateway answer for a credential they
// refuse, matched case-insensitively. Each was read from a real answer: the
// first from the gns-sockets log (2026-11-23), the rest from the
// gns-auth-renew skill's documented triggers.
var rejectionPhrases = []string{
	"credential is invalid or expired",
	"invalid or expired jwt",
	"authentication failed for",
	"not logged in",
	"token expired",
	"not authenticated",
}

// IsRejection reports whether output is GNS refusing the stored credential.
func IsRejection(output string) bool {
	lower := strings.ToLower(output)
	for _, phrase := range rejectionPhrases {
		if strings.Contains(lower, phrase) {
			return true
		}
	}
	return false
}

var signInPage = regexp.MustCompile(`https://\S+`)

// SignInURL is the page the device-code sign-in printed.
func SignInURL(output string) (string, error) {
	url := signInPage.FindString(output)
	if url == "" {
		return "", fmt.Errorf("the device-code sign-in printed no sign-in page: %s", strings.TrimSpace(output))
	}
	return url, nil
}

// Runner runs argv in dir and answers its combined output and exit code
// (scriptrunner.Runner).
type Runner interface {
	Run(ctx context.Context, dir string, argv []string) (string, int, error)
}

// Opener opens a page in the Chrome profile an account signs in as
// (externalbrowser.Opener).
type Opener interface {
	Open(ctx context.Context, log dlog.Logger, url, accountEmail string) error
}

// Deps is what a Renewer works with. Every field is required.
type Deps struct {
	Run Runner
	// Dir is the directory gns runs in; the calls do not depend on it.
	Dir  string
	Open Opener
	// WorkEmail answers the work account's address, which routes the sign-in
	// page to its Chrome profile.
	WorkEmail func(ctx context.Context) (string, error)
	Clock     clock.Clock
	// Life bounds every renewal beyond its own bounds: a renewal outlives the
	// call that raised it, never the daemon.
	Life context.Context
	Log  dlog.Logger
	// Env is the GNS environment kept signed in ("production").
	Env string
}

// Renewer is the daemon's one owner of the GNS sign-in. Build it with New.
type Renewer struct {
	deps Deps

	mu sync.Mutex
	// renewing is true while a renewal runs.
	renewing bool
	// notBefore is the instant a failed renewal's cooldown ends; the zero
	// time when none is standing.
	notBefore time.Time
}

// New builds a Renewer.
func New(deps Deps) (*Renewer, error) {
	switch {
	case deps.Run == nil:
		return nil, errors.New("gnsauth: a runner is required")
	case deps.Open == nil:
		return nil, errors.New("gnsauth: a browser opener is required")
	case deps.WorkEmail == nil:
		return nil, errors.New("gnsauth: the work account's address source is required")
	case deps.Clock == nil:
		return nil, errors.New("gnsauth: a clock is required")
	case deps.Life == nil:
		return nil, errors.New("gnsauth: a lifetime context is required")
	case deps.Log == nil:
		return nil, errors.New("gnsauth: a logger is required")
	case deps.Env == "":
		return nil, errors.New("gnsauth: the GNS environment is required")
	}
	return &Renewer{deps: deps}, nil
}

// Observe raises a renewal when output, a gns call's combined output, is GNS
// refusing the credential, and reports whether it was.
func (r *Renewer) Observe(output string) bool {
	if !IsRejection(output) {
		return false
	}
	r.trigger("a gns call was refused")
	return true
}

// Watch runs the standing probe until ctx ends: first at once, then every
// ProbeEvery.
func (r *Renewer) Watch(ctx context.Context) error {
	return clock.Repeat(ctx, r.deps.Clock, 0, ProbeEvery, r.probe)
}

// probe asks the server, with `gns whoami`, whether the credential works. A
// refusal raises a renewal. Any other failure is logged and raises nothing: a
// network that is down says nothing about the credential.
func (r *Renewer) probe(ctx context.Context) {
	ctx, cancel := context.WithTimeout(ctx, ProbeBound)
	defer cancel()
	output, code, err := r.deps.Run.Run(ctx, r.deps.Dir, []string{"gns", "whoami"})
	switch {
	case ctx.Err() != nil && errors.Is(ctx.Err(), context.DeadlineExceeded):
		r.deps.Log.Warn(opProbe, "the credential probe ran past its bound; no sign-in was started", dlog.Context{
			"bound": ProbeBound.String(), "expected": "0.4-0.9s", "branch": "bound",
		})
	case err != nil:
		r.deps.Log.Error(opProbe, "the credential probe could not be run", dlog.Context{
			"error": err.Error(), "branch": "unrunnable",
		})
	case code == 0:
		r.deps.Log.Debug(opProbe, "the GNS credential is accepted", dlog.Context{"branch": "accepted"})
	case IsRejection(output):
		r.deps.Log.Warn(opProbe, "GNS refused the credential; a sign-in is starting", dlog.Context{
			"exit_code": code, "branch": "rejected", "output": outputtext.LastLine(output),
		})
		r.trigger("the probe was refused")
	default:
		r.deps.Log.Warn(opProbe, "the credential probe failed without refusing the credential; no sign-in was started", dlog.Context{
			"exit_code": code, "branch": "not-a-rejection", "output": outputtext.LastLine(output),
		})
	}
}

// trigger starts a renewal unless one runs or a failed one's cooldown stands.
// The decision and the mark that a renewal runs are made under one lock, so
// two callers can never start two sign-ins.
func (r *Renewer) trigger(reason string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	now := r.deps.Clock.Now()
	switch {
	case r.renewing:
		r.deps.Log.Debug(opRenew, "a sign-in is already running", dlog.Context{"reason": reason, "branch": "running"})
		return
	case now.Before(r.notBefore):
		r.deps.Log.Debug(opRenew, "a failed sign-in's cooldown stands", dlog.Context{
			"reason": reason, "branch": "cooldown", "until": r.notBefore.Format(time.RFC3339),
		})
		return
	}
	r.renewing = true
	inflight.Go(func() { r.run(reason) })
}

// run is one renewal's whole life: it records the outcome and sets the
// cooldown on failure.
func (r *Renewer) run(reason string) {
	err := r.renew(r.deps.Life)
	r.mu.Lock()
	defer r.mu.Unlock()
	r.renewing = false
	if err != nil {
		r.notBefore = r.deps.Clock.Now().Add(Cooldown)
		r.deps.Log.Error(opRenew, "the GNS sign-in did not complete", dlog.Context{
			"error": err.Error(), "reason": reason, "env": r.deps.Env, "cooldown": Cooldown.String(),
		})
		return
	}
	r.notBefore = time.Time{}
	r.deps.Log.Info(opRenew, "the GNS credential is renewed", dlog.Context{"reason": reason, "env": r.deps.Env})
}

// renew runs the device-code sign-in in the work account's Chrome profile.
func (r *Renewer) renew(ctx context.Context) error {
	email, err := r.deps.WorkEmail(ctx)
	if err != nil {
		return fmt.Errorf("reading the work account: %w", err)
	}
	if strings.TrimSpace(email) == "" {
		return usersetup.Errorf("no work account is signed in, so there is no Chrome profile to open the GNS sign-in in")
	}
	started, err := r.gns(ctx, StartBound, "auth", "login", "--env", r.deps.Env, "--device")
	if err != nil {
		return fmt.Errorf("starting the device-code sign-in: %w", err)
	}
	url, err := SignInURL(started)
	if err != nil {
		return err
	}
	if err := r.deps.Open.Open(ctx, r.deps.Log, url, email); err != nil {
		return fmt.Errorf("opening the sign-in page in %s's Chrome profile: %w", email, err)
	}
	r.deps.Log.Info(opRenew, "opened the GNS sign-in page in the work account's Chrome profile", dlog.Context{
		"url": url, "email": email, "env": r.deps.Env,
	})
	if err := r.awaitSignIn(ctx); err != nil {
		return err
	}
	status, err := r.gns(ctx, StatusBound, "auth", "status")
	if err != nil {
		return fmt.Errorf("confirming the sign-in: %w", err)
	}
	if !strings.Contains(strings.ToLower(status), strings.ToLower(email)) {
		return fmt.Errorf("the %s environment does not read as %s after the sign-in: %s", r.deps.Env, email, outputtext.LastLine(status))
	}
	return nil
}

// awaitSignIn polls the device code until the person completes the sign-in:
// exit 0 is done, 1 pending, anything else a refusal.
func (r *Renewer) awaitSignIn(ctx context.Context) error {
	ctx, cancel := context.WithTimeout(ctx, SignInBound)
	defer cancel()
	for {
		pollCtx, pollCancel := context.WithTimeout(ctx, PollBound)
		output, code, err := r.deps.Run.Run(pollCtx, r.deps.Dir, []string{"gns", "auth", "login", "--env", r.deps.Env, "--device-poll"})
		pollCancel()
		switch {
		case ctx.Err() != nil:
			return fmt.Errorf("the sign-in was not completed within %s: %w", SignInBound, ctx.Err())
		case err != nil && errors.Is(pollCtx.Err(), context.DeadlineExceeded):
			// A poll held open past its bound is one pending poll, not a refusal.
		case err != nil:
			return fmt.Errorf("polling the sign-in: %w", err)
		case code == 0:
			return nil
		case code != 1:
			return fmt.Errorf("the sign-in was refused (device-poll exit %d): %s", code, outputtext.LastLine(output))
		}
	}
}

// gns runs `gns args...` under bound, answering its output, and an error for
// a run that could not start, ran past its bound, or exited non-zero.
func (r *Renewer) gns(ctx context.Context, bound time.Duration, args ...string) (string, error) {
	ctx, cancel := context.WithTimeout(ctx, bound)
	defer cancel()
	output, code, err := r.deps.Run.Run(ctx, r.deps.Dir, append([]string{"gns"}, args...))
	switch {
	case errors.Is(ctx.Err(), context.DeadlineExceeded):
		return output, fmt.Errorf("gns %s ran past its %s bound", args[0]+" "+args[1], bound)
	case err != nil:
		return output, err
	case code != 0:
		return output, fmt.Errorf("gns exited %d: %s", code, outputtext.LastLine(output))
	}
	return output, nil
}

// ObserveRunner is a Runner that tells r about every gns call GNS refuses.
func ObserveRunner(inner Runner, r *Renewer) Runner { return observed{inner: inner, r: r} }

type observed struct {
	inner Runner
	r     *Renewer
}

func (o observed) Run(ctx context.Context, dir string, argv []string) (string, int, error) {
	output, code, err := o.inner.Run(ctx, dir, argv)
	if err == nil && code != 0 && len(argv) > 0 && argv[0] == "gns" {
		o.r.Observe(output)
	}
	return output, code, err
}
