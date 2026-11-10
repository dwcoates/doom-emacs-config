package deploy

import (
	"context"
	"fmt"
	"path/filepath"
	"regexp"
	"strconv"
)

// Launchctl is macOS's ServiceManager: launchctl in the user's GUI domain,
// with each service's plist installed in PlistDir as `<label>.plist`.
type Launchctl struct {
	// Binary is the launchctl to run ("launchctl" resolves it off PATH).
	Binary string
	// UID is the user whose GUI domain the services live in.
	UID int
	// PlistDir is where the installed plists live (~/Library/LaunchAgents).
	PlistDir string
	// Dir is the working directory the commands run in.
	Dir    string
	Runner Runner
}

var _ ServiceManager = (*Launchctl)(nil)

// launchdPID is launchctl print's pid line.
var launchdPID = regexp.MustCompile(`(?m)^\s*pid = (\d+)\s*$`)

// exitServiceNotFound is launchctl's exit code for a label the domain does not
// hold: `launchctl error 113` reads "Could not find specified service". It is
// what `print` answers for a service that is not loaded — the ordinary answer
// the sidecar stop polls for after its bootout (2026-09-24 13:32:25, the one
// exit the deploy's log recorded) — and what `bootout` answers for a service
// that had already left the domain.
const exitServiceNotFound = 113

func (l *Launchctl) domain() string { return "gui/" + strconv.Itoa(l.UID) }

// Status implements ServiceManager: whether the domain holds the label.
func (l *Launchctl) Status(ctx context.Context, label string) (bool, int, error) {
	args := []string{"print", l.domain() + "/" + label}
	out, code, err := l.run(ctx, args...)
	if err != nil {
		return false, 0, err
	}
	if code != 0 {
		if code == exitServiceNotFound {
			return false, 0, nil
		}
		return false, 0, toolExited(l.Binary, args, code, out)
	}
	match := launchdPID.FindStringSubmatch(out)
	if match == nil {
		return true, 0, nil
	}
	pid, err := strconv.Atoi(match[1])
	if err != nil {
		return false, 0, fmt.Errorf("deploy: launchctl print %s named an unreadable pid %q: %w", label, match[1], err)
	}
	return true, pid, nil
}

// Restart implements ServiceManager: kickstart -k.
func (l *Launchctl) Restart(ctx context.Context, label string) error {
	return l.must(ctx, "kickstart", "-k", l.domain()+"/"+label)
}

// Stop implements ServiceManager: bootout, which takes the service out of the
// domain and its KeepAlive with it. A label the domain does not hold answers
// ErrServiceNotLoaded, wrapped with launchctl's words, so the caller can tell
// "already gone" from a bootout that failed.
func (l *Launchctl) Stop(ctx context.Context, label string) error {
	args := []string{"bootout", l.domain() + "/" + label}
	out, code, err := l.run(ctx, args...)
	if err != nil {
		return err
	}
	switch code {
	case 0:
		return nil
	case exitServiceNotFound:
		return fmt.Errorf("%w: %w", ErrServiceNotLoaded, toolExited(l.Binary, args, code, out))
	default:
		return toolExited(l.Binary, args, code, out)
	}
}

// Start implements ServiceManager: bootstrap from the label's plist, which
// RunAtLoad starts.
func (l *Launchctl) Start(ctx context.Context, label string) error {
	return l.must(ctx, "bootstrap", l.domain(), l.Definition(label))
}

// Definition implements ServiceManager: the label's installed plist.
func (l *Launchctl) Definition(label string) string {
	return filepath.Join(l.PlistDir, label+".plist")
}

// must runs a verb whose non-zero exit is a failure.
func (l *Launchctl) must(ctx context.Context, args ...string) error {
	return mustTool(ctx, l.Runner, l.Dir, l.Binary, args...)
}

func (l *Launchctl) run(ctx context.Context, args ...string) (string, int, error) {
	return runTool(ctx, l.Runner, l.Dir, l.Binary, args...)
}
