package deploy

import (
	"context"
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

// Runner runs one command and reports its combined output with its exit code;
// scriptrunner.Runner is the production one.
type Runner interface {
	Run(ctx context.Context, dir string, argv []string) (output string, exitCode int, err error)
}

// Launchctl is the production Launchd: launchctl in the user's GUI domain.
type Launchctl struct {
	// Binary is the launchctl to run ("launchctl" resolves it off PATH).
	Binary string
	// UID is the user whose GUI domain the services live in.
	UID int
	// Dir is the working directory the commands run in.
	Dir    string
	Runner Runner
}

var _ Launchd = (*Launchctl)(nil)

// launchdPID is launchctl print's pid line.
var launchdPID = regexp.MustCompile(`(?m)^\s*pid = (\d+)\s*$`)

// notFound is launchctl print's answer for a label the domain does not hold.
const notFound = "Could not find service"

func (l *Launchctl) domain() string { return "gui/" + strconv.Itoa(l.UID) }

// Print implements Launchd.
func (l *Launchctl) Print(ctx context.Context, label string) (bool, int, error) {
	out, code, err := l.run(ctx, "print", l.domain()+"/"+label)
	if err != nil {
		return false, 0, err
	}
	if code != 0 {
		if strings.Contains(out, notFound) {
			return false, 0, nil
		}
		return false, 0, fmt.Errorf("deploy: launchctl print %s exited %d: %s", label, code, strings.TrimSpace(out))
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

// Kickstart implements Launchd.
func (l *Launchctl) Kickstart(ctx context.Context, label string) error {
	return l.must(ctx, "kickstart", "-k", l.domain()+"/"+label)
}

// Bootout implements Launchd.
func (l *Launchctl) Bootout(ctx context.Context, label string) error {
	return l.must(ctx, "bootout", l.domain()+"/"+label)
}

// Bootstrap implements Launchd.
func (l *Launchctl) Bootstrap(ctx context.Context, plist string) error {
	return l.must(ctx, "bootstrap", l.domain(), plist)
}

// must runs a verb whose non-zero exit is a failure.
func (l *Launchctl) must(ctx context.Context, args ...string) error {
	out, code, err := l.run(ctx, args...)
	if err != nil {
		return err
	}
	if code != 0 {
		return fmt.Errorf("deploy: launchctl %s exited %d: %s", strings.Join(args, " "), code, strings.TrimSpace(out))
	}
	return nil
}

func (l *Launchctl) run(ctx context.Context, args ...string) (string, int, error) {
	argv := append([]string{l.Binary}, args...)
	out, code, err := l.Runner.Run(ctx, l.Dir, argv)
	if err != nil {
		return "", 0, fmt.Errorf("deploy: run %s: %w", strings.Join(argv, " "), err)
	}
	return out, code, nil
}
