package deploy

import (
	"context"
	"fmt"
	"path/filepath"
	"strconv"
	"strings"
)

// SystemdUser is Linux's ServiceManager: the user's own systemd manager
// (`systemctl <verb> --user`, the verb first so each call names it as its
// first word), with each service's unit installed in UnitDir as
// `<label>.service`. Every unit is Restart=always, so systemd relaunches a
// process that dies until the unit is stopped.
type SystemdUser struct {
	// Binary is the systemctl to run ("systemctl" resolves it off PATH).
	Binary string
	// UnitDir is where the installed units live (~/.config/systemd/user).
	UnitDir string
	// Dir is the working directory the commands run in.
	Dir    string
	Runner Runner
}

var _ ServiceManager = (*SystemdUser)(nil)

// exitUnitNotLoaded is systemctl's exit code for a unit its manager has not
// loaded ("Unit x.service not loaded."), which is what `stop` answers for a
// unit that was never installed.
const exitUnitNotLoaded = 5

// heldStates are the ActiveStates in which systemd is keeping a unit running:
// up, coming up (including the pause before an automatic restart), reloading,
// or on its way down. `inactive` and `failed` are a unit nothing relaunches.
var heldStates = map[string]bool{"active": true, "activating": true, "reloading": true, "refreshing": true, "deactivating": true}

func unitName(label string) string { return label + ".service" }

// Status implements ServiceManager: the unit's ActiveState and MainPID.
func (s *SystemdUser) Status(ctx context.Context, label string) (bool, int, error) {
	args := []string{"show", "--user", unitName(label), "--property=ActiveState", "--property=MainPID"}
	out, code, err := runTool(ctx, s.Runner, s.Dir, s.Binary, args...)
	if err != nil {
		return false, 0, err
	}
	if code != 0 {
		return false, 0, toolExited(s.Binary, args, code, out)
	}
	props := map[string]string{}
	for _, line := range strings.Split(out, "\n") {
		if k, v, ok := strings.Cut(strings.TrimSpace(line), "="); ok {
			props[k] = v
		}
	}
	state, ok := props["ActiveState"]
	if !ok {
		return false, 0, fmt.Errorf("deploy: systemctl %s answered no ActiveState: %s", strings.Join(args, " "), strings.TrimSpace(out))
	}
	if !heldStates[state] {
		return false, 0, nil
	}
	pid, err := strconv.Atoi(props["MainPID"])
	if err != nil {
		return false, 0, fmt.Errorf("deploy: systemctl %s named an unreadable MainPID %q: %w", strings.Join(args, " "), props["MainPID"], err)
	}
	return true, pid, nil
}

// Restart implements ServiceManager.
func (s *SystemdUser) Restart(ctx context.Context, label string) error {
	return mustTool(ctx, s.Runner, s.Dir, s.Binary, "restart", "--user", unitName(label))
}

// Stop implements ServiceManager: stop, after which Restart=always no longer
// relaunches it. A unit the manager has not loaded answers ErrServiceNotLoaded;
// a loaded unit that is already inactive stops without complaint.
func (s *SystemdUser) Stop(ctx context.Context, label string) error {
	args := []string{"stop", "--user", unitName(label)}
	out, code, err := runTool(ctx, s.Runner, s.Dir, s.Binary, args...)
	if err != nil {
		return err
	}
	switch code {
	case 0:
		return nil
	case exitUnitNotLoaded:
		return fmt.Errorf("%w: %w", ErrServiceNotLoaded, toolExited(s.Binary, args, code, out))
	default:
		return toolExited(s.Binary, args, code, out)
	}
}

// Start implements ServiceManager.
func (s *SystemdUser) Start(ctx context.Context, label string) error {
	return mustTool(ctx, s.Runner, s.Dir, s.Binary, "start", "--user", unitName(label))
}

// Definition implements ServiceManager: the label's installed unit file.
func (s *SystemdUser) Definition(label string) string {
	return filepath.Join(s.UnitDir, unitName(label))
}
