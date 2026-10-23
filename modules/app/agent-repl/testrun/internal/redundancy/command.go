package redundancy

import (
	"agentrepl/testrun/internal/cores"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"agentrepl/testrun/internal/redundancy/proc"
)

// What every adapter's command shares: its common flags, its refusal to run
// at normal priority, its scratch directory and its two outputs.

// EnvBackground is the marker bin/background.sh exports; a measurement is
// the heaviest test load there is and never runs at normal priority.
const EnvBackground = cores.EnvBackground

// DefaultOverhead is the fixed weight each kept case adds to its runtime,
// so that among sets of equal runtime the smaller one wins.
const DefaultOverhead = 100 * time.Microsecond

// Common are the flags every adapter's command takes.
type Common struct {
	Out         string
	Universe    Universe
	Overhead    time.Duration
	Budget      Budget
	KeepScratch bool
	// Delete is a deletion set to verify instead of minimizing.
	Delete []string
	// Pins are cases kept whatever they cover.
	Pins []string
	// MaxLoad and QuietWait are the load gate's (proc.Gate).
	MaxLoad   float64
	QuietWait time.Duration
	// Procs caps every process the measurement runs (proc.Counter).
	Procs int
}

// Register adds the common flags to fs; the answer finishes them once fs
// has parsed.
func (c *Common) Register(fs *flag.FlagSet) func() error {
	universe := fs.String("universe", "kills-only", "kills-only or strict (killed mutants and reached blocks)")
	maxDrop := fs.Float64("max-coverage-drop", DefaultMaxDrop, "the most statement-coverage points any system may lose")
	baseline := fs.String("baseline", "", "the pinned coverage baseline (testrun/coverage-baseline.json); without it each system is held to the measured population's own coverage")
	deleteFile := fs.String("delete", "", "a file naming a deletion set to verify, one case per line, instead of minimizing")
	pins := fs.String("pin", "", "comma-separated cases kept whatever they cover")
	fs.StringVar(&c.Out, "out", "", "directory the measurement and report are written to (required)")
	fs.DurationVar(&c.Overhead, "overhead", DefaultOverhead, "fixed weight per kept case")
	fs.BoolVar(&c.KeepScratch, "keep-scratch", false, "keep the scratch directory")
	fs.Float64Var(&c.MaxLoad, "max-load", proc.DefaultMaxLoad, "the most one-minute load average a phase or a mutant starts under")
	fs.DurationVar(&c.QuietWait, "quiet-wait", proc.DefaultQuietWait, "how long to wait for the load to drop before failing")
	fs.IntVar(&c.Procs, "procs", proc.DefaultProcs, "GOMAXPROCS for every process the measurement runs and they spawn")
	return func() error {
		if c.Out == "" {
			return fmt.Errorf("-out is required")
		}
		if c.Procs < 1 {
			return fmt.Errorf("-procs must be at least 1, got %d", c.Procs)
		}
		u, err := ParseUniverse(*universe)
		if err != nil {
			return err
		}
		c.Universe = u
		if c.Budget, err = LoadBudget(*maxDrop, *baseline); err != nil {
			return err
		}
		if *deleteFile != "" {
			if c.Delete, err = ReadDeleteSet(*deleteFile); err != nil {
				return err
			}
		}
		c.Pins = SplitList(*pins)
		return nil
	}
}

// SplitList splits a comma-separated flag, dropping empty entries.
func SplitList(s string) []string {
	var out []string
	for _, p := range strings.Split(s, ",") {
		if p = strings.TrimSpace(p); p != "" {
			out = append(out, p)
		}
	}
	return out
}

// ReadDeleteSet reads a -delete file: one case per line; blank lines and
// lines starting with # are skipped.
func ReadDeleteSet(path string) ([]string, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read -delete %s: %w", path, err)
	}
	var out []string
	for _, l := range strings.Split(string(b), "\n") {
		if l = strings.TrimSpace(l); l != "" && !strings.HasPrefix(l, "#") {
			out = append(out, l)
		}
	}
	if len(out) == 0 {
		return nil, fmt.Errorf("-delete %s names no case", path)
	}
	return out, nil
}

// CommandLog is the module's one logging path (run.Log), as a command uses
// it.
type CommandLog interface {
	Infof(format string, args ...any)
	Errorf(format string, args ...any)
}

// RefusedAtNormalPriority answers an adapter command's exit status when it
// was not started through bin/background.sh (78, logged), else 0. Every
// command asks it first, before reading its arguments.
func RefusedAtNormalPriority(log CommandLog) int {
	if os.Getenv(EnvBackground) == "" {
		log.Errorf("redundancy: refused at normal priority; run it through bin/background.sh")
		return 78
	}
	return 0
}

// Run is an adapter command's body around its measurement: a scratch
// directory measure owns (kept with -keep-scratch), and measurement.json and
// report.md written under -out. subject names what is measured in the one
// error a failure logs.
func Run(log CommandLog, c Common, subject string, measure func(scratch string) (*Measurement, error)) int {
	if err := os.MkdirAll(c.Out, 0o755); err != nil {
		log.Errorf("redundancy: create -out %s: %v", c.Out, err)
		return 1
	}
	scratch, err := os.MkdirTemp("/tmp", "agent-repl-redundancy-")
	if err != nil {
		log.Errorf("redundancy: make the scratch directory: %v", err)
		return 1
	}
	if c.KeepScratch {
		log.Infof("redundancy: scratch kept at %s", scratch)
	} else {
		defer os.RemoveAll(scratch)
	}
	m, err := measure(scratch)
	if err != nil {
		log.Errorf("redundancy: measure %s: %v", subject, err)
		return 1
	}
	if err := WriteOutputs(c.Out, m, Options{Universe: c.Universe, Overhead: c.Overhead, Budget: c.Budget}); err != nil {
		log.Errorf("redundancy: %v", err)
		return 1
	}
	log.Infof("redundancy: wrote %s and %s", filepath.Join(c.Out, "measurement.json"), filepath.Join(c.Out, "report.md"))
	return 0
}

// WriteOutputs writes measurement.json and report.md under dir.
func WriteOutputs(dir string, m *Measurement, o Options) error {
	if err := WriteMeasurement(filepath.Join(dir, "measurement.json"), m); err != nil {
		return err
	}
	f, err := os.Create(filepath.Join(dir, "report.md"))
	if err != nil {
		return fmt.Errorf("redundancy: create the report: %w", err)
	}
	if err := Report(f, m, o); err != nil {
		f.Close()
		return err
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("redundancy: close the report: %w", err)
	}
	return nil
}

// RealPath answers dir absolute with every symlink resolved: go list and
// vitest's coverage answer real paths, and a directory may sit behind a
// symlink (/tmp is /private/tmp on macOS).
func RealPath(dir string) (string, error) {
	abs, err := filepath.Abs(dir)
	if err != nil {
		return "", fmt.Errorf("redundancy: resolve %s: %w", dir, err)
	}
	if abs, err = filepath.EvalSymlinks(abs); err != nil {
		return "", fmt.Errorf("redundancy: resolve %s: %w", dir, err)
	}
	return abs, nil
}

// FindRoot is the agent-repl module root, by its real path: the nearest
// directory at or above dir holding the roster.
func FindRoot(dir string) (string, error) {
	abs, err := RealPath(dir)
	if err != nil {
		return "", err
	}
	for d := abs; ; d = filepath.Dir(d) {
		if _, err := os.Stat(filepath.Join(d, "testrun", "roster", "roster.go")); err == nil {
			return d, nil
		}
		if d == filepath.Dir(d) {
			return "", fmt.Errorf("redundancy: no directory at or above %s holds testrun/roster", dir)
		}
	}
}
