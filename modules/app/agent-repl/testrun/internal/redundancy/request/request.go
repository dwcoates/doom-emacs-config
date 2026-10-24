// Package request turns a testrun.v1.RedundancyRequest (the daemon's
// agentrepl.v1.RunRedundancy) into `testrun redundancy`'s own command line.
//
// THE FLAGS STAY THE ONE PARSER. Each adapter already parses, defaults and
// validates its flags; the request is lowered onto exactly those flags, so a
// request and a hand-written command line can never mean different things.
// A field left unset passes no flag, and the tool's own default holds.
//
// Paths in the request are relative to the repository root (out_dir and
// measurement_file are absolute); each becomes absolute here. A deletion set
// and a hand-named suite list, which the flags take as files, are written
// under dir.
package request

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agentrepl/testrun/internal/redundancy/gomeasure"
)

// Argv answers the `testrun redundancy` command line req asks for: its
// adapter's name, then its flags. repo is the repository root; dir is where
// the files the flags name are written.
func Argv(req *testrunv1.RedundancyRequest, repo, dir string) ([]string, error) {
	switch c := req.GetCommand().(type) {
	case *testrunv1.RedundancyRequest_Go:
		return goArgv(c.Go, repo, dir)
	case *testrunv1.RedundancyRequest_Vitest:
		return vitestArgv(c.Vitest, repo, dir)
	case *testrunv1.RedundancyRequest_Report:
		return reportArgv(c.Report, repo)
	}
	return nil, errors.New("the request names no command (go, vitest or report)")
}

func goArgv(g *testrunv1.RedundancyGo, repo, dir string) ([]string, error) {
	argv, err := measurementFlags([]string{"go"}, g.GetMeasurement(), repo, dir)
	if err != nil {
		return nil, err
	}
	if g.GetModuleDir() != "" {
		module, err := under(repo, "module_dir", g.GetModuleDir())
		if err != nil {
			return nil, err
		}
		argv = append(argv, "-module", module)
	}
	switch s := g.GetSubject().(type) {
	case *testrunv1.RedundancyGo_Package:
		p := s.Package
		if p.GetPackage() == "" {
			return nil, errors.New("go: package names no package")
		}
		argv = append(argv, "-pkg", p.GetPackage())
		argv = list(argv, "-tests", p.GetTestFiles())
		if p.GetTags() != "" {
			argv = append(argv, "-tags", p.GetTags())
		}
		argv = list(argv, "-programs", p.GetPrograms())
	case *testrunv1.RedundancyGo_RosterSuites:
		if len(s.RosterSuites.GetNames()) == 0 {
			return nil, errors.New("go: roster_suites names no suite")
		}
		argv = append(argv, "-roster-suite", strings.Join(s.RosterSuites.GetNames(), ","))
	case *testrunv1.RedundancyGo_Suites:
		path, err := writeSuites(s.Suites.GetSuites(), dir)
		if err != nil {
			return nil, err
		}
		argv = append(argv, "-suites", path)
	default:
		return nil, errors.New("go: names no subject (package, roster_suites or suites)")
	}
	argv = list(argv, "-mutate", g.GetMutate())
	if g.Runs != nil {
		argv = append(argv, "-runs", strconv.Itoa(int(g.GetRuns())))
	}
	if g.GetPinCalls() != nil {
		argv = append(argv, "-pin-calls", strings.Join(g.GetPinCalls().GetCalls(), ","))
	}
	if g.GetEstimate() {
		argv = append(argv, "-estimate")
	}
	if g.HangBoundMs != nil {
		argv = append(argv, "-hang-bound", ms(g.GetHangBoundMs()))
	}
	return argv, nil
}

func vitestArgv(v *testrunv1.RedundancyVitest, repo, dir string) ([]string, error) {
	argv, err := measurementFlags([]string{"vitest"}, v.GetMeasurement(), repo, dir)
	if err != nil {
		return nil, err
	}
	pkg, err := under(repo, "package_dir", v.GetPackageDir())
	if err != nil {
		return nil, err
	}
	if len(v.GetMutate()) == 0 {
		return nil, errors.New("vitest: mutate names no source glob")
	}
	argv = append(argv, "-package", pkg, "-mutate", strings.Join(v.GetMutate(), ","))
	if v.GetSystem() != "" {
		argv = append(argv, "-system", v.GetSystem())
	}
	if v.GetConfig() != "" {
		argv = append(argv, "-config", v.GetConfig())
	}
	if v.Runs != nil {
		argv = append(argv, "-runs", strconv.Itoa(int(v.GetRuns())))
	}
	return argv, nil
}

func reportArgv(r *testrunv1.RedundancyReport, repo string) ([]string, error) {
	if !filepath.IsAbs(r.GetMeasurementFile()) {
		return nil, fmt.Errorf("report: measurement_file %q is not an absolute path", r.GetMeasurementFile())
	}
	argv := []string{"report", "-in", r.GetMeasurementFile()}
	argv = universe(argv, r.GetUniverse())
	if r.OverheadUs != nil {
		argv = append(argv, "-overhead", us(r.GetOverheadUs()))
	}
	if r.MaxCoverageDrop != nil {
		argv = append(argv, "-max-coverage-drop", float(r.GetMaxCoverageDrop()))
	}
	if r.GetBaseline() != "" {
		baseline, err := under(repo, "baseline", r.GetBaseline())
		if err != nil {
			return nil, err
		}
		argv = append(argv, "-baseline", baseline)
	}
	return argv, nil
}

// measurementFlags appends the common flags every measurement takes.
func measurementFlags(argv []string, m *testrunv1.RedundancyMeasurement, repo, dir string) ([]string, error) {
	if m == nil {
		return nil, fmt.Errorf("%s: names no measurement settings", argv[0])
	}
	if !filepath.IsAbs(m.GetOutDir()) {
		return nil, fmt.Errorf("%s: out_dir %q is not an absolute path", argv[0], m.GetOutDir())
	}
	argv = append(argv, "-out", m.GetOutDir())
	argv = universe(argv, m.GetUniverse())
	if m.MaxCoverageDrop != nil {
		argv = append(argv, "-max-coverage-drop", float(m.GetMaxCoverageDrop()))
	}
	if m.GetBaseline() != "" {
		baseline, err := under(repo, "baseline", m.GetBaseline())
		if err != nil {
			return nil, err
		}
		argv = append(argv, "-baseline", baseline)
	}
	if len(m.GetDeleteCases()) > 0 {
		path := filepath.Join(dir, "delete-cases.txt")
		if err := os.WriteFile(path, []byte(strings.Join(m.GetDeleteCases(), "\n")+"\n"), 0o644); err != nil {
			return nil, fmt.Errorf("write the deletion set: %w", err)
		}
		argv = append(argv, "-delete", path)
	}
	argv = list(argv, "-pin", m.GetPins())
	if m.OverheadUs != nil {
		argv = append(argv, "-overhead", us(m.GetOverheadUs()))
	}
	if m.GetKeepScratch() {
		argv = append(argv, "-keep-scratch")
	}
	if m.MaxLoad != nil {
		argv = append(argv, "-max-load", float(m.GetMaxLoad()))
	}
	if m.QuietWaitMs != nil {
		argv = append(argv, "-quiet-wait", ms(m.GetQuietWaitMs()))
	}
	if m.Procs != nil {
		argv = append(argv, "-procs", strconv.Itoa(int(m.GetProcs())))
	}
	return argv, nil
}

// writeSuites writes a hand-named suite list where -suites reads it.
func writeSuites(suites []*testrunv1.RedundancyGoSuite, dir string) (string, error) {
	if len(suites) == 0 {
		return "", errors.New("go: suites names no suite")
	}
	out := make([]gomeasure.Suite, 0, len(suites))
	for _, s := range suites {
		out = append(out, gomeasure.Suite{Package: s.GetPackage(), Tags: s.GetTags(), Tests: s.GetTestFiles(),
			Programs: s.GetPrograms(), Kept: s.GetKept(), Module: s.GetModule()})
	}
	data, err := json.Marshal(out)
	if err != nil {
		return "", fmt.Errorf("encode the suite list: %w", err)
	}
	path := filepath.Join(dir, "suites.json")
	if err := os.WriteFile(path, data, 0o644); err != nil {
		return "", fmt.Errorf("write the suite list: %w", err)
	}
	return path, nil
}

// under answers rel (relative to the repository root) as an absolute path,
// refusing one that is absolute or climbs out of the repository.
func under(repo, field, rel string) (string, error) {
	clean := filepath.Clean(filepath.FromSlash(rel))
	if rel == "" || filepath.IsAbs(rel) || clean == ".." || strings.HasPrefix(clean, ".."+string(filepath.Separator)) {
		return "", fmt.Errorf("%s %q is not a path inside the repository, relative to its root", field, rel)
	}
	return filepath.Join(repo, clean), nil
}

func universe(argv []string, u testrunv1.RedundancyUniverse) []string {
	switch u {
	case testrunv1.RedundancyUniverse_REDUNDANCY_UNIVERSE_KILLS_ONLY:
		return append(argv, "-universe", "kills-only")
	case testrunv1.RedundancyUniverse_REDUNDANCY_UNIVERSE_STRICT:
		return append(argv, "-universe", "strict")
	}
	return argv
}

func list(argv []string, flag string, items []string) []string {
	if len(items) == 0 {
		return argv
	}
	return append(argv, flag, strings.Join(items, ","))
}

func ms(n int64) string { return (time.Duration(n) * time.Millisecond).String() }

func us(n int64) string { return (time.Duration(n) * time.Microsecond).String() }

func float(f float64) string { return strconv.FormatFloat(f, 'g', -1, 64) }
