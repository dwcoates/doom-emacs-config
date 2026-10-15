package suites

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"agentrepl/testrun/roster"
)

// goModFile reads a go.mod's module path and the module paths it requires.
func goModFile(dir string) (string, []string, error) {
	path := filepath.Join(dir, "go.mod")
	f, err := os.Open(path)
	if err != nil {
		return "", nil, fmt.Errorf("suites: read %s: %w", path, err)
	}
	defer f.Close()
	var module string
	var requires []string
	inRequire := false
	sc := bufio.NewScanner(f)
	for sc.Scan() {
		line, _, _ := strings.Cut(sc.Text(), "//")
		fields := strings.Fields(line)
		switch {
		case len(fields) == 0:
		case inRequire && fields[0] == ")":
			inRequire = false
		case inRequire:
			requires = append(requires, fields[0])
		case fields[0] == "module" && len(fields) == 2:
			module = fields[1]
		case fields[0] == "require" && len(fields) == 2 && fields[1] == "(":
			inRequire = true
		case fields[0] == "require" && len(fields) >= 3:
			requires = append(requires, fields[1])
		}
	}
	if err := sc.Err(); err != nil {
		return "", nil, fmt.Errorf("suites: read %s: %w", path, err)
	}
	if module == "" {
		return "", nil, fmt.Errorf("suites: %s names no module", path)
	}
	return module, requires, nil
}

// SystemModules maps the module path of every Go coverage system
// (roster.CoverageSystems) to its suite's name.
func SystemModules(l Layout) (map[string]string, error) {
	out := map[string]string{}
	for _, name := range roster.CoverageSystems {
		s, ok := roster.Lookup(name)
		if !ok {
			return nil, fmt.Errorf("suites: coverage system %q is no roster suite", name)
		}
		if s.Kind != roster.GoModule {
			continue
		}
		module, _, err := goModFile(l.resolve(s.Path))
		if err != nil {
			return nil, err
		}
		out[module] = name
	}
	return out, nil
}

// coverPkg is the -coverpkg a coverage build of a program in the module dir
// instruments: the module's own packages and those of every coverage system
// it requires, so the code a spawned system runs of another system's module
// counts toward that system too.
func coverPkg(l Layout, dir string) (string, error) {
	module, requires, err := goModFile(dir)
	if err != nil {
		return "", err
	}
	systems, err := SystemModules(l)
	if err != nil {
		return "", err
	}
	patterns := []string{"./..."}
	var deps []string
	for _, req := range requires {
		if _, ok := systems[req]; ok && req != module {
			deps = append(deps, req+"/...")
		}
	}
	sort.Strings(deps)
	return strings.Join(append(patterns, deps...), ","), nil
}

// SpawnedRoot is where a coverage run's spawned systems write their counters,
// one directory per Go package that spawns them, under which the harnesses
// make one directory per binary (harness.CoverageDir).
func (l Layout) SpawnedRoot() string { return filepath.Join(l.Work, "cover", "spawned") }

// SpawnedCovDir is the spawned-counter directory of the suite's i-th package.
func SpawnedCovDir(l Layout, suite string, i int) string {
	return filepath.Join(l.SpawnedRoot(), fmt.Sprintf("%s-%03d", suite, i))
}
