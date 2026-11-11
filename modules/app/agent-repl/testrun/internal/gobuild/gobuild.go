// Package gobuild builds every unit-test binary of one Go module with ONE
// `go test` command, and hands each package's binary, or its compiler output,
// to the units that run it.
//
// ONE BUILD COMMAND PER MODULE, NEVER ONE PER PACKAGE. Measured 2026-11-10
// (gate archive abc9582429d943eb-tests-1): the daemon's 167 per-package
// `go test -c` units cost 281 CPU-seconds of a 976s gate, about 1.7 each,
// though every compile was cached: each command reloaded the module's package
// graph, linked one binary and waited its own turn at the CPU governor. One
// command over all 166 packages costs about 88 CPU-seconds.
//
// `go test -c -o DIR` cannot build them, since it refuses two packages whose
// binaries share a name (agent-repld and agent-repld/cmd/agent-repld). So the
// build runs `go test -exec STASH -run '^$'` over every package: the go
// command compiles and links each test binary and, instead of running it,
// runs STASH (`testrun stash-test`) in the package's directory, which hard
// links the binary to BinPath. The go command's own scratch directory
// (GOTMPDIR) is inside the binary directory, so the link never crosses a
// filesystem.
//
// A PACKAGE THAT DOES NOT COMPILE FAILS ONLY ITS OWN UNITS. The build reads
// `go test -json`, writes each failed package's compiler output beside where
// its binary would be (FailurePath), and passes: the other packages' tests
// still run, and the failed package's test units (ExecTest, Check) fail with
// its compiler output.
package gobuild

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"syscall"

	"agentrepl/testrun/internal/outtail"
)

// Options is one module build.
type Options struct {
	// Module is the module's directory, absolute.
	Module string
	// BinDir is where every binary goes (BinPath).
	BinDir string
	// Tags are the build tags, comma-separated; "" is none.
	Tags string
	// Cover instruments every binary (-cover -coverpkg=./...).
	Cover bool
	// Packages are the packages to build, relative to Module ("." the root).
	Packages []string
	// Self is the testrun binary the build's -exec runs (`testrun stash-test`).
	Self string
}

// binName is every binary's file name under its package's directory in
// BinDir. A name starting with "_" is one the go command never matches as a
// package directory, so no package's directory there collides with it.
const binName = "_.test"

// BinPath is where package rel's test binary is built under binDir: its own
// directory, mirroring rel, so no two packages' binaries can collide.
func BinPath(binDir, rel string) string {
	return filepath.Join(binDir, filepath.FromSlash(rel), binName)
}

// FailurePath is the file holding the compiler output of a package whose
// binary at bin did not build.
func FailurePath(bin string) string { return bin + ".build-failed" }

// GoTmpDir is the go command's scratch directory for a build into binDir,
// inside it so the stash's hard link stays on one filesystem.
func GoTmpDir(binDir string) string { return filepath.Join(binDir, ".gotmp") }

// GoTestArgs is the `go test` argument list the build runs in Module.
func GoTestArgs(o Options) ([]string, error) {
	if len(o.Packages) == 0 {
		return nil, errors.New("gobuild: a build of no packages")
	}
	exec, err := joinQuoted([]string{o.Self, "stash-test", "-module", o.Module, "-bindir", o.BinDir})
	if err != nil {
		return nil, err
	}
	args := []string{"test", "-json", "-vet=off", "-count=1", "-exec", exec}
	if o.Tags != "" {
		args = append(args, "-tags", o.Tags)
	}
	if o.Cover {
		args = append(args, "-cover", "-coverpkg=./...")
	}
	args = append(args, "-run", "^$")
	for _, rel := range o.Packages {
		args = append(args, packageArg(rel))
	}
	return args, nil
}

func packageArg(rel string) string {
	if rel == "." {
		return "."
	}
	return "./" + rel
}

// joinQuoted spells words as one -exec value, which the go command splits on
// spaces, honoring single and double quotes (with no escapes inside them).
func joinQuoted(words []string) (string, error) {
	out := make([]string, len(words))
	for i, w := range words {
		switch {
		case w != "" && !strings.ContainsAny(w, " \t\n'\""):
			out[i] = w
		case !strings.Contains(w, "'"):
			out[i] = "'" + w + "'"
		case !strings.Contains(w, `"`):
			out[i] = `"` + w + `"`
		default:
			return "", fmt.Errorf("gobuild: %q holds both quote characters, which -exec cannot spell", w)
		}
	}
	return strings.Join(out, " "), nil
}

// GoTest runs `go test` with args in dir, adding env to the inherited
// environment, and answers its stdout and stderr. An exit status other than
// zero is no error: a package that fails to build exits the command 1.
type GoTest func(dir string, args, env []string) (stdout, stderr []byte, err error)

// Result is what one module build produced.
type Result struct {
	// Built are the packages whose binaries were linked.
	Built []string
	// Failed maps each package that did not compile to its compiler output.
	Failed map[string]string
}

// Build runs the module build and writes each failed package's output to
// its FailurePath. It fails only when the build's result is not one it can
// hand to the test units: the go command could not run, its output is not
// `go test -json`, or a package ended neither built nor failed.
func Build(o Options, goTest GoTest) (Result, error) {
	modulePath, err := ModulePath(o.Module)
	if err != nil {
		return Result{}, err
	}
	args, err := GoTestArgs(o)
	if err != nil {
		return Result{}, err
	}
	if err := os.MkdirAll(GoTmpDir(o.BinDir), 0o755); err != nil {
		return Result{}, fmt.Errorf("gobuild: create the build's scratch directory: %w", err)
	}
	stdout, stderr, err := goTest(o.Module, args, []string{"GOTMPDIR=" + GoTmpDir(o.BinDir)})
	if err != nil {
		return Result{}, fmt.Errorf("gobuild: run go test in %s: %w: %s", o.Module, err, strings.TrimSpace(string(stderr)))
	}
	failed, err := FailedBuilds(stdout)
	if err != nil {
		return Result{}, fmt.Errorf("gobuild: %w; go test's stderr: %s", err, strings.TrimSpace(string(stderr)))
	}
	res := Result{Failed: map[string]string{}}
	var lost []string
	for _, rel := range o.Packages {
		bin := BinPath(o.BinDir, rel)
		if out, ok := failed[ImportPath(modulePath, rel)]; ok {
			if err := os.MkdirAll(filepath.Dir(bin), 0o755); err != nil {
				return Result{}, fmt.Errorf("gobuild: create %s: %w", filepath.Dir(bin), err)
			}
			if err := os.WriteFile(FailurePath(bin), []byte(out), 0o644); err != nil {
				return Result{}, fmt.Errorf("gobuild: record %s's compiler output: %w", rel, err)
			}
			res.Failed[rel] = out
			continue
		}
		if _, err := os.Stat(bin); err != nil {
			lost = append(lost, rel)
			continue
		}
		res.Built = append(res.Built, rel)
	}
	if len(lost) > 0 {
		return Result{}, fmt.Errorf("gobuild: go test neither built nor failed to build %s; its stderr: %s; its output: %s",
			strings.Join(lost, ", "), strings.TrimSpace(string(stderr)), quotedTail(stdout))
	}
	return res, nil
}

// quotedTail is the end of a long output, for an error that quotes it.
func quotedTail(b []byte) string { return strings.TrimSpace(outtail.Of(b)) }

// event is the part of a `go test -json` record the build reads.
type event struct {
	Action      string
	Package     string
	ImportPath  string
	Output      string
	FailedBuild string
}

// FailedBuilds reads a `go test -json` stream for every package whose test
// binary did not build, mapped to the compiler output of the package that
// failed (the package itself, or a dependency it imports).
func FailedBuilds(stream []byte) (map[string]string, error) {
	output := map[string]*strings.Builder{}
	failed := map[string]string{}
	sc := bufio.NewScanner(bytes.NewReader(stream))
	sc.Buffer(make([]byte, 0, 64*1024), 64*1024*1024)
	for sc.Scan() {
		line := sc.Bytes()
		if len(bytes.TrimSpace(line)) == 0 {
			continue
		}
		var e event
		if err := json.Unmarshal(line, &e); err != nil {
			return nil, fmt.Errorf("read go test -json output %q: %w", line, err)
		}
		switch {
		case e.Action == "build-output":
			b := output[e.ImportPath]
			if b == nil {
				b = &strings.Builder{}
				output[e.ImportPath] = b
			}
			b.WriteString(e.Output)
		case e.Action == "fail" && e.FailedBuild != "":
			failed[e.Package] = e.FailedBuild
		}
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("read go test -json output: %w", err)
	}
	out := map[string]string{}
	for pkg, culprit := range failed {
		b := output[culprit]
		if b == nil {
			return nil, fmt.Errorf("package %s failed to build on %s, which printed no compiler output", pkg, culprit)
		}
		out[pkg] = b.String()
	}
	return out, nil
}

// ModulePath reads the module path from dir's go.mod.
func ModulePath(dir string) (string, error) {
	data, err := os.ReadFile(filepath.Join(dir, "go.mod"))
	if err != nil {
		return "", fmt.Errorf("gobuild: read the module's go.mod: %w", err)
	}
	for line := range strings.SplitSeq(string(data), "\n") {
		rest, ok := strings.CutPrefix(strings.TrimSpace(line), "module")
		if !ok || rest == "" || (rest[0] != ' ' && rest[0] != '\t' && rest[0] != '"') {
			continue
		}
		path := strings.Trim(strings.TrimSpace(rest), `"`)
		if path == "" {
			break
		}
		return path, nil
	}
	return "", fmt.Errorf("gobuild: %s has no module directive", filepath.Join(dir, "go.mod"))
}

// ImportPath is package rel's import path in the module at modulePath.
func ImportPath(modulePath, rel string) string {
	if rel == "." {
		return modulePath
	}
	return modulePath + "/" + filepath.ToSlash(rel)
}

// Stash is `testrun stash-test`: the build's -exec, run in a package's
// directory with the binary the go command linked for it. It hard links the
// binary to the package's BinPath.
func Stash(module, binDir, cwd, binary string) error {
	mod, err := filepath.EvalSymlinks(module)
	if err != nil {
		return fmt.Errorf("gobuild: resolve the module %s: %w", module, err)
	}
	dir, err := filepath.EvalSymlinks(cwd)
	if err != nil {
		return fmt.Errorf("gobuild: resolve the package directory %s: %w", cwd, err)
	}
	rel, err := filepath.Rel(mod, dir)
	if err != nil || rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		return fmt.Errorf("gobuild: the package directory %s is not in the module %s", cwd, module)
	}
	bin := BinPath(binDir, filepath.ToSlash(rel))
	if err := os.MkdirAll(filepath.Dir(bin), 0o755); err != nil {
		return fmt.Errorf("gobuild: create %s: %w", filepath.Dir(bin), err)
	}
	if err := os.Link(binary, bin); err != nil {
		return fmt.Errorf("gobuild: link %s's test binary to %s: %w", rel, bin, err)
	}
	return nil
}

// ErrDidNotCompile is a test unit whose package's binary did not build.
var ErrDidNotCompile = errors.New("the package's tests did not compile")

// Check answers nil when bin was built, and ErrDidNotCompile, after copying
// the package's compiler output to out, when it was not. A binary neither
// built nor failed is an error: the build that should have made it passed.
func Check(bin string, out io.Writer) error {
	data, err := os.ReadFile(FailurePath(bin))
	switch {
	case err == nil:
		if _, werr := out.Write(data); werr != nil {
			return fmt.Errorf("gobuild: copy %s's compiler output: %w", bin, werr)
		}
		return fmt.Errorf("%w: %s", ErrDidNotCompile, FailurePath(bin))
	case !errors.Is(err, fs.ErrNotExist):
		return fmt.Errorf("gobuild: read %s: %w", FailurePath(bin), err)
	}
	if _, err := os.Stat(bin); err != nil {
		return fmt.Errorf("gobuild: the test binary %s was neither built nor failed: %w", bin, err)
	}
	return nil
}

// ExecTest is `testrun exec-test`: a test unit's command. It replaces itself
// with the package's binary bin, given args, once Check says it built.
func ExecTest(bin string, args []string, out io.Writer, exec func(string, []string, []string) error) error {
	if err := Check(bin, out); err != nil {
		return err
	}
	err := exec(bin, append([]string{bin}, args...), os.Environ())
	return fmt.Errorf("gobuild: exec %s: %w", bin, err)
}

// SyscallExec is ExecTest's exec over the real kernel.
func SyscallExec(path string, argv, env []string) error { return syscall.Exec(path, argv, env) }

// ParseBuildArgs reads `testrun build-tests`' arguments:
// -module DIR -bindir DIR [-tags T] [-cover] -- PACKAGE...
func ParseBuildArgs(argv []string) (Options, error) {
	var o Options
	for i := 0; i < len(argv); i++ {
		a := argv[i]
		value := func() (string, error) {
			if i+1 >= len(argv) {
				return "", fmt.Errorf("gobuild: %s needs a value", a)
			}
			i++
			return argv[i], nil
		}
		var err error
		switch a {
		case "-module":
			o.Module, err = value()
		case "-bindir":
			o.BinDir, err = value()
		case "-tags":
			o.Tags, err = value()
		case "-cover":
			o.Cover = true
		case "--":
			o.Packages = append([]string(nil), argv[i+1:]...)
			i = len(argv)
		default:
			err = fmt.Errorf("gobuild: unknown argument %q", a)
		}
		if err != nil {
			return Options{}, err
		}
	}
	switch {
	case o.Module == "" || o.BinDir == "":
		return Options{}, errors.New("gobuild: build-tests needs -module and -bindir")
	case !filepath.IsAbs(o.Module) || !filepath.IsAbs(o.BinDir):
		return Options{}, errors.New("gobuild: build-tests' -module and -bindir must be absolute")
	case len(o.Packages) == 0:
		return Options{}, errors.New("gobuild: build-tests names no package after --")
	}
	return o, nil
}

// Args is ParseBuildArgs' inverse: the arguments after `testrun build-tests`.
func (o Options) Args() []string {
	args := []string{"-module", o.Module, "-bindir", o.BinDir}
	if o.Tags != "" {
		args = append(args, "-tags", o.Tags)
	}
	if o.Cover {
		args = append(args, "-cover")
	}
	return append(append(args, "--"), o.Packages...)
}
