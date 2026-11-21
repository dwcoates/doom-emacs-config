// Package govet vets the packages of one Go module pass with ONE `go vet`
// command, and hands each package's verdict to the unit that checks it.
//
// VET ONLY WHAT CHANGED. A module-wide `go vet ./...` re-analyzes every
// package whenever any input of the module changed: 13 CPU-seconds over the
// daemon's 188 packages with every compile cached, and 15-22 in the merge
// gates of 2026-11-20, where reuse skipped half of the module's test packages.
// So each package's vet verdict belongs to that package's own reuse unit,
// whose inputs (its test binary's `go list -deps -test` graph) are everything
// vet reads to analyze it: a package whose unit is reused is not vetted
// again, and one vet command (`testrun vet-module`) vets the rest.
//
// A PACKAGE WITH FINDINGS FAILS ONLY ITS OWN UNIT. The command reads
// `go vet -json`, writes each package's verdict beside the others
// (ResultPath), and passes; each package's check unit (`testrun vet-check`,
// Check) then passes, or fails with its own findings. A package vet could
// not analyze (a dependency did not compile) fails its check with vet's whole
// output.
package govet

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"

	"agentrepl/testrun/internal/command"
	"agentrepl/testrun/internal/gobuild"
)

// Flags is the analyzer subset `go test` vets with (`go help test`): a
// compiled test binary is never vetted, so every package is vetted with
// exactly these, and nothing `go test` checked is lost.
var Flags = []string{
	"-atomic", "-bool", "-buildtags", "-directive", "-errorsas",
	"-ifaceassert", "-nilfunc", "-printf", "-stringintconv", "-tests",
}

// Options is one module pass's vet.
type Options struct {
	// Module is the module's directory, absolute.
	Module string
	// ResultDir is where every package's verdict goes (ResultPath).
	ResultDir string
	// Tags are the build tags, comma-separated; "" is none.
	Tags string
	// Packages are the packages to vet, relative to Module ("." the root).
	Packages []string
}

// resultName is every verdict's file name under its package's directory in
// ResultDir; "_" starts a name the go command never matches as a package.
const resultName = "_.vet"

// ResultPath is where package rel's verdict is written under resultDir: an
// empty file for a package vet passed, and FailurePath for one it did not.
func ResultPath(resultDir, rel string) string {
	return filepath.Join(resultDir, filepath.FromSlash(rel), resultName)
}

// FailurePath is the file holding why the package whose verdict is at
// result did not pass vet.
func FailurePath(result string) string { return result + ".failed" }

// GoVetArgs is the `go vet` argument list the vet runs in Module.
func GoVetArgs(o Options) ([]string, error) {
	if len(o.Packages) == 0 {
		return nil, errors.New("govet: a vet of no packages")
	}
	args := append([]string{"vet", "-json"}, Flags...)
	if o.Tags != "" {
		args = append(args, "-tags", o.Tags)
	}
	for _, rel := range o.Packages {
		if rel == "." {
			args = append(args, ".")
			continue
		}
		args = append(args, "./"+rel)
	}
	return args, nil
}

// GoVet runs `go vet` with args in dir and answers its stderr, which holds
// all of `go vet -json`'s output, and whether it exited zero. An exit status
// other than zero is no error: a package vet could not analyze exits it 1.
type GoVet func(dir string, args []string) (stderr []byte, exitedZero bool, err error)

// Exec is GoVet over the real go command. A package vet could not analyze
// exits go vet 1; Vet judges which from its output.
func Exec(dir string, args []string) ([]byte, bool, error) {
	cmd := exec.Command("go", args...)
	cmd.Dir = dir
	_, stderr, exitedZero, err := command.Verdict(cmd)
	return stderr, exitedZero, err
}

// Result is what one module vet found.
type Result struct {
	// Passed are the packages vet analyzed and found nothing in.
	Passed []string
	// Failed maps each other package to why: its findings, or vet's output
	// when vet could not analyze it.
	Failed map[string]string
}

// Vet runs the module vet and writes each package's verdict. It fails only
// when the verdicts cannot be trusted: the go command could not run, its
// output is not `go vet -json`'s, or it exited non-zero though every package
// it was given passed.
func Vet(o Options, goVet GoVet) (Result, error) {
	modulePath, err := gobuild.ModulePath(o.Module)
	if err != nil {
		return Result{}, err
	}
	args, err := GoVetArgs(o)
	if err != nil {
		return Result{}, err
	}
	stderr, exitedZero, err := goVet(o.Module, args)
	if err != nil {
		return Result{}, fmt.Errorf("govet: run go vet in %s: %w", o.Module, err)
	}
	sections, err := Sections(stderr)
	if err != nil {
		return Result{}, err
	}
	res := Result{Failed: map[string]string{}}
	for _, rel := range o.Packages {
		why, err := verdict(sections, gobuild.ImportPath(modulePath, rel), stderr)
		if err != nil {
			return Result{}, fmt.Errorf("govet: %s: %w", rel, err)
		}
		if why == "" {
			res.Passed = append(res.Passed, rel)
		} else {
			res.Failed[rel] = why
		}
	}
	if !exitedZero && len(res.Failed) == 0 {
		return Result{}, fmt.Errorf("govet: go vet exited non-zero, but every package it was given passed; its output: %s", strings.TrimSpace(string(stderr)))
	}
	for _, rel := range o.Packages {
		if err := write(o.ResultDir, rel, res.Failed[rel]); err != nil {
			return Result{}, err
		}
	}
	return res, nil
}

// write records package rel's verdict: why it failed, or "" for a pass.
func write(resultDir, rel, why string) error {
	result := ResultPath(resultDir, rel)
	if err := os.MkdirAll(filepath.Dir(result), 0o755); err != nil {
		return fmt.Errorf("govet: create %s: %w", filepath.Dir(result), err)
	}
	if why != "" {
		if err := os.WriteFile(FailurePath(result), []byte(why), 0o644); err != nil {
			return fmt.Errorf("govet: record %s's findings: %w", rel, err)
		}
	}
	if err := os.WriteFile(result, nil, 0o644); err != nil {
		return fmt.Errorf("govet: record %s's verdict: %w", rel, err)
	}
	return nil
}

// Section is one `# <import path>` block of `go vet -json`'s output: the
// JSON report of a package vet analyzed, or the text of an error.
type Section struct {
	// Package is the header's import path, without a variant suffix
	// ("x/a" for "# x/a [x/a.test]").
	Package string
	Body    string
}

// Sections splits `go vet -json`'s output into its blocks, in order. A line
// "# [x/a]" names the test variant of the block it is in, and is no block of
// its own. Output before the first header is refused: it is no
// `go vet -json` output.
func Sections(out []byte) ([]Section, error) {
	var sections []Section
	var body strings.Builder
	flush := func() {
		if len(sections) > 0 {
			sections[len(sections)-1].Body = body.String()
		}
		body.Reset()
	}
	sc := bufio.NewScanner(bytes.NewReader(out))
	sc.Buffer(make([]byte, 0, 64*1024), 64*1024*1024)
	for sc.Scan() {
		line := sc.Text()
		if rest, ok := strings.CutPrefix(line, "# "); ok && !strings.HasPrefix(rest, "[") {
			flush()
			pkg, _, _ := strings.Cut(rest, " ")
			sections = append(sections, Section{Package: pkg})
			continue
		}
		if strings.HasPrefix(line, "# [") {
			continue
		}
		if len(sections) == 0 {
			if strings.TrimSpace(line) == "" {
				continue
			}
			return nil, fmt.Errorf("govet: go vet printed %q before any package header", line)
		}
		body.WriteString(line)
		body.WriteByte('\n')
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("govet: read go vet's output: %w", err)
	}
	flush()
	return sections, nil
}

// diagnostic is one finding of `go vet -json`.
type diagnostic struct {
	Posn    string `json:"posn"`
	Message string `json:"message"`
}

// verdict is why the package at importPath did not pass vet, or "" when vet
// analyzed it (and its external test package) and found nothing. A package
// with no block of its own or of its external test package was not analyzed,
// since a dependency failed: its verdict is vet's whole output. A directory
// holding only external test files (package x_test) has only the second.
func verdict(sections []Section, importPath string, all []byte) (string, error) {
	var why []string
	seen := false
	for _, s := range sections {
		if s.Package != importPath && s.Package != importPath+"_test" {
			continue
		}
		seen = true
		findings, ok, err := findings(s.Body)
		if err != nil {
			return "", err
		}
		if !ok {
			why = append(why, "# "+s.Package+"\n"+s.Body)
			continue
		}
		why = append(why, findings...)
	}
	if !seen {
		return "vet did not analyze the package; its whole output:\n" + string(all), nil
	}
	return strings.Join(why, ""), nil
}

// findings reads one block's body: its findings, one line each, and true
// when the body is a JSON report; false when it is an error's text.
func findings(body string) ([]string, bool, error) {
	var report map[string]map[string]json.RawMessage
	if err := json.Unmarshal([]byte(body), &report); err != nil {
		return nil, false, nil
	}
	var out []string
	for _, pkg := range sortedKeys(report) {
		for _, analyzer := range sortedKeys(report[pkg]) {
			raw := report[pkg][analyzer]
			var diags []diagnostic
			if err := json.Unmarshal(raw, &diags); err == nil {
				for _, d := range diags {
					out = append(out, fmt.Sprintf("%s: %s (%s)\n", d.Posn, d.Message, analyzer))
				}
				continue
			}
			var failed struct {
				Error string `json:"error"`
			}
			if err := json.Unmarshal(raw, &failed); err != nil || failed.Error == "" {
				return nil, false, fmt.Errorf("govet: %s's %s report %s is neither findings nor an error", pkg, analyzer, raw)
			}
			out = append(out, fmt.Sprintf("%s: %s failed: %s\n", pkg, analyzer, failed.Error))
		}
	}
	return out, true, nil
}

func sortedKeys[V any](m map[string]V) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}

// ErrFindings is a package vet did not pass.
var ErrFindings = errors.New("the package did not pass vet")

// Check answers nil when the package whose verdict is at result passed vet,
// and ErrFindings, after copying why to out, when it did not. A verdict
// neither written is an error: the vet that should have written it passed.
func Check(result string, out io.Writer) error {
	data, err := os.ReadFile(FailurePath(result))
	switch {
	case err == nil:
		if _, werr := out.Write(data); werr != nil {
			return fmt.Errorf("govet: copy %s's findings: %w", result, werr)
		}
		return fmt.Errorf("%w: %s", ErrFindings, FailurePath(result))
	case !errors.Is(err, fs.ErrNotExist):
		return fmt.Errorf("govet: read %s: %w", FailurePath(result), err)
	}
	if _, err := os.Stat(result); err != nil {
		return fmt.Errorf("govet: the package's verdict %s was never written: %w", result, err)
	}
	return nil
}

// ParseArgs reads `testrun vet-module`'s arguments:
// -module DIR -resultdir DIR [-tags T] -- PACKAGE...
func ParseArgs(argv []string) (Options, error) {
	var o Options
	for i := 0; i < len(argv); i++ {
		a := argv[i]
		value := func() (string, error) {
			if i+1 >= len(argv) {
				return "", fmt.Errorf("govet: %s needs a value", a)
			}
			i++
			return argv[i], nil
		}
		var err error
		switch a {
		case "-module":
			o.Module, err = value()
		case "-resultdir":
			o.ResultDir, err = value()
		case "-tags":
			o.Tags, err = value()
		case "--":
			o.Packages = append([]string(nil), argv[i+1:]...)
			i = len(argv)
		default:
			err = fmt.Errorf("govet: unknown argument %q", a)
		}
		if err != nil {
			return Options{}, err
		}
	}
	switch {
	case o.Module == "" || o.ResultDir == "":
		return Options{}, errors.New("govet: vet-module needs -module and -resultdir")
	case !filepath.IsAbs(o.Module) || !filepath.IsAbs(o.ResultDir):
		return Options{}, errors.New("govet: vet-module's -module and -resultdir must be absolute")
	case len(o.Packages) == 0:
		return Options{}, errors.New("govet: vet-module names no package after --")
	}
	return o, nil
}

// Args is ParseArgs' inverse: the arguments after `testrun vet-module`.
func (o Options) Args() []string {
	args := []string{"-module", o.Module, "-resultdir", o.ResultDir}
	if o.Tags != "" {
		args = append(args, "-tags", o.Tags)
	}
	return append(append(args, "--"), o.Packages...)
}
