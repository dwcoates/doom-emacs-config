package reuse

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strings"
	"sync"
)

// GoPackage is the part of one `go list -json` record the input set needs.
type GoPackage struct {
	ImportPath string
	Dir        string
	Standard   bool
	Module     *GoModule
	// CgoFiles are the package's files that import "C". Their C sources
	// include headers by relative path from any subdirectory of the package
	// (tree-sitter's "tree_sitter/parser.h"), which no listing names.
	CgoFiles []string
	// Deps are every package a build of this one imports, transitively, by
	// import path: for a test main ("P.test"), every package its binary
	// links, the test variants ("Q [P.test]") included.
	Deps []string
	// EmbedFiles, TestEmbedFiles and XTestEmbedFiles are the files the
	// package's //go:embed patterns match, relative to Dir; a pattern may
	// reach into a subdirectory no flat placement holds.
	EmbedFiles, TestEmbedFiles, XTestEmbedFiles []string
}

// GoModule is a package's module, as `go list -json` reports it.
type GoModule struct {
	Path  string
	Dir   string
	GoMod string
}

// GoLister lists every package a build of patterns in dir needs, with the
// tests' own dependencies when tests is set, under the comma-separated build
// tags.
type GoLister func(dir, tags string, tests bool, patterns ...string) ([]GoPackage, error)

// ExecGoList is GoLister over the real `go list -deps -json`.
func ExecGoList(dir, tags string, tests bool, patterns ...string) ([]GoPackage, error) {
	args := []string{"list", "-deps", "-json=ImportPath,Dir,Standard,Module,CgoFiles,Deps,EmbedFiles,TestEmbedFiles,XTestEmbedFiles"}
	if tests {
		args = append(args, "-test")
	}
	if tags != "" {
		args = append(args, "-tags", tags)
	}
	args = append(args, patterns...)
	cmd := exec.Command("go", args...)
	cmd.Dir = dir
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("reuse: go %s in %s: %w: %s", strings.Join(args, " "), dir, err, strings.TrimSpace(stderr.String()))
	}
	return DecodeGoList(out)
}

// DecodeGoList reads the stream of JSON records `go list -json` prints.
func DecodeGoList(out []byte) ([]GoPackage, error) {
	dec := json.NewDecoder(bytes.NewReader(out))
	var pkgs []GoPackage
	for {
		var p GoPackage
		err := dec.Decode(&p)
		if errors.Is(err, io.EOF) {
			return pkgs, nil
		}
		if err != nil {
			return nil, fmt.Errorf("reuse: read go list output: %w", err)
		}
		pkgs = append(pkgs, p)
	}
}

// AddGoPackages places what a Go build of pkgs reads from the checkout at
// repo: each package directory inside it (its listing, every file directly in
// it, and its testdata tree, which `go test` gives the package by convention;
// a cgo package's WHOLE tree, since its C includes headers from any
// subdirectory by relative path) and each such module's go.mod and go.sum. Standard-library packages and
// packages outside the checkout (the module cache) are covered by the
// toolchain version and go.sum, not by the file set.
func AddGoPackages(set *InputSet, repo string, pkgs []GoPackage) error {
	for _, p := range pkgs {
		if p.Standard || p.Dir == "" {
			continue
		}
		rel, inside, err := relInside(repo, p.Dir)
		if err != nil {
			return err
		}
		if !inside {
			continue
		}
		if len(p.CgoFiles) > 0 {
			if err := set.AddTree(rel); err != nil {
				return fmt.Errorf("reuse: cgo package %s: %w", p.ImportPath, err)
			}
		} else if err := addPackageDir(set, repo, rel); err != nil {
			return fmt.Errorf("reuse: package %s: %w", p.ImportPath, err)
		}
		if err := addEmbedded(set, rel, p); err != nil {
			return fmt.Errorf("reuse: package %s: %w", p.ImportPath, err)
		}
		if p.Module == nil || p.Module.GoMod == "" {
			return fmt.Errorf("reuse: package %s in the checkout reports no go.mod", p.ImportPath)
		}
		modRel, modInside, err := relInside(repo, p.Module.GoMod)
		if err != nil {
			return err
		}
		if !modInside {
			return fmt.Errorf("reuse: package %s sits in the checkout but its go.mod %s does not", p.ImportPath, p.Module.GoMod)
		}
		if err := set.AddFile(modRel); err != nil {
			return err
		}
		sum := filepath.ToSlash(filepath.Join(filepath.Dir(filepath.FromSlash(modRel)), "go.sum"))
		if _, err := os.Lstat(filepath.Join(repo, filepath.FromSlash(sum))); err == nil {
			if err := set.AddFile(sum); err != nil {
				return err
			}
		} else if !errors.Is(err, fs.ErrNotExist) {
			return fmt.Errorf("reuse: stat %s: %w", sum, err)
		}
	}
	return nil
}

// addEmbedded places every file the package embeds, by its path under the
// package's directory rel.
func addEmbedded(set *InputSet, rel string, p GoPackage) error {
	for _, files := range [][]string{p.EmbedFiles, p.TestEmbedFiles, p.XTestEmbedFiles} {
		for _, f := range files {
			if err := set.AddFile(path.Join(rel, filepath.ToSlash(f))); err != nil {
				return err
			}
		}
	}
	return nil
}

// addPackageDir places one package directory: flat, plus its testdata tree.
func addPackageDir(set *InputSet, repo, rel string) error {
	if err := set.AddFlatDir(rel); err != nil {
		return err
	}
	td := rel + "/testdata"
	info, err := os.Lstat(filepath.Join(repo, filepath.FromSlash(td)))
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		return fmt.Errorf("stat %s: %w", td, err)
	}
	if info.IsDir() {
		return set.AddTree(td)
	}
	return nil
}

// relInside answers abs relative to repo, slash-separated, and whether it is
// inside repo at all. Both are resolved through symlinks first, so a checkout
// reached through /tmp still places /private/tmp paths.
func relInside(repo, abs string) (string, bool, error) {
	real, err := filepath.EvalSymlinks(abs)
	if err != nil {
		return "", false, fmt.Errorf("reuse: resolve %s: %w", abs, err)
	}
	rel, err := filepath.Rel(repo, real)
	if err != nil {
		return "", false, fmt.Errorf("reuse: place %s under %s: %w", real, repo, err)
	}
	if rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		return "", false, nil
	}
	return filepath.ToSlash(rel), true, nil
}

// AddPatternWalk places the listing of every directory a `./...` pattern
// rooted at the module directory dir (repository-relative) walks, the way the
// go command walks it: everything beneath, except testdata, names starting
// with "." or "_", and nested modules (a directory with its own go.mod). The
// listings are what decide which packages the pattern matches, so a new
// package anywhere in the module moves the key.
func AddPatternWalk(set *InputSet, repo, dir string) error {
	root := filepath.Join(repo, filepath.FromSlash(dir))
	return filepath.WalkDir(root, func(abs string, d fs.DirEntry, err error) error {
		if err != nil {
			return fmt.Errorf("reuse: walk %s: %w", abs, err)
		}
		if !d.IsDir() {
			return nil
		}
		if abs != root {
			name := d.Name()
			if name == "testdata" || strings.HasPrefix(name, ".") || strings.HasPrefix(name, "_") {
				return filepath.SkipDir
			}
			if _, err := os.Lstat(filepath.Join(abs, "go.mod")); err == nil {
				// The go command reads a nested module's directory, its go.mod
				// and its files' build constraints, to rule it out of the
				// pattern, and stops there.
				rel, err := filepath.Rel(repo, abs)
				if err != nil {
					return fmt.Errorf("reuse: place %s under %s: %w", abs, repo, err)
				}
				if err := set.AddFlatDir(filepath.ToSlash(rel)); err != nil {
					return err
				}
				return filepath.SkipDir
			} else if !errors.Is(err, fs.ErrNotExist) {
				return fmt.Errorf("reuse: stat %s: %w", filepath.Join(abs, "go.mod"), err)
			}
		}
		rel, err := filepath.Rel(repo, abs)
		if err != nil {
			return fmt.Errorf("reuse: place %s under %s: %w", abs, repo, err)
		}
		return set.AddDir(filepath.ToSlash(rel))
	})
}

// MemoGoList is l answering each distinct listing once per run: every keyed
// suite lists the runner, and a suite's packages (Derive) read the
// same `go list -deps -test ./...` its input set did, so the run's planning
// spends each listing's CPU, outside any unit, once. Callers must not change
// the packages they are answered.
func MemoGoList(l GoLister) GoLister {
	type answer struct {
		once sync.Once
		pkgs []GoPackage
		err  error
	}
	var mu sync.Mutex
	answers := map[string]*answer{}
	return func(dir, tags string, tests bool, patterns ...string) ([]GoPackage, error) {
		key := fmt.Sprintf("%q %q %v %q", dir, tags, tests, patterns)
		mu.Lock()
		a := answers[key]
		if a == nil {
			a = &answer{}
			answers[key] = a
		}
		mu.Unlock()
		a.once.Do(func() { a.pkgs, a.err = l(dir, tags, tests, patterns...) })
		return a.pkgs, a.err
	}
}
