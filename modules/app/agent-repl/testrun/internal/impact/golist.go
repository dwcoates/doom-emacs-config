package impact

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os/exec"
	"strings"
)

// Pkg is the part of one `go list -json` record static analysis needs.
type Pkg struct {
	ImportPath string
	// ForTest is the package a test variant ("P [P.test]") recompiles.
	ForTest  string
	Dir      string
	Standard bool
	GoFiles  []string
	CgoFiles []string
	// ImportMap redirects an import path to the package that serves it in
	// this one (a test variant, a vendored copy).
	ImportMap  map[string]string
	EmbedFiles []string
	Module     *PkgModule
}

// PkgModule is a package's module.
type PkgModule struct {
	Path  string
	Dir   string
	GoMod string
}

// key names the package in a listing: its import path, which carries the
// variant suffix for a test variant.
func (p Pkg) key() string { return p.ImportPath }

// Lister lists every package a build of patterns in dir needs, with the
// tests' own dependencies when test is set, under the comma-separated build
// tags.
type Lister func(dir, tags string, test bool, patterns ...string) ([]Pkg, error)

const listFields = "ImportPath,ForTest,Dir,Standard,GoFiles,CgoFiles,ImportMap,EmbedFiles,Module"

// ExecLister is Lister over the real `go list -deps -json`.
func ExecLister(dir, tags string, test bool, patterns ...string) ([]Pkg, error) {
	args := []string{"list", "-deps", "-json=" + listFields}
	if test {
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
		return nil, fmt.Errorf("impact: go %s in %s: %w: %s", strings.Join(args, " "), dir, err, strings.TrimSpace(stderr.String()))
	}
	dec := json.NewDecoder(bytes.NewReader(out))
	var pkgs []Pkg
	for {
		var p Pkg
		err := dec.Decode(&p)
		if errors.Is(err, io.EOF) {
			return pkgs, nil
		}
		if err != nil {
			return nil, fmt.Errorf("impact: read go list output: %w", err)
		}
		pkgs = append(pkgs, p)
	}
}
