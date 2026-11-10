package gobuild

import (
	"bytes"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func write(t *testing.T, path, body string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
}

func TestBinPathGivesEveryPackageItsOwnDirectory(t *testing.T) {
	tests := []struct {
		rel, want string
	}{
		{".", "/bin/_.test"},
		{"a", "/bin/a/_.test"},
		{"a/b", "/bin/a/b/_.test"},
		// Two packages whose binaries `go test -c -o DIR` would both name
		// agent-repld.test.
		{"cmd/agent-repld", "/bin/cmd/agent-repld/_.test"},
	}
	for _, tt := range tests {
		t.Run(tt.rel, func(t *testing.T) {
			// Act
			got := BinPath("/bin", tt.rel)

			// Assert
			if got != tt.want {
				t.Fatalf("BinPath(/bin, %s) = %s, want %s", tt.rel, got, tt.want)
			}
		})
	}
}

func TestGoTestArgs(t *testing.T) {
	tests := []struct {
		name string
		o    Options
		want []string
	}{
		{
			name: "a plain build",
			o:    Options{Module: "/m", BinDir: "/b", Self: "/testrun", Packages: []string{".", "a"}},
			want: []string{"test", "-json", "-vet=off", "-count=1", "-exec", "/testrun stash-test -module /m -bindir /b", "-run", "^$", ".", "./a"},
		},
		{
			name: "tags and coverage",
			o:    Options{Module: "/m", BinDir: "/b", Self: "/testrun", Tags: "integration", Cover: true, Packages: []string{"a"}},
			want: []string{"test", "-json", "-vet=off", "-count=1", "-exec", "/testrun stash-test -module /m -bindir /b", "-tags", "integration", "-cover", "-coverpkg=./...", "-run", "^$", "./a"},
		},
		{
			name: "a path with a space is quoted for -exec",
			o:    Options{Module: "/my mod", BinDir: "/b", Self: "/testrun", Packages: []string{"a"}},
			want: []string{"test", "-json", "-vet=off", "-count=1", "-exec", "/testrun stash-test -module '/my mod' -bindir /b", "-run", "^$", "./a"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := GoTestArgs(tt.o)

			// Assert
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("GoTestArgs = %q, %v; want %q", got, err, tt.want)
			}
		})
	}
}

func TestGoTestArgsRefuses(t *testing.T) {
	tests := []struct {
		name string
		o    Options
		want string
	}{
		{"no packages", Options{Module: "/m", BinDir: "/b", Self: "/t"}, "a build of no packages"},
		{"a path -exec cannot spell", Options{Module: `/a'b"c`, BinDir: "/b", Self: "/t", Packages: []string{"a"}}, "both quote characters"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := GoTestArgs(tt.o)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("GoTestArgs = %v, want %q", err, tt.want)
			}
		})
	}
}

const failedStream = `{"ImportPath":"x/b [x/b.test]","Action":"build-output","Output":"# x/b [x/b.test]\n"}
{"ImportPath":"x/b [x/b.test]","Action":"build-output","Output":"b/b_test.go:3:27: undefined: y\n"}
{"ImportPath":"x/b [x/b.test]","Action":"build-fail"}
{"ImportPath":"x/dep","Action":"build-output","Output":"dep/dep.go:1: bad\n"}
{"Action":"start","Package":"x/a"}
{"Action":"output","Package":"x/a","Output":"ok  \tx/a\t0.005s\n"}
{"Action":"pass","Package":"x/a"}
{"Action":"fail","Package":"x/b","FailedBuild":"x/b [x/b.test]"}
{"Action":"fail","Package":"x/c","FailedBuild":"x/dep"}
`

func TestFailedBuildsNamesEachPackagesCompilerOutput(t *testing.T) {
	// Act
	got, err := FailedBuilds([]byte(failedStream))

	// Assert
	want := map[string]string{
		"x/b": "# x/b [x/b.test]\nb/b_test.go:3:27: undefined: y\n",
		// A package that fails on a dependency gets the dependency's output.
		"x/c": "dep/dep.go:1: bad\n",
	}
	if err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("FailedBuilds = %q, %v; want %q", got, err, want)
	}
}

func TestFailedBuildsRefuses(t *testing.T) {
	tests := []struct {
		name, stream, want string
	}{
		{"output that is not go test -json", "FAIL x/b [build failed]\n", "read go test -json output"},
		{"a failed build that printed nothing", `{"Action":"fail","Package":"x/b","FailedBuild":"x/b [x/b.test]"}` + "\n", "printed no compiler output"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := FailedBuilds([]byte(tt.stream))

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("FailedBuilds = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestModulePath(t *testing.T) {
	tests := []struct {
		name, gomod, want, wantErr string
	}{
		{"a plain directive", "module x/y\n\ngo 1.24\n", "x/y", ""},
		{"a quoted directive", "// c\nmodule \"x/y\"\n", "x/y", ""},
		{"a line that only starts with module", "modulex y\n", "", "no module directive"},
		{"no directive", "go 1.24\n", "", "no module directive"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			write(t, filepath.Join(dir, "go.mod"), tt.gomod)

			// Act
			got, err := ModulePath(dir)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("ModulePath = %q, %v; want %q", got, err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("ModulePath = %q, %v; want %q", got, err, tt.want)
			}
		})
	}
}

func TestModulePathOfADirectoryWithoutAGoMod(t *testing.T) {
	// Act
	_, err := ModulePath(t.TempDir())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "read the module's go.mod") {
		t.Fatalf("ModulePath = %v, want the missing go.mod named", err)
	}
}

// fakeBuild is a GoTest that links a stand-in binary for each of built and
// prints stream, as the go command and its -exec would.
func fakeBuild(t *testing.T, binDir string, built []string, stream string) (GoTest, *[]string) {
	var gotEnv []string
	return func(dir string, args, env []string) ([]byte, []byte, error) {
		gotEnv = env
		for _, rel := range built {
			write(t, BinPath(binDir, rel), "binary")
		}
		return []byte(stream), []byte("stderr text"), nil
	}, &gotEnv
}

func buildFixture(t *testing.T) Options {
	t.Helper()
	module := t.TempDir()
	write(t, filepath.Join(module, "go.mod"), "module x\n")
	return Options{Module: module, BinDir: filepath.Join(t.TempDir(), "bin"), Self: "/testrun", Packages: []string{"a", "b", "c"}}
}

func TestBuildRecordsEachFailedPackageAndPasses(t *testing.T) {
	// Arrange
	o := buildFixture(t)
	goTest, env := fakeBuild(t, o.BinDir, []string{"a"}, failedStream)

	// Act
	res, err := Build(o, goTest)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(res.Built, []string{"a"}) || len(res.Failed) != 2 {
		t.Fatalf("Build = %+v, want a built and b, c failed", res)
	}
	data, err := os.ReadFile(FailurePath(BinPath(o.BinDir, "b")))
	if err != nil || !strings.Contains(string(data), "undefined: y") {
		t.Fatalf("b's failure record = %q, %v", data, err)
	}
	if want := []string{"GOTMPDIR=" + GoTmpDir(o.BinDir)}; !reflect.DeepEqual(*env, want) {
		t.Fatalf("go test ran with env %v, want %v", *env, want)
	}
	if info, err := os.Stat(GoTmpDir(o.BinDir)); err != nil || !info.IsDir() {
		t.Fatalf("the go command's scratch directory was not made: %v", err)
	}
}

func TestBuildFails(t *testing.T) {
	tests := []struct {
		name   string
		built  []string
		stream string
		runErr error
		want   string
	}{
		{"a package neither built nor failed", []string{"a"}, `{"Action":"pass","Package":"x/a"}` + "\n", nil, "neither built nor failed to build b, c"},
		{"go test could not run", nil, "", errors.New("exec: no go"), "run go test"},
		{"output that is not go test -json", nil, "garbage\n", nil, "read go test -json output"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			o := buildFixture(t)
			goTest, _ := fakeBuild(t, o.BinDir, tt.built, tt.stream)
			if tt.runErr != nil {
				goTest = func(string, []string, []string) ([]byte, []byte, error) { return nil, []byte("boom"), tt.runErr }
			}

			// Act
			_, err := Build(o, goTest)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("Build = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestStashLinksTheBinaryToItsPackagesPath(t *testing.T) {
	// Arrange
	module := t.TempDir()
	pkg := filepath.Join(module, "a", "b")
	binDir := filepath.Join(t.TempDir(), "bin")
	linked := filepath.Join(t.TempDir(), "b.test")
	write(t, filepath.Join(pkg, "b.go"), "package b\n")
	write(t, linked, "binary")

	// Act
	err := Stash(module, binDir, pkg, linked)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	a, errA := os.Stat(linked)
	b, errB := os.Stat(BinPath(binDir, "a/b"))
	if errA != nil || errB != nil || !os.SameFile(a, b) {
		t.Fatalf("the stash is not a hard link of the binary: %v %v", errA, errB)
	}
}

func TestStashFails(t *testing.T) {
	tests := []struct {
		name string
		cwd  func(module string) string
		pre  func(t *testing.T, binDir string)
		want string
	}{
		{"a directory outside the module", func(string) string { return os.TempDir() }, nil, "is not in the module"},
		{"a binary already stashed", func(m string) string { return m }, func(t *testing.T, binDir string) { write(t, BinPath(binDir, "."), "old") }, "link .'s test binary"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			module := t.TempDir()
			binDir := filepath.Join(t.TempDir(), "bin")
			linked := filepath.Join(t.TempDir(), "x.test")
			write(t, linked, "binary")
			if tt.pre != nil {
				tt.pre(t, binDir)
			}

			// Act
			err := Stash(module, binDir, tt.cwd(module), linked)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("Stash = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestCheck(t *testing.T) {
	tests := []struct {
		name    string
		bin     bool
		failure string
		wantErr string
		wantOut string
	}{
		{"a built binary", true, "", "", ""},
		{"a package that did not compile", false, "a.go:1: bad\n", "did not compile", "a.go:1: bad\n"},
		{"a binary neither built nor failed", false, "", "neither built nor failed", ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin := filepath.Join(t.TempDir(), "_.test")
			if tt.bin {
				write(t, bin, "binary")
			}
			if tt.failure != "" {
				write(t, FailurePath(bin), tt.failure)
			}
			var out bytes.Buffer

			// Act
			err := Check(bin, &out)

			// Assert
			if tt.wantErr == "" && err != nil || tt.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tt.wantErr)) {
				t.Fatalf("Check = %v, want %q", err, tt.wantErr)
			}
			if out.String() != tt.wantOut {
				t.Fatalf("Check printed %q, want %q", out.String(), tt.wantOut)
			}
		})
	}
}

func TestExecTestReplacesItselfWithTheBuiltBinary(t *testing.T) {
	// Arrange
	bin := filepath.Join(t.TempDir(), "_.test")
	write(t, bin, "binary")
	var gotPath string
	var gotArgv []string
	exec := func(path string, argv, _ []string) error {
		gotPath, gotArgv = path, argv
		return errors.New("exec refused")
	}

	// Act
	err := ExecTest(bin, []string{"-test.v"}, &bytes.Buffer{}, exec)

	// Assert
	if gotPath != bin || !reflect.DeepEqual(gotArgv, []string{bin, "-test.v"}) {
		t.Fatalf("exec(%s, %v), want the binary with its arguments", gotPath, gotArgv)
	}
	if err == nil || !strings.Contains(err.Error(), "exec refused") {
		t.Fatalf("ExecTest = %v, want the failed exec surfaced", err)
	}
}

func TestExecTestOfAPackageThatDidNotCompileNeverExecs(t *testing.T) {
	// Arrange
	bin := filepath.Join(t.TempDir(), "_.test")
	write(t, FailurePath(bin), "a.go:1: bad\n")
	exec := func(string, []string, []string) error {
		t.Fatal("exec ran for a package that did not compile")
		return nil
	}
	var out bytes.Buffer

	// Act
	err := ExecTest(bin, nil, &out, exec)

	// Assert
	if !errors.Is(err, ErrDidNotCompile) || out.String() != "a.go:1: bad\n" {
		t.Fatalf("ExecTest = %v printing %q, want the compiler output and ErrDidNotCompile", err, out.String())
	}
}

func TestParseBuildArgsRoundTripsArgs(t *testing.T) {
	// Arrange
	o := Options{Module: "/m", BinDir: "/b", Tags: "integration", Cover: true, Packages: []string{".", "a"}}

	// Act
	got, err := ParseBuildArgs(o.Args())

	// Assert
	if err != nil || !reflect.DeepEqual(got, o) {
		t.Fatalf("ParseBuildArgs(Args()) = %+v, %v; want %+v", got, err, o)
	}
}

func TestParseBuildArgsRefuses(t *testing.T) {
	tests := []struct {
		name string
		argv []string
		want string
	}{
		{"a flag without its value", []string{"-module"}, "-module needs a value"},
		{"an unknown argument", []string{"-x"}, `unknown argument "-x"`},
		{"no module", []string{"-bindir", "/b", "--", "a"}, "needs -module and -bindir"},
		{"a relative directory", []string{"-module", "m", "-bindir", "/b", "--", "a"}, "must be absolute"},
		{"no packages", []string{"-module", "/m", "-bindir", "/b", "--"}, "names no package"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := ParseBuildArgs(tt.argv)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("ParseBuildArgs(%q) = %v, want %q", tt.argv, err, tt.want)
			}
		})
	}
}

// stashEnv makes this test binary act as `testrun stash-test` (TestMain), so
// the real go command's -exec can run it.
const stashEnv = "GOBUILD_TEST_ACT_AS_STASH"

func TestMain(m *testing.M) {
	if os.Getenv(stashEnv) == "1" {
		// os.Args: stash-test -module DIR -bindir DIR BINARY ARGS...
		cwd, err := os.Getwd()
		if err == nil {
			err = Stash(os.Args[3], os.Args[5], cwd, os.Args[6])
		}
		if err != nil {
			os.Stderr.WriteString(err.Error() + "\n")
			os.Exit(1)
		}
		os.Exit(0)
	}
	os.Exit(m.Run())
}

// The real go command, so the -json events and the -exec contract Build
// reads are the toolchain's own: one module of a package that builds, one
// that does not, and two whose binaries `go test -c -o DIR` would collide on.
func TestBuildOverTheRealGoCommand(t *testing.T) {
	// Arrange
	self, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	module := t.TempDir()
	write(t, filepath.Join(module, "go.mod"), "module x\n\ngo 1.24\n")
	write(t, filepath.Join(module, "x.go"), "package x\n")
	write(t, filepath.Join(module, "x_test.go"), "package x\nimport \"testing\"\nfunc TestX(t *testing.T) {}\n")
	write(t, filepath.Join(module, "cmd", "x", "x.go"), "package main\nfunc main() {}\n")
	write(t, filepath.Join(module, "cmd", "x", "x_test.go"), "package main\nimport \"testing\"\nfunc TestCmd(t *testing.T) {}\n")
	write(t, filepath.Join(module, "bad", "bad.go"), "package bad\n")
	write(t, filepath.Join(module, "bad", "bad_test.go"), "package bad\nimport \"testing\"\nfunc TestBad(t *testing.T) { undefinedName }\n")
	o := Options{Module: module, BinDir: filepath.Join(t.TempDir(), "bin"), Self: self, Packages: []string{".", "bad", "cmd/x"}}
	goTest := func(dir string, args, env []string) ([]byte, []byte, error) {
		c := exec.Command("go", args...)
		c.Dir = dir
		c.Env = append(append(os.Environ(), stashEnv+"=1", "GOWORK=off"), env...)
		var stdout, stderr bytes.Buffer
		c.Stdout, c.Stderr = &stdout, &stderr
		err := c.Run()
		var exit *exec.ExitError
		if errors.As(err, &exit) {
			err = nil
		}
		return stdout.Bytes(), stderr.Bytes(), err
	}

	// Act
	res, err := Build(o, goTest)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(res.Built, []string{".", "cmd/x"}) {
		t.Fatalf("built %v, want . and cmd/x", res.Built)
	}
	if !strings.Contains(res.Failed["bad"], "undefinedName") {
		t.Fatalf("bad's compiler output = %q, want the undefined name", res.Failed["bad"])
	}
	out, err := exec.Command(BinPath(o.BinDir, "cmd/x"), "-test.v").CombinedOutput()
	if err != nil || !strings.Contains(string(out), "--- PASS: TestCmd") {
		t.Fatalf("cmd/x's stashed binary ran %v: %s", err, out)
	}
}
