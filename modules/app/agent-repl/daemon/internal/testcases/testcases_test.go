package testcases

import (
	"slices"
	"testing"

	testrunv1 "agentrepl/proto/testrun/v1"

	"google.golang.org/protobuf/proto"
)

const pkg = "modules/app/agent-repl/daemon/internal/merge"

func TestOfSplitsARunIntoItsCases(t *testing.T) {
	tests := []struct {
		name string
		sels []*testrunv1.TestSelection
		want []*testrunv1.TestSelection
	}{
		{name: "a whole Go package is one case", sels: []*testrunv1.TestSelection{GoPackage(pkg, nil)}, want: []*testrunv1.TestSelection{GoPackage(pkg, nil)}},
		{name: "each named Go test is its own case", sels: []*testrunv1.TestSelection{GoPackage(pkg, []string{"TestA", "TestB"})},
			want: []*testrunv1.TestSelection{GoPackage(pkg, []string{"TestA"}), GoPackage(pkg, []string{"TestB"})}},
		{name: "a Go subtest path is its top-level test", sels: []*testrunv1.TestSelection{GoPackage(pkg, []string{"TestA/a_case", "TestA/b_case"})},
			want: []*testrunv1.TestSelection{GoPackage(pkg, []string{"TestA"})}},
		{name: "each vitest name is its own case", sels: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_VitestFile{VitestFile: &testrunv1.TestSelectionVitestFile{
			File: "w/a.test.ts", Tests: &testrunv1.TestSelectionVitestFile_Named{Named: &testrunv1.TestSelectionVitestNamedTests{FullNames: []string{"a x", "a y"}}}}}}},
			want: []*testrunv1.TestSelection{VitestFile("w/a.test.ts", "a x"), VitestFile("w/a.test.ts", "a y")}},
		{name: "a whole vitest file is one case", sels: []*testrunv1.TestSelection{VitestFile("w/a.test.ts", "")}, want: []*testrunv1.TestSelection{VitestFile("w/a.test.ts", "")}},
		{name: "each ERT test is its own case", sels: []*testrunv1.TestSelection{ErtFile("test-x.el", []string{"t-a", "t-b"})},
			want: []*testrunv1.TestSelection{ErtFile("test-x.el", []string{"t-a"}), ErtFile("test-x.el", []string{"t-b"})}},
		{name: "a whole ERT file is one case", sels: []*testrunv1.TestSelection{ErtFile("test-x.el", nil)}, want: []*testrunv1.TestSelection{ErtFile("test-x.el", nil)}},
		{name: "each harness group is its own case", sels: []*testrunv1.TestSelection{Harness("bin/t.sh", []string{"g1", "g2"})},
			want: []*testrunv1.TestSelection{Harness("bin/t.sh", []string{"g1"}), Harness("bin/t.sh", []string{"g2"})}},
		{name: "a whole harness is one case", sels: []*testrunv1.TestSelection{Harness("bin/t.sh", nil)}, want: []*testrunv1.TestSelection{Harness("bin/t.sh", nil)}},
		{name: "a suite is one case", sels: []*testrunv1.TestSelection{Suite("daemon")}, want: []*testrunv1.TestSelection{Suite("daemon")}},
		{name: "the whole roster is no case", sels: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_EverySuite{EverySuite: &testrunv1.TestSelectionEverySuite{}}}}},
		{name: "a case named twice is one case", sels: []*testrunv1.TestSelection{GoPackage(pkg, []string{"TestA"}), GoPackage(pkg, []string{"TestA/sub"})},
			want: []*testrunv1.TestSelection{GoPackage(pkg, []string{"TestA"})}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Of(tt.sels)

			// Assert
			if !slices.EqualFunc(got, tt.want, func(a, b *testrunv1.TestSelection) bool { return proto.Equal(a, b) }) {
				t.Fatalf("Of = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestKeyNamesOneCase(t *testing.T) {
	tests := []struct {
		name      string
		a, b      *testrunv1.TestSelection
		wantEqual bool
	}{
		{name: "the same case built twice", a: GoPackage(pkg, []string{"TestA"}), b: GoPackage(pkg, []string{"TestA"}), wantEqual: true},
		{name: "two tests of a package", a: GoPackage(pkg, []string{"TestA"}), b: GoPackage(pkg, []string{"TestB"})},
		{name: "a test and its whole package", a: GoPackage(pkg, []string{"TestA"}), b: GoPackage(pkg, nil)},
		{name: "one path as two kinds", a: VitestFile("x", ""), b: ErtFile("x", nil)},
		{name: "a suite and a harness of its name", a: Suite("x"), b: Harness("x", nil)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			equal := Key(tt.a) == Key(tt.b)

			// Assert
			if equal != tt.wantEqual {
				t.Fatalf("Key(%v) == Key(%v) is %v, want %v", tt.a, tt.b, equal, tt.wantEqual)
			}
		})
	}
}

func TestGoPackageReadsEachNameWholeOrAsASubtestPath(t *testing.T) {
	// Act
	got := GoPackage(pkg, []string{"TestA/x/y", "TestB", "TestA/z", "TestC/q", "TestC"})

	// Assert
	want := &testrunv1.TestSelection{Target: &testrunv1.TestSelection_GoPackage{GoPackage: &testrunv1.TestSelectionGoPackage{PackageDir: pkg,
		Tests: &testrunv1.TestSelectionGoPackage_Named{Named: &testrunv1.TestSelectionGoNamedTests{Tests: []*testrunv1.TestSelectionGoTest{
			{Name: "TestA", Scope: &testrunv1.TestSelectionGoTest_Subtests{Subtests: &testrunv1.TestSelectionGoSubtests{Paths: []*testrunv1.TestSelectionGoSubtestPath{{Segments: []string{"x", "y"}}, {Segments: []string{"z"}}}}}},
			{Name: "TestB", Scope: &testrunv1.TestSelectionGoTest_Whole{Whole: &testrunv1.TestSelectionGoWholeTest{}}},
			{Name: "TestC", Scope: &testrunv1.TestSelectionGoTest_Whole{Whole: &testrunv1.TestSelectionGoWholeTest{}}},
		}}}}}}
	if !proto.Equal(got, want) {
		t.Fatalf("GoPackage = %v, want %v", got, want)
	}
}

func TestFlagsSpellsACaseAsTheCommandLineSelectsIt(t *testing.T) {
	tests := []struct {
		name string
		sel  *testrunv1.TestSelection
		want []string
	}{
		{name: "a whole Go package", sel: GoPackage(pkg, nil), want: []string{"-go '/w/" + pkg + "'"}},
		{name: "Go tests and a subtest path", sel: GoPackage(pkg, []string{"TestA", "TestB/x y"}), want: []string{"-go '/w/" + pkg + "=TestA,TestB/x y'"}},
		{name: "one vitest name", sel: VitestFile("webapp/a.test.ts", "a x"), want: []string{"-vitest '/w/webapp/a.test.ts=a x'"}},
		{name: "several vitest names spell one flag each", sel: &testrunv1.TestSelection{Target: &testrunv1.TestSelection_VitestFile{VitestFile: &testrunv1.TestSelectionVitestFile{
			File: "webapp/a.test.ts", Tests: &testrunv1.TestSelectionVitestFile_Named{Named: &testrunv1.TestSelectionVitestNamedTests{FullNames: []string{"a x", "a y"}}}}}},
			want: []string{"-vitest '/w/webapp/a.test.ts=a x'", "-vitest '/w/webapp/a.test.ts=a y'"}},
		{name: "a bare ERT file stays bare", sel: ErtFile("test-x.el", []string{"t-a", "t-b"}), want: []string{"-ert 'test-x.el=t-a,t-b'"}},
		{name: "an ERT file by its path", sel: ErtFile("lisp/test-x.el", nil), want: []string{"-ert '/w/lisp/test-x.el'"}},
		{name: "a harness's groups", sel: Harness("bin/t.sh", []string{"g1"}), want: []string{"-harness '/w/bin/t.sh=g1'"}},
		{name: "a suite", sel: Suite("daemon"), want: []string{"-suite 'daemon'"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Flags("/w", tt.sel)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("Flags = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestACaseWithNoTargetIsADefect(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("Key of a selection with no target returned; want a panic")
		}
	}()

	// Act
	Key(&testrunv1.TestSelection{})
}
