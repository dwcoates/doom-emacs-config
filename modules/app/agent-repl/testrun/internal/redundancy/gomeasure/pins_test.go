package gomeasure

import (
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func writeFiles(t *testing.T, files map[string]string) string {
	t.Helper()
	dir := t.TempDir()
	for name, src := range files {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return dir
}

// pinFixture holds one test per pin verdict, written with the shapes the
// daemon's tests use: dlog's FindRecord and Records, and the integration
// harness's record and operation awaits.
const pinFixture = `package p

import "testing"

const (
	LevelWarn  = "warn"
	LevelInfo  = "info"
)

type Record struct{ Level, Operation, Message string; Context map[string]any }
type RecordQuery struct{ Level, Operation, Message string }
type logger struct{}
type daemon struct{}

func FindRecord(r []Record, q RecordQuery) (Record, bool) { return Record{}, true }
func (logger) Records() []Record { return nil }
func (daemon) AwaitLogRecord(path, what string, pred func(Record) bool) Record { return Record{} }
func (daemon) AwaitRunLogOperation(op string) Record { return Record{} }

var l logger
var d daemon

func warned(t *testing.T) { FindRecord(l.Records(), RecordQuery{Level: LevelWarn, Operation: "op"}) }

func atLevel(level string) { FindRecord(l.Records(), RecordQuery{Level: level}) }

func TestMain(m *testing.M) {}

func TestWarnQuery(t *testing.T) { FindRecord(l.Records(), RecordQuery{Level: LevelWarn}) }

func TestErrorPredicate(t *testing.T) {
	d.AwaitLogRecord("p", "x", func(r Record) bool { return r.Level == "error" && r.Operation == "op" })
}

func TestMessageQuery(t *testing.T) { FindRecord(l.Records(), RecordQuery{Operation: "op", Message: "m"}) }

func TestContextOfTheAwaited(t *testing.T) {
	rec := d.AwaitLogRecord("p", "x", func(r Record) bool { return r.Operation == "op" })
	if rec.Context["k"] != "v" {
		t.Fatalf("context %v", rec.Context)
	}
}

func TestThroughAHelper(t *testing.T) { warned(t) }

func TestALevelOnlyKnownAtRunTime(t *testing.T) { atLevel("whatever") }

func TestAPredicateNamedElsewhere(t *testing.T) {
	pred := func(r Record) bool { return true }
	d.AwaitLogRecord("p", "x", pred)
}

func TestRecordsInspectedByHand(t *testing.T) {
	for _, r := range l.Records() {
		_ = r
	}
}

func TestABarrierAwait(t *testing.T) { d.AwaitRunLogOperation("op") }

func TestAnExistenceCheck(t *testing.T) {
	if _, ok := FindRecord(l.Records(), RecordQuery{Level: LevelInfo, Operation: "op"}); !ok {
		t.Fatal("no record")
	}
}

func TestAnOperationPredicate(t *testing.T) {
	d.AwaitLogRecord("p", "x", func(r Record) bool { return r.Operation == "op" })
}

func TestAnErrorTypedPredicateIsNoLevel(t *testing.T) {
	_ = func() error { return nil }
	d.AwaitRunLogOperation("op")
}

func TestPlain(t *testing.T) {}

func Testable(t *testing.T) {}

func (logger) TestMethod(t *testing.T) {}
`

var calls = []string{"FindRecord", "Records", "AwaitLogRecord", "AwaitRunLogOperation"}

func TestTestFuncsListsOnlyTests(t *testing.T) {
	// Arrange
	dir := writeFiles(t, map[string]string{"p_test.go": pinFixture})

	// Act
	got, err := TestFuncs(dir, []string{"p_test.go"})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 13 || got[0] != "TestWarnQuery" || got[len(got)-1] != "TestPlain" {
		t.Fatalf("TestFuncs = %v", got)
	}
}

func TestTestFuncsRefusesAFileWithNoTests(t *testing.T) {
	// Arrange
	dir := writeFiles(t, map[string]string{"p_test.go": "package p\n"})

	// Act
	_, err := TestFuncs(dir, []string{"p_test.go"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "declare no tests") {
		t.Fatalf("err = %v, want no tests named", err)
	}
}

func TestLogPinsClassifiesEachAssertion(t *testing.T) {
	// Arrange
	dir := writeFiles(t, map[string]string{"p_test.go": pinFixture})
	tests, err := TestFuncs(dir, []string{"p_test.go"})
	if err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := LogPins(dir, tests, calls)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	want := map[string]string{
		"TestWarnQuery":                "(a) a WARN or ERROR record: TestWarnQuery -> FindRecord",
		"TestErrorPredicate":           "(a) a WARN or ERROR record: TestErrorPredicate -> AwaitLogRecord",
		"TestMessageQuery":             "(b) asserts a record's fields: TestMessageQuery -> FindRecord",
		"TestContextOfTheAwaited":      "(b) asserts a record's fields: TestContextOfTheAwaited -> AwaitLogRecord",
		"TestThroughAHelper":           "(a) a WARN or ERROR record: TestThroughAHelper -> warned -> FindRecord",
		"TestALevelOnlyKnownAtRunTime": "(uncertain): TestALevelOnlyKnownAtRunTime -> atLevel -> FindRecord",
		"TestAPredicateNamedElsewhere": "(uncertain): TestAPredicateNamedElsewhere -> AwaitLogRecord",
		"TestRecordsInspectedByHand":   "(uncertain): TestRecordsInspectedByHand -> Records",
	}
	if !reflect.DeepEqual(got, want) {
		for k, v := range got {
			if want[k] != v {
				t.Errorf("%s: %q, want %q", k, v, want[k])
			}
		}
		for k := range want {
			if _, ok := got[k]; !ok {
				t.Errorf("%s not pinned", k)
			}
		}
	}
}

func TestLogPinsRefusesATestNoFileDeclares(t *testing.T) {
	// Arrange
	dir := writeFiles(t, map[string]string{"p_test.go": pinFixture})

	// Act
	_, err := LogPins(dir, []string{"TestGone"}, calls)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "TestGone is declared in no test file") {
		t.Fatalf("err = %v, want the missing test named", err)
	}
}

func TestLogPinsRefusesAPackageWithNoTestFiles(t *testing.T) {
	// Arrange
	dir := writeFiles(t, map[string]string{"p.go": "package p\n"})

	// Act
	_, err := LogPins(dir, []string{"TestA"}, calls)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "has no test files") {
		t.Fatalf("err = %v, want no test files named", err)
	}
}
