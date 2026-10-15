package harness

import (
	"bytes"
	"errors"
	"path/filepath"
	"strings"
	"testing"
)

func TestASuiteErrorFailsTheSuiteAndIsSaid(t *testing.T) {
	// Arrange
	var out bytes.Buffer
	s := newSuiteTB(&out)

	// Act
	s.Errorf("the store died: %s", "exit 1")

	// Assert
	if !s.Failed() || !strings.Contains(out.String(), "suite: FAIL: the store died: exit 1") {
		t.Fatalf("Failed=%v out=%q, want the failure recorded and said", s.Failed(), out.String())
	}
}

func TestASuiteFatalPanicsWithItsMessageAndFailsTheSuite(t *testing.T) {
	// Arrange
	s := newSuiteTB(&bytes.Buffer{})

	// Act
	fatal, skip := RecoverSuiteBuild(func() { s.Fatalf("no %s", "node") })

	// Assert
	if fatal == nil || fatal.Message != "no node" || skip != nil || !s.Failed() {
		t.Fatalf("fatal=%v skip=%v Failed=%v, want SuiteFatal{no node} and a failed suite", fatal, skip, s.Failed())
	}
}

func TestASuiteSkipPanicsWithItsReasonAndFailsNothing(t *testing.T) {
	// Arrange
	s := newSuiteTB(&bytes.Buffer{})

	// Act
	fatal, skip := RecoverSuiteBuild(func() { s.Skipf("no %s", "docker") })

	// Assert
	if skip == nil || skip.Reason != "no docker" || fatal != nil || s.Failed() {
		t.Fatalf("fatal=%v skip=%v Failed=%v, want SuiteSkip{no docker} and a clean suite", fatal, skip, s.Failed())
	}
}

func TestRecoverSuiteBuildAnswersNothingForABuildThatFinished(t *testing.T) {
	// Act
	fatal, skip := RecoverSuiteBuild(func() {})

	// Assert
	if fatal != nil || skip != nil {
		t.Fatalf("fatal=%v skip=%v, want neither", fatal, skip)
	}
}

func TestRecoverSuiteBuildReraisesAPanicThatIsNotTheSuites(t *testing.T) {
	// Arrange
	boom := errors.New("boom")
	var got any

	// Act
	func() {
		defer func() { got = recover() }()
		RecoverSuiteBuild(func() { panic(boom) })
	}()

	// Assert
	if got != boom {
		t.Fatalf("recovered %v, want the original panic re-raised", got)
	}
}

func TestSuiteCleanupsRunLastRegisteredFirst(t *testing.T) {
	// Arrange
	s := newSuiteTB(&bytes.Buffer{})
	var order []string
	s.Cleanup(func() { order = append(order, "store") })
	s.Cleanup(func() { order = append(order, "daemon") })

	// Act
	code := s.finish()

	// Assert
	if strings.Join(order, ",") != "daemon,store" || code != 0 {
		t.Fatalf("order=%v code=%d, want daemon,store and 0", order, code)
	}
}

func TestAPanickingSuiteCleanupFailsTheSuiteAndTheRestStillRun(t *testing.T) {
	// Arrange
	var out bytes.Buffer
	s := newSuiteTB(&out)
	ran := false
	s.Cleanup(func() { ran = true })
	s.Cleanup(func() { panic("teardown broke") })

	// Act
	code := s.finish()

	// Assert
	if !ran || code != 1 || !strings.Contains(out.String(), "a suite cleanup panicked: teardown broke") {
		t.Fatalf("ran=%v code=%d out=%q, want the later cleanup run, exit 1 and the panic said", ran, code, out.String())
	}
}

func TestAFailedSuiteFinishesRedWithEveryFailureListed(t *testing.T) {
	// Arrange
	var out bytes.Buffer
	s := newSuiteTB(&out)
	s.Errorf("one")
	s.Errorf("two")

	// Act
	code := s.finish()

	// Assert
	if code != 1 || !strings.Contains(out.String(), "failed 2 time(s):\n  one\n  two") {
		t.Fatalf("code=%d out=%q, want exit 1 listing both failures", code, out.String())
	}
}

func TestASuiteTempDirLivesUnderTheRunRoot(t *testing.T) {
	// Arrange
	s := newSuiteTB(&bytes.Buffer{})

	// Act
	dir := s.TempDir()

	// Assert
	if filepath.Dir(dir) != RunRoot() || !strings.HasPrefix(filepath.Base(dir), "suite-001-") {
		t.Fatalf("TempDir = %s, want suite-001-* directly under the run root %s", dir, RunRoot())
	}
}

func TestSetenvOnTheSuiteIsRefused(t *testing.T) {
	// Arrange
	s := newSuiteTB(&bytes.Buffer{})

	// Act
	fatal, _ := RecoverSuiteBuild(func() { s.Setenv("HOME", "/nowhere") })

	// Assert
	if fatal == nil || !strings.Contains(fatal.Message, "Setenv(HOME) on the suite") {
		t.Fatalf("fatal = %v, want Setenv refused", fatal)
	}
}
