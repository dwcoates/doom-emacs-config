package gomeasure

import (
	"context"
	"strings"
	"testing"
	"time"
)

// stepClock advances by whatever it is asked to wait, at once.
type stepClock struct{ now time.Time }

func (c *stepClock) Now() time.Time { return c.now }

func (c *stepClock) After(d time.Duration) <-chan time.Time {
	c.now = c.now.Add(d)
	ch := make(chan time.Time, 1)
	ch <- c.now
	return ch
}

func TestMeasureWaitsForTheLoadToDropBeforeItMeasures(t *testing.T) {
	// Arrange: the host is loaded at the first two reads.
	cfg, dir := fakeConfig(t)
	cfg.QuietWait = time.Hour
	cfg.Clock = &stepClock{now: time.Unix(0, 0)}
	f := &fakeGo{dir: dir, reaches: map[string]bool{"TestA": true}, loads: []string{"{ 80.0 1 1 }", "{ 40.0 1 1 }", "{ 3.0 1 1 }"}}
	pass := func(p string) Result { return Result{Lines: passLines(withParents(leavesOf(p))...)} }
	f.mutant = map[string]func(string) Result{"1": pass, "2": pass}
	log, out := quietLog()

	// Act
	_, err := Measure(context.Background(), cfg, f.exec, log)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(out.String(), "load 80.0 is over 32; waiting before discovery") || !strings.Contains(out.String(), "load 3.0, resuming discovery") {
		t.Fatalf("log = %q", out.String())
	}
	for i, call := range f.calls {
		if strings.HasPrefix(call, "cover-") {
			if !strings.HasPrefix(f.calls[i-1], "sysctl") {
				t.Fatalf("the first test run did not follow a load read: %v", f.calls[:i+1])
			}
			break
		}
	}
}

func TestMeasureFailsRatherThanMeasureUnderLoad(t *testing.T) {
	// Arrange: the load never drops.
	cfg, dir := fakeConfig(t)
	cfg.QuietWait = time.Minute
	cfg.Clock = &stepClock{now: time.Unix(0, 0)}
	f := &fakeGo{dir: dir, loads: []string{"{ 99.0 1 1 }"}}
	log, _ := quietLog()

	// Act
	_, err := Measure(context.Background(), cfg, f.exec, log)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "load stayed over 32 for 1m0s before discovery; nothing was measured under it") {
		t.Fatalf("err = %v", err)
	}
	for _, call := range f.calls {
		if strings.HasPrefix(call, "cover-") {
			t.Fatalf("a case ran under load: %s", call)
		}
	}
}

func TestMeasureRefusesNoLoadGate(t *testing.T) {
	// Arrange
	cfg, dir := fakeConfig(t)
	cfg.MaxLoad = 0
	log, _ := quietLog()

	// Act
	_, err := Measure(context.Background(), cfg, (&fakeGo{dir: dir}).exec, log)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the load gate needs a positive max load") {
		t.Fatalf("err = %v", err)
	}
}
