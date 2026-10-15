package main

import (
	"context"
	"sync/atomic"
	"testing"
)

func TestStopEndsTheLoopAndWaitsForItToReturn(t *testing.T) {
	// Arrange: a loop that records, as it returns, that it saw its end.
	var returned atomic.Bool
	l := newNoticeLifetime(func(ctx context.Context) {
		<-ctx.Done()
		returned.Store(true)
	})
	l.Start()

	// Act.
	l.Stop()

	// Assert.
	if !returned.Load() {
		t.Fatal("Stop returned before the loop did")
	}
}

func TestStopRunsALoopNeverStarted(t *testing.T) {
	// Arrange.
	var runs atomic.Int32
	l := newNoticeLifetime(func(ctx context.Context) {
		runs.Add(1)
		<-ctx.Done()
	})

	// Act.
	l.Stop()

	// Assert.
	if got := runs.Load(); got != 1 {
		t.Fatalf("runs = %d, want the never-started loop run once to deliver what was pending", got)
	}
}

func TestASecondStartRunsNoSecondLoop(t *testing.T) {
	// Arrange.
	var runs atomic.Int32
	l := newNoticeLifetime(func(ctx context.Context) {
		runs.Add(1)
		<-ctx.Done()
	})
	l.Start()

	// Act.
	l.Start()
	l.Stop()

	// Assert.
	if got := runs.Load(); got != 1 {
		t.Fatalf("runs = %d, want one", got)
	}
}
