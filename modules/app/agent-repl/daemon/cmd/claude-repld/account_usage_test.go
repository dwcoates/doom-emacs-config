package main

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/wsm"
)

// fakeUsageWriter records SetAccountUsage calls and answers err.
type fakeUsageWriter struct {
	calls []wsm.AccountUsage
	err   error
}

func (f *fakeUsageWriter) SetAccountUsage(_ context.Context, usage wsm.AccountUsage) error {
	f.calls = append(f.calls, usage)
	return f.err
}

func TestAccountUsageSinkWritesTheEvidence(t *testing.T) {
	// Arrange.
	db := &fakeUsageWriter{}
	usage := wsm.AccountUsage{ConfigDir: "/config", NoAllowance: true}

	// Act.
	err := accountUsageSink(db)(usage)

	// Assert.
	if err != nil || len(db.calls) != 1 || db.calls[0].ConfigDir != "/config" || !db.calls[0].NoAllowance {
		t.Fatalf("sink = %v, calls = %+v, want exactly the evidence written", err, db.calls)
	}
}

func TestAccountUsageSinkReturnsAFailedWrite(t *testing.T) {
	// Arrange.
	db := &fakeUsageWriter{err: errors.New("disk full")}

	// Act.
	err := accountUsageSink(db)(wsm.AccountUsage{ConfigDir: "/config"})

	// Assert.
	if err == nil {
		t.Fatal("sink swallowed the store's failure")
	}
}
