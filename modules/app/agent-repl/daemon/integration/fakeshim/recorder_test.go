package main

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"google.golang.org/protobuf/proto"
)

func turnRequest(value string) *shimv1.StartTurnRequest {
	return &shimv1.StartTurnRequest{Turn: &conversationv1.TurnId{Value: value}}
}

func TestRecorderExpectReturnsAlreadyReceivedRequest(t *testing.T) {
	// Arrange
	rec := NewRecorder()
	rec.Record(RPCStartTurn, turnRequest("t-1"))

	// Act
	got, err := rec.Expect(context.Background(), RPCStartTurn)

	// Assert
	if err != nil {
		t.Fatalf("Expect = error %v, want the recorded request", err)
	}
	if !proto.Equal(got, turnRequest("t-1")) {
		t.Fatalf("Expect = %v, want the turn t-1 request", got)
	}
}

func TestRecorderExpectWaitsForALaterRequest(t *testing.T) {
	// Arrange
	rec := NewRecorder()
	recorded := make(chan struct{})
	go func() {
		defer close(recorded)
		rec.Record(RPCStartTurn, turnRequest("t-late"))
	}()

	// Act
	got, err := rec.Expect(context.Background(), RPCStartTurn)
	<-recorded

	// Assert
	if err != nil {
		t.Fatalf("Expect = error %v, want the late request", err)
	}
	if !proto.Equal(got, turnRequest("t-late")) {
		t.Fatalf("Expect = %v, want the turn t-late request", got)
	}
}

func TestRecorderExpectPopsInArrivalOrder(t *testing.T) {
	// Arrange
	rec := NewRecorder()
	rec.Record(RPCStartTurn, turnRequest("first"))
	rec.Record(RPCStartTurn, turnRequest("second"))

	// Act
	first, err1 := rec.Expect(context.Background(), RPCStartTurn)
	second, err2 := rec.Expect(context.Background(), RPCStartTurn)

	// Assert
	if err1 != nil || err2 != nil {
		t.Fatalf("Expect errors = %v, %v, want none", err1, err2)
	}
	if !proto.Equal(first, turnRequest("first")) || !proto.Equal(second, turnRequest("second")) {
		t.Fatalf("Expect order = %v then %v, want first then second", first, second)
	}
}

func TestRecorderExpectSurfacesContextCancellation(t *testing.T) {
	// Arrange
	rec := NewRecorder()
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	_, err := rec.Expect(ctx, RPCStartTurn)

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Expect on a cancelled context = %v, want context.Canceled", err)
	}
}

func TestRecorderExpectKeepsVerbsSeparate(t *testing.T) {
	// Arrange
	rec := NewRecorder()
	rec.Record(RPCKillTurn, &shimv1.KillTurnRequest{Turn: &conversationv1.TurnId{Value: "k-1"}})
	rec.Record(RPCStartTurn, turnRequest("s-1"))

	// Act
	got, err := rec.Expect(context.Background(), RPCStartTurn)

	// Assert
	if err != nil {
		t.Fatalf("Expect = error %v, want the StartTurn request", err)
	}
	if !proto.Equal(got, turnRequest("s-1")) {
		t.Fatalf("Expect(StartTurn) = %v, want the s-1 request and never the KillTurn one", got)
	}
}

func TestRecorderCountReportsEveryReceipt(t *testing.T) {
	// Arrange
	rec := NewRecorder()
	rec.Record(RPCStartTurn, turnRequest("a"))
	rec.Record(RPCStartTurn, turnRequest("b"))
	if _, err := rec.Expect(context.Background(), RPCStartTurn); err != nil {
		t.Fatalf("Expect = error %v, want to consume one request", err)
	}

	// Act
	got := rec.Count(RPCStartTurn)

	// Assert
	if got != 2 {
		t.Fatalf("Count = %d, want 2: reading a request does not unrecord it", got)
	}
}

func TestRecorderCountOfAnUnseenVerbIsZero(t *testing.T) {
	// Arrange
	rec := NewRecorder()

	// Act
	got := rec.Count(RPCHibernate)

	// Assert
	if got != 0 {
		t.Fatalf("Count(Hibernate) = %d, want 0", got)
	}
}

func TestRecorderRecordsACopyOfTheRequest(t *testing.T) {
	// Arrange
	rec := NewRecorder()
	req := turnRequest("original")
	rec.Record(RPCStartTurn, req)
	req.Turn.Value = "mutated"

	// Act
	got, err := rec.Expect(context.Background(), RPCStartTurn)

	// Assert
	if err != nil {
		t.Fatalf("Expect = error %v, want the recorded request", err)
	}
	if got.(*shimv1.StartTurnRequest).GetTurn().GetValue() != "original" {
		t.Fatalf("recorded turn = %q, want the value as received", got.(*shimv1.StartTurnRequest).GetTurn().GetValue())
	}
}
