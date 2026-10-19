package wsm

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// paidAt is the instant every fixture payment was made.
var paidAt = time.Date(2026, 10, 19, 14, 38, 30, 0, time.UTC)

// payment is a well-formed payment for the read at LASTREQUEST.
func payment(lastRequest int64) ColdReadPayment {
	return ColdReadPayment{VendorSessionID: "vendor-1", LastRequestAtMs: lastRequest, Model: "claude-opus-5-5", PaidAt: paidAt}
}

// paymentOf reads WS's payment back, failing the test on an error.
func paymentOf(t *testing.T, s *store, ws WorkspaceID) (ColdReadPayment, bool) {
	t.Helper()
	p, found, err := s.ColdReadPaymentOf(context.Background(), ws)
	if err != nil {
		t.Fatalf("ColdReadPaymentOf: %v", err)
	}
	return p, found
}

func TestAColdReadPaymentRoundTrips(t *testing.T) {
	tests := []struct {
		name string
		p    ColdReadPayment
	}{
		{name: "naming its model", p: payment(1_760_000_000_000)},
		{name: "naming no model", p: ColdReadPayment{VendorSessionID: "vendor-1", LastRequestAtMs: 1, PaidAt: paidAt}},
		{name: "at the epoch", p: ColdReadPayment{VendorSessionID: "vendor-1", LastRequestAtMs: 0, Model: "opus", PaidAt: paidAt}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			if err := s.PutColdReadPayment(context.Background(), ws.ID, tt.p); err != nil {
				t.Fatalf("PutColdReadPayment: %v", err)
			}

			// Assert
			got, found := paymentOf(t, s, ws.ID)
			if !found || got != tt.p {
				t.Fatalf("payment = %+v (found=%v), want %+v", got, found, tt.p)
			}
		})
	}
}

func TestAWorkspaceThatNeverPaidReadsNoPayment(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, found := paymentOf(t, s, ws.ID)

	// Assert
	if found {
		t.Fatal("a workspace that never paid read a payment")
	}
}

func TestPayingAgainReplacesThePaymentStanding(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutColdReadPayment(context.Background(), ws.ID, payment(1)); err != nil {
		t.Fatalf("PutColdReadPayment: %v", err)
	}
	want := payment(2)

	// Act
	if err := s.PutColdReadPayment(context.Background(), ws.ID, want); err != nil {
		t.Fatalf("PutColdReadPayment: %v", err)
	}

	// Assert
	if got, _ := paymentOf(t, s, ws.ID); got != want {
		t.Fatalf("payment = %+v, want the replacement %+v", got, want)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM cold_read_payments`); n != 1 {
		t.Fatalf("%d payment rows stand, want 1", n)
	}
}

func TestAPaymentForAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.PutColdReadPayment(context.Background(), "ws-gone", payment(1))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("PutColdReadPayment = %v, want ErrNotFound", err)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM cold_read_payments`); n != 0 {
		t.Fatalf("%d payment rows stand, want none", n)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "debug", Operation: "daemon.wsm.put_cold_read_payment"}); !ok {
		t.Fatalf("no debug record of the unknown workspace: %+v", log.Records())
	}
}

func TestAMalformedPaymentIsRefused(t *testing.T) {
	tests := []struct {
		name string
		p    ColdReadPayment
	}{
		{name: "no conversation", p: ColdReadPayment{LastRequestAtMs: 1, PaidAt: paidAt}},
		{name: "a negative last request", p: ColdReadPayment{VendorSessionID: "vendor-1", LastRequestAtMs: -1, PaidAt: paidAt}},
		{name: "no paid instant", p: ColdReadPayment{VendorSessionID: "vendor-1", LastRequestAtMs: 1}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := s.PutColdReadPayment(context.Background(), ws.ID, tt.p)

			// Assert
			if err == nil {
				t.Fatalf("PutColdReadPayment accepted %+v, want a refusal", tt.p)
			}
			if _, found := paymentOf(t, s, ws.ID); found {
				t.Fatal("a refused payment was stored")
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.put_cold_read_payment"})
			if !ok || rec.Context["workspace"] != string(ws.ID) || rec.Context["vendor_session_id"] != tt.p.VendorSessionID {
				t.Fatalf("no error record naming the workspace and the conversation: %+v", log.Records())
			}
		})
	}
}

func TestAStoredPaymentNoAnswerProducesIsADecodeError(t *testing.T) {
	// Arrange: a row written behind the store's back, with no conversation.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.db().Exec(`INSERT INTO cold_read_payments (workspace_id, vendor_session_id, last_request_at_ms, model, paid_at) VALUES (?, '', 1, '', 1)`, ws.ID); err != nil {
		t.Fatalf("seed the malformed row: %v", err)
	}

	// Act
	_, _, err := s.ColdReadPaymentOf(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Table != "cold_read_payments" {
		t.Fatalf("ColdReadPaymentOf = %v, want a DecodeError on cold_read_payments", err)
	}
}

func TestDroppingAPaymentDeletesIt(t *testing.T) {
	tests := []struct {
		name string
		seed bool
	}{
		{name: "one stands", seed: true},
		{name: "none stands", seed: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if tt.seed {
				if err := s.PutColdReadPayment(context.Background(), ws.ID, payment(1)); err != nil {
					t.Fatalf("PutColdReadPayment: %v", err)
				}
			}

			// Act
			err := s.DropColdReadPayment(context.Background(), ws.ID)

			// Assert
			if err != nil {
				t.Fatalf("DropColdReadPayment = %v, want nil", err)
			}
			if _, found := paymentOf(t, s, ws.ID); found {
				t.Fatal("the payment still stands after its drop")
			}
		})
	}
}
