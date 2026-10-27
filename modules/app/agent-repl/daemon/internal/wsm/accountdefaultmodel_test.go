package wsm

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
)

func TestAccountDefaultModelIsAbsentWhenNobodyChoseOne(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, found, err := s.AccountDefaultModel(context.Background(), "/Users/dev/.claude")

	// Assert
	if err != nil || found {
		t.Fatalf("AccountDefaultModel = found %v, err %v; want absent with no error", found, err)
	}
}

func TestPutAccountDefaultModelRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	if err := s.PutAccountDefaultModel(context.Background(), "/Users/dev/.claude", "sonnet"); err != nil {
		t.Fatalf("PutAccountDefaultModel: %v", err)
	}
	got, found, err := s.AccountDefaultModel(context.Background(), "/Users/dev/.claude")

	// Assert
	if err != nil || !found || got != "sonnet" {
		t.Fatalf("AccountDefaultModel = %q, %v, %v; want sonnet", got, found, err)
	}
}

func TestPutAccountDefaultModelReplacesTheLoginsDefault(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if err := s.PutAccountDefaultModel(context.Background(), "/Users/dev/.claude", "sonnet"); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	if err := s.PutAccountDefaultModel(context.Background(), "/Users/dev/.claude", "haiku"); err != nil {
		t.Fatalf("PutAccountDefaultModel: %v", err)
	}

	// Assert
	got, _, err := s.AccountDefaultModel(context.Background(), "/Users/dev/.claude")
	if err != nil || got != "haiku" {
		t.Fatalf("AccountDefaultModel = %q, %v; want the replacement haiku", got, err)
	}
}

func TestAccountDefaultModelIsPerLogin(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if err := s.PutAccountDefaultModel(context.Background(), "/Users/dev/.claude-work", "sonnet"); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	_, found, err := s.AccountDefaultModel(context.Background(), "/Users/dev/.claude")

	// Assert
	if err != nil || found {
		t.Fatalf("personal login found %v, err %v; want the work login's default not to leak", found, err)
	}
}

func TestPutAccountDefaultModelRefusesABlankModel(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.PutAccountDefaultModel(context.Background(), "/Users/dev/.claude", " ")

	// Assert
	if err == nil {
		t.Fatalf("PutAccountDefaultModel accepted a blank model")
	}
	if _, found, _ := s.AccountDefaultModel(context.Background(), "/Users/dev/.claude"); found {
		t.Fatalf("a refused default was persisted")
	}
	assertLoggedError(t, log, "daemon.wsm.put_account_default_model")
}

func TestPutAccountDefaultModelRefusesABlankRoot(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.PutAccountDefaultModel(context.Background(), "", "sonnet")

	// Assert
	if err == nil {
		t.Fatalf("PutAccountDefaultModel accepted a blank root")
	}
	assertLoggedError(t, log, "daemon.wsm.put_account_default_model")
}

func TestAccountDefaultModelRefusesAStoredBlankModel(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if _, err := s.db().Exec(`INSERT INTO account_default_models (config_dir, model) VALUES (?, '')`, "/Users/dev/.claude"); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	_, _, err := s.AccountDefaultModel(context.Background(), "/Users/dev/.claude")

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Table != "account_default_models" {
		t.Fatalf("err = %v, want a DecodeError on account_default_models", err)
	}
}

// assertLoggedError fails unless LOG holds an ERROR under OPERATION.
func assertLoggedError(t *testing.T, log *dlog.TestLogger, operation string) {
	t.Helper()
	for _, rec := range log.Records() {
		if rec.Level == "error" && rec.Operation == operation {
			return
		}
	}
	t.Fatalf("no ERROR was logged under %s", operation)
}
