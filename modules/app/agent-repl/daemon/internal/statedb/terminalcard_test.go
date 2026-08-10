package statedb

import (
	"path/filepath"
	"testing"
)

// openTerminalCards opens a fresh state store on disk and installs the terminal
// failure card table on it.
func openTerminalCards(t *testing.T) *TerminalFailureCards {
	t.Helper()
	db, err := Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = db.Close() })
	cards, err := NewTerminalFailureCards(db)
	if err != nil {
		t.Fatalf("NewTerminalFailureCards: %v", err)
	}
	return cards
}

// aCard is one fenced session's standing card.
func aCard() TerminalFailureCard {
	return TerminalFailureCard{
		SessionID: "s1",
		Workspace: "/ws",
		UUID:      "start_failed:s1",
		Card:      []byte{0x0a, 0x02, 'h', 'i'},
		AtMs:      1_000,
	}
}

func TestARecordedTerminalCardStands(t *testing.T) {
	// Arrange.
	cards := openTerminalCards(t)

	// Act.
	if err := cards.Record(aCard()); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Assert.
	got, standing, err := cards.Standing("s1")
	if err != nil {
		t.Fatalf("Standing: %v", err)
	}
	if !standing || got.UUID != "start_failed:s1" || got.AtMs != 1_000 {
		t.Fatalf("Standing = %+v (standing=%v), want the recorded card", got, standing)
	}
}

func TestASessionWithNoCardStandsForNothing(t *testing.T) {
	// Arrange.
	cards := openTerminalCards(t)

	// Act.
	_, standing, err := cards.Standing("s-never-fenced")
	// Assert.
	if err != nil {
		t.Fatalf("Standing: %v", err)
	}
	if standing {
		t.Fatal("a session that was never fenced holds a standing card")
	}
}

// A re-fence must RESTATE one claim rather than accumulate a second, which is
// what the primary key plus the upsert buys.
func TestARerecordedTerminalCardReplacesTheStandingOne(t *testing.T) {
	// Arrange.
	cards := openTerminalCards(t)
	if err := cards.Record(aCard()); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Act.
	refenced := aCard()
	refenced.AtMs = 5_000
	if err := cards.Record(refenced); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Assert.
	got, _, err := cards.Standing("s1")
	if err != nil {
		t.Fatalf("Standing: %v", err)
	}
	if got.AtMs != 5_000 {
		t.Fatalf("standing card at_ms = %d, want the re-fence's own instant", got.AtMs)
	}
}

func TestWithdrawingAStandingCardReportsThatOneStood(t *testing.T) {
	// Arrange.
	cards := openTerminalCards(t)
	if err := cards.Record(aCard()); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Act.
	stood, err := cards.Withdraw("s1")

	// Assert.
	if err != nil {
		t.Fatalf("Withdraw: %v", err)
	}
	if !stood {
		t.Fatal("Withdraw reported nothing stood; the recorded card was the standing claim")
	}
}

func TestWithdrawingNothingIsNotAnError(t *testing.T) {
	// Arrange.
	cards := openTerminalCards(t)

	// Act.
	stood, err := cards.Withdraw("s-never-fenced")

	// Assert.
	if err != nil {
		t.Fatalf("Withdraw: %v", err)
	}
	if stood {
		t.Fatal("Withdraw claimed a card stood for a session that was never fenced")
	}
}

func TestAWithdrawnCardNoLongerStands(t *testing.T) {
	// Arrange.
	cards := openTerminalCards(t)
	if err := cards.Record(aCard()); err != nil {
		t.Fatalf("Record: %v", err)
	}
	if _, err := cards.Withdraw("s1"); err != nil {
		t.Fatalf("Withdraw: %v", err)
	}

	// Act.
	_, standing, err := cards.Standing("s1")

	// Assert.
	if err != nil {
		t.Fatalf("Standing: %v", err)
	}
	if standing {
		t.Fatal("a withdrawn card still stands; a resync would serve it after the fence was cleared")
	}
}

func TestATerminalCardWithNoSessionIsRefused(t *testing.T) {
	// Arrange.
	cards := openTerminalCards(t)
	rec := aCard()
	rec.SessionID = ""

	// Act.
	err := cards.Record(rec)

	// Assert.
	if err == nil {
		t.Fatal("a card with no session id was recorded; nothing could ever address it")
	}
}

func TestATerminalCardWithNoWorkspaceIsRefused(t *testing.T) {
	// Arrange.
	cards := openTerminalCards(t)
	rec := aCard()
	rec.Workspace = ""

	// Act.
	err := cards.Record(rec)

	// Assert.
	if err == nil {
		t.Fatal("a card with no workspace was recorded; nothing could publish it")
	}
}

func TestATerminalCardWithNoUUIDIsRefused(t *testing.T) {
	// Arrange.
	cards := openTerminalCards(t)
	rec := aCard()
	rec.UUID = ""

	// Act.
	err := cards.Record(rec)

	// Assert.
	if err == nil {
		t.Fatal("a card with no uuid was recorded; a later push could not update the live one")
	}
}

func TestAnEmptyTerminalCardBodyIsRefused(t *testing.T) {
	// Arrange.
	cards := openTerminalCards(t)
	rec := aCard()
	rec.Card = nil

	// Act.
	err := cards.Record(rec)

	// Assert.
	if err == nil {
		t.Fatal("a card carrying no rendering was recorded; serving it would explain nothing")
	}
}

func TestALookupWithNoSessionIsRefused(t *testing.T) {
	// Arrange.
	cards := openTerminalCards(t)

	// Act.
	_, _, err := cards.Standing("")

	// Assert.
	if err == nil {
		t.Fatal("a lookup with no session id succeeded; it names no session to answer for")
	}
}

func TestAWithdrawalWithNoSessionIsRefused(t *testing.T) {
	// Arrange.
	cards := openTerminalCards(t)

	// Act.
	_, err := cards.Withdraw("")

	// Assert.
	if err == nil {
		t.Fatal("a withdrawal with no session id succeeded; it names no claim to withdraw")
	}
}

func TestNewTerminalFailureCardsRefusesAClosedStore(t *testing.T) {
	// Arrange, Act.
	_, err := NewTerminalFailureCards(nil)

	// Assert.
	if err == nil {
		t.Fatal("the card store installed itself on no database at all")
	}
}
