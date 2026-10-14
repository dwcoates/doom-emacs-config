package wsm

import (
	"context"
	"database/sql"
	"database/sql/driver"
	"errors"
	"testing"
)

func TestOpenInMemoryRefusesOutsideATestBinary(t *testing.T) {
	// Arrange
	isTestBinary = func() bool { return false }
	t.Cleanup(func() { isTestBinary = testingTesting })

	// Act
	_, err := OpenInMemory(context.Background())

	// Assert
	if !errors.Is(err, errNotATest) {
		t.Fatalf("OpenInMemory outside a test binary = %v, want errNotATest", err)
	}
}

func TestOpenInMemoryStartsAtThisBuildsLayout(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	version := scalar[int](t, s, `SELECT version FROM layout WHERE id = 1`)

	// Assert
	if version != LayoutVersion {
		t.Fatalf("layout version = %d, want %d", version, LayoutVersion)
	}
}

func TestOpenInMemoryGivesEachHandleItsOwnDatabase(t *testing.T) {
	// Arrange
	first, _ := testStore(t)
	second, _ := testStore(t)
	if _, err := first.CreateTask(context.Background(), "only in the first"); err != nil {
		t.Fatalf("CreateTask: %v", err)
	}

	// Act
	count := scalar[int](t, second, `SELECT count(*) FROM tasks`)

	// Assert
	if count != 0 {
		t.Fatalf("the second in-memory handle sees %d task rows, want 0", count)
	}
}

func TestOpenInMemoryLeavesTheTemplateUntouched(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if _, err := s.CreateTask(context.Background(), "written after the copy"); err != nil {
		t.Fatalf("CreateTask: %v", err)
	}

	// Act
	var count int
	err := templateHandle.QueryRowContext(context.Background(), `SELECT count(*) FROM tasks`).Scan(&count)

	// Assert
	if err != nil {
		t.Fatalf("count the template's tasks: %v", err)
	}
	if count != 0 {
		t.Fatalf("the template holds %d task rows, want 0", count)
	}
}

func TestOpenInMemoryEnforcesForeignKeys(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got := scalar[int](t, s, `PRAGMA foreign_keys`)

	// Assert
	if got != 1 {
		t.Fatalf("PRAGMA foreign_keys = %d, want 1", got)
	}
}

func TestOpenInMemoryKeepsItsDatabaseWhenThePoolReplacesItsConnection(t *testing.T) {
	// Arrange: a row written, then the handle's one connection discarded, as
	// database/sql discards a connection a cancelled query interrupted.
	s, _ := testStore(t)
	if _, err := s.CreateTask(context.Background(), "written before the replacement"); err != nil {
		t.Fatalf("CreateTask: %v", err)
	}
	conn, err := s.db().Conn(context.Background())
	if err != nil {
		t.Fatalf("reach the handle's connection: %v", err)
	}
	if err := conn.Raw(func(any) error { return driver.ErrBadConn }); !errors.Is(err, driver.ErrBadConn) {
		t.Fatalf("discarding the connection answered %v, want driver.ErrBadConn", err)
	}
	conn.Close()

	// Act
	count := scalar[int](t, s, `SELECT count(*) FROM tasks`)

	// Assert
	if count != 1 {
		t.Fatalf("after its connection was replaced the handle sees %d task rows, want 1", count)
	}
}

func TestOpenInMemoryReleasesItsDatabaseOnClose(t *testing.T) {
	// Arrange
	db, err := OpenInMemory(context.Background())
	if err != nil {
		t.Fatalf("OpenInMemory: %v", err)
	}
	keeper := db.(*store).keeper.(*memoryKeeper)

	// Act
	closeErr := db.Close()

	// Assert
	if closeErr != nil {
		t.Fatalf("Close: %v", closeErr)
	}
	if err := keeper.conn.PingContext(context.Background()); !errors.Is(err, sql.ErrConnDone) {
		t.Fatalf("the keeper's connection answered %v after Close, want sql.ErrConnDone", err)
	}
}

func TestTheSchemaTemplateSurvivesItsPoolReplacingItsConnection(t *testing.T) {
	// Arrange: the template built, then its handle's connection discarded.
	testStore(t)
	conn, err := templateHandle.Conn(context.Background())
	if err != nil {
		t.Fatalf("reach the template's connection: %v", err)
	}
	if err := conn.Raw(func(any) error { return driver.ErrBadConn }); !errors.Is(err, driver.ErrBadConn) {
		t.Fatalf("discarding the connection answered %v, want driver.ErrBadConn", err)
	}
	conn.Close()

	// Act
	s, _ := testStore(t)

	// Assert
	if version := scalar[int](t, s, `SELECT version FROM layout WHERE id = 1`); version != LayoutVersion {
		t.Fatalf("a handle copied after the replacement is at layout %d, want %d", version, LayoutVersion)
	}
}

func TestAnInMemoryStoreClosedTwiceAnswersNilTheSecondTime(t *testing.T) {
	// Arrange
	db, err := OpenInMemory(context.Background())
	if err != nil {
		t.Fatalf("OpenInMemory: %v", err)
	}
	if err := db.Close(); err != nil {
		t.Fatalf("the first Close: %v", err)
	}

	// Act
	err = db.Close()

	// Assert
	if err != nil {
		t.Fatalf("the second Close = %v, want nil as *sql.DB's", err)
	}
}
