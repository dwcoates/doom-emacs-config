package db

import (
	"errors"
	"testing"
)

func TestOpenInMemoryRefusesOutsideATestBinary(t *testing.T) {
	// Arrange
	isTestBinary = func() bool { return false }
	t.Cleanup(func() { isTestBinary = testingTesting })
	_, log := newSink(t)

	// Act
	_, err := openInMemory(log, Options{})

	// Assert
	if !errors.Is(err, errNotATest) {
		t.Fatalf("openInMemory outside a test binary = %v, want errNotATest", err)
	}
}

func TestOpenInMemoryStartsAtThisBinarysSchema(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	version, tables, err := d.inspectSchema(ctx())

	// Assert
	if err != nil {
		t.Fatalf("inspectSchema: %v", err)
	}
	if version != SchemaVersion || !slicesEqual(shapeTables(tables), shapeTables(schemaTables)) {
		t.Fatalf("schema = version %d tables %v, want version %d tables %v", version, tables, SchemaVersion, schemaTables)
	}
}

func TestOpenInMemoryGivesEachHandleItsOwnDatabase(t *testing.T) {
	// Arrange
	first, _ := newStore(t)
	second, _ := newStore(t)
	writeOK(t, first, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))

	// Act
	var count int
	err := second.read.QueryRowContext(ctx(), `SELECT count(*) FROM entry`).Scan(&count)

	// Assert
	if err != nil {
		t.Fatalf("count the second handle's entries: %v", err)
	}
	if count != 0 {
		t.Fatalf("the second in-memory handle holds %d entries, want 0", count)
	}
}

func TestOpenInMemoryLeavesTheTemplateUntouched(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))

	// Act
	var count int
	err := templateHandle.read.QueryRowContext(ctx(), `SELECT count(*) FROM entry`).Scan(&count)

	// Assert
	if err != nil {
		t.Fatalf("count the template's entries: %v", err)
	}
	if count != 0 {
		t.Fatalf("the template holds %d entries, want 0", count)
	}
}

func TestOpenInMemorySharesOneDatabaseAcrossItsPools(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))

	// Act
	var count int
	err := d.read.QueryRowContext(ctx(), `SELECT count(*) FROM entry`).Scan(&count)

	// Assert
	if err != nil {
		t.Fatalf("count through the read pool: %v", err)
	}
	if count != 1 {
		t.Fatalf("the read pool sees %d entries, want the write connection's 1", count)
	}
}
