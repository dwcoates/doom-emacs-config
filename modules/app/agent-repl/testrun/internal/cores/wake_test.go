package cores

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestWakeGovernorWakesTheListener(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	wake, stop, err := listenWake(dir)
	if err != nil {
		t.Fatal(err)
	}
	defer stop()

	// Act
	if err := wakeGovernor(dir); err != nil {
		t.Fatal(err)
	}

	// Assert
	<-wake
}

func TestListenWakeReusesTheFIFOAnEarlierGovernorMade(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	_, stop, err := listenWake(dir)
	if err != nil {
		t.Fatal(err)
	}
	if err := stop(); err != nil {
		t.Fatal(err)
	}

	// Act
	wake, stop, err := listenWake(dir)
	if err != nil {
		t.Fatal(err)
	}
	defer stop()
	if err := wakeGovernor(dir); err != nil {
		t.Fatal(err)
	}

	// Assert
	<-wake
}

func TestWakeGovernorWithNoGovernorIsNoFailure(t *testing.T) {
	cases := []struct {
		name string
		fifo bool
	}{
		{name: "no governor ever made the FIFO"},
		{name: "the FIFO's governor stopped listening", fifo: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			if tc.fifo {
				_, stop, err := listenWake(dir)
				if err != nil {
					t.Fatal(err)
				}
				if err := stop(); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			err := wakeGovernor(dir)

			// Assert
			if err != nil {
				t.Fatalf("wakeGovernor = %v; want no failure", err)
			}
		})
	}
}

func TestListenWakeRefusesAPathThatIsNoFIFO(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, WakeFile), nil, 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	_, _, err := listenWake(dir)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "is not a FIFO") {
		t.Fatalf("listenWake = %v; want the plain file refused", err)
	}
}

func TestWakeGovernorSurfacesAWakePathItCannotOpen(t *testing.T) {
	// Arrange: the wake path is a directory.
	dir := t.TempDir()
	if err := os.Mkdir(filepath.Join(dir, WakeFile), 0o755); err != nil {
		t.Fatal(err)
	}

	// Act
	err := wakeGovernor(dir)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "open the governor's wake FIFO") {
		t.Fatalf("wakeGovernor = %v; want the failure surfaced", err)
	}
}
