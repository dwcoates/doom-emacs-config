package main

import (
	"os"
	"reflect"
	"strconv"
	"strings"
	"testing"
)

func TestForwardEvents(t *testing.T) {
	// Arrange: an open file standing in for the invoker's events stream.
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	defer r.Close()
	defer w.Close()
	fd := strconv.Itoa(int(w.Fd()))

	// Act
	argv, extra, err := forwardEvents([]string{"--module", "/m", "--events-fd", fd})

	// Assert
	if err != nil || !reflect.DeepEqual(argv, []string{"--module", "/m", "--events-fd", "4"}) || len(extra) != 1 || extra[0].Fd() != w.Fd() {
		t.Fatalf("forwardEvents = %v, %v, %v; want the runner told fd 4 and the stream forwarded", argv, extra, err)
	}
}

func TestForwardEventsWithoutAStream(t *testing.T) {
	// Act
	argv, extra, err := forwardEvents([]string{"--module", "/m"})

	// Assert
	if err != nil || !reflect.DeepEqual(argv, []string{"--module", "/m"}) || extra != nil {
		t.Fatalf("forwardEvents = %v, %v, %v; want argv unchanged and nothing forwarded", argv, extra, err)
	}
}

func TestForwardEventsRefusesAStandardStream(t *testing.T) {
	// Act
	_, _, err := forwardEvents([]string{"--events-fd", "1"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "not a file descriptor past stderr") {
		t.Fatalf("forwardEvents = %v, want the standard stream refused", err)
	}
}
