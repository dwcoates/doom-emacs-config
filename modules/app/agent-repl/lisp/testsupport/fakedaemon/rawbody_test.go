package main

import (
	"strings"
	"testing"
)

// /_fake/calls echoes the request EXACTLY as the client wrote it, because a
// re-marshal of the decoded message cannot distinguish an explicit `false'
// from an omitted one.
func TestRecordedCallEchoesTheRawRequestBody(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	sent := `{"workspace":{"id":"ws-a","dir":"/tmp/ws-a"},"force":false}`

	// Act.
	if status, body := rawUnary(t, baseURL, "RestartWorkspace", sent); status != 200 {
		t.Fatalf("RestartWorkspace answered %d: %s", status, body)
	}
	_, listing := controlGet(t, baseURL, "/_fake/calls")
	calls := decodeCalls(t, listing)

	// Assert: the raw bytes carry the explicit false the body re-marshal drops.
	if len(calls) != 1 {
		t.Fatalf("want 1 recorded call, got %d", len(calls))
	}
	if calls[0].Raw != sent {
		t.Fatalf("raw = %q, want %q", calls[0].Raw, sent)
	}
	if strings.Contains(string(calls[0].Body), "force") {
		t.Fatalf("body unexpectedly kept the false `force': %s", calls[0].Body)
	}
}

// An omitted bool and an explicit false differ in `raw' and nowhere else —
// which is the whole reason the field exists.
func TestRawBodyDistinguishesAnOmittedBoolFromAnExplicitFalse(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	if status, body := rawUnary(t, baseURL, "RestartWorkspace",
		`{"workspace":{"id":"ws-a","dir":"/tmp/ws-a"}}`); status != 200 {
		t.Fatalf("RestartWorkspace answered %d: %s", status, body)
	}
	_, listing := controlGet(t, baseURL, "/_fake/calls")
	calls := decodeCalls(t, listing)

	// Assert.
	if len(calls) != 1 {
		t.Fatalf("want 1 recorded call, got %d", len(calls))
	}
	if strings.Contains(calls[0].Raw, "force") {
		t.Fatalf("raw named `force' although the client omitted it: %s", calls[0].Raw)
	}
}
