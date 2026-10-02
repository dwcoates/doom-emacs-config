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
	sent := `{"force":false}`

	// Act.
	if status, body := rawUnary(t, baseURL, "Deploy", sent); status != 200 {
		t.Fatalf("Deploy answered %d: %s", status, body)
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
	if status, body := rawUnary(t, baseURL, "Deploy", `{}`); status != 200 {
		t.Fatalf("Deploy answered %d: %s", status, body)
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

// A body over the recording cap is passed through UNTOUCHED — the handler
// still sees every byte — and simply records no raw. Truncating the request
// to fit the recording would corrupt the very round-trip this fake exists to
// check.
func TestAnOversizedBodyReachesTheHandlerButRecordsNoRaw(t *testing.T) {
	// Arrange: a valid RestartWorkspace request padded past the cap with a
	// name field long enough that the whole body exceeds maxRecordedRawBody.
	_, baseURL := newTestServer(t)
	padding := strings.Repeat("p", maxRecordedRawBody)
	sent := `{"workspace":{"id":"ws-a","dir":"/tmp/` + padding + `"}}`
	if len(sent) <= maxRecordedRawBody {
		t.Fatalf("the fixture body is %d bytes, which does not exceed the cap", len(sent))
	}

	// Act.
	if status, body := rawUnary(t, baseURL, "RestartWorkspace", sent); status != 200 {
		t.Fatalf("RestartWorkspace answered %d: %s", status, body)
	}
	_, listing := controlGet(t, baseURL, "/_fake/calls")
	calls := decodeCalls(t, listing)

	// Assert: the handler decoded the whole body, and raw is absent rather
	// than a truncated lie.
	if len(calls) != 1 {
		t.Fatalf("want 1 recorded call, got %d", len(calls))
	}
	if calls[0].Raw != "" {
		t.Fatalf("raw = %q, want nothing recorded for an oversized body", calls[0].Raw)
	}
	if !strings.Contains(string(calls[0].Body), padding) {
		t.Fatalf("body lost the padding, so the handler did not see every byte: %s", calls[0].Body)
	}
}
