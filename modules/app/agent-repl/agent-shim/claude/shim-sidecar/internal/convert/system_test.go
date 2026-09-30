package convert

// system_test.go — the api_error taxonomy, and the withholding classes.

import (
	"testing"
	"time"
)

func apiErrorLine(uuid, detail, extra string) string {
	line := `{"type":"system","subtype":"api_error","uuid":"` + uuid + `","isSidechain":false,` +
		`"timestamp":"` + ts1 + `","error":` + detail
	if extra != "" {
		line += "," + extra
	}
	return line + "}"
}

func TestApiErrorIsAPageLineAndNeverATerminal(t *testing.T) {
	// Arrange. EVIDENCE, NEVER A TERMINAL: a system/api_error is a request that
	// failed mid-turn and was recorded; the turn went on. The turn's END is the
	// frame-level failure arm and nothing else.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, apiErrorLine("e1", `{"message":"Connection error.","formatted":"Connection interrupted"}`, ""))

	// Assert.
	entry := entryByKey(t, entries, SessionKey("api_error", "e1"))
	frame := frameOf(entry)
	if frame.GetUpdate().GetApiError() == nil {
		t.Fatal("an api_error must land on AgentUpdate.api_error")
	}
	if frame.GetFailure() != nil {
		t.Fatal("an api_error must NEVER be a frame-level failure: that arm is the turn's end")
	}
	if got := frame.GetUpdate().GetApiError().GetMessage(); got != "Connection interrupted" {
		t.Fatalf("message = %q, want the vendor's formatted wording", got)
	}
}

func TestApiErrorKindsTable(t *testing.T) {
	// Arrange. THE KIND IS THE VENDOR'S, NOT A CLASSIFICATION. A type this schema
	// does not model arrives as `unmodeled` carrying the name rather than as a
	// silently mishandled value.
	cases := []struct {
		name    string
		detail  string
		extra   string
		wantArm string
	}{
		{name: "429 by type", detail: `{"type":"rate_limit_error","message":"slow down"}`, wantArm: "rate_limited"},
		{name: "429 by status", detail: `{"status":429,"message":"slow down"}`, wantArm: "rate_limited"},
		{name: "529 overloaded", detail: `{"type":"overloaded_error","message":"busy"}`, wantArm: "overloaded"},
		{name: "401 authentication", detail: `{"type":"authentication_error","message":"bad key"}`, wantArm: "authentication_failed"},
		{name: "403 permission", detail: `{"type":"permission_error","message":"nope"}`, wantArm: "permission_denied"},
		{name: "400 invalid request", detail: `{"type":"invalid_request_error","message":"bad"}`, wantArm: "invalid_request"},
		{name: "413 too large", detail: `{"type":"request_too_large","message":"big"}`, wantArm: "request_too_large"},
		{name: "404 not found", detail: `{"type":"not_found_error","message":"gone"}`, wantArm: "not_found"},
		{name: "500 internal", detail: `{"type":"api_error","message":"oops"}`, wantArm: "internal"},
		{name: "billing", detail: `{"type":"billing_error","message":"pay"}`, wantArm: "billing_error"},
		{name: "org not allowed", detail: `{"type":"oauth_org_not_allowed","message":"org"}`, wantArm: "oauth_org_not_allowed"},
		{name: "max output tokens", detail: `{"type":"max_output_tokens","message":"ceiling"}`, wantArm: "max_output_tokens"},
		{name: "a type the schema does not model", detail: `{"type":"brand_new_error","message":"?"}`, wantArm: "unmodeled"},
		{name: "a transport failure has no vendor type", detail: `{"message":"Connection error.","connection":{"code":"StreamSuspended"}}`, wantArm: "unmodeled"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := newTestConverter(t)

			// Act.
			entries := convertLines(t, c, apiErrorLine("e1", tc.detail, tc.extra))

			// Assert.
			failed := frameOf(entryByKey(t, entries, SessionKey("api_error", "e1"))).GetUpdate().GetApiError()
			if failed == nil {
				t.Fatal("no api_error produced")
			}
			if got := apiKindName(failed); got != tc.wantArm {
				t.Fatalf("kind arm = %q, want %q", got, tc.wantArm)
			}
		})
	}
}

func TestRateLimitCarriesTheVendorsRetryHint(t *testing.T) {
	// Arrange. The retry hint lives on the ONE kind that carries it. UNSET means
	// the vendor said nothing, which is different from "retry now".
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, apiErrorLine("e1", `{"type":"rate_limit_error","message":"slow"}`, `"retryInMs":549`))

	// Assert.
	limited := frameOf(entryByKey(t, entries, SessionKey("api_error", "e1"))).GetUpdate().GetApiError().GetRateLimited()
	if limited == nil {
		t.Fatal("expected the rate_limited arm")
	}
	if limited.RetryAfterMs == nil || limited.GetRetryAfterMs() != 549 {
		t.Fatalf("retry_after_ms = %v, want 549", limited.RetryAfterMs)
	}
}

func TestAbsentRetryHintStaysUnset(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, apiErrorLine("e1", `{"type":"rate_limit_error","message":"slow"}`, ""))

	// Assert.
	limited := frameOf(entryByKey(t, entries, SessionKey("api_error", "e1"))).GetUpdate().GetApiError().GetRateLimited()
	if limited.RetryAfterMs != nil {
		t.Fatal(`an absent hint must stay UNSET: "wait 0ms" is a different claim from "the vendor said nothing"`)
	}
}

func TestUnmodeledApiErrorKeepsTheVendorsTypeName(t *testing.T) {
	// Arrange. So a later schema knows what to add.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, apiErrorLine("e1", `{"type":"brand_new_error","message":"?"}`, ""))

	// Assert.
	unmodeled := frameOf(entryByKey(t, entries, SessionKey("api_error", "e1"))).GetUpdate().GetApiError().GetUnmodeled()
	if got := unmodeled.GetType(); got != "brand_new_error" {
		t.Fatalf("type = %q, want the vendor's own name", got)
	}
}

func TestConnectionFailureNamesTheVendorsConnectionCode(t *testing.T) {
	// Arrange. A transport failure carries no vendor error TYPE — the vendor
	// records a `connection` object instead — so the code is named rather than a
	// modeled kind being guessed at.
	c := newTestConverter(t)
	detail := `{"message":"Connection error.","connection":{"code":"StreamSuspended"}}`

	// Act.
	entries := convertLines(t, c, apiErrorLine("e1", detail, ""))

	// Assert.
	unmodeled := frameOf(entryByKey(t, entries, SessionKey("api_error", "e1"))).GetUpdate().GetApiError().GetUnmodeled()
	if got := unmodeled.GetType(); got != "connection/StreamSuspended" {
		t.Fatalf("type = %q, want the connection code named", got)
	}
}

func TestApiErrorCarriesTheVendorsRetrySchedule(t *testing.T) {
	// Arrange. A connection failure, as the 2026-09-30 outage recorded it.
	c := newTestConverter(t)
	detail := `{"message":"Connection error.","connection":{"code":"ENOTFOUND"}}`

	// Act.
	entries := convertLines(t, c, apiErrorLine("e1", detail, `"retryInMs":32234.5,"retryAttempt":8,"maxRetries":10`))

	// Assert: the next attempt is the failure's own instant plus the delay.
	retry := frameOf(entryByKey(t, entries, SessionKey("api_error", "e1"))).GetUpdate().GetApiError().GetRetry()
	at, err := time.Parse(time.RFC3339Nano, ts1)
	if err != nil {
		t.Fatalf("parse ts1: %v", err)
	}
	if retry.GetAttempt() != 8 || retry.GetMaxRetries() != 10 || retry.GetNextAttemptAtMs() != at.UnixMilli()+32234 {
		t.Fatalf("retry = %v, want attempt 8 of 10, next at ts1 + 32234ms", retry)
	}
}

func TestApiErrorWithAPartialRetryScheduleCarriesNone(t *testing.T) {
	// Arrange: a delay with no attempt count.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, apiErrorLine("e1", `{"type":"rate_limit_error","message":"slow"}`, `"retryInMs":549`))

	// Assert.
	if retry := frameOf(entryByKey(t, entries, SessionKey("api_error", "e1"))).GetUpdate().GetApiError().GetRetry(); retry != nil {
		t.Fatalf("retry = %v, want none from a partial schedule", retry)
	}
}

func TestWithheldSystemSubtypesTable(t *testing.T) {
	// Arrange. Records that must never become feed rows are CLASSIFIED AT INGEST,
	// so no resolver ever sees them as prose and no history page regrows them.
	subtypes := []string{"informational", "turn_duration", "stop_hook_summary", "away_summary",
		"scheduled_task_fire", "agents_killed", "model_refusal_fallback", "local_command"}
	for _, subtype := range subtypes {
		t.Run(subtype, func(t *testing.T) {
			c := newTestConverter(t)
			line := `{"type":"system","subtype":"` + subtype + `","uuid":"s1","isSidechain":false,` +
				`"timestamp":"` + ts1 + `","content":"x"}`

			// Act.
			entries := convertLines(t, c, line)

			// Assert.
			if len(entries) != 1 {
				t.Fatalf("entries = %d, want 1", len(entries))
			}
			if got := vendorKindOf(entries[0]); got != "system/"+subtype {
				t.Fatalf("kind = %q, want system/%s", got, subtype)
			}
			if pageLine(entries[0]) != nil {
				t.Fatal("a withheld record must never be a page line")
			}
		})
	}
}

func TestWithheldTopLevelLineTypesTable(t *testing.T) {
	// Arrange. CLI slash-command bookkeeping and machinery.
	kinds := []string{"mode", "permission-mode", "queue-operation", "last-prompt", "ai-title",
		"pr-link", "frame-link", "file-history-snapshot", "file-history-delta", "attribution-snapshot"}
	for _, kind := range kinds {
		t.Run(kind, func(t *testing.T) {
			c := newTestConverter(t)

			// Act.
			entries := convertLines(t, c, `{"type":"`+kind+`","sessionId":"s"}`)

			// Assert.
			if got := vendorKindOf(entries[0]); got != kind {
				t.Fatalf("kind = %q, want %q", got, kind)
			}
		})
	}
}

func TestUnknownTopLevelTypeIsUnknownNotVendorSpecific(t *testing.T) {
	// Arrange. The two arms exist so the follow-up each needs is
	// distinguishable: a CONVERTER for vendor_specific, a MODEL for unknown.
	// Filing a brand-new type as vendor_specific would assert an understanding
	// nobody has.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, `{"type":"brand-new-line-type","x":1}`)

	// Assert.
	unknown := entries[0].GetAgentUpdate().GetUnservedItem().GetUnknown()
	if unknown == nil {
		t.Fatal("an unrecognized top-level type must land on the unknown arm")
	}
	if got := unknown.GetDiscriminator(); got != "brand-new-line-type" {
		t.Fatalf("discriminator = %q, want the type", got)
	}
	if got := unknown.GetDiscriminatorField(); got != "type" {
		t.Fatalf("discriminator_field = %q, want %q: a producer that looked in the wrong place and a genuinely new kind are otherwise identical", got, "type")
	}
}

func TestSystemLineWithNoSubtypeIsUnknown(t *testing.T) {
	// Arrange. It parsed, so it is not unparsed; we simply cannot say what it is.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, `{"type":"system","uuid":"s1","isSidechain":false,"timestamp":"`+ts1+`"}`)

	// Assert.
	unknown := entries[0].GetAgentUpdate().GetUnservedItem().GetUnknown()
	if unknown == nil || unknown.GetDiscriminatorField() != "subtype" {
		t.Fatalf("want unknown keyed on the missing subtype, got %v", unknown)
	}
}

// TestAnObservedApiErrorIsRecordedAtInfo pins the SEVERITY of the api_error
// trace, not its prose. The vendor recorded the failure; this converter's own
// handling of it succeeded whole, so the record states an observed event rather
// than accusing the sidecar of an owned fault.
//
// It is a table over the shapes the owner's log actually carried — a bare
// connection drop and a typed vendor error — because the level must not depend
// on which taxonomy arm the kind mapped onto.
func TestAnObservedApiErrorIsRecordedAtInfo(t *testing.T) {
	cases := []struct {
		name   string
		detail string
	}{
		{name: "connection drop with no vendor type", detail: `{"message":"Unable to connect to API (ECONNRESET)"}`},
		{name: "typed vendor error", detail: `{"type":"overloaded_error","message":"overloaded"}`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c, sink := loggedConverter(t)

			// Act.
			convertLines(t, c, apiErrorLine("e1", tc.detail, ""))

			// Assert.
			if got := levelForMessage(t, sink, "vendor api_error recorded mid-turn"); got != "info" {
				t.Fatalf("the observed api_error was recorded at %q, want info (the fault is the vendor's; this conversion succeeded)", got)
			}
		})
	}
}
