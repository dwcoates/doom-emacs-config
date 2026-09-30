package convert

// system.go — the `system` records: the context lifecycle, the vendor's own
// recorded API failures, and the harness narrating itself.

import (
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// systemLine converts a `system` record by its subtype.
func (c *Converter) systemLine(record map[string]any, at Attribution, next map[string]any) []*storev1.StoreEntry {
	env := readEnvelope(record)
	agent := c.frameAgent(at, env)
	subtype := str(record["subtype"])

	switch subtype {
	case "":
		c.log.With(at.ctxWarn("convert-line")).
			Log("system line carries no %q field; stored as unknown residue", "subtype")
		return []*storev1.StoreEntry{UnknownEntry(at, "", "subtype", record)}
	case "compact_boundary":
		return []*storev1.StoreEntry{c.contextCompacted(record, at, env, agent, next)}
	case "api_error":
		return []*storev1.StoreEntry{c.apiError(record, at, env, agent)}
	case "local_command":
		// The expanded local-command envelope: CLI machinery, never a feed row.
		// A `/clear` inside one is recognized on the USER record that carries it.
		c.log.With(at.ctxFor("withhold")).
			LogVerbose("system/local_command withheld as vendor_specific")
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "system/local_command", record)}
	default:
		// Informational, turn_duration, stop_hook_summary, away_summary,
		// scheduled_task_fire, the model-refusal notices, agents_killed: the
		// harness narrating its own bookkeeping. UNDERSTOOD and not carried.
		c.log.With(at.ctxFor("withhold")).
			LogVerbose("system/%s withheld as vendor_specific", subtype)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "system/"+subtype, record)}
	}
}

// ---------------------------------------------------------------------------
// api errors
// ---------------------------------------------------------------------------

// apiKind is one arm of ApiRequestFailed's kind oneof. The generated interface is
// unexported, so a builder outside the proto package names the arms structurally
// and lets the assignment below pick the field.
type apiKind any

// apiError converts the vendor's own recorded API failure.
//
// EVIDENCE, NEVER A TERMINAL. A `system/api_error` line is a request that failed
// MID-TURN and was recorded; the turn went on (usually into a retry). The turn's
// END is the frame-level failure arm and nothing else, so this lands on
// AgentUpdate.api_error as a page line and never as AgentFrame.failure.
func (c *Converter) apiError(record map[string]any, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	detail := obj(record["error"])
	message := firstNonEmpty(str(detail["formatted"]), str(detail["message"]), str(record["content"]))

	failed := &conversationv1.ApiRequestFailed{Message: message, Retry: apiRetry(record, env.timestampMs)}
	setAPIKind(failed, apiErrorKind(detail, record))

	// AN OBSERVED FAULT IS NOT AN OWNED ONE — info, not warn. This branch is the
	// converter's ORDINARY, fully-modelled path for a record the vendor itself
	// wrote: the request that failed was the vendor's, the failure was recorded
	// by the vendor's own transcript writer, and everything this converter does
	// with it succeeds — the line is read, its kind is mapped onto the taxonomy,
	// and it lands on `AgentUpdate.api_error` as the page line a reader sees.
	// Nothing here is degraded and nothing was refused, which is what AGENTS.md
	// reserves `warn` for ("invariant violations and refusals are warn; owned
	// failures are error"). Warning about it made the sidecar's log claim a
	// sidecar defect for what was, on this machine, the owner's own DNS and
	// connection drops (ENOTFOUND/ECONNRESET, 80 records).
	//
	// IT IS NOT DEMOTED TO DEBUG EITHER. Unlike the per-line conversion traces
	// beside it, this fires only when the vendor recorded a real failure, and an
	// operator correlating a stalled turn with the network wants it in a
	// default-level log. `info` is the level that states an event without
	// accusing this process of causing it.
	c.log.With(at.ctxFor("api-error")).With(logging.Context{UpsertKey: SessionKey("api_error", env.uuid)}).
		Log("vendor api_error recorded mid-turn: %s", message)

	frame := updateFrame(agent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ApiError{ApiError: failed},
	})
	return c.landFrame(at, agent, SessionKey("api_error", env.uuid), "api_error", frame)
}

// apiErrorKind maps the vendor's own declared error type onto its taxonomy.
//
// THE KIND IS THE VENDOR'S, NOT A CLASSIFICATION: whether anything can be DONE
// about it is the daemon's judgement. A type this schema does not model arrives
// as `unmodeled` carrying the name, rather than as a silently mishandled value.
func apiErrorKind(detail, record map[string]any) apiKind {
	retry := retryAfterMs(record)
	kind := strings.ToLower(firstNonEmpty(
		str(pick(detail, "type", "errorType")),
		str(pick(obj(detail["error"]), "type")),
	))
	if kind == "" {
		kind = inferKindFromStatus(detail)
	}

	switch kind {
	case "rate_limit_error", "rate_limited":
		return &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{RetryAfterMs: retry}}
	case "overloaded_error", "overloaded":
		return &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{RetryAfterMs: retry}}
	case "authentication_error":
		return &conversationv1.ApiRequestFailed_AuthenticationFailed{AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}}
	case "permission_error":
		return &conversationv1.ApiRequestFailed_PermissionDenied{PermissionDenied: &conversationv1.ApiPermissionDenied{}}
	case "invalid_request_error":
		return &conversationv1.ApiRequestFailed_InvalidRequest{InvalidRequest: &conversationv1.ApiInvalidRequest{}}
	case "request_too_large":
		return &conversationv1.ApiRequestFailed_RequestTooLarge{RequestTooLarge: &conversationv1.ApiRequestTooLarge{}}
	case "not_found_error":
		return &conversationv1.ApiRequestFailed_NotFound{NotFound: &conversationv1.ApiNotFound{}}
	case "api_error", "internal_server_error":
		return &conversationv1.ApiRequestFailed_Internal{Internal: &conversationv1.ApiInternal{}}
	case "billing_error":
		return &conversationv1.ApiRequestFailed_BillingError{BillingError: &conversationv1.ApiBillingError{}}
	case "oauth_org_not_allowed":
		return &conversationv1.ApiRequestFailed_OauthOrgNotAllowed{OauthOrgNotAllowed: &conversationv1.ApiOauthOrgNotAllowed{}}
	case "max_output_tokens":
		return &conversationv1.ApiRequestFailed_MaxOutputTokens{MaxOutputTokens: &conversationv1.ApiMaxOutputTokens{}}
	default:
		// A CONNECTION failure has no vendor type at all — the vendor records a
		// `connection` object instead. It is the API's own internal failure from
		// this reader's side, but claiming a modeled kind for it would be a
		// classification nobody made, so it is named rather than guessed.
		if kind == "" {
			kind = connectionKind(detail)
		}
		return &conversationv1.ApiRequestFailed_Unmodeled{Unmodeled: &conversationv1.ApiUnmodeledError{Type: kind}}
	}
}

// connectionKind names a transport failure by the vendor's own connection code.
func connectionKind(detail map[string]any) string {
	if connection := obj(detail["connection"]); connection != nil {
		return "connection/" + firstNonEmpty(str(connection["code"]), "unknown")
	}
	return "unclassified"
}

// inferKindFromStatus reads the vendor's numeric status where it gave one and no
// type string. The mapping is the API's own documented status-to-type pairing.
func inferKindFromStatus(detail map[string]any) string {
	switch int(number(pick(detail, "status", "statusCode", "code"))) {
	case 400:
		return "invalid_request_error"
	case 401:
		return "authentication_error"
	case 403:
		return "permission_error"
	case 404:
		return "not_found_error"
	case 413:
		return "request_too_large"
	case 429:
		return "rate_limit_error"
	case 500:
		return "api_error"
	case 529:
		return "overloaded_error"
	default:
		return ""
	}
}

// apiRetry reads the vendor's retry schedule off a recorded failure: the retry
// it makes next, how many it allows, and when the next starts (the failure's
// own instant plus the delay it stated). Nil when the record states no
// schedule; a record stating only part of one is stated whole or not at all,
// because a countdown to a guessed instant would be a guess drawn as a fact.
func apiRetry(record map[string]any, atMs int64) *conversationv1.ApiRetry {
	delay := retryAfterMs(record)
	attempt, hasAttempt := record["retryAttempt"].(float64)
	limit, hasLimit := record["maxRetries"].(float64)
	if delay == nil || !hasAttempt || !hasLimit || atMs == 0 {
		return nil
	}
	return &conversationv1.ApiRetry{
		Attempt:         uint32(attempt),
		MaxRetries:      uint32(limit),
		NextAttemptAtMs: atMs + *delay,
	}
}

// retryAfterMs reads the vendor's retry hint. UNSET means it said nothing, which
// is different from "retry now".
func retryAfterMs(record map[string]any) *int64 {
	raw, ok := record["retryInMs"]
	if !ok || raw == nil {
		return nil
	}
	f, ok := raw.(float64)
	if !ok {
		return nil
	}
	ms := int64(f)
	return &ms
}

// setAPIKind assigns one built arm onto the failure. Every arm this converter can
// build is listed; an unhandled one panics rather than producing a failure with
// no kind at all, which the validation invariant forbids.
func setAPIKind(failed *conversationv1.ApiRequestFailed, kind apiKind) {
	switch k := kind.(type) {
	case *conversationv1.ApiRequestFailed_RateLimited:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_Overloaded:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_AuthenticationFailed:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_PermissionDenied:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_InvalidRequest:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_RequestTooLarge:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_NotFound:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_Internal:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_BillingError:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_OauthOrgNotAllowed:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_MaxOutputTokens:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_Unmodeled:
		failed.Kind = k
	default:
		panic("convert: unhandled api error kind")
	}
}
