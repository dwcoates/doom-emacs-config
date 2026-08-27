// Package healthcheck implements the one-shot correlated shim-store health
// probe used by agent-shim-doctor.
//
// ITS PROTOCOL IS GONE. The probe owned the client half of protocol.v1
// HealthCheck/HealthStatus, and the store.v1 redesign deleted both messages
// without minting a replacement: the store's Connect service
// (store/v1/service.proto) declares seven rpcs and none of them is a health
// verb. The JSON Result contract and the exit-code vocabulary are kept —
// agent-shim-doctor parses them, and they are pure Go rather than proto — but
// Probe now REFUSES LOUDLY instead of dialing a socket for a message no peer
// can answer. Inferring readiness from socket presence alone is exactly what
// this package exists to refuse, so no such fallback is substituted.
package healthcheck

import (
	"time"

	"agentrepl/shim-store/internal/logging"
)

const (
	ExitOK                  = 0
	ExitUsage               = 2
	ExitMissingSocket       = 10
	ExitConnectFailure      = 11
	ExitWriteFailure        = 12
	ExitTimeout             = 13
	ExitDecodeFailure       = 14
	ExitMismatchedRequestID = 15
	ExitUnhealthyResponse   = 16
	ExitClientFailure       = 17
)

const (
	FailureMissingSocket       = "missing_socket"
	FailureConnectFailure      = "connect_failure"
	FailureWriteFailure        = "write_failure"
	FailureTimeout             = "timeout"
	FailureDecodeFailure       = "decode_failure"
	FailureMismatchedRequestID = "mismatched_request_id"
	FailureUnhealthyResponse   = "unhealthy_response"
	FailureClientFailure       = "client_failure"
)

// Config is the complete, explicit health-probe input.  The request ID is a
// correlation invariant: an empty ID is invalid rather than a request the
// client can safely send.
type Config struct {
	SocketPath string
	RequestID  string
	Timeout    time.Duration
}

// Result is the JSON contract written by the shim-store health-check mode.
// It is intentionally independent of internal Go errors so doctor can report
// the exact probe outcome without parsing human-readable text.
type Result struct {
	RequestID    string `json:"request_id"`
	LatencyMS    int64  `json:"latency_ms"`
	Component    string `json:"component"`
	Healthy      bool   `json:"healthy"`
	FailureClass string `json:"failure_class"`
	Reason       string `json:"reason"`
}

// ProbeUnavailableReason is the single account every probe now returns. It
// names the deleted protocol rather than a transport symptom, so an operator
// reading doctor's output is not sent hunting a socket that is fine.
const ProbeUnavailableReason = "shim-store health probe is unavailable: protocol.v1 HealthCheck/HealthStatus were deleted by the store.v1 redesign and store/v1/service.proto declares no health rpc; readiness is NOT inferred from socket presence"

// Probe reports the probe's unavailability.
//
// Config is still validated first, so a malformed doctor invocation is still
// reported as usage rather than being masked by the protocol gap.
func Probe(config Config, log *logging.Logger) (Result, int) {
	result := Result{RequestID: config.RequestID}
	finish := func(exitCode int, failureClass, reason string) (Result, int) {
		result.Component = ""
		result.Healthy = false
		result.FailureClass = failureClass
		result.Reason = reason
		log.Log(logging.Fields{Component: "store", Socket: config.SocketPath, RequestID: config.RequestID, Operation: "health-check", Level: "error"},
			"health probe outcome exit=%d class=%q healthy=false latency_ms=0 reason=%q", exitCode, failureClass, reason)
		return result, exitCode
	}
	if config.SocketPath == "" {
		return finish(ExitUsage, "usage", "socket path is required")
	}
	if config.RequestID == "" {
		return finish(ExitUsage, "usage", "health request id is required")
	}
	if config.Timeout <= 0 {
		return finish(ExitUsage, "usage", "health timeout must be positive")
	}
	return finish(ExitClientFailure, FailureClientFailure, ProbeUnavailableReason)
}
