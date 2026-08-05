// Package healthcheck implements the one-shot correlated shim-store health
// probe used by agent-shim-doctor.  It owns the client side of the existing
// core.v1 HealthCheck/HealthStatus protocol; it does not infer readiness from
// socket presence alone.
package healthcheck

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"time"

	"agentrepl/shim-store/internal/logging"
	"agentrepl/wire"

	corev1 "agentrepl/proto/agentshim/core/v1"
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
)

const (
	FailureMissingSocket       = "missing_socket"
	FailureConnectFailure      = "connect_failure"
	FailureWriteFailure        = "write_failure"
	FailureTimeout             = "timeout"
	FailureDecodeFailure       = "decode_failure"
	FailureMismatchedRequestID = "mismatched_request_id"
	FailureUnhealthyResponse   = "unhealthy_response"
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

type deps struct {
	stat func(string) (os.FileInfo, error)
	dial func(context.Context, string) (net.Conn, error)
	now  func() time.Time
}

func productionDeps() deps {
	dialer := &net.Dialer{}
	return deps{
		stat: os.Stat,
		dial: func(ctx context.Context, socketPath string) (net.Conn, error) {
			return dialer.DialContext(ctx, "unix", socketPath)
		},
		now: time.Now,
	}
}

// Probe sends exactly one HealthCheck and accepts only the correlated healthy
// HealthStatus response.  Each terminal result is logged once by this client,
// which owns transport and protocol-result classification.
func Probe(config Config, log *logging.Logger) (Result, int) {
	return probe(config, log, productionDeps())
}

func probe(config Config, log *logging.Logger, d deps) (Result, int) {
	started := d.now()
	result := Result{RequestID: config.RequestID}
	finish := func(exitCode int, failureClass, reason, component string, healthy bool) (Result, int) {
		result.LatencyMS = d.now().Sub(started).Milliseconds()
		result.Component = component
		result.Healthy = healthy
		result.FailureClass = failureClass
		result.Reason = reason
		level := "info"
		if exitCode != ExitOK {
			level = "error"
		}
		log.Log(logging.Fields{Component: "store", Socket: config.SocketPath, RequestID: config.RequestID, Operation: "health-check", Level: level},
			"health probe outcome exit=%d class=%q healthy=%t component=%q latency_ms=%d reason=%q", exitCode, failureClass, healthy, component, result.LatencyMS, reason)
		return result, exitCode
	}

	if config.SocketPath == "" {
		return finish(ExitUsage, "usage", "socket path is required", "", false)
	}
	if config.RequestID == "" {
		return finish(ExitUsage, "usage", "health request id is required", "", false)
	}
	if config.Timeout <= 0 {
		return finish(ExitUsage, "usage", "health timeout must be positive", "", false)
	}
	if _, err := d.stat(config.SocketPath); err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return finish(ExitMissingSocket, FailureMissingSocket, err.Error(), "", false)
		}
		return finish(ExitConnectFailure, FailureConnectFailure, fmt.Sprintf("stat socket: %v", err), "", false)
	}

	ctx, cancel := context.WithTimeout(context.Background(), config.Timeout)
	defer cancel()
	conn, err := d.dial(ctx, config.SocketPath)
	if err != nil {
		if isTimeout(err) || errors.Is(ctx.Err(), context.DeadlineExceeded) {
			return finish(ExitTimeout, FailureTimeout, err.Error(), "", false)
		}
		return finish(ExitConnectFailure, FailureConnectFailure, err.Error(), "", false)
	}
	defer conn.Close()
	if err := conn.SetDeadline(started.Add(config.Timeout)); err != nil {
		return finish(ExitConnectFailure, FailureConnectFailure, fmt.Sprintf("set probe deadline: %v", err), "", false)
	}
	if err := wire.WriteAny(conn, &corev1.HealthCheck{RequestId: config.RequestID}); err != nil {
		if isTimeout(err) {
			return finish(ExitTimeout, FailureTimeout, err.Error(), "", false)
		}
		return finish(ExitWriteFailure, FailureWriteFailure, err.Error(), "", false)
	}
	message, err := wire.ReadAny(conn)
	if err != nil {
		if isTimeout(err) {
			return finish(ExitTimeout, FailureTimeout, err.Error(), "", false)
		}
		return finish(ExitDecodeFailure, FailureDecodeFailure, err.Error(), "", false)
	}
	status, ok := message.(*corev1.HealthStatus)
	if !ok {
		return finish(ExitDecodeFailure, FailureDecodeFailure, fmt.Sprintf("unexpected response type %T", message), "", false)
	}
	if status.GetRequestId() != config.RequestID {
		return finish(ExitMismatchedRequestID, FailureMismatchedRequestID,
			fmt.Sprintf("response request_id %q does not match request_id %q", status.GetRequestId(), config.RequestID), status.GetComponent(), false)
	}
	if !status.GetHealthy() {
		return finish(ExitUnhealthyResponse, FailureUnhealthyResponse, status.GetReason(), status.GetComponent(), false)
	}
	if status.GetComponent() == "" {
		return finish(ExitUnhealthyResponse, FailureUnhealthyResponse, status.GetReason(), "", false)
	}
	return finish(ExitOK, "", status.GetReason(), status.GetComponent(), true)
}

func isTimeout(err error) bool {
	if errors.Is(err, context.DeadlineExceeded) || errors.Is(err, os.ErrDeadlineExceeded) {
		return true
	}
	var networkErr net.Error
	return errors.As(err, &networkErr) && networkErr.Timeout()
}
