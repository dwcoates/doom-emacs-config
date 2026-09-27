package main

import (
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
)

// Control operations. The integration harness scripts a running fake through
// a newline-delimited JSON protocol on `<uds>.ctl`; one Command per line, one
// Reply per line.
const (
	OpPushSessionUpdate = "push_session_update"
	OpPushAgentFrame    = "push_agent_frame"
	OpPushUserPrompt    = "push_user_prompt"
	OpPushBash          = "push_bash"
	OpAnswer            = "answer"
	OpExpect            = "expect"
	OpExit              = "exit"
	OpHang              = "hang"
	OpUnhang            = "unhang"
	OpDropStream        = "drop_stream"
	OpInfo              = "info"
	OpCount             = "count"
	// OpSetLiveWork states the live membership later re-announcements carry;
	// its payload is a SessionStarted whose live_work is taken.
	OpSetLiveWork = "set_live_work"
	// OpSilenceBash makes every later WatchBash open for Work go unanswered.
	OpSilenceBash = "silence_bash"
)

// Stream names accepted by drop_stream.
const (
	StreamSession = "session"
	StreamAgent   = "agent"
	StreamBash    = "bash"
)

// Command is one scripted control instruction.
type Command struct {
	Op string `json:"op"`
	// RPC names the shim.v1 verb for `answer`, `expect` and `count`.
	RPC string `json:"rpc,omitempty"`
	// Agent addresses push_agent_frame; empty means the session's main agent.
	Agent string `json:"agent,omitempty"`
	// Pointer overrides the HistoryPointer push_agent_frame delivers the frame
	// at. Empty mints the next per-stream pointer, which is the normal case.
	//
	// IT EXISTS BECAUSE ONE STORE ROW CAN BE DELIVERED TWICE. The shim's stream
	// plane and the sidecar's file plane write the SAME store entry for one
	// fact, and every write of an entry is delivered on the agent's tail — at
	// the entry's OWN position both times, because an upsert supersedes a row's
	// content and leaves its place in the book. A fake that always minted a
	// fresh pointer could not express that, so a test for it would be testing a
	// shape production never produces.
	Pointer string `json:"pointer,omitempty"`
	// Turn stamps push_agent_frame's entry with the turn it was produced
	// within (HistoryEntryAt.turn), as the real shim stamps every row of an
	// open turn. Empty delivers it unstamped, which is how pre-contract data
	// reaches the daemon.
	Turn string `json:"turn,omitempty"`
	// Work addresses push_bash (a DetachedWorkId value).
	Work string `json:"work,omitempty"`
	// Stream names the stream family drop_stream severs.
	Stream string `json:"stream,omitempty"`
	// Code is the process exit status for `exit`.
	Code int `json:"code,omitempty"`
	// Payload is the base64 of a binary-encoded protobuf message whose type is
	// implied by the op (and, for `answer`, by RPC).
	Payload string `json:"payload,omitempty"`
	// Fail makes `answer` queue a transport-level error instead of a response.
	Fail string `json:"fail,omitempty"`
	// Stderr is the failure evidence an `exit` leaves behind.
	Stderr string `json:"stderr,omitempty"`
	// TimeoutMS bounds a waiting op (`expect`) so a request that never arrives
	// fails by name instead of hanging the suite. Zero means unbounded.
	TimeoutMS int `json:"timeout_ms,omitempty"`
}

// Reply is the fake's answer to one Command.
type Reply struct {
	OK      bool   `json:"ok"`
	Error   string `json:"error,omitempty"`
	Payload string `json:"payload,omitempty"`
	Count   int    `json:"count,omitempty"`
	Info    *Info  `json:"info,omitempty"`
}

// Info reports what the fake was launched with, so the suite can assert the
// spawn contract without reading the daemon's own logs.
type Info struct {
	Argv            []string          `json:"argv"`
	Env             map[string]string `json:"env"`
	Cwd             string            `json:"cwd"`
	PID             int               `json:"pid"`
	Listen          string            `json:"listen"`
	StoreSocket     string            `json:"store_socket"`
	LogFD           int               `json:"log_fd"`
	Fake            bool              `json:"fake"`
	WorkspaceLock   string            `json:"workspace_lock"`
	SessionLock     string            `json:"session_lock"`
	VendorSessionID string            `json:"vendor_session_id"`
}

// Payload decodes the command's base64 payload.
func (c Command) Bytes() ([]byte, error) {
	if c.Payload == "" {
		return nil, nil
	}
	return base64.StdEncoding.DecodeString(c.Payload)
}

// ErrUnknownOp reports a control line naming an operation the fake does not
// implement. It is never silently ignored.
var ErrUnknownOp = errors.New("fakeshim: unknown control op")

// ParseCommand decodes and validates one control line. Every op's required
// fields are checked here so a malformed script fails loudly at the seam
// rather than as a mysterious timeout later.
func ParseCommand(line []byte) (Command, error) {
	var c Command
	dec := json.NewDecoder(strings.NewReader(string(line)))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&c); err != nil {
		return Command{}, fmt.Errorf("fakeshim: malformed control line: %w", err)
	}
	switch c.Op {
	case OpPushSessionUpdate:
		if c.Payload == "" {
			return Command{}, fmt.Errorf("%s: payload is required", c.Op)
		}
	case OpPushAgentFrame:
		if c.Payload == "" {
			return Command{}, fmt.Errorf("%s: payload is required", c.Op)
		}
	case OpPushUserPrompt:
		if c.Payload == "" {
			return Command{}, fmt.Errorf("%s: payload is required", c.Op)
		}
	case OpPushBash:
		if c.Work == "" {
			return Command{}, fmt.Errorf("%s: work is required", c.Op)
		}
		if c.Payload == "" {
			return Command{}, fmt.Errorf("%s: payload is required", c.Op)
		}
	case OpAnswer:
		if c.RPC == "" {
			return Command{}, fmt.Errorf("%s: rpc is required", c.Op)
		}
		if !answerable(c.RPC) {
			return Command{}, fmt.Errorf("%s: rpc %q takes no scripted answer", c.Op, c.RPC)
		}
		if c.Payload == "" && c.Fail == "" {
			return Command{}, fmt.Errorf("%s: payload or fail is required", c.Op)
		}
	case OpExpect, OpCount:
		if c.RPC == "" {
			return Command{}, fmt.Errorf("%s: rpc is required", c.Op)
		}
		if !knownRPC(c.RPC) {
			return Command{}, fmt.Errorf("%s: unknown rpc %q", c.Op, c.RPC)
		}
	case OpDropStream:
		switch c.Stream {
		case StreamSession, StreamAgent, StreamBash:
		default:
			return Command{}, fmt.Errorf("%s: unknown stream %q", c.Op, c.Stream)
		}
	case OpSetLiveWork:
		// An EMPTY payload is a well-formed SessionStarted naming nothing
		// live, which is exactly what stating an empty membership encodes to.
	case OpSilenceBash:
		if c.Work == "" {
			return Command{}, fmt.Errorf("%s: work is required", c.Op)
		}
	case OpExit:
		if c.Code < 0 || c.Code > 125 {
			return Command{}, fmt.Errorf("%s: exit code %d out of range", c.Op, c.Code)
		}
	case OpHang, OpUnhang, OpInfo:
		// no arguments
	case "":
		return Command{}, errors.New("fakeshim: control line has no op")
	default:
		return Command{}, fmt.Errorf("%w: %q", ErrUnknownOp, c.Op)
	}
	return c, nil
}
