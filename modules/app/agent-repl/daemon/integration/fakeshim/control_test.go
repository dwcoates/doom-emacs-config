package main

import (
	"encoding/base64"
	"errors"
	"strings"
	"testing"
)

func TestParseCommandAcceptsWellFormedOps(t *testing.T) {
	payload := base64.StdEncoding.EncodeToString([]byte{0x01})
	tests := []struct {
		name string
		line string
		want Command
	}{
		{
			name: "push_session_update carries a payload",
			line: `{"op":"push_session_update","payload":"` + payload + `"}`,
			want: Command{Op: OpPushSessionUpdate, Payload: payload},
		},
		{
			name: "push_agent_frame addresses an agent",
			line: `{"op":"push_agent_frame","agent":"sub-1","payload":"` + payload + `"}`,
			want: Command{Op: OpPushAgentFrame, Agent: "sub-1", Payload: payload},
		},
		{
			name: "push_agent_frame may state the entry's place",
			line: `{"op":"push_agent_frame","place_ms":1700000000000,"payload":"` + payload + `"}`,
			want: Command{Op: OpPushAgentFrame, PlaceMs: 1_700_000_000_000, Payload: payload},
		},
		{
			name: "push_user_prompt addresses an agent",
			line: `{"op":"push_user_prompt","agent":"sub-1","payload":"` + payload + `"}`,
			want: Command{Op: OpPushUserPrompt, Agent: "sub-1", Payload: payload},
		},
		{
			name: "push_retired carries the retired entry",
			line: `{"op":"push_retired","agent":"sub-1","payload":"` + payload + `"}`,
			want: Command{Op: OpPushRetired, Agent: "sub-1", Payload: payload},
		},
		{
			name: "push_bash addresses a detached work id",
			line: `{"op":"push_bash","work":"w-1","payload":"` + payload + `"}`,
			want: Command{Op: OpPushBash, Work: "w-1", Payload: payload},
		},
		{
			name: "answer queues a response for a verb",
			line: `{"op":"answer","rpc":"StartTurn","payload":"` + payload + `"}`,
			want: Command{Op: OpAnswer, RPC: RPCStartTurn, Payload: payload},
		},
		{
			name: "answer may queue a transport failure instead",
			line: `{"op":"answer","rpc":"StartTurn","fail":"boom"}`,
			want: Command{Op: OpAnswer, RPC: RPCStartTurn, Fail: "boom"},
		},
		{
			name: "expect names a stream verb",
			line: `{"op":"expect","rpc":"WatchSession","timeout_ms":250}`,
			want: Command{Op: OpExpect, RPC: RPCWatchSession, TimeoutMS: 250},
		},
		{
			name: "drop_stream names a stream family",
			line: `{"op":"drop_stream","stream":"session"}`,
			want: Command{Op: OpDropStream, Stream: StreamSession},
		},
		{
			name: "exit carries a status and evidence",
			line: `{"op":"exit","code":9,"stderr":"died"}`,
			want: Command{Op: OpExit, Code: 9, Stderr: "died"},
		},
		{
			name: "hang takes no arguments",
			line: `{"op":"hang"}`,
			want: Command{Op: OpHang},
		},
		{
			name: "info takes no arguments",
			line: `{"op":"info"}`,
			want: Command{Op: OpInfo},
		},
		{
			name: "set_live_work carries a membership",
			line: `{"op":"set_live_work","payload":"` + payload + `"}`,
			want: Command{Op: OpSetLiveWork, Payload: payload},
		},
		{
			name: "set_live_work may state an empty membership",
			line: `{"op":"set_live_work"}`,
			want: Command{Op: OpSetLiveWork},
		},
		{
			name: "silence_reannouncement takes no arguments",
			line: `{"op":"silence_reannouncement"}`,
			want: Command{Op: OpSilenceReannouncement},
		},
		{
			name: "silence_bash addresses a detached work id",
			line: `{"op":"silence_bash","work":"w-1"}`,
			want: Command{Op: OpSilenceBash, Work: "w-1"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			got, err := ParseCommand([]byte(tc.line))

			// Assert
			if err != nil {
				t.Fatalf("ParseCommand(%s) = error %v, want a command", tc.line, err)
			}
			if got != tc.want {
				t.Fatalf("ParseCommand(%s) = %+v, want %+v", tc.line, got, tc.want)
			}
		})
	}
}

func TestParseCommandRejectsMalformedOps(t *testing.T) {
	tests := []struct {
		name    string
		line    string
		wantSub string
	}{
		{name: "not json", line: `{`, wantSub: "malformed control line"},
		{name: "no op", line: `{"rpc":"StartTurn"}`, wantSub: "no op"},
		{name: "unknown op", line: `{"op":"teleport"}`, wantSub: "unknown control op"},
		{name: "unknown field", line: `{"op":"hang","nonsense":1}`, wantSub: "malformed control line"},
		{name: "push_session_update without payload", line: `{"op":"push_session_update"}`, wantSub: "payload is required"},
		{name: "push_user_prompt without payload", line: `{"op":"push_user_prompt"}`, wantSub: "payload is required"},
		{name: "push_retired without payload", line: `{"op":"push_retired"}`, wantSub: "payload is required"},
		{name: "push_bash without work", line: `{"op":"push_bash","payload":"AA=="}`, wantSub: "work is required"},
		{name: "answer without rpc", line: `{"op":"answer","payload":"AA=="}`, wantSub: "rpc is required"},
		{name: "answer for a stream verb", line: `{"op":"answer","rpc":"WatchSession","payload":"AA=="}`, wantSub: "takes no scripted answer"},
		{name: "answer without a body", line: `{"op":"answer","rpc":"StartTurn"}`, wantSub: "payload or fail is required"},
		{name: "expect an unknown verb", line: `{"op":"expect","rpc":"Teleport"}`, wantSub: "unknown rpc"},
		{name: "drop an unknown stream", line: `{"op":"drop_stream","stream":"webapp"}`, wantSub: "unknown stream"},
		{name: "exit out of range", line: `{"op":"exit","code":900}`, wantSub: "out of range"},
		{name: "silence_bash without work", line: `{"op":"silence_bash"}`, wantSub: "work is required"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			_, err := ParseCommand([]byte(tc.line))

			// Assert
			if err == nil {
				t.Fatalf("ParseCommand(%s) = no error, want one naming %q", tc.line, tc.wantSub)
			}
			if !strings.Contains(err.Error(), tc.wantSub) {
				t.Fatalf("ParseCommand(%s) = %v, want an error naming %q", tc.line, err, tc.wantSub)
			}
		})
	}
}

func TestParseCommandUnknownOpIsIdentifiable(t *testing.T) {
	// Arrange / Act
	_, err := ParseCommand([]byte(`{"op":"teleport"}`))

	// Assert
	if !errors.Is(err, ErrUnknownOp) {
		t.Fatalf("ParseCommand unknown op = %v, want it to wrap ErrUnknownOp", err)
	}
}

func TestCommandBytesDecodesPayload(t *testing.T) {
	// Arrange
	cmd := Command{Payload: base64.StdEncoding.EncodeToString([]byte{0xde, 0xad})}

	// Act
	got, err := cmd.Bytes()

	// Assert
	if err != nil {
		t.Fatalf("Bytes() = error %v, want the decoded payload", err)
	}
	if len(got) != 2 || got[0] != 0xde || got[1] != 0xad {
		t.Fatalf("Bytes() = %x, want deadbytes de ad", got)
	}
}

func TestCommandBytesOnEmptyPayloadIsEmpty(t *testing.T) {
	// Arrange
	cmd := Command{Op: OpHang}

	// Act
	got, err := cmd.Bytes()

	// Assert
	if err != nil {
		t.Fatalf("Bytes() = error %v, want no error", err)
	}
	if got != nil {
		t.Fatalf("Bytes() = %v, want nil for an absent payload", got)
	}
}
