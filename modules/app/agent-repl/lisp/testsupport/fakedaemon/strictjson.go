package main

import (
	"errors"
	"fmt"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"
)

// connect-go's built-in JSON codec unmarshals with DiscardUnknown: true, so
// a client that misspells a field gets a silent success.  That would destroy
// the whole point of this fake: the round-trip through the REAL generated
// types is the check on elisp's encoders, and an unknown field must be
// refused exactly as Go protojson refuses it by default.
//
// strictJSONCodec restores protojson's default strictness and is registered
// under connect's JSON codec names so it replaces the built-in one.
type strictJSONCodec struct{ name string }

const (
	strictJSONName            = "json"
	strictJSONCharsetUTF8Name = "json; charset=utf-8"
)

func (c *strictJSONCodec) Name() string { return c.name }

func (c *strictJSONCodec) IsBinary() bool { return false }

func (c *strictJSONCodec) Marshal(message any) ([]byte, error) {
	m, ok := message.(proto.Message)
	if !ok {
		return nil, fmt.Errorf("fakedaemon strict json codec: %T is not a proto message", message)
	}
	return protojson.MarshalOptions{}.Marshal(m)
}

func (c *strictJSONCodec) MarshalAppend(dst []byte, message any) ([]byte, error) {
	m, ok := message.(proto.Message)
	if !ok {
		return nil, fmt.Errorf("fakedaemon strict json codec: %T is not a proto message", message)
	}
	return protojson.MarshalOptions{}.MarshalAppend(dst, m)
}

func (c *strictJSONCodec) Unmarshal(data []byte, message any) error {
	m, ok := message.(proto.Message)
	if !ok {
		return fmt.Errorf("fakedaemon strict json codec: %T is not a proto message", message)
	}
	if len(data) == 0 {
		return errors.New("fakedaemon strict json codec: empty body")
	}
	// DiscardUnknown stays FALSE: an unknown field is a contract breach.
	if err := (protojson.UnmarshalOptions{}).Unmarshal(data, m); err != nil {
		logWarn("fakedaemon.codec.unknown-or-invalid-field", "refused a request body the schema rejects",
			map[string]any{"message": string(m.ProtoReflect().Descriptor().FullName()), "error": err.Error()})
		return fmt.Errorf("unmarshal into %s: %w", m.ProtoReflect().Descriptor().FullName(), err)
	}
	return nil
}

func (c *strictJSONCodec) MarshalStable(message any) ([]byte, error) {
	return c.Marshal(message)
}

// strictJSONOptions replaces connect's JSON codec under both of the names it
// dispatches on (`json` and the charset-qualified spelling).
func strictJSONOptions() []connect.HandlerOption {
	return []connect.HandlerOption{
		connect.WithCodec(&strictJSONCodec{name: strictJSONName}),
		connect.WithCodec(&strictJSONCodec{name: strictJSONCharsetUTF8Name}),
	}
}
