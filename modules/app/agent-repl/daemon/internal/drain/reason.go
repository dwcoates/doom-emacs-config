package drain

import (
	"fmt"
	"strconv"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"google.golang.org/protobuf/encoding/protojson"

	"claude-repld/internal/wsm"
)

// EncodeReason renders a typed DrainReason for the WSM row. The reason is
// stored WHOLE rather than as an arm name, because DrainReasonOperator carries
// a note that an arm name alone would drop — and a drain banner that lost the
// operator's own sentence across a restart would be a different fact.
func EncodeReason(reason *agentreplv1.DrainReason) (string, error) {
	if reason == nil || reason.GetKind() == nil {
		return "", fmt.Errorf("drain: a drain reason states its arm")
	}
	if op := reason.GetOperator(); op != nil && op.GetNote() == "" {
		return "", fmt.Errorf("drain: an operator drain reason states a non-blank note")
	}
	out, err := protojson.Marshal(reason)
	if err != nil {
		return "", fmt.Errorf("drain: encode the drain reason: %w", err)
	}
	return string(out), nil
}

// DecodeReason reads back what EncodeReason wrote. A row that will not decode
// is an error rather than an untyped fallback: the banner names the arm, and a
// guessed arm would name the wrong one.
func DecodeReason(stored string) (*agentreplv1.DrainReason, error) {
	var reason agentreplv1.DrainReason
	if err := protojson.Unmarshal([]byte(stored), &reason); err != nil {
		return nil, fmt.Errorf("drain: decode the stored drain reason %q: %w", stored, err)
	}
	if reason.GetKind() == nil {
		return nil, fmt.Errorf("drain: the stored drain reason %q names no arm", stored)
	}
	return &reason, nil
}

// ScheduleID is a schedule's stable identity, derived from the instant it was
// put in force. The holds tray stamps it onto every prompt held for the drain,
// so a hold names the schedule it is waiting on rather than merely "a drain";
// deriving it from SetAt means the id survives a restart without a column.
func ScheduleID(s wsm.DrainSchedule) string {
	return "drain-" + strconv.FormatInt(s.SetAt.UnixNano(), 10)
}

// milliseconds renders an instant as the epoch milliseconds the contract's
// *_ms fields carry.
func milliseconds(at time.Time) int64 { return at.UnixNano() / int64(time.Millisecond) }
