package dlog

import (
	"errors"
	"reflect"
	"testing"
)

func TestMergeOverlaysExtraOnACopyOfBase(t *testing.T) {
	tests := []struct {
		name  string
		base  Context
		extra Context
		want  Context
	}{
		{name: "disjoint keys are both kept", base: Context{"a": 1}, extra: Context{"b": 2}, want: Context{"a": 1, "b": 2}},
		{name: "a shared key takes extra's value", base: Context{"a": 1}, extra: Context{"a": 2}, want: Context{"a": 2}},
		{name: "a nil base is no fields", extra: Context{"b": 2}, want: Context{"b": 2}},
		{name: "a nil extra is no change", base: Context{"a": 1}, want: Context{"a": 1}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Merge(tt.base, tt.extra)

			// Assert.
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Merge = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestMergeNeverWritesThroughToItsArguments(t *testing.T) {
	// Arrange.
	base, extra := Context{"a": 1}, Context{"b": 2}

	// Act.
	got := Merge(base, extra)
	got["c"] = 3

	// Assert.
	if !reflect.DeepEqual(base, Context{"a": 1}) || !reflect.DeepEqual(extra, Context{"b": 2}) {
		t.Fatalf("base = %v, extra = %v; want both untouched", base, extra)
	}
}

func TestWithCauseStampsTheErrorOnACopy(t *testing.T) {
	// Arrange.
	fields := Context{"workspace": "ws-1"}

	// Act.
	got := WithCause(fields, errors.New("the store is gone"))

	// Assert.
	if !reflect.DeepEqual(got, Context{"workspace": "ws-1", "cause": "the store is gone"}) {
		t.Fatalf("WithCause = %v, want the fields and the cause", got)
	}
	if _, stamped := fields["cause"]; stamped {
		t.Fatalf("fields = %v, want the caller's map untouched", fields)
	}
}
