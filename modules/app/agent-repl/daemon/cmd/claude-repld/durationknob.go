package main

import (
	"fmt"
	"strings"
	"time"
)

// parsePositiveDuration reads one duration knob's value, naming the knob in
// its refusal. It is the ONE parse every positive-duration knob in this
// command goes through, so every such knob refuses a malformed or
// non-positive value in the same words: a knob that silently did nothing
// would make the run it was set for report a window it never used.
//
// The caller decides what an UNSET knob means (a default, or zero for the
// component's own default) before calling this; the value here is always one
// somebody set.
func parsePositiveDuration(name, value string) (time.Duration, error) {
	d, err := time.ParseDuration(value)
	if err != nil {
		return 0, fmt.Errorf("claude-repld: %s=%q is not a duration: %w", name, value, err)
	}
	if d <= 0 {
		return 0, fmt.Errorf("claude-repld: %s=%q is not a positive duration", name, value)
	}
	return d, nil
}

// resolveDurationKnob is a positive-duration knob whose UNSET value is a
// default: blank answers fallback, anything else goes through
// parsePositiveDuration.
func resolveDurationKnob(name, value string, fallback time.Duration) (time.Duration, error) {
	if strings.TrimSpace(value) == "" {
		return fallback, nil
	}
	return parsePositiveDuration(name, value)
}
