// Package figures holds the daemon's canonical numeric formatters. ONE
// FORMATTER PER FIGURE, daemon-wide: a token count drawn in the feed, in the
// footer and in the topbar is the same string, because a second copy of the
// rounding rules is a second authority on the same number, and two
// authorities eventually disagree.
package figures

import (
	"strconv"
	"strings"
)

// Tokens renders a token count: bare digits under a thousand, then scaled by
// the RENDERED unit with exactly one fractional digit and a trailing ".0"
// trimmed ("1k", "1.2k", "182.4k", "1.2M").
//
// THE UNIT IS CHOSEN BY THE RENDERED VALUE, not by the raw count: a count
// whose thousands-rendering would read "1000k" renders "1M" instead, so no
// drawn figure ever names a magnitude it has already outgrown.
//
// The formatter emits the FIGURE ONLY; suffixes ("18.2k in", "12.4k tok") are
// composed by the call site.
func Tokens(n uint64) string {
	if n < 1_000 {
		return strconv.FormatUint(n, 10)
	}
	if scaled, ok := scale(n, 1_000); ok {
		return scaled + "k"
	}
	scaled, _ := scale(n, 1_000_000)
	return scaled + "M"
}

// scale renders n/unit to one fractional digit, trims a trailing ".0", and
// reports false when the RENDERED value reached the next magnitude — which is
// the signal to render in that larger unit instead.
func scale(n uint64, unit float64) (string, bool) {
	rendered := strconv.FormatFloat(float64(n)/unit, 'f', 1, 64)
	value, err := strconv.ParseFloat(rendered, 64)
	if err != nil || value >= 1_000 {
		return "", false
	}
	return strings.TrimSuffix(rendered, ".0"), true
}
