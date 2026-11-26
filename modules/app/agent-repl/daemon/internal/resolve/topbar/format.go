package topbar

import (
	"fmt"
	"strings"

	"agent-repld/internal/figures"
)

// formatTokens renders a token count the topbar carries as a SIGNED figure.
// The rendering itself is figures.Tokens — the one daemon-wide token format —
// and this adapter only settles what a negative count means: the topbar's
// sources subtract, and a negative remainder draws as nothing left rather than
// as a figure no reader could act on.
func formatTokens(n int64) string {
	if n < 0 {
		n = 0
	}
	return figures.Tokens(uint64(n))
}

// trimZero drops a trailing ".0" so "18.0k" draws as "18k".
func trimZero(s string) string {
	if len(s) > 2 && s[len(s)-2:] == ".0" {
		return s[:len(s)-2]
	}
	return s
}

// truncate shortens a composed line. The daemon truncates, never the client.
func truncate(s string, max int) string {
	if max <= 1 || len([]rune(s)) <= max {
		return s
	}
	r := []rune(s)
	return string(r[:max-1]) + "…"
}

// percentOf renders a whole-percent share of a basis, empty when no basis
// exists to divide by.
func percentOf(tokens, basis int64) string {
	if basis <= 0 {
		return ""
	}
	return fmt.Sprintf("%d%%", (tokens*100+basis/2)/basis)
}

// permilleOf renders a share in permille (0..1000), already rounded, or false
// when no basis applies.
func permilleOf(tokens, basis int64) (int32, bool) {
	if basis <= 0 {
		return 0, false
	}
	value := (tokens*1000 + basis/2) / basis
	if value > 1000 {
		value = 1000
	}
	if value < 0 {
		value = 0
	}
	return int32(value), true
}

// joinNonEmpty joins the parts that carry anything, with the separator. It is
// how the composed lines avoid drawing a bare separator around a fact the
// producer did not state.
func joinNonEmpty(sep string, parts ...string) string {
	kept := make([]string, 0, len(parts))
	for _, part := range parts {
		if strings.TrimSpace(part) != "" {
			kept = append(kept, part)
		}
	}
	return strings.Join(kept, sep)
}
