package desktopnotify

// Italic answers s with every ASCII letter in Unicode's Mathematical
// Sans-Serif Italic, the one italic a banner can carry: neither
// terminal-notifier nor every notify-send server renders markup, so the slant
// is in the characters themselves. Every other character, digits included, is
// left as it is (the block has no italic digits).
func Italic(s string) string {
	out := make([]rune, 0, len(s))
	for _, r := range s {
		switch {
		case r >= 'A' && r <= 'Z':
			out = append(out, sansItalicCapitalA+(r-'A'))
		case r >= 'a' && r <= 'z':
			out = append(out, sansItalicSmallA+(r-'a'))
		default:
			out = append(out, r)
		}
	}
	return string(out)
}

// The Mathematical Sans-Serif Italic block's first capital and first small
// letter. The block, unlike the serif italic one, has no holes.
const (
	sansItalicCapitalA = '\U0001D608'
	sansItalicSmallA   = '\U0001D622'
)
