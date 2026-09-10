package paint

import (
	"strconv"
	"strings"
)

// The precedence slots ansi_precedence may name. A slot this parser does not
// model is refused at construction rather than silently ignored.
const (
	slotFG        = "fg"
	slotBG        = "bg"
	slotBold      = "bold"
	slotDim       = "dim"
	slotItalic    = "italic"
	slotUnderline = "underline"
	slotStrike    = "strike"
	slotInverse   = "inverse"
)

var attributeSlots = []string{slotBold, slotDim, slotItalic, slotUnderline, slotStrike, slotInverse}

func knownPrecedenceSlot(slot string) bool {
	if slot == slotFG || slot == slotBG {
		return true
	}
	for _, s := range attributeSlots {
		if s == slot {
			return true
		}
	}
	return false
}

// baseColors are the eight SGR base color names, indexed by 30..37 / 40..47.
var baseColors = [8]string{"black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"}

// palette16 is the xterm rendering of the sixteen named colors. It exists only
// to map a 256-color index or a truecolor triple onto the closed inventory of
// sixteen names — the daemon names a color, it never ships an RGB value.
var palette16 = [16][3]int{
	{0, 0, 0}, {205, 0, 0}, {0, 205, 0}, {205, 205, 0},
	{0, 0, 238}, {205, 0, 205}, {0, 205, 205}, {229, 229, 229},
	{127, 127, 127}, {255, 0, 0}, {0, 255, 0}, {255, 255, 0},
	{92, 92, 255}, {255, 0, 255}, {0, 255, 255}, {255, 255, 255},
}

// cubeLevels are the six channel values of the 6x6x6 color cube (indices
// 16..231).
var cubeLevels = [6]int{0, 95, 135, 175, 215, 255}

// colorName renders one of the sixteen palette indices as its inventory name.
func colorName(index int) string {
	if index < 8 {
		return baseColors[index]
	}
	return "bright-" + baseColors[index-8]
}

// nearest16 maps an RGB triple onto the closest of the sixteen named colors by
// squared euclidean distance.
func nearest16(r, g, b int) string {
	best, bestDist := 0, 1<<30
	for i, c := range palette16 {
		dr, dg, db := r-c[0], g-c[1], b-c[2]
		if d := dr*dr + dg*dg + db*db; d < bestDist {
			best, bestDist = i, d
		}
	}
	return colorName(best)
}

// color256 maps a 256-color index onto one of the sixteen named colors.
func color256(n int) string {
	switch {
	case n < 0 || n > 255:
		return ""
	case n < 16:
		return colorName(n)
	case n < 232:
		n -= 16
		return nearest16(cubeLevels[n/36], cubeLevels[(n/6)%6], cubeLevels[n%6])
	default:
		v := 8 + 10*(n-232)
		return nearest16(v, v, v)
	}
}

// sgrState is the SGR attributes in force. Empty color names mean the
// terminal's default, which carries no class.
type sgrState struct {
	fg, bg string
	attrs  map[string]bool
}

func newSGRState() *sgrState { return &sgrState{attrs: map[string]bool{}} }

func (s *sgrState) reset() {
	s.fg, s.bg = "", ""
	s.attrs = map[string]bool{}
}

// class answers the one class this state paints, taking the strongest slot the
// precedence names. A state with nothing in force paints plain.
func (s *sgrState) class(rank []string) string {
	for _, slot := range rank {
		switch slot {
		case slotFG:
			if s.fg != "" {
				return "ansi-fg-" + s.fg
			}
		case slotBG:
			if s.bg != "" {
				return "ansi-bg-" + s.bg
			}
		default:
			if s.attrs[slot] {
				return "ansi-" + slot
			}
		}
	}
	return ""
}

// apply folds one SGR parameter list into the state. An unmodeled parameter is
// dropped: it produces no class and never invents one.
func (s *sgrState) apply(params []int) {
	if len(params) == 0 {
		s.reset()
		return
	}
	for i := 0; i < len(params); i++ {
		n := params[i]
		switch {
		case n == 0:
			s.reset()
		case n == 1:
			s.attrs[slotBold] = true
		case n == 2:
			s.attrs[slotDim] = true
		case n == 3:
			s.attrs[slotItalic] = true
		case n == 4:
			s.attrs[slotUnderline] = true
		case n == 7:
			s.attrs[slotInverse] = true
		case n == 9:
			s.attrs[slotStrike] = true
		case n == 21 || n == 22:
			delete(s.attrs, slotBold)
			delete(s.attrs, slotDim)
		case n == 23:
			delete(s.attrs, slotItalic)
		case n == 24:
			delete(s.attrs, slotUnderline)
		case n == 27:
			delete(s.attrs, slotInverse)
		case n == 29:
			delete(s.attrs, slotStrike)
		case n >= 30 && n <= 37:
			s.fg = baseColors[n-30]
		case n == 38:
			consumed, name := extendedColor(params[i+1:])
			i += consumed
			if name != "" {
				s.fg = name
			}
		case n == 39:
			s.fg = ""
		case n >= 40 && n <= 47:
			s.bg = baseColors[n-40]
		case n == 48:
			consumed, name := extendedColor(params[i+1:])
			i += consumed
			if name != "" {
				s.bg = name
			}
		case n == 49:
			s.bg = ""
		case n >= 90 && n <= 97:
			s.fg = "bright-" + baseColors[n-90]
		case n >= 100 && n <= 107:
			s.bg = "bright-" + baseColors[n-100]
		}
	}
}

// extendedColor reads a 38/48 continuation: `5;<index>` or `2;<r>;<g>;<b>`. It
// answers how many parameters it consumed and the mapped color name, empty
// when the continuation is malformed.
func extendedColor(rest []int) (int, string) {
	if len(rest) == 0 {
		return 0, ""
	}
	switch rest[0] {
	case 5:
		if len(rest) < 2 {
			return len(rest), ""
		}
		return 2, color256(rest[1])
	case 2:
		if len(rest) < 4 {
			return len(rest), ""
		}
		return 4, nearest16(rest[1], rest[2], rest[3])
	default:
		return 1, ""
	}
}

// parseParams splits an SGR parameter string. A parameter that is not a number
// is read as 0, which is how terminals read an empty one.
func parseParams(body string) []int {
	if body == "" {
		return nil
	}
	fields := strings.Split(body, ";")
	params := make([]int, 0, len(fields))
	for _, f := range fields {
		if f == "" {
			params = append(params, 0)
			continue
		}
		// A colon-subparameter form (38:5:1) reduces to its first field; the
		// rest is a form this parser does not model.
		if idx := strings.IndexByte(f, ':'); idx >= 0 {
			for _, sub := range strings.Split(f, ":") {
				n, err := strconv.Atoi(sub)
				if err != nil {
					n = 0
				}
				params = append(params, n)
			}
			continue
		}
		n, err := strconv.Atoi(f)
		if err != nil {
			n = 0
		}
		params = append(params, n)
	}
	return params
}

// ParseANSI turns process output carrying SGR escape sequences into spans.
func (p *painter) ParseANSI(text string) (Spans, error) {
	state := newSGRState()
	spans := Spans{}
	var run strings.Builder
	class := ""

	flush := func() error {
		if run.Len() == 0 {
			return nil
		}
		next, err := p.emit(spans, run.String(), class)
		if err != nil {
			return err
		}
		spans = next
		run.Reset()
		return nil
	}

	for i := 0; i < len(text); {
		if text[i] != 0x1b {
			run.WriteByte(text[i])
			i++
			continue
		}
		if i+1 >= len(text) {
			// A trailing lone ESC is an escape this parser does not model: it
			// is dropped from the text and produces no class.
			i++
			continue
		}
		switch text[i+1] {
		case '[':
			end := i + 2
			for end < len(text) && (text[end] < '@' || text[end] > '~') {
				end++
			}
			if end >= len(text) {
				// An unterminated CSI: drop the remainder.
				i = len(text)
				continue
			}
			if text[end] == 'm' {
				if err := flush(); err != nil {
					return nil, err
				}
				state.apply(parseParams(text[i+2 : end]))
				class = state.class(p.ansiRank)
			}
			i = end + 1
		case ']':
			// OSC: runs to BEL or ST.
			end := i + 2
			for end < len(text) {
				if text[end] == 0x07 {
					end++
					break
				}
				if text[end] == 0x1b && end+1 < len(text) && text[end+1] == '\\' {
					end += 2
					break
				}
				end++
			}
			i = end
		case '(', ')', '*', '+':
			// A charset designation (ESC ( B and friends): three bytes.
			i += 3
		default:
			// A two-byte escape this parser does not model.
			i += 2
		}
	}
	if err := flush(); err != nil {
		return nil, err
	}
	return spans, nil
}
