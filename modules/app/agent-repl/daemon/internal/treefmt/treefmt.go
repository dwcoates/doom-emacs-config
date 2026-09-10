// Package treefmt wraps the numbered Unicode trees inside a document so no
// line exceeds a column limit.
//
// This is a ONE-TO-ONE PORT of format_trees.py from the explanation-engine
// repository's create-or-update-pr skill (632 lines, no dependencies), which
// the owner wrote and ruled must be taken verbatim: the same parser, the same
// packer, the same width model, the same idempotence, the same error surface.
// Every function here carries its Python name in snake_case in its doc
// comment so the two can be read side by side, and the test file ports the
// Python suite case for case with the same fixture. The reason it lives in Go
// at all is speed: the daemon runs it on every settled response before the
// response is served to the webapp and to Emacs, and a Python child per
// response is not an option there.
//
// A tree line is a branch: an optional run of 4-column connector segments
// (`│   `, `    `, `├── `, `└── `), a dotted hierarchical label
// (`1.`, `1.2.`, `1.2.3.`), and the branch text. A branch whose rendered
// width exceeds the limit is wrapped onto continuation lines that
//
//   - start at the column where the branch text starts, so the wrapped
//     remainder reads as a hanging indent under its own branch, and
//   - carry the vertical connectors of every sibling branch the wrap now
//     bisects (`├── ` becomes `│   `, `└── ` becomes four spaces), and
//     hold open the connector column of the branch's own children when any
//     are rendered beneath the wrap, so the tree's vertical rules stay
//     unbroken through the wrap.
//
// Width is measured in RENDERED columns, not source bytes: HTML tags
// contribute nothing, HTML entities count as the single character they
// denote, and emoji count as two columns. Inline elements are atomic, so a
// wrap never lands between a tag and its text; an element too long to fit
// alone is split with its tags closed at the end of one line and reopened at
// the start of the next.
//
// Only the interior of a `<pre>` block or a triple-backtick fence is
// considered, and inside such a block only lines that parse as a branch are
// touched. Already-wrapped branches are joined before being re-wrapped, so
// formatting is idempotent.
package treefmt

import (
	"errors"
	"flag"
	"fmt"
	"html"
	"io"
	"regexp"
	"strings"
	"unicode"
	"unicode/utf8"

	"golang.org/x/text/width"
)

// DefaultWidth is the column limit when none is given (DEFAULT_WIDTH).
const DefaultWidth = 105

// segmentWidth is one tree-prefix segment, in columns (SEGMENT_WIDTH). A
// segment either continues an ancestor's vertical rule, is blank because that
// ancestor was its parent's last child, or is the branch's own connector
// (which is always the final segment).
const segmentWidth = 4

// segmentContinuations maps each prefix segment to what must appear beneath
// it on a continuation line (SEGMENT_CONTINUATIONS). A `├── ` connector
// means the branch has following siblings, so the vertical rule must continue
// past the wrap; a `└── ` connector means it does not, so the column goes
// blank.
var segmentContinuations = map[string]string{
	"│   ": "│   ",
	"|   ": "|   ",
	"    ": "    ",
	"├── ": "│   ",
	"└── ": "    ",
	"|-- ": "|   ",
	"+-- ": "|   ",
	"`-- ": "    ",
}

// connectorSegments are the segments that terminate a prefix
// (CONNECTOR_SEGMENTS): a branch has exactly one connector, and it is the
// last segment before the label.
var connectorSegments = map[string]bool{
	"├── ": true,
	"└── ": true,
	"|-- ": true,
	"+-- ": true,
	"`-- ": true,
}

// Python's `\s` on a str is Unicode white space; Go's RE2 `\s` is ASCII only.
// This class is the port's spelling of the Python one wherever a regular
// expression is kept.
const ws = `[\s\p{Z}\x{85}]`

var (
	tagRE      = regexp.MustCompile(`<[^>]*>`)
	tagNameRE  = regexp.MustCompile(`^</?` + ws + `*([A-Za-z][A-Za-z0-9]*)`)
	preOpenRE  = regexp.MustCompile(`^` + ws + `*<pre>` + ws + `*$`)
	preCloseRE = regexp.MustCompile(`^` + ws + `*</pre>` + ws + `*$`)
	fenceRE    = regexp.MustCompile(`^` + ws + `*(` + "```" + `+|~~~+)` + ws + `*[^\s\p{Z}\x{85}]*` + ws + `*$`)
)

var voidTags = map[string]bool{
	"br": true, "hr": true, "img": true, "wbr": true, "input": true, "meta": true, "link": true,
}

const (
	variationSelector16 = '️'
	zeroWidthJoiner     = '‍'
)

// ---------------------------------------------------------------------------
// Rendered width
// ---------------------------------------------------------------------------

// StripTags returns raw with HTML tags removed and entities decoded
// (strip_tags).
func StripTags(raw string) string {
	return html.UnescapeString(tagRE.ReplaceAllString(raw, ""))
}

// charWidth returns the number of columns char occupies when rendered
// (char_width). A character carrying the emoji variation selector renders as
// an emoji and therefore takes two columns even when its own East Asian width
// says otherwise; the variation selector itself takes none. Combining marks
// and format characters occupy no column of their own.
func charWidth(char rune, followedByVS16 bool) int {
	if char == zeroWidthJoiner || unicode.In(char, unicode.Mn, unicode.Me, unicode.Cf) {
		return 0
	}
	if followedByVS16 {
		return 2
	}
	switch width.LookupRune(char).Kind() {
	case width.EastAsianWide, width.EastAsianFullwidth:
		return 2
	}
	return 1
}

// textWidth returns the rendered column width of already-decoded text
// (text_width).
func textWidth(text string) int {
	runes := []rune(text)
	total := 0
	for i, char := range runes {
		next := rune(-1)
		if i+1 < len(runes) {
			next = runes[i+1]
		}
		total += charWidth(char, next == variationSelector16)
	}
	return total
}

// VisibleWidth returns the rendered column width of raw, which may contain
// markup (visible_width).
func VisibleWidth(raw string) int {
	return textWidth(StripTags(raw))
}

// ---------------------------------------------------------------------------
// Markup-aware tokenization
// ---------------------------------------------------------------------------

// run is a contiguous stretch of branch text that is either markup or content
// (Run).
type run struct {
	raw   string
	isTag bool
}

func (r run) width() int {
	if r.isTag {
		return 0
	}
	return textWidth(html.UnescapeString(r.raw))
}

// subword is a whitespace-delimited word, with the tag stack open around it
// (Subword).
type subword struct {
	raw       string
	width     int
	openAfter []string
}

// Atom is an unbreakable wrap unit: a word, or a whole inline element (Atom).
// Keeping an element atomic is what guarantees a continuation line's
// connector characters never land inside an `<a>` or `<mark>` element,
// where they would render as link or highlight text.
type Atom struct {
	runs []run
}

// Raw is the atom's source text.
func (a Atom) Raw() string {
	var b strings.Builder
	for _, r := range a.runs {
		b.WriteString(r.raw)
	}
	return b.String()
}

// Width is the atom's rendered width.
func (a Atom) Width() int {
	total := 0
	for _, r := range a.runs {
		total += r.width()
	}
	return total
}

// subwords splits into whitespace-delimited words, tracking the open tag
// stack (Atom.subwords). Used only as the fallback for an atom too wide to
// fit a line on its own; the tag stack lets each resulting line close and
// reopen whatever element the split lands inside.
func (a Atom) subwords() []subword {
	var result []subword
	var stack []string
	pending := ""
	pendingWidth := 0
	flush := func() {
		if pending != "" {
			result = append(result, subword{pending, pendingWidth, cloneStack(stack)})
			pending = ""
			pendingWidth = 0
		}
	}
	for _, r := range a.runs {
		if r.isTag {
			applyTag(&stack, r.raw)
			if strings.HasPrefix(r.raw, "</") && pending == "" && len(result) > 0 {
				// A close tag separated from its text by whitespace still
				// belongs to the word it closes, not to the word after it.
				last := result[len(result)-1]
				result[len(result)-1] = subword{last.raw + r.raw, last.width, cloneStack(stack)}
			} else {
				pending += r.raw
			}
			continue
		}
		for _, segment := range splitWhitespace(html.UnescapeString(r.raw)) {
			if isSpace(segment) {
				flush()
			} else {
				pending += segment
				pendingWidth += textWidth(segment)
			}
		}
	}
	flush()
	return result
}

func cloneStack(stack []string) []string {
	return append([]string(nil), stack...)
}

// applyTag updates stack for the HTML tag, ignoring void and self-closing
// tags (apply_tag).
func applyTag(stack *[]string, tag string) {
	m := tagNameRE.FindStringSubmatch(tag)
	if m == nil {
		return
	}
	name := strings.ToLower(m[1])
	if voidTags[name] || strings.HasSuffix(tag, "/>") {
		return
	}
	if strings.HasPrefix(tag, "</") {
		for i := len(*stack) - 1; i >= 0; i-- {
			if tagName((*stack)[i]) == name {
				*stack = append((*stack)[:i], (*stack)[i+1:]...)
				return
			}
		}
		return
	}
	*stack = append(*stack, tag)
}

// tagName is the lower-cased element name of tag, or "" (tag_name).
func tagName(tag string) string {
	m := tagNameRE.FindStringSubmatch(tag)
	if m == nil {
		return ""
	}
	return strings.ToLower(m[1])
}

// closeSequence closes every open element, innermost first (close_sequence).
func closeSequence(stack []string) string {
	var b strings.Builder
	for i := len(stack) - 1; i >= 0; i-- {
		b.WriteString("</" + tagName(stack[i]) + ">")
	}
	return b.String()
}

// openSequence reopens every open element in order (open_sequence).
func openSequence(stack []string) string {
	return strings.Join(stack, "")
}

// isSpace reports whether every rune of s is white space, and s is not empty
// (str.isspace).
func isSpace(s string) bool {
	if s == "" {
		return false
	}
	for _, r := range s {
		if !unicode.IsSpace(r) {
			return false
		}
	}
	return true
}

// splitWhitespace splits text into alternating whitespace and non-whitespace
// segments (split_whitespace, re.split(r"(\s+)") with empties dropped).
func splitWhitespace(text string) []string {
	var out []string
	start := 0
	inSpace := false
	for i, r := range text {
		space := unicode.IsSpace(r)
		if i == 0 {
			inSpace = space
			continue
		}
		if space != inSpace {
			out = append(out, text[start:i])
			start = i
			inSpace = space
		}
	}
	if start < len(text) {
		out = append(out, text[start:])
	}
	return out
}

// parseRuns splits raw into its markup and content runs (parse_runs).
func parseRuns(raw string) []run {
	var runs []run
	position := 0
	for _, loc := range tagRE.FindAllStringIndex(raw, -1) {
		if loc[0] > position {
			runs = append(runs, run{raw[position:loc[0]], false})
		}
		runs = append(runs, run{raw[loc[0]:loc[1]], true})
		position = loc[1]
	}
	if position < len(raw) {
		runs = append(runs, run{raw[position:], false})
	}
	return runs
}

// Tokenize splits branch text into the atoms a wrap may be placed between
// (tokenize). Whitespace inside an inline element does not separate atoms, so
// an element stays whole; whitespace outside any element does.
func Tokenize(raw string) []Atom {
	var atoms []Atom
	var stack []string
	current := Atom{}
	flush := func() {
		if len(current.runs) > 0 {
			atoms = append(atoms, current)
			current = Atom{}
		}
	}
	for _, r := range parseRuns(raw) {
		if r.isTag {
			current.runs = append(current.runs, r)
			applyTag(&stack, r.raw)
			continue
		}
		if len(stack) > 0 {
			current.runs = append(current.runs, r)
			continue
		}
		for _, segment := range splitWhitespace(r.raw) {
			if isSpace(segment) {
				flush()
			} else {
				current.runs = append(current.runs, run{segment, false})
			}
		}
	}
	flush()
	return atoms
}

// ---------------------------------------------------------------------------
// Branch parsing
// ---------------------------------------------------------------------------

// Branch is a parsed tree line, in the pieces the wrapper needs (Branch).
type Branch struct {
	Prefix string
	Label  string
	Body   string
}

// ContinuationPrefix is the prefix a wrapped remainder of this branch carries
// (Branch.continuation_prefix). Every ancestor's vertical rule is preserved,
// and the branch's own connector becomes the vertical rule of the siblings
// below it (or blank when the branch is its parent's last child).
func (b Branch) ContinuationPrefix() string {
	var out strings.Builder
	for _, segment := range segments(b.Prefix) {
		out.WriteString(segmentContinuations[segment])
	}
	return out.String()
}

// TextColumn is the column at which the branch text starts
// (Branch.text_column).
func (b Branch) TextColumn() int {
	return VisibleWidth(b.Prefix) + VisibleWidth(b.Label)
}

// continuationIndent is the full indent a wrapped remainder of this branch
// carries (Branch.continuation_indent). The label's columns become padding,
// except that the first of them holds a vertical rule when the branch has
// children rendered beneath the wrap: that column is where those children's
// own connectors sit, so blanking it would sever the branch from its subtree.
func (b Branch) continuationIndent(hasChildren bool) string {
	labelWidth := VisibleWidth(b.Label)
	var padding string
	if hasChildren && labelWidth > 0 {
		padding = "│" + strings.Repeat(" ", labelWidth-1)
	} else {
		padding = strings.Repeat(" ", labelWidth)
	}
	return b.ContinuationPrefix() + padding
}

// IsParentOf reports whether other is a direct child of this branch
// (Branch.is_parent_of). A direct child's prefix is exactly this branch's
// continuation prefix followed by the child's own connector segment.
func (b Branch) IsParentOf(other Branch) bool {
	cont := b.ContinuationPrefix()
	return strings.HasPrefix(other.Prefix, cont) &&
		utf8.RuneCountInString(other.Prefix) == utf8.RuneCountInString(cont)+segmentWidth
}

// segments cuts prefix into its 4-column pieces (segments). Python slices by
// code point, so this does too.
func segments(prefix string) []string {
	runes := []rune(prefix)
	var out []string
	for i := 0; i < len(runes); i += segmentWidth {
		end := i + segmentWidth
		if end > len(runes) {
			end = len(runes)
		}
		out = append(out, string(runes[i:end]))
	}
	return out
}

// parsePrefix splits line into its tree prefix and the remainder after it
// (parse_prefix). A prefix is a run of known 4-column segments in which a
// connector, if present, is the last segment; the prefix is empty for a root
// branch.
func parsePrefix(line string) (prefix, remainder string) {
	runes := []rune(line)
	position := 0
	var collected strings.Builder
	for {
		end := position + segmentWidth
		if end > len(runes) {
			end = len(runes)
		}
		segment := string(runes[position:end])
		if _, ok := segmentContinuations[segment]; !ok {
			break
		}
		collected.WriteString(segment)
		position += segmentWidth
		if connectorSegments[segment] {
			break
		}
	}
	return collected.String(), string(runes[position:])
}

// matchLabel is LABEL_RE, `^(\d+(?:\.\d+)*\.?)(\s+)`, with Python's Unicode
// classes: it returns the label (digits, dots, and the trailing whitespace
// run) and the index where the remainder starts, or ok=false.
func matchLabel(s string) (label string, end int, ok bool) {
	runes := []rune(s)
	i := 0
	digits := func() bool {
		start := i
		for i < len(runes) && unicode.IsDigit(runes[i]) {
			i++
		}
		return i > start
	}
	if !digits() {
		return "", 0, false
	}
	for i+1 < len(runes) && runes[i] == '.' && unicode.IsDigit(runes[i+1]) {
		i++
		digits()
	}
	if i < len(runes) && runes[i] == '.' {
		i++
	}
	start := i
	for i < len(runes) && unicode.IsSpace(runes[i]) {
		i++
	}
	if i == start {
		return "", 0, false
	}
	return string(runes[:i]), len(string(runes[:i])), true
}

// ParseBranch parses line as a branch head, or returns ok=false
// (parse_branch).
func ParseBranch(line string) (Branch, bool) {
	if strings.TrimSpace(line) == "" {
		return Branch{}, false
	}
	prefix, remainder := parsePrefix(line)
	segs := segments(prefix)
	hasConnector := prefix != "" && connectorSegments[segs[len(segs)-1]]
	label, end, ok := matchLabel(remainder)
	if !ok {
		// A branch with no label is still a branch when it carries a
		// connector.
		if !hasConnector {
			return Branch{}, false
		}
		return Branch{prefix, "", strings.TrimSpace(remainder)}, true
	}
	return Branch{prefix, label, strings.TrimSpace(remainder[end:])}, true
}

// ParseContinuation returns the (text column, text) of line if it can be a
// wrapped remainder (parse_continuation). A continuation carries no connector
// and no label: it is vertical rules and blanks, then alignment padding, then
// text.
func ParseContinuation(line string) (column int, text string, ok bool) {
	if strings.TrimSpace(line) == "" {
		return 0, "", false
	}
	prefix, remainder := parsePrefix(line)
	if prefix != "" {
		segs := segments(prefix)
		if connectorSegments[segs[len(segs)-1]] {
			return 0, "", false
		}
	}
	// The padding region may carry the branch's held-open child connector,
	// so a leading vertical rule counts as padding rather than as text.
	stripped := strings.TrimLeft(remainder, " │|")
	padding := utf8.RuneCountInString(remainder) - utf8.RuneCountInString(stripped)
	text = stripped
	if text == "" {
		return 0, "", false
	}
	if _, _, labelled := matchLabel(text); labelled {
		return 0, "", false
	}
	return VisibleWidth(prefix) + padding, strings.TrimRightFunc(text, unicode.IsSpace), true
}

// ---------------------------------------------------------------------------
// Wrapping
// ---------------------------------------------------------------------------

// OverflowError is raised when a single word cannot be made to fit the column
// limit (Overflow).
type OverflowError struct {
	Message string
}

func (e *OverflowError) Error() string { return e.Message }

// Piece is a unit the packer places on a line, plus what a break before it
// costs (Piece). An atom that fits a line on its own is one piece and breaks
// for free. An atom too wide for that becomes one piece per word inside it,
// each carrying the markup needed to close the elements open at that word and
// reopen them on the line the break starts.
type Piece struct {
	Raw         string
	Width       int
	BreakSuffix string
	BreakPrefix string
}

// ToPieces turns atoms into packer pieces, expanding any atom too wide for a
// line (to_pieces).
func ToPieces(atoms []Atom, width int) []Piece {
	var pieces []Piece
	for _, atom := range atoms {
		if atom.Width() <= width {
			pieces = append(pieces, Piece{Raw: atom.Raw(), Width: atom.Width()})
			continue
		}
		var stack []string
		for _, sw := range atom.subwords() {
			pieces = append(pieces, Piece{sw.raw, sw.width, closeSequence(stack), openSequence(stack)})
			stack = sw.openAfter
		}
	}
	return pieces
}

// Pack greedily packs pieces into lines of at most width rendered columns
// (pack). It returns the packed lines plus every word that could not be made
// to fit, which the caller surfaces rather than silently truncating.
func Pack(pieces []Piece, width int) (lines, overflows []string) {
	current := ""
	currentWidth := 0
	placed := false
	for _, piece := range pieces {
		if placed && currentWidth+1+piece.Width > width {
			lines = append(lines, current+piece.BreakSuffix)
			current = piece.BreakPrefix
			currentWidth = 0
			placed = false
		}
		if placed {
			current += " "
			currentWidth++
		} else if piece.Width > width {
			overflows = append(overflows, StripTags(piece.Raw))
		}
		current += piece.Raw
		currentWidth += piece.Width
		placed = true
	}
	if placed {
		lines = append(lines, current)
	}
	if len(lines) == 0 {
		lines = []string{""}
	}
	return lines, overflows
}

// WrapBranch renders branch as one or more lines, none wider than width
// (wrap_branch).
func WrapBranch(branch Branch, width int, hasChildren bool) (lines, overflows []string, err error) {
	prefixWidth := branch.TextColumn()
	field := width - prefixWidth
	if field <= 0 {
		return nil, nil, &OverflowError{fmt.Sprintf(
			"branch prefix occupies %d columns, leaving no room within %d: %s%s",
			prefixWidth, width, branch.Prefix, branch.Label)}
	}
	packed, overflows := Pack(ToPieces(Tokenize(branch.Body), field), field)
	indent := branch.continuationIndent(hasChildren)
	lines = []string{branch.Prefix + branch.Label + packed[0]}
	for _, piece := range packed[1:] {
		lines = append(lines, indent+piece)
	}
	for i, line := range lines {
		lines[i] = strings.TrimRightFunc(line, unicode.IsSpace)
	}
	return lines, overflows, nil
}

// ---------------------------------------------------------------------------
// Block formatting
// ---------------------------------------------------------------------------

// Result is a formatted document, plus whatever could not be made to fit
// (Result). TooWide covers only lines the formatter is responsible for —
// branches inside a block — because prose outside a block is deliberately
// left as one continuous line for the renderer to soft-wrap.
type Result struct {
	Text      string
	Overflows []string
	TooWide   []string
}

// Entry is one line of a block after joining: a parsed branch (its text
// rejoined) or a raw line that is not part of a tree and is passed through
// untouched. Branch is nil for a raw line.
type Entry struct {
	Branch *Branch
	Raw    string
}

// JoinWrapped collapses already-wrapped branches back into one entry each
// (join_wrapped).
func JoinWrapped(lines []string) []Entry {
	var entries []Entry
	for _, line := range lines {
		if branch, ok := ParseBranch(line); ok {
			b := branch
			entries = append(entries, Entry{&b, line})
			continue
		}
		if column, text, ok := ParseContinuation(line); ok && len(entries) > 0 && entries[len(entries)-1].Branch != nil {
			head := entries[len(entries)-1].Branch
			if column == head.TextColumn() {
				head.Body = strings.TrimSpace(head.Body + " " + text)
				continue
			}
		}
		entries = append(entries, Entry{nil, line})
	}
	return entries
}

// FormatBlock wraps every branch in one block's lines (format_block). It is
// the entry point for text that IS a tree — a settled response under the
// metaprompt is one tree with nothing outside it — where FormatText, which
// looks only inside <pre> and fenced blocks, would touch nothing.
func FormatBlock(lines []string, width int) (output, overflows []string, err error) {
	entries := JoinWrapped(lines)
	for i, entry := range entries {
		if entry.Branch == nil {
			output = append(output, entry.Raw)
			continue
		}
		hasChildren := false
		if i+1 < len(entries) && entries[i+1].Branch != nil {
			hasChildren = entry.Branch.IsParentOf(*entries[i+1].Branch)
		}
		wrapped, branchOverflows, err := WrapBranch(*entry.Branch, width, hasChildren)
		if err != nil {
			return nil, nil, err
		}
		output = append(output, wrapped...)
		overflows = append(overflows, branchOverflows...)
	}
	return output, overflows, nil
}

// FormatText wraps the trees in every `<pre>` block and fenced block of
// text (format_text). The only error is an *OverflowError, for a branch whose
// prefix alone exceeds the limit.
func FormatText(text string, width int) (Result, error) {
	lines := strings.Split(text, "\n")
	var output, overflows, tooWide []string
	index := 0
	for index < len(lines) {
		line := lines[index]
		closer := blockCloser(line)
		if closer == nil {
			output = append(output, line)
			index++
			continue
		}
		var body []string
		cursor := index + 1
		for cursor < len(lines) && !closer(lines[cursor]) {
			body = append(body, lines[cursor])
			cursor++
		}
		if cursor >= len(lines) {
			// Unterminated block: nothing here is known to be a tree.
			output = append(output, lines[index:]...)
			break
		}
		formatted, blockOverflows, err := FormatBlock(body, width)
		if err != nil {
			return Result{}, err
		}
		output = append(output, line)
		output = append(output, formatted...)
		output = append(output, lines[cursor])
		overflows = append(overflows, blockOverflows...)
		for _, candidate := range formatted {
			if VisibleWidth(candidate) > width {
				tooWide = append(tooWide, candidate)
			}
		}
		index = cursor + 1
	}
	return Result{strings.Join(output, "\n"), overflows, tooWide}, nil
}

// blockCloser returns a predicate matching the given block opener's closer,
// or nil (block_closer).
func blockCloser(line string) func(string) bool {
	if preOpenRE.MatchString(line) {
		return preCloseRE.MatchString
	}
	if m := fenceRE.FindStringSubmatch(line); m != nil {
		marker := strings.Repeat(string(m[1][0]), 3)
		return func(candidate string) bool {
			return strings.HasPrefix(strings.TrimSpace(candidate), marker)
		}
	}
	return nil
}

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

// Report writes the formatter's warnings to stream (report).
func Report(result Result, stream io.Writer) {
	for _, word := range result.Overflows {
		fmt.Fprintf(stream, "WARNING: word does not fit the column limit: %s\n", word)
	}
	for _, line := range result.TooWide {
		fmt.Fprintf(stream, "WARNING: line exceeds the column limit (%d columns): %s\n", VisibleWidth(line), line)
	}
}

// Main is the command-line entry point (main): it reads stdin, formats it,
// and answers the exit code the Python script answers — 0 on success, 1 when
// --check finds unformatted input, 2 for a bad argument or a branch that
// cannot fit at all.
func Main(argv []string, stdin io.Reader, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("format_trees", flag.ContinueOnError)
	fs.SetOutput(stderr)
	widthFlag := fs.Int("width", DefaultWidth, fmt.Sprintf("column limit (default: %d)", DefaultWidth))
	check := fs.Bool("check", false, "exit 1 without writing when the input is not already formatted")
	if err := fs.Parse(argv); err != nil {
		return 2
	}
	if *widthFlag <= 0 {
		fmt.Fprintln(stderr, "error: --width must be positive")
		return 2
	}
	sourceBytes, err := io.ReadAll(stdin)
	if err != nil {
		fmt.Fprintf(stderr, "ERROR: %v\n", err)
		return 2
	}
	source := string(sourceBytes)
	result, err := FormatText(source, *widthFlag)
	if err != nil {
		var overflow *OverflowError
		if errors.As(err, &overflow) {
			fmt.Fprintf(stderr, "ERROR: %s\n", overflow.Message)
			return 2
		}
		fmt.Fprintf(stderr, "ERROR: %v\n", err)
		return 2
	}
	Report(result, stderr)
	if *check {
		if result.Text != source {
			fmt.Fprintf(stderr, "ERROR: trees are not wrapped to %d columns\n", *widthFlag)
			return 1
		}
		return 0
	}
	io.WriteString(stdout, result.Text)
	return 0
}
