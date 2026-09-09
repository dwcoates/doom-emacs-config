package treefmt

// A one-to-one port of format_trees_test.py: every test class and every case
// below has the same name and the same input and expectation as the Python
// suite it mirrors, so the port is checked against the original's own
// evidence rather than against a rewrite of it. The Python RunShTest, which
// drives the skill's bash wrapper, has no counterpart here because the bash
// wrapper is the skill's, not the formatter's.

import (
	"bytes"
	"errors"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// pre wraps body in a <pre> block the way a PR description does.
func pre(body string) string {
	return "<pre>\n" + strings.TrimRight(body, "\n") + "\n</pre>"
}

// wrap formats body as a <pre> block and returns the block's interior.
func wrap(t *testing.T, body string, width int) string {
	t.Helper()
	result, err := FormatText(pre(body), width)
	if err != nil {
		t.Fatalf("FormatText: %v", err)
	}
	lines := strings.Split(result.Text, "\n")
	return strings.Join(lines[1:len(lines)-1], "\n")
}

// runCLI drives Main in-process, returning (stdout, stderr, exit code).
func runCLI(argv []string, stdin string) (string, string, int) {
	var stdout, stderr bytes.Buffer
	code := Main(argv, strings.NewReader(stdin), &stdout, &stderr)
	return stdout.String(), stderr.String(), code
}

func mustEqual(t *testing.T, got, want string) {
	t.Helper()
	if got != want {
		t.Fatalf("mismatch\n got: %q\nwant: %q", got, want)
	}
}

// --- WidthTest: rendered width ignores markup and counts emoji as two ---

func TestWidths(t *testing.T) {
	cases := []struct {
		name string
		raw  string
		want int
	}{
		{"plain ascii", "abc", 3},
		{"tags contribute nothing", `<a href="https://x/y">abc</a>`, 3},
		{"mark and bold contribute nothing", "<mark><b>abc</b></mark>", 3},
		{"entity counts as one character", "&lt;plugin&gt;", 8},
		{"ampersand entity counts as one", "a&amp;b", 3},
		{"box drawing is single width", "├── ", 4},
		{"wide emoji counts as two", "🎯", 2},
		{"variation-selector emoji counts as two", "✏️", 2},
		{"zero-width joiner is free", "a‍b", 2},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := VisibleWidth(tc.raw); got != tc.want {
				t.Fatalf("VisibleWidth(%q) = %d, want %d", tc.raw, got, tc.want)
			}
		})
	}
}

// --- ContinuationPrefixTest: a wrap extends every bisected rule ---

func TestPrefixes(t *testing.T) {
	cases := []struct{ name, prefix, want string }{
		{"root has no prefix", "", ""},
		{"non-last child keeps its column", "├── ", "│   "},
		{"last child blanks its column", "└── ", "    "},
		{"ancestor rule is preserved", "│   ├── ", "│   │   "},
		{"ancestor blank is preserved", "    └── ", "        "},
		{"depth three mixes both", "│   │   └── ", "│   │       "},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			mustEqual(t, Branch{tc.prefix, "1.1. ", "x"}.ContinuationPrefix(), tc.want)
		})
	}
}

// --- ParseBranchTest ---

func TestParses(t *testing.T) {
	cases := []struct{ name, line, prefix, label, body string }{
		{"root branch", "1. 🎯 Goal.", "", "1. ", "🎯 Goal."},
		{"child branch", "├── 1.1. Text.", "├── ", "1.1. ", "Text."},
		{"last child", "└── 2.2.1. Text.", "└── ", "2.2.1. ", "Text."},
		{"nested child", "│   └── 1.2.3. Text.", "│   └── ", "1.2.3. ", "Text."},
		{"connector without label", "├── unlabelled.", "├── ", "", "unlabelled."},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			branch, ok := ParseBranch(tc.line)
			if !ok {
				t.Fatalf("ParseBranch(%q) = not a branch", tc.line)
			}
			if branch.Prefix != tc.prefix || branch.Label != tc.label || branch.Body != tc.body {
				t.Fatalf("ParseBranch(%q) = %+v, want prefix %q label %q body %q", tc.line, branch, tc.prefix, tc.label, tc.body)
			}
		})
	}
}

func TestRejectsNonBranches(t *testing.T) {
	for _, line := range []string{"", "   ", "Some prose sentence.", "#### Heading", "    indented prose"} {
		t.Run(line, func(t *testing.T) {
			if _, ok := ParseBranch(line); ok {
				t.Fatalf("ParseBranch(%q) parsed a branch", line)
			}
		})
	}
}

// --- WrapTest ---

func TestShortBranchIsUntouched(t *testing.T) {
	body := "├── 1.1. Short enough.\n└── 1.2. Also short."
	mustEqual(t, wrap(t, body, 100), body)
}

func TestContinuationStartsAtTheTextColumn(t *testing.T) {
	// Width 15 fits "├── 1.3. foo" (12 columns) but not " bar".
	mustEqual(t, wrap(t, "├── 1.3. foo bar", 15), "├── 1.3. foo\n│        bar")
}

func TestLastChildContinuationBlanksItsColumn(t *testing.T) {
	mustEqual(t, wrap(t, "└── 1.3. foo bar", 15), "└── 1.3. foo\n         bar")
}

func TestRootBranchContinuationAlignsUnderItsText(t *testing.T) {
	mustEqual(t, wrap(t, "1. foo bar", 8), "1. foo\n   bar")
}

func TestNestedContinuationExtendsEveryAncestorRule(t *testing.T) {
	mustEqual(t, wrap(t, "│   ├── 1.2.1. foo bar", 21), "│   ├── 1.2.1. foo\n│   │          bar")
}

func TestContinuationHoldsOpenTheColumnOfItsOwnChildren(t *testing.T) {
	// A wrap above a subtree keeps the child connector column occupied.
	body := "├── 1.1. aaaa bbbb cccc\n│   └── 1.1.1. Leaf."
	mustEqual(t, wrap(t, body, 22), "├── 1.1. aaaa bbbb\n│   │    cccc\n│   └── 1.1.1. Leaf.")
}

func TestLastChildWithItsOwnChildrenStillHoldsTheColumn(t *testing.T) {
	// Being a last child blanks the sibling rule, not the child rule.
	body := "└── 1.2. aaaa bbbb cccc\n    └── 1.2.1. Leaf."
	mustEqual(t, wrap(t, body, 22), "└── 1.2. aaaa bbbb\n    │    cccc\n    └── 1.2.1. Leaf.")
}

func TestContinuationBlanksTheColumnWhenTheNextBranchIsASibling(t *testing.T) {
	// A following branch at the same depth is not a child.
	body := "├── 1.1. aaaa bbbb cccc\n└── 1.2. Leaf."
	mustEqual(t, wrap(t, body, 22), "├── 1.1. aaaa bbbb\n│        cccc\n└── 1.2. Leaf.")
}

func TestContinuationBlanksTheColumnWhenTheNextBranchIsShallower(t *testing.T) {
	// A following branch shallower than a child is not a child.
	body := "│   ├── 1.2.1. foo bar\n└── 2. Leaf."
	mustEqual(t, wrap(t, body, 21), "│   ├── 1.2.1. foo\n│   │          bar\n└── 2. Leaf.")
}

func TestChildHoldingWrapIsIdempotent(t *testing.T) {
	// Re-wrapping a held-open continuation rejoins rather than mangles.
	body := "├── 1.1. aaaa bbbb cccc\n│   └── 1.1.1. Leaf."
	once := wrap(t, body, 22)
	mustEqual(t, wrap(t, once, 22), once)
}

func TestWrapsRepeatedlyWhenOneContinuationIsNotEnough(t *testing.T) {
	mustEqual(t, wrap(t, "├── 1.1. aaa bbb ccc ddd", 13), "├── 1.1. aaa\n│        bbb\n│        ccc\n│        ddd")
}

func TestEmojiPrefixedBranchWrapsUnderItsEmoji(t *testing.T) {
	mustEqual(t, wrap(t, "1. 🎯 goal here", 12), "1. 🎯 goal\n   here")
}

func TestBlankLineBetweenRootsIsPreserved(t *testing.T) {
	body := "1. First.\n\n2. Second."
	mustEqual(t, wrap(t, body, 100), body)
}

// --- MarkupTest ---

func TestAnchorIsNeverSplitWhenItFits(t *testing.T) {
	body := `├── 1.1. see <a href="https://x/y">symbol</a> now`
	// Field is 16 - 9 = 7 columns, so each of "see", the anchor, and "now"
	// lands on its own line and the anchor stays whole.
	mustEqual(t, wrap(t, body, 16), "├── 1.1. see\n│        <a href=\"https://x/y\">symbol</a>\n│        now")
}

func TestMarkElementKeepsItsInternalSpaces(t *testing.T) {
	body := "├── 1.1. call <mark><b>a = 3</b></mark> once"
	mustEqual(t, wrap(t, body, 14), "├── 1.1. call\n│        <mark><b>a = 3</b></mark>\n│        once")
}

func TestOverlongElementSplitsWithTagsReopened(t *testing.T) {
	body := "1. <mark><b>alpha beta gamma</b></mark>"
	mustEqual(t, wrap(t, body, 13), "1. <mark><b>alpha beta</b></mark>\n   <mark><b>gamma</b></mark>")
}

func TestAnchorSplitRepeatsTheHref(t *testing.T) {
	body := `1. <a href="https://x/y">alpha beta</a>`
	mustEqual(t, wrap(t, body, 9), "1. <a href=\"https://x/y\">alpha</a>\n   <a href=\"https://x/y\">beta</a>")
}

func TestEntitiesAreMeasuredAsOneCharacter(t *testing.T) {
	// "&lt;plugin&gt;" renders as 8 columns, so it fits a 12-column limit.
	mustEqual(t, wrap(t, "1. &lt;plugin&gt;", 12), "1. &lt;plugin&gt;")
}

func TestUnsplittableWordOverflowsAndIsReported(t *testing.T) {
	result, err := FormatText(pre("1. aaaaaaaaaaaaaaaaaaaa"), 10)
	if err != nil {
		t.Fatal(err)
	}
	if len(result.Overflows) != 1 || result.Overflows[0] != "aaaaaaaaaaaaaaaaaaaa" {
		t.Fatalf("Overflows = %q", result.Overflows)
	}
	if len(result.TooWide) != 1 {
		t.Fatalf("TooWide = %q", result.TooWide)
	}
}

// --- PackerTest: the single greedy packer both plain words and split elements go through ---

func TestAtomThatFitsStaysOnePiece(t *testing.T) {
	pieces := ToPieces(Tokenize("<mark><b>a b</b></mark> tail"), 10)
	if pieces[0].Raw != "<mark><b>a b</b></mark>" || pieces[0].Width != 3 {
		t.Fatalf("first piece = %+v", pieces[0])
	}
	if pieces[0].BreakSuffix != "" {
		t.Fatalf("BreakSuffix = %q", pieces[0].BreakSuffix)
	}
}

func TestAtomTooWideIsExpandedIntoWords(t *testing.T) {
	pieces := ToPieces(Tokenize("<mark><b>alpha beta</b></mark>"), 5)
	raws := []string{}
	prefixes := []string{}
	suffixes := []string{}
	for _, p := range pieces {
		raws = append(raws, p.Raw)
		prefixes = append(prefixes, p.BreakPrefix)
		suffixes = append(suffixes, p.BreakSuffix)
	}
	mustEqual(t, strings.Join(raws, "|"), "<mark><b>alpha|beta</b></mark>")
	mustEqual(t, strings.Join(prefixes, "|"), "|<mark><b>")
	mustEqual(t, strings.Join(suffixes, "|"), "|</b></mark>")
}

func TestPackJoinsWithOneSpaceAndBreaksOnTheLimit(t *testing.T) {
	lines, overflows := Pack(ToPieces(Tokenize("aa bb cc"), 5), 5)
	mustEqual(t, strings.Join(lines, "|"), "aa bb|cc")
	if len(overflows) != 0 {
		t.Fatalf("overflows = %q", overflows)
	}
}

func TestPackReportsAPieceItCannotFit(t *testing.T) {
	lines, overflows := Pack(ToPieces(Tokenize("aa bbbbbb"), 5), 5)
	mustEqual(t, strings.Join(lines, "|"), "aa|bbbbbb")
	mustEqual(t, strings.Join(overflows, "|"), "bbbbbb")
}

func TestPackOfNothingYieldsOneEmptyLine(t *testing.T) {
	lines, overflows := Pack(nil, 5)
	if len(lines) != 1 || lines[0] != "" || len(overflows) != 0 {
		t.Fatalf("Pack(nil) = %q, %q", lines, overflows)
	}
}

// --- IdempotenceTest ---

func TestAlreadyWrappedBranchIsRejoinedBeforeRewrapping(t *testing.T) {
	once := wrap(t, "├── 1.1. aaa bbb ccc\n└── 1.2. x", 13)
	mustEqual(t, wrap(t, once, 13), once)
}

func TestRewrappingAtAWiderLimitUnwraps(t *testing.T) {
	narrow := wrap(t, "├── 1.1. aaa bbb ccc\n└── 1.2. x", 13)
	mustEqual(t, wrap(t, narrow, 100), "├── 1.1. aaa bbb ccc\n└── 1.2. x")
}

func TestContinuationOfADifferentColumnIsLeftAlone(t *testing.T) {
	// Indented at 3 columns while the branch's text starts at 9, so it is
	// not this branch's continuation and must not be absorbed.
	body := "├── 1.1. text\n   stray"
	mustEqual(t, wrap(t, body, 100), body)
}

// --- BlockScopeTest ---

func TestTextOutsideABlockIsUntouched(t *testing.T) {
	source := "1. this root branch is outside any block and stays one line\n"
	result, err := FormatText(source, 10)
	if err != nil {
		t.Fatal(err)
	}
	mustEqual(t, result.Text, source)
}

func TestFencedBlockIsFormatted(t *testing.T) {
	result, err := FormatText("```\n├── 1.3. foo bar\n```\n", 15)
	if err != nil {
		t.Fatal(err)
	}
	mustEqual(t, result.Text, "```\n├── 1.3. foo\n│        bar\n```\n")
}

func TestUnterminatedBlockIsUntouched(t *testing.T) {
	source := "<pre>\n├── 1.3. foo bar\n"
	result, err := FormatText(source, 15)
	if err != nil {
		t.Fatal(err)
	}
	mustEqual(t, result.Text, source)
}

func TestSurroundingProseIsPreserved(t *testing.T) {
	source := "#### Heading\n\n<pre>\n1. foo bar\n</pre>\n\ntrailing prose\n"
	result, err := FormatText(source, 8)
	if err != nil {
		t.Fatal(err)
	}
	mustEqual(t, result.Text, "#### Heading\n\n<pre>\n1. foo\n   bar\n</pre>\n\ntrailing prose\n")
}

func TestMultipleBlocksAreEachFormatted(t *testing.T) {
	source := "<pre>\n1. foo bar\n</pre>\n<pre>\n2. baz qux\n</pre>\n"
	result, err := FormatText(source, 8)
	if err != nil {
		t.Fatal(err)
	}
	mustEqual(t, result.Text, "<pre>\n1. foo\n   bar\n</pre>\n<pre>\n2. baz\n   qux\n</pre>\n")
}

// --- FixtureTest: invariants over a real, unwrapped PR description ---
//
// The fixture is the description of the "AnalysisAnnotations" PR (#7337),
// whose trees are written as single continuous lines.

func loadFixture(t *testing.T) (source string, result Result) {
	t.Helper()
	raw, err := os.ReadFile(filepath.Join("testdata", "pr-7337-body.md"))
	if err != nil {
		t.Fatal(err)
	}
	source = string(raw)
	result, err = FormatText(source, 100)
	if err != nil {
		t.Fatal(err)
	}
	return source, result
}

func TestSourceFixtureHasOverWideLines(t *testing.T) {
	source, _ := loadFixture(t)
	over := 0
	for _, line := range strings.Split(source, "\n") {
		if VisibleWidth(line) > 100 {
			over++
		}
	}
	if over == 0 {
		t.Fatal("fixture must exercise wrapping")
	}
}

func TestNoLineExceedsTheLimit(t *testing.T) {
	_, result := loadFixture(t)
	if len(result.TooWide) != 0 {
		t.Fatalf("TooWide = %q", result.TooWide)
	}
	if len(result.Overflows) != 0 {
		t.Fatalf("Overflows = %q", result.Overflows)
	}
}

func TestFormattingIsIdempotent(t *testing.T) {
	_, result := loadFixture(t)
	again, err := FormatText(result.Text, 100)
	if err != nil {
		t.Fatal(err)
	}
	mustEqual(t, again.Text, result.Text)
}

func TestRenderedTextIsPreserved(t *testing.T) {
	source, result := loadFixture(t)
	logical := func(text string) []string {
		var entries []string
		inside := false
		var block []string
		for _, line := range strings.Split(text, "\n") {
			if preOpenRE.MatchString(line) {
				inside, block = true, nil
				continue
			}
			if preCloseRE.MatchString(line) {
				inside = false
				for _, entry := range JoinWrapped(block) {
					if entry.Branch != nil {
						entries = append(entries, strings.Join(strings.Fields(StripTags(entry.Branch.Body)), " "))
					}
				}
				continue
			}
			if inside {
				block = append(block, line)
			}
		}
		return entries
	}
	got, want := logical(result.Text), logical(source)
	if strings.Join(got, "\n") != strings.Join(want, "\n") {
		t.Fatalf("logical text changed:\n got %d entries\nwant %d entries", len(got), len(want))
	}
}

func TestEveryWrappedLineCarriesItsAncestorsRules(t *testing.T) {
	// Every continuation line's prefix columns must match the branch it
	// continues, with the branch's own connector column turned into a rule.
	_, result := loadFixture(t)
	for _, block := range strings.Split(result.Text, "<pre>")[1:] {
		body := strings.Split(strings.Trim(strings.Split(block, "</pre>")[0], "\n"), "\n")
		var head *Branch
		for _, line := range body {
			if branch, ok := ParseBranch(line); ok {
				b := branch
				head = &b
				continue
			}
			column, _, ok := ParseContinuation(line)
			if !ok {
				continue
			}
			if head == nil {
				t.Fatalf("continuation with no branch above it: %q", line)
			}
			if column != head.TextColumn() {
				t.Fatalf("continuation column %d != text column %d: %q", column, head.TextColumn(), line)
			}
			if !strings.HasPrefix(line, head.ContinuationPrefix()) {
				t.Fatalf("continuation does not carry its ancestors' rules: %q", line)
			}
		}
	}
}

// --- ErrorTest: nothing that cannot be made to fit is silently swallowed ---

func TestPrefixWiderThanTheLimitFailsHard(t *testing.T) {
	_, err := FormatText(pre("│   ├── 1.2.1. text"), 10)
	var overflow *OverflowError
	if !errors.As(err, &overflow) {
		t.Fatalf("err = %v, want *OverflowError", err)
	}
}

func TestCLIReportsATooNarrowLimit(t *testing.T) {
	stdout, stderr, code := runCLI([]string{"--width", "10"}, pre("│   ├── 1.2.1. text"))
	if code != 2 {
		t.Fatalf("code = %d", code)
	}
	mustEqual(t, stdout, "")
	if !strings.Contains(stderr, "leaving no room") {
		t.Fatalf("stderr = %q", stderr)
	}
}

func TestCLIWarnsAboutAnUnsplittableWord(t *testing.T) {
	stdout, stderr, code := runCLI([]string{"--width", "10"}, pre("1. aaaaaaaaaaaaaaaaaaaa"))
	if code != 0 {
		t.Fatalf("code = %d", code)
	}
	for _, want := range []string{"word does not fit", "line exceeds the column limit"} {
		if !strings.Contains(stderr, want) {
			t.Fatalf("stderr %q lacks %q", stderr, want)
		}
	}
	if !strings.Contains(stdout, "aaaaaaaaaaaaaaaaaaaa") {
		t.Fatalf("stdout = %q", stdout)
	}
}

// --- CliTest ---

func TestFormattedBodyGoesToStdout(t *testing.T) {
	stdout, _, code := runCLI([]string{"--width", "15"}, pre("├── 1.1. foo bar"))
	if code != 0 {
		t.Fatalf("code = %d", code)
	}
	mustEqual(t, stdout, pre("├── 1.1. foo\n│        bar"))
}

func TestCheckModeRejectsUnformattedInput(t *testing.T) {
	stdout, stderr, code := runCLI([]string{"--check", "--width", "15"}, pre("├── 1.1. foo bar"))
	if code != 1 {
		t.Fatalf("code = %d", code)
	}
	mustEqual(t, stdout, "")
	if !strings.Contains(stderr, "not wrapped to 15 columns") {
		t.Fatalf("stderr = %q", stderr)
	}
}

func TestCheckModeAcceptsFormattedInput(t *testing.T) {
	formatted := pre(wrap(t, "├── 1.1. foo bar", 15))
	if _, _, code := runCLI([]string{"--check", "--width", "15"}, formatted); code != 0 {
		t.Fatalf("code = %d", code)
	}
}

func TestZeroWidthIsRejected(t *testing.T) {
	if _, _, code := runCLI([]string{"--width", "0"}, ""); code != 2 {
		t.Fatalf("code = %d", code)
	}
}

// --- Port-only: the regular expressions kept from the Python are Unicode-aware ---

func TestFenceRegexMatchesThePythonShape(t *testing.T) {
	for _, line := range []string{"```", "```go", "  ~~~ ", "````"} {
		if !fenceRE.MatchString(line) {
			t.Fatalf("fenceRE rejected %q", line)
		}
	}
	for _, line := range []string{"``", "``` two words", "text"} {
		if fenceRE.MatchString(line) {
			t.Fatalf("fenceRE accepted %q", line)
		}
	}
	_ = regexp.MustCompile
}

// --- Port-only: FormatBlock is the entry point for text that is itself a tree ---

func TestFormatBlockWrapsABareTree(t *testing.T) {
	lines, overflows, err := FormatBlock([]string{"├── 1.3. foo bar", "└── 1.4. x"}, 15)
	if err != nil {
		t.Fatal(err)
	}
	mustEqual(t, strings.Join(lines, "\n"), "├── 1.3. foo\n│        bar\n└── 1.4. x")
	if len(overflows) != 0 {
		t.Fatalf("overflows = %q", overflows)
	}
}
