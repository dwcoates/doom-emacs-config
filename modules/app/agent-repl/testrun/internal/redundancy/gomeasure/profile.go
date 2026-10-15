package gomeasure

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
)

// Block is one coverage block: a file (by import path) and its span.
type Block struct {
	File                     string
	Line0, Col0, Line1, Col1 int
	// Statements is the block's statement count; not part of its Key.
	Statements int
}

// Key is the block's identity in a measurement.
func (b Block) Key() string {
	return fmt.Sprintf("%s:%d.%d,%d.%d", b.File, b.Line0, b.Col0, b.Line1, b.Col1)
}

// Contains says the position line:col of the block's file lies in it.
func (b Block) Contains(line, col int) bool {
	after := line > b.Line0 || (line == b.Line0 && col >= b.Col0)
	before := line < b.Line1 || (line == b.Line1 && col <= b.Col1)
	return after && before
}

// ParseProfile reads a `go tool covdata textfmt` profile and returns every
// block in it and the ones with a nonzero count.
func ParseProfile(text string) (all, covered []Block, err error) {
	lines := strings.Split(strings.TrimRight(text, "\n"), "\n")
	if len(lines) == 0 || !strings.HasPrefix(lines[0], "mode: ") {
		return nil, nil, fmt.Errorf("gomeasure: a coverage profile starts with its mode, got %q", firstLine(text))
	}
	for _, l := range lines[1:] {
		b, count, err := parseProfileLine(l)
		if err != nil {
			return nil, nil, err
		}
		all = append(all, b)
		if count > 0 {
			covered = append(covered, b)
		}
	}
	return all, covered, nil
}

func firstLine(s string) string {
	l, _, _ := strings.Cut(s, "\n")
	return l
}

// parseProfileLine reads "file:l0.c0,l1.c1 statements count".
func parseProfileLine(l string) (Block, int, error) {
	bad := func() (Block, int, error) {
		return Block{}, 0, fmt.Errorf("gomeasure: malformed coverage profile line %q", l)
	}
	colon := strings.LastIndex(l, ":")
	if colon <= 0 {
		return bad()
	}
	fields := strings.Fields(l[colon+1:])
	if len(fields) != 3 {
		return bad()
	}
	span := strings.Split(fields[0], ",")
	if len(span) != 2 {
		return bad()
	}
	var nums [4]int
	for i, part := range []string{span[0], span[1]} {
		lc := strings.Split(part, ".")
		if len(lc) != 2 {
			return bad()
		}
		for j, s := range lc {
			n, err := strconv.Atoi(s)
			if err != nil {
				return bad()
			}
			nums[2*i+j] = n
		}
	}
	stmts, err := strconv.Atoi(fields[1])
	if err != nil {
		return bad()
	}
	count, err := strconv.Atoi(fields[2])
	if err != nil {
		return bad()
	}
	return Block{File: l[:colon], Line0: nums[0], Col0: nums[1], Line1: nums[2], Col1: nums[3], Statements: stmts}, count, nil
}

// blockIndex finds the blocks of one file that hold a position.
type blockIndex map[string][]Block

func newBlockIndex(blocks []Block) blockIndex {
	idx := blockIndex{}
	for _, b := range blocks {
		idx[b.File] = append(idx[b.File], b)
	}
	for _, bs := range idx {
		sort.Slice(bs, func(i, j int) bool { return bs[i].Line0 < bs[j].Line0 })
	}
	return idx
}

// holding is every block of file containing line:col, innermost first (Go's
// blocks do not nest, but a function literal's blocks lie inside the span of
// the statement holding it, so both may match).
func (idx blockIndex) holding(file string, line, col int) []Block {
	var out []Block
	for _, b := range idx[file] {
		if b.Line0 > line {
			break
		}
		if b.Contains(line, col) {
			out = append(out, b)
		}
	}
	sort.SliceStable(out, func(i, j int) bool { return span(out[i]) < span(out[j]) })
	return out
}

func span(b Block) int { return (b.Line1-b.Line0)*10000 + b.Col1 - b.Col0 }
