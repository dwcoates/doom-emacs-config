package vitestmeasure

import (
	"fmt"
	"regexp"
	"strings"
)

// mutateMatcher answers whether a package-relative path is one Stryker
// mutates under its -mutate globs: matched by a glob, and by no "!" glob.
type mutateMatcher struct {
	include, exclude []*regexp.Regexp
}

func newMutateMatcher(globs []string) (*mutateMatcher, error) {
	m := &mutateMatcher{}
	for _, g := range globs {
		neg := strings.HasPrefix(g, "!")
		re, err := globRegexp(strings.TrimPrefix(g, "!"))
		if err != nil {
			return nil, err
		}
		if neg {
			m.exclude = append(m.exclude, re)
		} else {
			m.include = append(m.include, re)
		}
	}
	if len(m.include) == 0 {
		return nil, fmt.Errorf("vitestmeasure: the -mutate globs %v include nothing", globs)
	}
	return m, nil
}

func (m *mutateMatcher) match(rel string) bool {
	in := false
	for _, re := range m.include {
		in = in || re.MatchString(rel)
	}
	for _, re := range m.exclude {
		if re.MatchString(rel) {
			return false
		}
	}
	return in
}

// globRegexp compiles a Stryker glob: "**" any run of directories, "*" and
// "?" within one path segment.
func globRegexp(glob string) (*regexp.Regexp, error) {
	var b strings.Builder
	b.WriteString("^")
	for i := 0; i < len(glob); i++ {
		switch c := glob[i]; {
		case strings.HasPrefix(glob[i:], "**/"):
			b.WriteString("(?:.*/)?")
			i += 2
		case strings.HasPrefix(glob[i:], "**"):
			b.WriteString(".*")
			i++
		case c == '*':
			b.WriteString("[^/]*")
		case c == '?':
			b.WriteString("[^/]")
		default:
			b.WriteString(regexp.QuoteMeta(string(c)))
		}
	}
	b.WriteString("$")
	re, err := regexp.Compile(b.String())
	if err != nil {
		return nil, fmt.Errorf("vitestmeasure: the -mutate glob %q: %w", glob, err)
	}
	return re, nil
}
