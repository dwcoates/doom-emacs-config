package footer

import (
	"strconv"
	"strings"
	"time"
)

// cronNextFire resolves the next instant a MINIMAL five-field cron expression
// fires, strictly after `from`. The daemon resolves the instant because the
// contract ships instants and the client ticks durations.
//
// DELIBERATELY MINIMAL: five space-separated fields (minute, hour, day of
// month, month, day of week), each `*`, `*/n`, a number, a `a-b` range, or a
// comma-separated list of those. ANYTHING ELSE IS UNRESOLVABLE — six-field
// forms, `@daily` macros, names, step-on-range — and reports false rather than
// guessing, which is what leaves the row's next_fire UNSET.
func cronNextFire(expr string, from time.Time) (time.Time, bool) {
	fields := strings.Fields(expr)
	if len(fields) != 5 {
		return time.Time{}, false
	}
	minutes, ok := cronField(fields[0], 0, 59)
	if !ok {
		return time.Time{}, false
	}
	hours, ok := cronField(fields[1], 0, 23)
	if !ok {
		return time.Time{}, false
	}
	doms, ok := cronField(fields[2], 1, 31)
	if !ok {
		return time.Time{}, false
	}
	months, ok := cronField(fields[3], 1, 12)
	if !ok {
		return time.Time{}, false
	}
	dows, ok := cronField(fields[4], 0, 6)
	if !ok {
		return time.Time{}, false
	}
	domRestricted := fields[2] != "*"
	dowRestricted := fields[4] != "*"

	// A minute is the resolution, so the search starts at the next whole
	// minute and walks forward. Four years covers every February 29 case.
	t := from.Truncate(time.Minute).Add(time.Minute)
	limit := from.AddDate(4, 0, 0)
	for !t.After(limit) {
		if !months[int(t.Month())] {
			t = time.Date(t.Year(), t.Month(), 1, 0, 0, 0, 0, t.Location()).AddDate(0, 1, 0)
			continue
		}
		if !cronDayMatches(t, doms, dows, domRestricted, dowRestricted) {
			t = time.Date(t.Year(), t.Month(), t.Day(), 0, 0, 0, 0, t.Location()).AddDate(0, 0, 1)
			continue
		}
		if !hours[t.Hour()] {
			t = time.Date(t.Year(), t.Month(), t.Day(), t.Hour(), 0, 0, 0, t.Location()).Add(time.Hour)
			continue
		}
		if !minutes[t.Minute()] {
			t = t.Add(time.Minute)
			continue
		}
		return t, true
	}
	return time.Time{}, false
}

// cronDayMatches applies the vixie-cron day rule: when BOTH the day-of-month
// and the day-of-week fields are restricted the day matches if EITHER does;
// otherwise the restricted one alone decides.
func cronDayMatches(t time.Time, doms, dows map[int]bool, domRestricted, dowRestricted bool) bool {
	dom := doms[t.Day()]
	dow := dows[int(t.Weekday())]
	switch {
	case domRestricted && dowRestricted:
		return dom || dow
	case domRestricted:
		return dom
	case dowRestricted:
		return dow
	default:
		return true
	}
}

// cronField parses one field into the set of values it admits.
func cronField(field string, min, max int) (map[int]bool, bool) {
	out := map[int]bool{}
	for _, part := range strings.Split(field, ",") {
		if part == "" {
			return nil, false
		}
		step := 1
		if slash := strings.Index(part, "/"); slash >= 0 {
			n, err := strconv.Atoi(part[slash+1:])
			if err != nil || n <= 0 {
				return nil, false
			}
			step = n
			part = part[:slash]
		}
		lo, hi := min, max
		switch {
		case part == "*":
		case strings.Contains(part, "-"):
			bounds := strings.SplitN(part, "-", 2)
			a, err := strconv.Atoi(bounds[0])
			if err != nil {
				return nil, false
			}
			b, err := strconv.Atoi(bounds[1])
			if err != nil {
				return nil, false
			}
			lo, hi = a, b
		default:
			n, err := strconv.Atoi(part)
			if err != nil {
				return nil, false
			}
			lo, hi = n, n
		}
		if lo < min || hi > max || lo > hi {
			return nil, false
		}
		for v := lo; v <= hi; v += step {
			out[v] = true
		}
	}
	if len(out) == 0 {
		return nil, false
	}
	return out, true
}
