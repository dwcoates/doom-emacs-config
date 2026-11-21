package logsites

import (
	"context"
	"fmt"
	"sort"
	"time"

	"agent-repld/internal/wsm"
)

// A Site is one log site at one level, as it fired in a window.
type Site struct {
	Key       string
	Template  string
	Runtime   string
	Level     string
	Operation string
	Count     int64
	FirstSeen time.Time
	LastSeen  time.Time
	// The site's latest record in the window.
	SampleAt        time.Time
	SampleMessage   string
	SampleWorkspace string
	// SampleContext is the record's context, as JSON.
	SampleContext string
}

// An Answer is every site that fired in a window, and the hours read.
type Answer struct {
	Sites []Site
	// From and To are the covered hours: [From, To).
	From, To time.Time
	// Partial reports that the window began before the oldest kept row, so
	// the answer covers only from that row's hour on.
	Partial bool
}

// UnrecordedWindowError refuses a window that lies wholly before the oldest
// kept row: what fired in it is no longer known.
type UnrecordedWindowError struct {
	// Oldest is the oldest kept row's hour: the earliest window start the
	// store can answer.
	Oldest time.Time
}

func (e *UnrecordedWindowError) Error() string {
	return fmt.Sprintf("logsites: the window ends before the oldest kept log site row, at %s", e.Oldest.Format(time.RFC3339))
}

// MalformedWindowError refuses a window whose end precedes its start, or a
// level the contract does not have.
type MalformedWindowError struct{ Reason string }

func (e *MalformedWindowError) Error() string { return "logsites: " + e.Reason }

// levelRank orders the levels, lowest first.
var levelRank = map[string]int{"debug": 0, "info": 1, "warn": 2, "error": 3}

// Window answers every site that fired from SINCE to UNTIL at MINIMUM or
// above (every level when empty), after a pass brings the rows up to now. The
// window widens outward to whole hours: from the start of SINCE's hour to the
// end of UNTIL's.
func (t *Tallier) Window(ctx context.Context, since, until time.Time, minimum string) (Answer, error) {
	if until.Before(since) {
		return Answer{}, &MalformedWindowError{Reason: fmt.Sprintf("the window ends at %s, before it starts at %s", until.Format(time.RFC3339), since.Format(time.RFC3339))}
	}
	floor := 0
	if minimum != "" {
		rank, ok := levelRank[minimum]
		if !ok {
			return Answer{}, &MalformedWindowError{Reason: fmt.Sprintf("%q is no log level", minimum)}
		}
		floor = rank
	}
	if _, err := t.Pass(ctx, "window"); err != nil {
		return Answer{}, err
	}
	answer := Answer{From: since.Truncate(time.Hour), To: until.Truncate(time.Hour).Add(time.Hour)}
	oldest, found, err := t.store.OldestLogSiteHour(ctx)
	if err != nil {
		return Answer{}, err
	}
	if found {
		if !answer.To.After(oldest) {
			return Answer{}, &UnrecordedWindowError{Oldest: oldest}
		}
		if answer.From.Before(oldest) {
			answer.From, answer.Partial = oldest, true
		}
	}
	var wanted []string
	for _, l := range wsm.LogSiteLevels {
		if levelRank[l] >= floor {
			wanted = append(wanted, l)
		}
	}
	rows, err := t.store.LogSiteRows(ctx, answer.From, answer.To, wanted)
	if err != nil {
		return Answer{}, err
	}
	answer.Sites = aggregate(rows)
	return answer, nil
}

// aggregate folds ROWS into one Site per site and level, in the order
// ListLogSitesSuccess answers them: errors first, then the most often fired,
// then by key and the higher level.
func aggregate(rows []wsm.LogSiteRow) []Site {
	type key struct{ site, level string }
	sites := map[key]*Site{}
	for _, r := range rows {
		k := key{r.Site, r.Level}
		s, ok := sites[k]
		if !ok {
			s = &Site{Key: r.Site, Level: r.Level, FirstSeen: r.FirstSeen}
			sites[k] = s
		}
		s.Count += r.Count
		if r.FirstSeen.Before(s.FirstSeen) {
			s.FirstSeen = r.FirstSeen
		}
		if !r.LastSeen.Before(s.LastSeen) {
			s.LastSeen = r.LastSeen
			s.Template, s.Runtime, s.Operation = r.Template, r.Runtime, r.Operation
			s.SampleAt, s.SampleMessage, s.SampleWorkspace, s.SampleContext = r.LastSeen, r.SampleMessage, r.SampleWorkspace, r.SampleContext
		}
	}
	out := make([]Site, 0, len(sites))
	for _, s := range sites {
		out = append(out, *s)
	}
	sort.Slice(out, func(i, j int) bool {
		a, b := out[i], out[j]
		switch {
		case (a.Level == "error") != (b.Level == "error"):
			return a.Level == "error"
		case a.Count != b.Count:
			return a.Count > b.Count
		case a.Key != b.Key:
			return a.Key < b.Key
		default:
			return levelRank[a.Level] > levelRank[b.Level]
		}
	})
	return out
}
