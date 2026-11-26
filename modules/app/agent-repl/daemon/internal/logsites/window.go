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

// An Answer is every site that fired in a window, and the span read.
type Answer struct {
	Sites []Site
	// From and To are the covered span: [From, To). Each is the window's own
	// edge where the tally splits its rows there, and the edge widened to its
	// whole hour where it does not (Widened).
	From, To time.Time
	// Partial reports that the window began before the oldest kept row, so
	// the answer covers only from that row's bucket on.
	Partial bool
	// Widened reports that an edge of the window splits no tally row, so the
	// answer read that edge's whole hour: a row all of whose records lie
	// outside the window is still left out, but a site listed may have fired
	// in that hour only outside the window, and its counts may hold records
	// from outside it.
	Widened bool
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
// above (every level when empty), after a pass brings the rows up to now.
//
// AN EDGE THE TALLY SPLITS ITS ROWS AT IS READ EXACTLY: a whole hour, a mark
// (wsm.DB.RecordLogSiteMark) or a deploy's start, so no record from past it
// is answered. Any other edge widens outward to its whole hour, and the
// answer says so (Answer.Widened); a row whose records all lie past that edge
// is still left out, by its first and last records.
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
	fromEdge, err := t.edgeAt(ctx, since)
	if err != nil {
		return Answer{}, err
	}
	toEdge, err := t.edgeAt(ctx, until)
	if err != nil {
		return Answer{}, err
	}
	answer := Answer{From: since, To: until}
	if fromEdge == wsm.LogSiteEdgeNone {
		answer.From, answer.Widened = since.Truncate(time.Hour), true
	}
	if toEdge == wsm.LogSiteEdgeNone {
		answer.To, answer.Widened = until.Truncate(time.Hour).Add(time.Hour), true
	}
	oldest, found, err := t.store.OldestLogSiteHour(ctx)
	if err != nil {
		return Answer{}, err
	}
	if found {
		if !answer.To.After(oldest) {
			return Answer{}, &UnrecordedWindowError{Oldest: oldest}
		}
		if answer.From.Before(oldest) {
			answer.From, answer.Partial, fromEdge = oldest, true, wsm.LogSiteEdgeMark
		}
	}
	var wanted []string
	for _, l := range wsm.LogSiteLevels {
		if levelRank[l] >= floor {
			wanted = append(wanted, l)
		}
	}
	// Every record in [From, To) is in a bucket starting at or after From's
	// hour and before To: a bucket starts at or before its every record.
	rows, err := t.store.LogSiteRows(ctx, answer.From.Truncate(time.Hour), answer.To, wanted)
	if err != nil {
		return Answer{}, err
	}
	kept := rows[:0]
	for _, r := range rows {
		if onOrAfter(r, fromEdge, answer.From, since) && before(r, toEdge, answer.To, until) {
			kept = append(kept, r)
		}
	}
	answer.Sites = aggregate(kept)
	return answer, nil
}

// edgeAt answers what AT is to the tally's rows. A whole hour splits every
// row at its bucket's start, as a mark does.
func (t *Tallier) edgeAt(ctx context.Context, at time.Time) (wsm.LogSiteEdge, error) {
	if at.Equal(at.Truncate(time.Hour)) {
		return wsm.LogSiteEdgeMark, nil
	}
	edge, err := t.store.LogSiteEdgeAt(ctx, at)
	if err != nil {
		return wsm.LogSiteEdgeNone, fmt.Errorf("logsites: read what the window's edge at %s is to the tally: %w", at.Format(time.RFC3339Nano), err)
	}
	return edge, nil
}

// onOrAfter reports whether row R may hold a record at or after the window's
// start: exactly by its bucket or its deploy at an edge EDGE that splits rows
// at FROM, and by its last record against SINCE at one that does not.
func onOrAfter(r wsm.LogSiteRow, edge wsm.LogSiteEdge, from, since time.Time) bool {
	switch edge {
	case wsm.LogSiteEdgeMark:
		return !r.Bucket.Before(from)
	case wsm.LogSiteEdgeDeploy:
		return !r.Deploy.Before(from)
	default:
		return !r.LastSeen.Before(since)
	}
}

// before reports whether row R may hold a record before the window's end:
// exactly by its bucket or its deploy at an edge EDGE that splits rows at TO,
// and by its first record against UNTIL at one that does not.
func before(r wsm.LogSiteRow, edge wsm.LogSiteEdge, to, until time.Time) bool {
	switch edge {
	case wsm.LogSiteEdgeMark:
		return r.Bucket.Before(to)
	case wsm.LogSiteEdgeDeploy:
		return r.Deploy.Before(to)
	default:
		return r.FirstSeen.Before(until)
	}
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
