package server

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/types/known/structpb"

	"agent-repld/internal/dlog"
	"agent-repld/internal/logsites"
)

// LogSites answers the log sites that fired in a window (logsites.Tallier).
type LogSites interface {
	Window(ctx context.Context, since, until time.Time, minimum string) (logsites.Answer, error)
}

func (s *requestLoggingServer) ListLogSites(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListLogSitesRequest],
) (*connect.Response[agentreplv1.ListLogSitesResponse], error) {
	return serveUnary(s, ctx, "ListLogSites", req, s.server.ListLogSites)
}

// ListLogSites answers every log site that fired in a window, from the
// daemon's per-deploy tally (endpoint_list_log_sites.proto). A pass brings
// the tally up to now first (logsites.Tallier.Window).
func (s *server) ListLogSites(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListLogSitesRequest],
) (*connect.Response[agentreplv1.ListLogSitesResponse], error) {
	const rpc = "ListLogSites"
	since, until, minimum, cerr := listLogSitesQuery(req.Msg, time.Now())
	if cerr != nil {
		return nil, cerr
	}
	answer, err := s.deps.LogSites.Window(ctx, since, until, minimum)
	var (
		unrecorded *logsites.UnrecordedWindowError
		malformed  *logsites.MalformedWindowError
	)
	switch {
	case errors.As(err, &unrecorded):
		s.log.Info(rpc, "the window lies before every kept log site row", dlog.Context{
			"since": since, "until": until, "oldest": unrecorded.Oldest,
		})
		return connect.NewResponse(&agentreplv1.ListLogSitesResponse{Result: &agentreplv1.ListLogSitesResponse_Error{
			Error: &agentreplv1.ListLogSitesError{Cause: &agentreplv1.ListLogSitesError_UnrecordedWindow{
				UnrecordedWindow: &agentreplv1.ListLogSitesUnrecordedWindow{OldestSinceMs: unrecorded.Oldest.UnixMilli()},
			}},
		}}), nil
	case errors.As(err, &malformed):
		return nil, invalid("window", malformed.Reason)
	case err != nil:
		return nil, fail(s.log, rpc, err)
	}
	success, err := listLogSitesSuccess(answer)
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	return connect.NewResponse(&agentreplv1.ListLogSitesResponse{
		Result: &agentreplv1.ListLogSitesResponse_Success{Success: success},
	}), nil
}

// listLogSitesQuery reads the window and the minimum level a request asks
// for; an unset end is NOW.
func listLogSitesQuery(req *agentreplv1.ListLogSitesRequest, now time.Time) (time.Time, time.Time, string, *connect.Error) {
	w := req.GetWindow()
	if w == nil {
		return time.Time{}, time.Time{}, "", invalid("window", "is required")
	}
	since := time.UnixMilli(w.GetSinceMs())
	until := now
	if w.UntilMs != nil {
		until = time.UnixMilli(w.GetUntilMs())
	}
	if until.Before(since) {
		return time.Time{}, time.Time{}, "", invalid("window", fmt.Sprintf("until_ms %d precedes since_ms %d", until.UnixMilli(), since.UnixMilli()))
	}
	minimum := ""
	if m := req.MinimumLevel; m != nil {
		switch m.GetLevel().(type) {
		case *agentreplv1.ListLogSitesMinimumLevel_Debug:
			minimum = "debug"
		case *agentreplv1.ListLogSitesMinimumLevel_Info:
			minimum = "info"
		case *agentreplv1.ListLogSitesMinimumLevel_Warn:
			minimum = "warn"
		case *agentreplv1.ListLogSitesMinimumLevel_Error:
			minimum = "error"
		default:
			return time.Time{}, time.Time{}, "", invalid("minimum_level", "names no level")
		}
	}
	return since, until, minimum, nil
}

// listLogSitesSuccess is ANSWER on the wire.
func listLogSitesSuccess(answer logsites.Answer) (*agentreplv1.ListLogSitesSuccess, error) {
	out := &agentreplv1.ListLogSitesSuccess{Covered: &agentreplv1.ListLogSitesCovered{}}
	if answer.Partial {
		out.Covered.Extent = &agentreplv1.ListLogSitesCovered_Partial{Partial: &agentreplv1.ListLogSitesCoveredPartial{
			SinceMs: answer.From.UnixMilli(), UntilMs: answer.To.UnixMilli(),
		}}
	} else {
		out.Covered.Extent = &agentreplv1.ListLogSitesCovered_Whole{Whole: &agentreplv1.ListLogSitesCoveredWhole{
			SinceMs: answer.From.UnixMilli(), UntilMs: answer.To.UnixMilli(),
		}}
	}
	for _, site := range answer.Sites {
		level, err := logSiteLevel(site.Level)
		if err != nil {
			return nil, err
		}
		var fields map[string]any
		if err := json.Unmarshal([]byte(site.SampleContext), &fields); err != nil {
			return nil, fmt.Errorf("the sample context of log site %s is not a JSON object: %w", site.Key, err)
		}
		sampleContext, err := structpb.NewStruct(fields)
		if err != nil {
			return nil, fmt.Errorf("the sample context of log site %s has no protobuf Struct: %w", site.Key, err)
		}
		sample := &agentreplv1.LogSiteSample{AtMs: site.SampleAt.UnixMilli(), Message: site.SampleMessage, Context: sampleContext}
		if site.SampleWorkspace != "" {
			ws := site.SampleWorkspace
			sample.WorkspaceId = &ws
		}
		out.Sites = append(out.Sites, &agentreplv1.LogSite{
			Key: site.Key, Template: site.Template, Runtime: site.Runtime, Level: level,
			Operation: site.Operation, Count: uint64(site.Count),
			FirstSeenMs: site.FirstSeen.UnixMilli(), LastSeenMs: site.LastSeen.UnixMilli(),
			Sample: sample,
		})
	}
	return out, nil
}

// logSiteLevel is LEVEL as its arm. A level the tally does not keep never
// reaches here (wsm's log_site_tally CHECK).
func logSiteLevel(level string) (*agentreplv1.LogSiteLevel, error) {
	switch level {
	case "debug":
		return &agentreplv1.LogSiteLevel{Level: &agentreplv1.LogSiteLevel_Debug{Debug: &agentreplv1.ClientLogLevelDebug{}}}, nil
	case "info":
		return &agentreplv1.LogSiteLevel{Level: &agentreplv1.LogSiteLevel_Info{Info: &agentreplv1.ClientLogLevelInfo{}}}, nil
	case "warn":
		return &agentreplv1.LogSiteLevel{Level: &agentreplv1.LogSiteLevel_Warn{Warn: &agentreplv1.ClientLogLevelWarn{}}}, nil
	case "error":
		return &agentreplv1.LogSiteLevel{Level: &agentreplv1.LogSiteLevel_Error{Error: &agentreplv1.ClientLogLevelError{}}}, nil
	}
	return nil, fmt.Errorf("log site level %q is no level the tally keeps", level)
}
