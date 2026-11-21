package main

import (
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// logsNoun is the agent API's noun over what the system logged.
const logsNoun = "logs"

// logsSites asks the daemon which log sites fired in a window
// (agentrepl.v1.ListLogSites): every site with its template, how often it
// fired, when it first and last did, and its latest record.
func logsSites(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	since := fs.String("since", "", "the window's start, required: an age (90m, 36h, 7d) or a date (2026-10-20, RFC 3339)")
	until := fs.String("until", "", "the window's end, spelled as -since is; unset is now")
	level := fs.String("level", "", "only records at debug, info, warn or error and above; unset is every level")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 0 {
		return c.usageFailed("nothing is positional")
	}
	if *since == "" {
		return c.usageFailed("-since is required")
	}
	now := c.deps.now()
	start, err := parseWhen(*since, now)
	if err != nil {
		return c.usageFailed("-since: %v", err)
	}
	window := &agentreplv1.ListLogSitesWindow{SinceMs: start.UnixMilli()}
	if *until != "" {
		end, err := parseWhen(*until, now)
		if err != nil {
			return c.usageFailed("-until: %v", err)
		}
		ms := end.UnixMilli()
		window.UntilMs = &ms
	}
	req := &agentreplv1.ListLogSitesRequest{Window: window}
	if *level != "" {
		minimum, ok := logSitesMinimum(*level)
		if !ok {
			return c.usageFailed("-level is debug, info, warn or error, not %q", *level)
		}
		req.MinimumLevel = minimum
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	resp, err := c.client.ListLogSites(c.ctx, connect.NewRequest(req))
	return answerDescribed(c, resp, err, func(r *agentreplv1.ListLogSitesResponse) (string, error) {
		if r.GetError().GetUnrecordedWindow() != nil {
			return "the window lies before every log site row the daemon keeps", nil
		}
		return "", fmt.Errorf("the ListLogSites refusal %v has no sentence", r.GetError())
	})
}

// logSitesMinimum is LEVEL as ListLogSites's minimum level.
func logSitesMinimum(level string) (*agentreplv1.ListLogSitesMinimumLevel, bool) {
	switch level {
	case "debug":
		return &agentreplv1.ListLogSitesMinimumLevel{Level: &agentreplv1.ListLogSitesMinimumLevel_Debug{Debug: &agentreplv1.ClientLogLevelDebug{}}}, true
	case "info":
		return &agentreplv1.ListLogSitesMinimumLevel{Level: &agentreplv1.ListLogSitesMinimumLevel_Info{Info: &agentreplv1.ClientLogLevelInfo{}}}, true
	case "warn":
		return &agentreplv1.ListLogSitesMinimumLevel{Level: &agentreplv1.ListLogSitesMinimumLevel_Warn{Warn: &agentreplv1.ClientLogLevelWarn{}}}, true
	case "error":
		return &agentreplv1.ListLogSitesMinimumLevel{Level: &agentreplv1.ListLogSitesMinimumLevel_Error{Error: &agentreplv1.ClientLogLevelError{}}}, true
	}
	return nil, false
}
