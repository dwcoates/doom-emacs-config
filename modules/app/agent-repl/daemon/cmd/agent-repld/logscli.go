package main

import (
	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/server"
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
		minimum, ok := server.LogSitesMinimum(*level)
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
		return describeLogSitesRefusal(r.GetError())
	})
}

// describeLogSitesRefusal says in one plain sentence why the sites that fired
// in a window could not be listed. Every ListLogSitesError arm has its own
// sentence.
func describeLogSitesRefusal(e *agentreplv1.ListLogSitesError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.ListLogSitesError_UnrecordedWindow:
		return "the window lies before every log site row the daemon keeps", nil
	}
	return "", undescribed("listing the log sites", e)
}
