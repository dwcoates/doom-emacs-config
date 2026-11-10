package externalbrowser

import (
	"net/url"
	"regexp"
	"strings"

	"agent-repld/internal/usersetup"
)

// A SLACK LINK OPENS IN THE SLACK APP, NEVER IN A BROWSER TAB.
//
// A Slack permalink handed to Chrome opens a tab whose page redirects to the
// desktop app and then STAYS: the tab outlives the redirect, and landing on it
// later navigates the app to that link again. The daemon therefore never hands
// one to a browser. It rewrites the permalink to the `slack://` deep link
// Slack's own redirect page answers with, and hands that to the desktop app,
// so no tab is ever made.
//
// The deep link names the Slack workspace by its TEAM ID, which a permalink
// does not carry: the permalink names only its subdomain. Which team ID a
// subdomain is belongs to the person running agent-repl, so it is read from
// EnvSlackTeams; a link whose subdomain is not named there goes to the browser
// as any other link does.

// EnvSlackTeams names the Slack workspaces whose links open in the Slack app,
// as comma-separated `<subdomain>=<team-id>` pairs, such as
// `acme=T0123ABCD`.
const EnvSlackTeams = "AGENT_REPL_SLACK_TEAMS"

// SlackOpenBinary is the program a Slack deep link is handed to. `open` hands
// a `slack://` url to the application registered for the scheme.
const SlackOpenBinary = "/usr/bin/open"

// SlackTeams is the team ID of each Slack subdomain, keyed by the subdomain.
type SlackTeams map[string]string

var (
	// slackSubdomainShape is what Slack accepts as a workspace subdomain.
	slackSubdomainShape = regexp.MustCompile(`^[a-z0-9][a-z0-9-]*$`)
	// slackTeamShape is a Slack team ID: T for a workspace, E for an
	// Enterprise Grid organization.
	slackTeamShape = regexp.MustCompile(`^[TE][A-Z0-9]+$`)
	// slackChannelShape is a conversation ID: a channel, a private channel or
	// group, or a direct message.
	slackChannelShape = regexp.MustCompile(`^[CGD][A-Z0-9]+$`)
	// slackPermalinkTS is a permalink's message segment, the message's
	// timestamp with its dot removed: ten digits of seconds, six of micros.
	slackPermalinkTS = regexp.MustCompile(`^p([0-9]{10})([0-9]{6})$`)
	// slackTS is a message timestamp as a query parameter carries it.
	slackTS = regexp.MustCompile(`^[0-9]{10}\.[0-9]{6}$`)
)

// SlackTeamsFromEnv reads EnvSlackTeams. Unset is no teams. A pair that does
// not parse, a subdomain or team ID of the wrong shape, and a subdomain named
// twice are each an error naming the pair, never a pair skipped.
func SlackTeamsFromEnv(getenv func(string) string) (SlackTeams, error) {
	pairs, err := usersetup.Pairs("externalbrowser", EnvSlackTeams, "<subdomain>=<team-id>", getenv(EnvSlackTeams))
	if err != nil {
		return nil, err
	}
	teams := SlackTeams{}
	for _, p := range pairs {
		if !slackSubdomainShape.MatchString(p.Key) {
			return nil, usersetup.Errorf("externalbrowser: %s pair %q names %q, which is not a lowercase Slack subdomain", EnvSlackTeams, p.Entry, p.Key)
		}
		if !slackTeamShape.MatchString(p.Value) {
			return nil, usersetup.Errorf("externalbrowser: %s pair %q names %q, which is not a Slack team ID", EnvSlackTeams, p.Entry, p.Value)
		}
		if _, dup := teams[p.Key]; dup {
			return nil, usersetup.Errorf("externalbrowser: %s names the subdomain %q twice", EnvSlackTeams, p.Key)
		}
		teams[p.Key] = p.Value
	}
	return teams, nil
}

// SlackDeepLink answers the `slack://` deep link that opens rawURL in the
// Slack app, and whether there is one.
//
// rawURL has one when it is a permalink, `https://<subdomain>.slack.com/
// archives/<channel>[/p<timestamp>][?thread_ts=<timestamp>]`, on a subdomain
// teams names. When it has none, why says what kept a slack.com link from the
// app, for the record; why is empty for a link that is not Slack's at all.
func SlackDeepLink(rawURL string, teams SlackTeams) (deep string, ok bool, why string) {
	u, err := url.Parse(rawURL)
	if err != nil {
		return "", false, ""
	}
	subdomain, isSlack := strings.CutSuffix(strings.ToLower(u.Hostname()), ".slack.com")
	if !isSlack || strings.Contains(subdomain, ".") {
		return "", false, ""
	}
	team, named := teams[subdomain]
	if !named {
		return "", false, "subdomain-unconfigured"
	}
	segments := strings.Split(strings.Trim(u.Path, "/"), "/")
	if len(segments) < 2 || len(segments) > 3 || segments[0] != "archives" || !slackChannelShape.MatchString(segments[1]) {
		return "", false, "not-a-permalink"
	}
	var b strings.Builder
	b.WriteString("slack://channel?team=" + team + "&id=" + segments[1])
	if len(segments) == 3 {
		ts := slackPermalinkTS.FindStringSubmatch(segments[2])
		if ts == nil {
			return "", false, "not-a-permalink"
		}
		b.WriteString("&message=" + ts[1] + "." + ts[2])
	}
	if thread := u.Query().Get("thread_ts"); thread != "" {
		if !slackTS.MatchString(thread) {
			return "", false, "not-a-permalink"
		}
		b.WriteString("&thread_ts=" + thread)
	}
	return b.String(), true, ""
}
