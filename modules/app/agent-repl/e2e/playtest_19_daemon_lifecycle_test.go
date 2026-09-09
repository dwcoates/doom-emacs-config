//go:build playtest

package e2e

import (
	"fmt"
	"strconv"
	"strings"
	"testing"
)

// OWNER 19 of PLAYTEST-PLAN.md's partition: J57-J60 -- scheduled shutdown,
// shutdown now, restarts, and handover.
//
// J.57 is here. J.58, J.59 and J.60 are unwritten.

// TestPlaytestScheduledDrainBanner is plan J.57: a scheduled shutdown, and
// the standing banner it raises in two places at once.
func TestPlaytestScheduledDrainBanner(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "19-drain",
		"Plan J.57. A scheduled shutdown, and the standing drain banner it puts on Emacs's mode "+
			"line and across the webapp.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)
	p.note("one repository registered with its panel open",
		"the panel's webview is live and the webapp drew its footer")

	// Five minutes out, so the daemon is still serving for the rest of the
	// playbook and its teardown. The command prompts for its reason, so the
	// reader is bound for the duration of the one call -- the standard ERT
	// way, which keeps the command running its own argument collection.
	const drainMinutes = 5
	const drainReason = "maintenance"
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) ` + elispString(drainReason) + `)))
              (agent-repl-daemon-shutdown-schedule ` + strconv.Itoa(drainMinutes) + `)
              t)`)
	e.AwaitTrue("the daemon's drain_scheduled push to reach Emacs", `(and agent-repl-link-drain t)`)
	if arm := e.EvalString(`(format "%s" (plist-get (plist-get agent-repl-link-drain :reason) :arm))`); arm != ":"+drainReason {
		t.Fatalf("the standing drain's reason arm is %q, want %q", arm, ":"+drainReason)
	}
	// The segment's own composition is the subject here, which is this
	// layer's sanctioned exception to "never scrape human text where a
	// variable exists".
	segment := e.EvalString(`(or agent-repl-link-drain-segment "")`)
	if !strings.HasPrefix(segment, "drain ") || !strings.HasSuffix(segment, "· "+drainReason) {
		t.Fatalf("the drain segment is %q, want \"drain HH:MM · %s\"", segment, drainReason)
	}
	s.awaitInPage(t, "the webapp's own drain banner to be drawn",
		`document.querySelector('[data-component="drain-banner"]') &&
         document.querySelector('[data-component="drain-banner"]').textContent.trim() !== ""`)
	p.capture("drain-scheduled", "`agent-repl-daemon-shutdown-schedule` five minutes out, reason \"maintenance\"",
		fmt.Sprintf("`agent-repl-link-drain` carries the reason arm `:%s`, `agent-repl-link-drain-segment` renders %q, and the webapp's own drain banner is non-empty", drainReason, segment),
		"The WEBAPP draws a STANDING DRAIN BANNER across the top of the panel, naming the reason "+
			"(\"maintenance\") and how long is left. Emacs's own `drain HH:MM · maintenance` segment is "+
			"asserted as a string rather than read off this picture: it lives in `global-mode-string`, "+
			"which Doom's mode line renders on the right of whatever line has room, so which line "+
			"carries it is not this playbook's claim.")
}
