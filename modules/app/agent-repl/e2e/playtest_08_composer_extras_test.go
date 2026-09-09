//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"image"
	"image/color"
	"image/png"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// OWNER 8 of PLAYTEST-PLAN.md's partition: C22-C24 -- the composer's extras.
//
//   - C22: a line, a region and a magit hunk turned into a prompt, through
//     the canned verb (`agent-repl-explain`) and the prompting one
//     (`agent-repl-explain-prompt`), with the file reference carried in
//     the prompt bubble.
//   - C23: an image attached to the composer, the thumbnail marker it
//     draws there, and the attachment travelling as its own `ImageBlock`
//     beside the words.
//   - C24: `agent-repl-history-search` recalling the last accepted prompt
//     into the composer.
//
// Every submission is read at the module's OWN outbound RPC boundary
// (`armSubmissionObserver`, the composer area's observer), and the daemon's
// acceptance is asserted as well through the drawn feed: a green step means
// Emacs composed the right words AND the daemon took them.
//
// WHERE THE PLAN AND THE PRODUCT DISAGREE ON A KEY. The plan writes the
// explain verbs as `SPC TAB e` / `SPC TAB E`. The product binds them under
// the `SPC j` ("claude") prefix as `SPC j e e` and `SPC j e E`
// (`lisp/keybindings.el`); the playbook asserts the product's own binding.

// playtestContextFile is the plain file C22 visits, and its lines are what
// the references count. Five lines, so a line-3 point and a 2-4 region are
// both interior and cannot be confused with the file's edges.
const (
	playtestContextFileName = "notes.txt"
	playtestContextFileBody = "line one\nline two\nline three\nline four\nline five\n"
)

// playtestExplainOriginContext and playtestExplainOriginPrompt are the
// `PromptOrigin` keywords the two explain verbs own; each origin has exactly
// one production send site, so the origin is what says WHICH verb sent.
const (
	playtestExplainOriginContext = ":command-explain-context"
	playtestExplainOriginPrompt  = ":command-explain-prompt"
)

// userPromptBubbleWith is a page predicate: some user prompt bubble's body
// carries TEXT.
func userPromptBubbleWith(text string) string {
	return `Array.prototype.some.call(
             document.querySelectorAll('[data-feed-row][data-row-kind="userPrompt"] .bubble-body'),
             function (b) { return b.textContent.indexOf(` + jsString(text) + `) !== -1; })`
}

// settledResponsesAtLeast is a page predicate: at least N response bubbles
// have settled. It is COUNT-based rather than arm-based on purpose: after
// the first turn settles, the roster arm stays settled until the next
// submission has been taken up, so "await a settled arm" right after a
// submit is satisfied by the previous turn.
func settledResponsesAtLeast(n int) string {
	return fmt.Sprintf(`document.querySelectorAll('[data-feed-row][data-row-kind="activity"][data-unit="response"][data-state="success"]').length >= %d`, n)
}

// ---------------------------------------------------------------------------
// C22. Line, region and hunk prompts
// ---------------------------------------------------------------------------

// TestPlaytestContextPrompts is plan C.22.
func TestPlaytestContextPrompts(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "08-composer-extras-context-prompts",
		"Plan C.22. A line, a region and a magit hunk turned into prompts by the canned verb "+
			"(`SPC j e E`) and the prompting verb (`SPC j e e`); each prompt bubble carries the "+
			"file reference the editor composed.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	// THE TREE IS DIRTY BEFORE ANYTHING READS IT. The scripted fake git
	// answers magit's worktree `diff` with one unified hunk on `dirty.txt`
	// only while the worktree is scripted dirty, and the first status
	// buffer over this repository must already carry that hunk. NO REAL GIT
	// RUNS: the hunk is fakegit's own text.
	repository.SetDirty(repository.Dir, true)
	contextPath := filepath.Join(repository.Dir, playtestContextFileName)
	if err := os.WriteFile(contextPath, []byte(playtestContextFileBody), 0o644); err != nil {
		t.Fatalf("write the context file %s: %v", contextPath, err)
	}

	s.register(t, repository.Dir)
	s.openPanel(t)
	armSubmissionObserver(t, e)

	// THE BINDINGS ARE LOOKED UP, not pressed: both verbs act on the buffer
	// the user stands in, and one of them prompts.
	if want, got := "agent-repl-explain-prompt", e.LeaderBinding("j e e"); got != want {
		t.Fatalf("SPC j e e resolves to %q, want %q", got, want)
	}
	if want, got := "agent-repl-explain", e.LeaderBinding("j e E"); got != want {
		t.Fatalf("SPC j e E resolves to %q, want %q", got, want)
	}
	p.note("the explain bindings looked up",
		"`SPC j e e` resolves to `agent-repl-explain-prompt` and `SPC j e E` to `agent-repl-explain`")

	template := e.EvalString(`agent-repl-explain-prompt-template`)

	// --- a line, through the CANNED verb ---------------------------------
	//
	// THE FILE IS VISITED THROUGH THE MODULE'S OWN EDITOR POPUP, not through
	// a raw `find-file`. `find-file` from the panel's selected window (the
	// composer, a dedicated input window) REPLACED THE WEBVIEW with the
	// visited file, so every capture below it showed the file alone and no
	// panel at all. `agent-repl-popup-open` is the one shared open-a-file
	// subroutine (lisp/popup.el): a right side window at half the frame,
	// which leaves the webview and the composer where they are. Point goes
	// to line 3 and the verb is invoked as a command, which is where a user
	// invoking it stands.
	e.Eval(`(progn
             (defvar agent-repl-playtest08--context nil)
             (setq agent-repl-playtest08--context
                   (agent-repl-popup-open ` + elispString(contextPath) + ` 3))
             t)`)
	if !e.EvalBool(`(and (window-live-p (get-buffer-window agent-repl-playtest08--context)) t)`) {
		t.Fatalf("the editor popup showing %s has no live window", playtestContextFileName)
	}
	p.note("the context file opened in the module's editor popup",
		"`agent-repl-popup-open` put "+playtestContextFileName+" in a live right-side window, and the panel's webview and composer are untouched")
	e.Eval(`(with-current-buffer agent-repl-playtest08--context
              (goto-char (point-min))
              (forward-line 2)
              (call-interactively #'agent-repl-explain)
              t)`)
	lineRef := playtestContextFileName + ":3"
	sent := awaitSubmissions(t, e, 1, "the line prompt to reach the RPC boundary")[0]
	if sent.Origin != playtestExplainOriginContext {
		t.Errorf("the line prompt's origin is %q, want %q", sent.Origin, playtestExplainOriginContext)
	}
	if want := fmt.Sprintf(template, lineRef); sent.Text != want {
		t.Errorf("the line prompt's text is %q, want %q", sent.Text, want)
	}
	s.awaitInPage(t, "the line prompt's bubble to carry the file:line reference", userPromptBubbleWith(lineRef))
	s.awaitInPage(t, "the first turn's response to settle", settledResponsesAtLeast(1))
	p.capture("line-canned", "`agent-repl-explain` (`SPC j e E`) with point on line 3 of "+playtestContextFileName+", no region",
		fmt.Sprintf("the RPC boundary saw origin %s with text %q, and a user prompt bubble carrying %q is drawn",
			playtestExplainOriginContext, sent.Text, lineRef),
		fmt.Sprintf("The webview fills the left of the frame and its feed's user prompt bubble reads "+
			"%q -- the canned template around the file:line reference -- with a prose response bubble "+
			"beneath it; the composer sits under the webview. The right half of the frame is the "+
			"editor popup showing %s, whose point is on line 3 (`line three`).",
			fmt.Sprintf(template, lineRef), playtestContextFileName))

	// --- a region, through the PROMPTING verb ----------------------------
	//
	// `agent-repl-explain-prompt` pre-fills `read-string` with the reference
	// and sends what the user made of it. The minibuffer read is stubbed
	// FOR THE DURATION OF THE ONE CALL, the way this layer drives every
	// prompting verb, and the stub EDITS the initial text rather than
	// replacing it, so the assertion proves the reference was the pre-fill.
	const suffix = " -- what does this block do?"
	e.Eval(`(with-current-buffer agent-repl-playtest08--context
              (let ((transient-mark-mode t))
                (goto-char (point-min))
                (forward-line 1)
                (push-mark (point) t t)
                (forward-line 2)
                (cl-letf (((symbol-function 'read-string)
                           (lambda (_prompt &optional initial &rest _)
                             (concat initial ` + elispString(suffix) + `))))
                  (call-interactively #'agent-repl-explain-prompt)))
              t)`)
	regionRef := playtestContextFileName + ":2-4"
	sent = awaitSubmissions(t, e, 2, "the region prompt to reach the RPC boundary")[1]
	if sent.Origin != playtestExplainOriginPrompt {
		t.Errorf("the region prompt's origin is %q, want %q", sent.Origin, playtestExplainOriginPrompt)
	}
	if want := regionRef + suffix; sent.Text != want {
		t.Errorf("the region prompt's text is %q, want %q", sent.Text, want)
	}
	s.awaitInPage(t, "the region prompt's bubble to carry the file:range reference", userPromptBubbleWith(regionRef))
	s.awaitInPage(t, "the second turn's response to settle", settledResponsesAtLeast(2))
	p.capture("region-prompt", "`agent-repl-explain-prompt` (`SPC j e e`) with lines 2-4 of "+playtestContextFileName+" as the active region, the minibuffer answered with the pre-filled reference plus the user's own words",
		fmt.Sprintf("the RPC boundary saw origin %s with text %q, and a user prompt bubble carrying %q is drawn",
			playtestExplainOriginPrompt, sent.Text, regionRef),
		fmt.Sprintf("The webview still fills the left of the frame, with the composer beneath it and "+
			"the editor popup showing %s on the right half. The webview's newest user prompt bubble "+
			"reads %q: the file:startline-endline reference FIRST, then the words the user added. Two "+
			"earlier bubbles (the line prompt and its answer) sit above it.",
			playtestContextFileName, regionRef+suffix))

	// --- a magit hunk, through the CANNED verb ---------------------------
	//
	// THE STATUS BUFFER IS OPENED THROUGH THE MODULE'S OWN DOOR,
	// `agent-repl--magit-status-same-window`, with the popup's window
	// SELECTED: the door forces same-window display, so magit replaces the
	// popup's buffer on the right half and the panel survives. A raw
	// `magit-status-setup-buffer` instead went through Doom's own magit
	// display function and filled the WHOLE FRAME, leaving no webview in the
	// picture at all. The door returns magit's log line and not the buffer,
	// so the buffer is asked for by mode.
	//
	// The popup's window is a SOFTLY DEDICATED side window, and
	// `display-buffer-same-window` refuses a dedicated window; the
	// dedication is lifted first so the same-window display can land there
	// rather than falling through to the fallback action (which would take
	// the webview's window).
	e.Eval(`(progn
             (require 'magit)
             (defvar agent-repl-playtest08--magit nil)
             (select-window (get-buffer-window agent-repl-playtest08--context))
             (set-window-dedicated-p (selected-window) nil)
             (agent-repl--magit-status-same-window ` + elispString(repository.Dir) + `)
             (setq agent-repl-playtest08--magit (magit-get-mode-buffer 'magit-status-mode))
             (and agent-repl-playtest08--magit t))`)
	e.AwaitTrue("the magit status buffer to draw the scripted dirty hunk",
		`(with-current-buffer agent-repl-playtest08--magit
           (save-excursion
             (goto-char (point-min))
             (and (re-search-forward "^@@ " nil t) t)))`)
	// THE FILE SECTION IS EXPANDED THE WAY A USER EXPANDS IT. magit's status
	// buffer opens dirty.txt's file section COLLAPSED: the hunk's text is in
	// the buffer (so the search above finds it) but invisible, so the
	// capture showed a `modified dirty.txt` line and no hunk under it. Point
	// goes to that line and the section is shown, exactly as `TAB` there
	// would. `magit-section-show` is unconditional, so it is also correct
	// for a section that some future magit opens already visible.
	e.Eval(`(with-current-buffer agent-repl-playtest08--magit
              (goto-char (point-min))
              (re-search-forward "^modified +dirty\\.txt")
              (beginning-of-line)
              (magit-section-show (magit-current-section))
              t)`)
	if !e.EvalBool(`(with-current-buffer agent-repl-playtest08--magit
                       (goto-char (point-min))
                       (re-search-forward "^@@ ")
                       (beginning-of-line)
                       (and (magit-section-match 'hunk) t))`) {
		t.Fatalf("point on the `@@` line of the status buffer is not inside a magit hunk section")
	}
	e.Eval(`(with-current-buffer agent-repl-playtest08--magit
              (call-interactively #'agent-repl-explain)
              t)`)
	// fakegit's one dirty path is `dirty.txt` and its hunk is `@@ -1 +1,2 @@`,
	// so the to-range is lines 1 through 2.
	const hunkRef = "dirty.txt:1-2"
	sent = awaitSubmissions(t, e, 3, "the hunk prompt to reach the RPC boundary")[2]
	if sent.Origin != playtestExplainOriginContext {
		t.Errorf("the hunk prompt's origin is %q, want %q", sent.Origin, playtestExplainOriginContext)
	}
	if want := fmt.Sprintf(template, hunkRef); sent.Text != want {
		t.Errorf("the hunk prompt's text is %q, want %q", sent.Text, want)
	}
	s.awaitInPage(t, "the hunk prompt's bubble to carry the hunk's file:range reference", userPromptBubbleWith(hunkRef))
	s.awaitInPage(t, "the third turn's response to settle", settledResponsesAtLeast(3))
	p.capture("hunk-canned", "`agent-repl-explain` (`SPC j e E`) with point on the `@@ -1 +1,2 @@` hunk of dirty.txt in the workspace's magit status buffer",
		fmt.Sprintf("the RPC boundary saw origin %s with text %q, and a user prompt bubble carrying %q is drawn",
			playtestExplainOriginContext, sent.Text, hunkRef),
		fmt.Sprintf("The webview fills the left of the frame with the composer beneath it, and its "+
			"newest user prompt bubble reads %q: the hunk's own file and to-range, taken from the "+
			"magit section rather than from point's line. The right half of the frame is the magit "+
			"status buffer (it replaced the editor popup's file), showing an `Unstaged changes` "+
			"section whose `modified   dirty.txt` entry is EXPANDED, so the `@@ -1 +1,2 @@` hunk and "+
			"its lines are visible under it.", fmt.Sprintf(template, hunkRef)))
}

// ---------------------------------------------------------------------------
// C23. Attach an image
// ---------------------------------------------------------------------------

// playtestImageWidth and playtestImageHeight size the image C23 attaches.
// Small, so the thumbnail the composer overlays is drawn at its natural size
// under `agent-repl-image-thumbnail-max-height`.
const (
	playtestImageWidth  = 96
	playtestImageHeight = 64
)

// writePlaytestPNG writes a small two-color PNG -- a blue field with a red
// block -- so the picture is unmistakable in a capture and cannot be taken
// for the composer's own background.
func writePlaytestPNG(t *testing.T, path string) {
	t.Helper()
	img := image.NewRGBA(image.Rect(0, 0, playtestImageWidth, playtestImageHeight))
	for y := 0; y < playtestImageHeight; y++ {
		for x := 0; x < playtestImageWidth; x++ {
			c := color.RGBA{R: 0x1e, G: 0x5a, B: 0xd6, A: 0xff}
			if x >= playtestImageWidth/4 && x < 3*playtestImageWidth/4 &&
				y >= playtestImageHeight/4 && y < 3*playtestImageHeight/4 {
				c = color.RGBA{R: 0xd6, G: 0x2c, B: 0x1e, A: 0xff}
			}
			img.SetRGBA(x, y, c)
		}
	}
	f, err := os.Create(path)
	if err != nil {
		t.Fatalf("create the playtest image %s: %v", path, err)
	}
	if err := png.Encode(f, img); err != nil {
		f.Close()
		t.Fatalf("encode the playtest image %s: %v", path, err)
	}
	if err := f.Close(); err != nil {
		t.Fatalf("close the playtest image %s: %v", path, err)
	}
}

// TestPlaytestClipboardImageAttachment is plan C.23.
//
// HOW THE IMAGE GETS IN. `agent-repl-attach-clipboard-image` captures the
// clipboard through `osascript` -- a macOS pasteboard read -- and this world
// is a Linux container with no X clipboard tool in its image (no xclip, no
// xsel), so the verb's own capture step has nothing it can read here. The
// playbook therefore drives the verb's own entry point BELOW the capture:
// `agent-repl-input-attach-image` registers the file on the composer and
// `agent-repl--image-insert-marker` draws the thumbnail marker, which is
// exactly what the verb does once its capture has produced a file. The
// binding to the verb is asserted so the door the user knocks on is known
// to be there.
func TestPlaytestClipboardImageAttachment(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "08-composer-extras-clipboard-image",
		"Plan C.23. A PNG attached to the composer through the attach verb's own entry point, the "+
			"thumbnail marker the composer draws, the `ImageBlock` that travels beside the words, "+
			"and the prompt bubble the feed draws for it.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)

	if want, got := "agent-repl-attach-clipboard-image", e.BindingForIn(s.Input, "C-c C-i"); got != want {
		t.Fatalf("composer C-c C-i resolves to %q, want %q", got, want)
	}
	p.note("the attach binding looked up in the composer",
		"`C-c C-i` in the composer resolves to `agent-repl-attach-clipboard-image`")

	// The file lands where the verb's own capture would put it: the
	// workspace's image directory, made by the module.
	imageDir := e.EvalString(`(agent-repl--image-dir ` + elispString(name) + `)`)
	imagePath := filepath.Join(imageDir, "clip-playtest.png")
	writePlaytestPNG(t, imagePath)
	mediaType := e.EvalString(`agent-repl--image-media-type`)
	marker := e.EvalString(`(agent-repl--image-marker-text ` + elispString(imagePath) + `)`)

	// The words first, then the attachment, the way a person composes.
	const words = "what is in this picture?"
	e.Eval(`(with-current-buffer ` + elispString(s.Input) + `
              (erase-buffer)
              (insert ` + elispString(words) + `)
              t)`)
	if got := composerText(e, s.Input); got != words {
		t.Fatalf("the composer text is %q, want %q", got, words)
	}
	p.capture("composer-words", "the words typed into the composer, nothing attached yet",
		"the composer text is exactly the typed words",
		"The composer window (beneath the webview) shows the typed words on its first line; the "+
			"webview above it shows the idle page: sidebar with the workspace, empty feed, footer status idle.")

	e.Eval(`(with-current-buffer ` + elispString(s.Input) + `
              (agent-repl-input-attach-image ` + elispString(imagePath) + ` ` + elispString(mediaType) + `)
              (agent-repl--image-insert-marker ` + elispString(imagePath) + ` ` + elispString(name) + `)
              t)`)

	// The attachment is REGISTERED, the text carries the MARKER and never
	// the path, and a thumbnail overlay is on the marker.
	attached := e.EvalStrings(`(mapcar (lambda (a) (concat (plist-get a :path) "|" (plist-get a :media-type)))
                                      (agent-repl-input-attachments ` + elispString(name) + `))`)
	if want := []string{imagePath + "|" + mediaType}; strings.Join(attached, "\n") != strings.Join(want, "\n") {
		t.Fatalf("the composer's attachments are %q, want %q", attached, want)
	}
	text := composerText(e, s.Input)
	if !strings.Contains(text, marker) {
		t.Fatalf("the composer text %q does not carry the marker %q", text, marker)
	}
	if strings.Contains(text, imagePath) {
		t.Fatalf("the composer text carries the image PATH as words: %q", text)
	}
	if !e.EvalBool(`(with-current-buffer ` + elispString(s.Input) + `
                       (and (seq-some (lambda (o) (and (overlay-get o 'agent-repl-image)
                                                       (overlay-get o 'display)))
                                      (overlays-in (point-min) (point-max)))
                            t))`) {
		t.Fatalf("no thumbnail overlay is on the composer's marker line")
	}
	// THE COMPOSER IS ON SCREEN BEFORE THE PICTURE IS TAKEN. This capture
	// came out with the webview white and the composer black and wordless
	// while the same run's other captures were painted, so the arrangement
	// itself is asserted here: the composer has a live window and that
	// window starts at the buffer's first character, which is where the
	// typed words and the marker line are. The `redisplay' is forced in the
	// same eval so a redisplay failure, if there is one, surfaces as an
	// elisp error attributed to THIS step rather than as a blank picture.
	if !e.EvalBool(`(let ((w (get-buffer-window ` + elispString(s.Input) + `)))
                       (prog1 (and w (window-live-p w)
                                   (with-current-buffer ` + elispString(s.Input) + `
                                     (= (window-start w) (point-min)))
                                   t)
                         (redisplay t)))`) {
		t.Fatalf("the composer has no live window showing its buffer from the top before the thumbnail capture")
	}
	p.capture("composer-thumbnail", "the words typed, then the image attached through `agent-repl-input-attach-image` and its marker inserted",
		fmt.Sprintf("`agent-repl-input-attachments` holds exactly {%s, %s}; the composer text carries the marker %q and not the path; an overlay with `agent-repl-image` and a `display` image is on the marker",
			imagePath, mediaType, marker),
		"The composer window (beneath the webview) shows the typed words on the first line and, on "+
			"the next line, a small THUMBNAIL of the attached picture: a blue field with a red block "+
			"in its middle. The file's path is nowhere in the composer's text.")

	// Submit with composer RET, and read the UserSaid at the RPC boundary as
	// a whole: a text block with the words and an image block naming the
	// path and media type, in that order.
	e.Eval(`(progn
             (defvar agent-repl-playtest08--said nil)
             (setq agent-repl-playtest08--said nil)
             (defun agent-repl-playtest08--record-said (_conn request &rest _)
               (setq agent-repl-playtest08--said (format "%S" (plist-get request :said))))
             (unless (advice-member-p 'agent-repl-playtest08--record-said 'agent-repl-rpc-submit-prompt)
               (advice-add 'agent-repl-rpc-submit-prompt :before #'agent-repl-playtest08--record-said))
             t)`)
	if want, got := "agent-repl-send", e.BindingForIn(s.Input, "RET"); got != want {
		t.Fatalf("composer RET resolves to %q, want %q", got, want)
	}
	e.KeysIn(s.Input, "RET")
	raw := e.AwaitEval("the submission to reach the RPC boundary", `agent-repl-playtest08--said`,
		func(raw json.RawMessage) bool { return decodeString(raw) != "" })
	said := decodeString(raw)
	textAt := strings.Index(said, `:text "`+words+`"`)
	imageAt := strings.Index(said, `:path "`+imagePath+`"`)
	if textAt < 0 {
		t.Fatalf("the submitted UserSaid carries no text block with %q:\n%s", words, said)
	}
	if imageAt < 0 {
		t.Fatalf("the submitted UserSaid carries no image block naming %q:\n%s", imagePath, said)
	}
	if !strings.Contains(said, `:media-type "`+mediaType+`"`) {
		t.Fatalf("the submitted image block does not state media type %q:\n%s", mediaType, said)
	}
	if imageAt < textAt {
		t.Errorf("the image block travels BEFORE the text block; want the words first:\n%s", said)
	}
	p.note("composer RET pressed",
		fmt.Sprintf("the RPC boundary saw one UserSaid with a text block %q followed by an image block {path %s, media type %s}",
			words, imagePath, mediaType))

	// Acceptance clears the composer AND its attachment list.
	e.AwaitEval("the daemon's acceptance to clear the composer",
		`(with-current-buffer `+elispString(s.Input)+` (buffer-string))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "" })
	if n := e.EvalInt(`(length (agent-repl-input-attachments ` + elispString(name) + `))`); n != 0 {
		t.Errorf("the composer still holds %d attachments after acceptance, want none", n)
	}
	p.note("the submission accepted",
		"the composer is empty and `agent-repl-input-attachments` is empty: the words and the attachment were both consumed by the accepted submission")

	// The bubble carries the words AND a second block for the image. Which
	// block the product draws for the image is READ rather than assumed:
	// `.prompt-block-image` is the attachment chip, and anything else is
	// what the reviewer must see and file.
	s.awaitInPage(t, "the prompt bubble to carry the words", userPromptBubbleWith(words))
	s.awaitInPage(t, "the prompt bubble to carry a second block beside the words",
		`document.querySelector('[data-feed-row][data-row-kind="userPrompt"] .bubble-body').querySelectorAll('.prompt-block').length >= 2`)
	s.awaitInPage(t, "the turn's response to settle", settledResponsesAtLeast(1))
	imageBlockClass := s.pageString(t, "the class of the bubble's image block",
		`(function () { var blocks = document.querySelector('[data-feed-row][data-row-kind="userPrompt"] .bubble-body').querySelectorAll('.prompt-block');
                       return blocks[blocks.length - 1].className; })()`)
	p.capture("bubble-attachment", "the accepted prompt drawn in the feed",
		fmt.Sprintf("a user prompt bubble carries %q and a second `.prompt-block` for the image, whose class the page reports as %q",
			words, imageBlockClass),
		fmt.Sprintf("The feed's user prompt bubble shows the words %q and, beneath them, the ATTACHED "+
			"IMAGE itself drawn as a chip: the blue field with the red block, loaded from the "+
			"daemon's resolution of the image reference (a `.prompt-block-image` element). A "+
			"placeholder reading `unsupported block: image` in its place is NOT the chip and is a "+
			"defect. A prose response bubble sits beneath.", words))
}

// pageString reads one JavaScript string expression out of the page.
func (s *playtestScenario) pageString(t *testing.T, what, expression string) string {
	t.Helper()
	s.E.Eval(`(setq agent-repl-playtest--js nil)`)
	raw := s.E.AwaitEvalFor(playtestPageBound, what,
		`(agent-repl-playtest--probe `+elispString(s.Name)+` `+elispString(expression)+`)`,
		func(raw json.RawMessage) bool { return decodeString(raw) != "" })
	return decodeString(raw)
}

// ---------------------------------------------------------------------------
// C24. History search recall
// ---------------------------------------------------------------------------

// TestPlaytestHistorySearchRecall is plan C.24.
//
// History is pushed on ACCEPTANCE, not on the keystroke (the composer area's
// scenario 25 says so), so the recall waits on the composer being cleared --
// the acceptance's own visible act. `agent-repl-history-search` picks
// through `completing-read`, which is stubbed for the duration of the one
// call to choose the FIRST candidate: index 0 is the most recent entry.
func TestPlaytestHistorySearchRecall(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "08-composer-extras-history-search",
		"Plan C.24. One prompt submitted and accepted, then recalled into the composer through "+
			"`agent-repl-history-search` (`C-M-r`).")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	if want, got := "agent-repl-history-search", e.BindingForIn(s.Input, "C-M-r"); got != want {
		t.Fatalf("composer C-M-r resolves to %q, want %q", got, want)
	}
	p.note("the history search binding looked up in the composer",
		"`C-M-r` in the composer resolves to `agent-repl-history-search`")

	const submitted = "remember this one for the history search"
	s.submit(t, submitted)
	e.AwaitEval("the daemon's acceptance to clear the composer",
		`(with-current-buffer `+elispString(s.Input)+` (buffer-string))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "" })
	s.awaitInPage(t, "the turn's response to settle", settledResponsesAtLeast(1))
	p.note("one prompt submitted with composer RET and accepted",
		"the composer is empty, which is the acceptance's own visible act and the moment the history holds the prompt")

	e.Eval(`(with-current-buffer ` + elispString(s.Input) + `
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt collection &rest _) (car collection))))
                (call-interactively #'agent-repl-history-search))
              t)`)
	if got := composerText(e, s.Input); got != submitted {
		t.Fatalf("history search put %q in the composer, want the accepted prompt %q", got, submitted)
	}
	p.capture("history-recalled", "`agent-repl-history-search` invoked in the empty composer, the completion answered with its first (most recent) candidate",
		fmt.Sprintf("the composer's text is exactly %q", submitted),
		fmt.Sprintf("The composer window (beneath the webview) holds the recalled prompt %q, and the "+
			"feed above it shows that same prompt's bubble followed by its prose answer.", submitted))
}
