// keydriver.swift — post REAL key events to one Emacs process and have its own
// keymap resolve them.
//
// A realtest's input is what the owner would send: a key event Emacs's keymap
// resolves. An elisp call that performs the act instead tests the function and
// says nothing about whether the chord reaches it, which is exactly the class of
// defect a realtest exists to catch — and the plan (docs/REALTEST-PLAN.md) rules
// that if key delivery to Emacs is impossible on macOS, that is SURFACED to the
// owner rather than worked around with elisp.
//
// WHY THIS ACTIVATES EMACS FOR THE INSTANT OF THE KEYPRESS. Run 3 posted
// `CGEventPostToPid` to the Emacs pid with no activation and the events never
// reached the keymap: `(recent-keys)` did not contain them. The reason is
// structural, not a bug in the post. A CGEvent addressed to a pid lands on that
// process's event queue, but AppKit only dispatches a key event to the KEY
// WINDOW, and a background application that was launched hidden (the realtest
// launches Emacs with `open -gj`) has no key window to receive it, so the event
// is dropped with no error — the worst failure mode a test can have. A
// session-level `CGEventPost(.cgSessionEventTap, ...)` would reach a key window,
// but it targets whatever is FRONTMOST, which is not Emacs here and is the owner
// we must not type into.
//
// The one route that reliably reaches a specific Emacs's keymap is to make that
// Emacs key for the keypress. So this helper records the frontmost application,
// activates the target Emacs, posts keyDown then keyUp addressed to its pid,
// and lets the event loop consume them while Emacs is still key.
//
// WHO HANDS FOCUS BACK, AND WHEN (OWNER RULING, 2026-09-13). It used to be this
// helper, immediately, once per press: activate, post, restore. That flickered
// the owner's desktop once per keystroke and left no way to tell by looking at
// the screen whether a sweep was still running. The policy now is that THE
// SWEEP steals focus once at its start (`--take`) and hands it back once at its
// end (`--give-back`), and a press made with `--keep-focus` leaves the target
// frontmost instead of restoring. A press that finds Emacs NOT frontmost — the
// owner clicked away, or the realtest just cold-started a new Emacs —
// re-activates it and says `refocused=yes` in its receipt. Without
// `--keep-focus` the old per-press restore is exactly what happens, for a
// caller that is not inside a sweep.
// (An accessibility route — AXUIElement posting to the focused UI element — was
// considered; macOS exposes no general key-event delivery to a non-frontmost
// app's first responder through it, so it is not a substitute here.)
//
// It needs accessibility trust: an untrusted process's synthetic events are
// dropped silently by the window server, so `--check` reports the trust state as
// its own answer and the caller refuses to interpret a silent success. The same
// trust is what lets it READ whether the target has a key window before
// posting — a reading it reports rather than a gate it refuses on, except on
// the unheld path where nothing downstream will check; see "WHETHER THE TARGET
// CAN RECEIVE A KEY IS A READING, NOT A GATE" below.
//
// WHY A KEY WINDOW AT THE INSTANT OF THE POST WAS NOT ENOUGH. The 2026-09-12
// sweep still lost keys after the focused-window precondition landed — three
// `C-g`s posted while a prompt stood, and the `<tab>` of a `SPC TAB o`. A
// posted CGEvent is not consumed at the moment it is posted: it is queued on
// the target process and dispatched by `-[NSApplication sendEvent:]` when that
// process next turns its run loop, and `sendEvent:` routes a key event to
// `[NSApp keyWindow]` AT DISPATCH TIME. This helper used to hand focus back
// 0.3s after posting, so an Emacs that was busy for longer than that — and a
// standing minibuffer with the panel drain running behind it is exactly that
// Emacs — resigned key status with the event still in its queue and dropped it
// when it finally looked. The window in which the target had to hold key was
// necessary and about a quarter of a second wide.
//
// So the helper can now HOLD the target key until the caller says the key was
// consumed. `--hold` posts, prints `keydriver-posted`, and keeps the target
// active until a line arrives on stdin or the hold ceiling expires, then
// restores focus. The caller (keys.go) reads Emacs's own account of its input
// in that window and releases as soon as Emacs says the key arrived, so the
// key-ness of the window covers the whole life of the event instead of a fixed
// guess. Nothing here decides delivery: the helper reports what it saw and the
// caller reads the editor.
//
// AND THERE IS ONE STATE IN WHICH NO ACTIVATION CAN SUCCEED AT ALL: A LOCKED
// SCREEN. While the login window stands in front of the session the window
// server grants activation to nobody, so `activate()` is accepted and changes
// nothing, the target never becomes frontmost, and the editor never sees a
// focus edge — while a pid-addressed CGEvent still reaches the process, so keys
// keep arriving and keep confirming. That pair is exactly the shape of the
// 2026-09-12 evening sweeps: zero key-delivery failures and a pre-creation
// queue that parked forever. It is REPORTED rather than worked around — there
// is no way to focus an application behind a locked screen — and `--session`
// exists so the caller can name it instead of waiting out a ceiling for a focus
// edge that cannot happen.
//
// Usage:
//   keydriver --check                      exit 0 trusted, 1 not trusted
//   keydriver --session                    print screenLocked=yes|no|unknown
//   keydriver --take [<pid>]               print where focus is now as a token,
//                                          and bring that pid forward if given
//   keydriver --give-back <token>          hand focus back to that token
//   keydriver [--hold[=SECONDS]] [--keep-focus] <pid> <keycode> [modifiers]
//                                          activate pid, post keyDown/keyUp,
//                                          hold the target key until released
//                                          (with --hold), and restore prior
//                                          focus unless --keep-focus says the
//                                          sweep is holding it
//
// `--hold` also says the CALLER WILL CONFIRM the key against the editor, so the
// key-window reading becomes advisory and the post goes ahead whatever it says.
// Without it nothing downstream checks, and a target the window server says
// cannot receive a key is refused instead of posted into.
//
// Modifiers are comma-separated: command, shift, option, control. They are
// spelled as words rather than as a bitmask so a caller's intent is readable in
// the process table when a run is being watched.

import AppKit
import ApplicationServices
import CoreGraphics
import Foundation

func fail(_ message: String) -> Never {
    FileHandle.standardError.write(Data(("keydriver: " + message + "\n").utf8))
    exit(2)
}

// activate brings one application forward, using the non-deprecated API where it
// is available and the older options form otherwise.
func activate(_ app: NSRunningApplication) {
    if #available(macOS 14.0, *) {
        app.activate()
    } else {
        app.activate(options: [.activateIgnoringOtherApps])
    }
}

// axMessagingTimeout bounds ONE accessibility query, so one unresponsive target
// cannot hang the helper past the caller's own ceiling; an expiry is an
// unanswered query, handled as one. Float because that is what the
// accessibility API takes.
let axMessagingTimeout: Float = 1.0

// spin runs the run loop for up to `seconds`, or until `done` returns true. It
// is how this helper WAITS on a state change rather than sleeping blindly: the
// activation is asynchronous, so posting before the target is key would drop the
// event exactly as the no-activation path did.
func spin(upTo seconds: TimeInterval, until done: () -> Bool) {
    let deadline = Date().addingTimeInterval(seconds)
    while !done() && Date() < deadline {
        RunLoop.current.run(mode: .default, before: Date().addingTimeInterval(0.02))
    }
}

// screenLockWord reads whether the login window stands in front of this
// session, which is the one state in which no application can be activated.
//
// Three answers, and the third is not folded into the second: an unlocked
// session simply has no `CGSSessionScreenIsLocked` key, so an ABSENT key is a
// definite no, while a session dictionary that could not be read at all is
// UNKNOWN and must never be reported as unlocked — a harness that asserts an
// unlocked screen it did not read would send the next reader looking in the
// wrong place.
func screenLockWord() -> String {
    guard let session = CGSessionCopyCurrentDictionary() as? [String: Any], !session.isEmpty else {
        return "unknown"
    }
    guard let locked = session["CGSSessionScreenIsLocked"] as? NSNumber else {
        return "no"
    }
    return locked.boolValue ? "yes" : "no"
}

let arguments = Array(CommandLine.arguments.dropFirst())

if arguments.first == "--session" {
    // Exit 0 whatever the answer: a locked screen is a reading the caller acts
    // on, not a failure of this helper, and an exit code would make the two
    // indistinguishable from a helper that could not run.
    print("screenLocked=\(screenLockWord())")
    exit(0)
}

if arguments.first == "--check" {
    // AXIsProcessTrusted answers for THIS process, which is the one posting the
    // events, so it is the only trust question that matters here. It is
    // deliberately the non-prompting variant: a run must never put a system
    // dialog in front of the owner.
    let trusted = AXIsProcessTrusted()
    print(trusted ? "trusted" : "untrusted")
    exit(trusted ? 0 : 1)
}

// ---- THE SWEEP'S FOCUS, STOLEN ONCE AND HANDED BACK ONCE -------------------
//
// OWNER RULING, 2026-09-13. Every press used to activate Emacs, post, and hand
// focus back — so a sweep flickered the owner's desktop once per keystroke and
// gave no way to tell, by looking at the screen, that the run was still going.
// The sweep now takes focus ONCE at its start and gives it back ONCE at its
// end, so the owner can watch the run and can see when it is over.
//
// The two halves are separate invocations because they belong to different
// processes: `bin/realtest.sh` takes focus before its first `go test` and gives
// it back from its EXIT trap, so a realtest that failed, a `go test` that
// panicked and an operator's interrupt all reach the handback.
//
// THE TOKEN IS A NAME, NOT A HANDLE. The taking process is gone by the time the
// handback runs, so what travels between them is a string: the bundle
// identifier where the application has one, its pid otherwise, and `none` when
// nothing was frontmost. A bundle identifier survives an application that
// relaunched between the two moments; a raw pid would hand focus to whatever
// inherited the number, or to nothing.

// sweepFocusSettleSeconds bounds ONE activation request's landing.
//
// Twice the accessibility messaging timeout above, for the same reason the
// press path spends two rounds: an activation is asynchronous and the window
// server may take a turn to grant it. It is a bound on waiting, never a bound
// on the answer — an activation that has not landed by then is REPORTED as
// declined rather than retried forever, because a sweep must not stall on the
// owner's desktop.
let sweepFocusSettleSeconds = TimeInterval(axMessagingTimeout) * 2

func focusToken(_ app: NSRunningApplication?) -> String {
    guard let app = app else { return "none" }
    if let bundle = app.bundleIdentifier, !bundle.isEmpty {
        return "bundle:" + bundle
    }
    return "pid:\(app.processIdentifier)"
}

func applicationForToken(_ token: String) -> NSRunningApplication? {
    if token.hasPrefix("bundle:") {
        let identifier = String(token.dropFirst("bundle:".count))
        return NSRunningApplication.runningApplications(withBundleIdentifier: identifier).first
    }
    if token.hasPrefix("pid:"), let value = Int32(token.dropFirst("pid:".count)) {
        return NSRunningApplication(processIdentifier: value)
    }
    return nil
}

// activateAndSettle asks for one application to come forward and answers
// whether the window server granted it, read from NSWorkspace rather than from
// the request returning.
func activateAndSettle(_ app: NSRunningApplication) -> Bool {
    activate(app)
    spin(upTo: sweepFocusSettleSeconds, until: {
        NSWorkspace.shared.frontmostApplication?.processIdentifier == app.processIdentifier
    })
    return NSWorkspace.shared.frontmostApplication?.processIdentifier == app.processIdentifier
}

if arguments.first == "--take" {
    // EXIT 0 WHATEVER HAPPENS, and say what happened on the line. A declined
    // activation, a pid that names no process and a locked screen are all
    // READINGS the sweep reports and carries on from: refusing to run a sweep
    // because the desktop would not come forward would throw away every
    // finding the sweep exists to collect. What must never happen is the
    // handback losing the name of where focus started, and that is printed
    // first on the line for exactly that reason.
    let rest = Array(arguments.dropFirst())
    if rest.count > 1 {
        fail("usage: keydriver --take [<pid>]")
    }
    let previous = focusToken(NSWorkspace.shared.frontmostApplication)
    guard let first = rest.first else {
        print("keydriver-took: previous=\(previous) target=none activated=no "
            + "reason=no editor was named, so only where focus started was recorded "
            + "screenLocked=\(screenLockWord())")
        exit(0)
    }
    guard let targetPid = Int32(first) else {
        fail("--take takes a pid, got \"\(first)\"")
    }
    guard let target = NSRunningApplication(processIdentifier: targetPid) else {
        print("keydriver-took: previous=\(previous) target=\(targetPid) activated=no "
            + "reason=no running application for that pid screenLocked=\(screenLockWord())")
        exit(0)
    }
    let granted = activateAndSettle(target)
    print("keydriver-took: previous=\(previous) target=\(targetPid) "
        + "activated=\(granted ? "yes" : "declined") screenLocked=\(screenLockWord())")
    exit(0)
}

if arguments.first == "--give-back" {
    guard arguments.count == 2 else {
        fail("usage: keydriver --give-back <bundle:ID|pid:N|none>")
    }
    let token = arguments[1]
    if token == "none" {
        print("keydriver-gave-back: previous=none restored=no "
            + "reason=nothing was frontmost when the sweep took focus, so there is nobody to hand it back to")
        exit(0)
    }
    guard let app = applicationForToken(token) else {
        print("keydriver-gave-back: previous=\(token) restored=no "
            + "reason=that application is no longer running screenLocked=\(screenLockWord())")
        exit(0)
    }
    let restored = activateAndSettle(app)
    print("keydriver-gave-back: previous=\(token) restored=\(restored ? "yes" : "no") "
        + "screenLocked=\(screenLockWord())")
    exit(0)
}

// THE HOLD IS OPT-IN AND ALWAYS BOUNDED. A caller that never releases must not
// be able to leave the owner's focus parked on Emacs, so the ceiling applies
// whatever the caller does — including a caller that dies mid-press.
let holdDefaultSeconds = 5.0
var holdSeconds: TimeInterval? = nil
// keepFocus IS THE SWEEP SAYING IT OWNS THE DESKTOP. With it the press still
// makes the target key — that is the only way AppKit will dispatch the event —
// and then LEAVES it there, because the sweep took focus once at its start and
// will hand it back once at its end. Without it the press restores whatever was
// frontmost before, which is the pre-2026-09-13 behaviour and is what a caller
// outside a sweep gets.
var keepFocus = false
var positional: [String] = []
for argument in arguments {
    if argument == "--keep-focus" {
        keepFocus = true
    } else if argument == "--hold" {
        holdSeconds = holdDefaultSeconds
    } else if argument.hasPrefix("--hold=") {
        guard let parsed = TimeInterval(argument.dropFirst("--hold=".count)), parsed > 0 else {
            fail("--hold takes a positive number of seconds, got \"\(argument)\"")
        }
        holdSeconds = parsed
    } else if argument.hasPrefix("--") {
        fail("unknown flag \"\(argument)\"")
    } else {
        positional.append(argument)
    }
}

guard positional.count >= 2, let pid = Int32(positional[0]), let keycode = UInt16(positional[1]) else {
    fail("usage: keydriver --check | keydriver --session | keydriver --take [<pid>] | "
        + "keydriver --give-back <token> | "
        + "keydriver [--hold[=SECONDS]] [--keep-focus] <pid> <keycode> [command,shift,option,control]")
}

var flags = CGEventFlags()
if positional.count >= 3 && !positional[2].isEmpty {
    for name in positional[2].split(separator: ",") {
        switch name.trimmingCharacters(in: .whitespaces) {
        case "command": flags.insert(.maskCommand)
        case "shift": flags.insert(.maskShift)
        case "option": flags.insert(.maskAlternate)
        case "control": flags.insert(.maskControl)
        default: fail("unknown modifier \"\(name)\"")
        }
    }
}

// A private event source, not .combinedSessionState: the run's synthetic events
// must not pick up modifier keys the OWNER happens to be holding down in another
// application at that instant, which is precisely the flakiness a combined state
// would introduce into a test that runs while someone works.
guard let source = CGEventSource(stateID: .privateState) else {
    fail("could not create a private event source")
}

guard let down = CGEvent(keyboardEventSource: source, virtualKey: keycode, keyDown: true),
      let up = CGEvent(keyboardEventSource: source, virtualKey: keycode, keyDown: false)
else {
    fail("could not create the keyboard events for keycode \(keycode)")
}

down.flags = flags
up.flags = flags

guard let target = NSRunningApplication(processIdentifier: pid) else {
    fail("no running application for pid \(pid)")
}
let workspace = NSWorkspace.shared
let previous = workspace.frontmostApplication

// WAS EMACS ALREADY FRONTMOST WHEN THIS PRESS ARRIVED?
//
// Under the sweep's focus policy the answer is normally yes: the sweep took
// focus before its first realtest and nothing has handed it back. A NO means
// one of two things the receipt must distinguish for the reader — the owner
// clicked away mid-sweep, or this is the first press against an Emacs the
// realtest just relaunched (realtests 1, 2, 3 and 5 through 8 all cold-start
// their own editor, and the new process is nobody's frontmost application).
// Either way the press re-takes focus and SAYS SO, and it still does not hand
// it back.
let alreadyFrontmost = previous?.processIdentifier == pid
let refocused = keepFocus && !alreadyFrontmost

// WHETHER THE TARGET CAN RECEIVE A KEY IS A READING, NOT A GATE — EXCEPT WHERE
// NOBODY WILL CHECK.
//
// AppKit dispatches a key event to the KEY WINDOW, so a post to an application
// that owns none is dropped with no error. That is the failure this file exists
// to avoid, and it used to be guarded by PREDICTING it: the helper asked
// whether the target was ready and refused to post when the answer was no.
//
// The prediction was worth what it cost only while a dropped key was
// undetectable. It no longer is. The caller now holds the target key and reads
// EMACS'S OWN MARKS — `(recent-keys)` and `quit-flag` — for every press
// (delivery.go), so a key that did not arrive is named as a harness failure
// after the fact, by the editor, rather than guessed at beforehand by the
// window server. A precondition that refuses on a healthy editor trades a
// solved problem for a new one, and that is exactly what happened: the
// 2026-09-12 16:12 sweep refused FORTY presses, every one of them on
// `NSRunningApplication.isActive` still reading false two seconds after
// `activate()`. Not one refusal came from the accessibility questions below.
//
// SO `isActive` IS NO LONGER ASKED AS A GATE. It is AppKit's cached, KVO-fed
// view of which application is frontmost, maintained for this process out of
// notifications a bundle-less command-line tool is a poor host for; the
// accessibility answers below come from the window server and the target
// itself, which is the same question asked of the authority instead of of a
// cache. `isActive` is still READ and still REPORTED in the receipt — a
// reading that disagrees with the window server is evidence, and dropping it
// would lose the only trace of the failure above — it simply no longer decides
// anything.
//
// THREE QUESTIONS ARE ASKED, AND THEY ARE ASKED OF THE AUTHORITY:
//
//   - the application element is `AXFrontmost`, which is the window server's
//     answer rather than the application's;
//   - the SYSTEM-WIDE accessibility element's `AXFocusedApplication` is this
//     pid, which is the closest accessibility gets to "this app owns the key
//     window";
//   - the application has an `AXFocusedWindow` — necessary but never
//     sufficient on its own, because an inactive background application still
//     answers it about the window that WOULD be key.
//
// A QUESTION THAT WENT UNANSWERED IS NOT A NO. Accessibility queries are
// serviced by the target's own main run loop, so an Emacs busy in its command
// loop — which is precisely the Emacs these presses go to — can leave a query
// unanswered until it times out. A busy Emacs with a key window queues the
// event and dispatches it when it looks, which is what the hold below is for.
// So an unanswered query is reported as UNVERIFIED and never refuses.
//
// AND WHAT A DEFINITE NO DOES DEPENDS ON WHO IS WATCHING:
//
//   - WITH `--hold` the caller is reading the editor back and will report an
//     undelivered key itself, so the reading is ADVISORY: the post goes ahead
//     and the receipt carries what was seen. Confirmation is authoritative,
//     and a key that really was dropped surfaces as `HARNESS KEY DELIVERY
//     FAILED` from Emacs's own marks.
//   - WITHOUT `--hold` there is no channel to the editor and NOBODY will
//     check, so a post into a target the window server says cannot receive it
//     would be exactly the silent loss this file forbids. It is refused and
//     the failure is named, as before.
//
// Nothing is posted-and-forgotten in either mode. The difference is only which
// system gets to say the key was lost, and the one that can actually see it
// now does.
let axTarget = AXUIElementCreateApplication(pid)
let axSystem = AXUIElementCreateSystemWide()

AXUIElementSetMessagingTimeout(axTarget, axMessagingTimeout)
AXUIElementSetMessagingTimeout(axSystem, axMessagingTimeout)

// axCopy answers three ways on purpose: the value, a definite absence, or "the
// target did not answer". Collapsing the last two is what made the old check
// weaker than it read.
enum AXAnswer {
    case value(CFTypeRef)
    case absent
    case unanswered(AXError)
}

func axCopy(_ element: AXUIElement, _ attribute: String) -> AXAnswer {
    var value: CFTypeRef?
    let status = AXUIElementCopyAttributeValue(element, attribute as CFString, &value)
    switch status {
    case .success:
        if let value = value { return .value(value) }
        return .absent
    case .noValue, .attributeUnsupported:
        return .absent
    default:
        return .unanswered(status)
    }
}

// Tri-state: true, false, or nil for "nobody answered".
func axIsTrue(_ element: AXUIElement, _ attribute: String) -> Bool? {
    switch axCopy(element, attribute) {
    case .value(let value):
        guard let number = value as? NSNumber else { return nil }
        return number.boolValue
    case .absent:
        return false
    case .unanswered:
        return nil
    }
}

func axHasValue(_ element: AXUIElement, _ attribute: String) -> Bool? {
    switch axCopy(element, attribute) {
    case .value:
        return true
    case .absent:
        return false
    case .unanswered:
        return nil
    }
}

func axFocusedApplicationIsTarget() -> Bool? {
    switch axCopy(axSystem, kAXFocusedApplicationAttribute as String) {
    case .value(let value):
        guard CFGetTypeID(value) == AXUIElementGetTypeID() else { return nil }
        let element = value as! AXUIElement
        var owner: pid_t = 0
        guard AXUIElementGetPid(element, &owner) == .success else { return nil }
        return owner == pid
    case .absent:
        return false
    case .unanswered:
        return nil
    }
}

// KeyFocus is one reading of the three questions, kept together so the receipt
// can say which of them answered what rather than only that the post was
// refused.
struct KeyFocus {
    var frontmost: Bool?
    var focusedApplication: Bool?
    var focusedWindow: Bool?
    // active is `NSRunningApplication.isActive`, carried for the receipt and
    // deliberately absent from `ready` and `refused`: see the block above for
    // why the cache does not get a vote.
    var active: Bool

    // ready is "every question answered yes".
    var ready: Bool {
        return frontmost == true && focusedApplication == true && focusedWindow == true
    }

    // refused names the first question answered a definite NO, or nil when
    // none was: an unanswered question never refuses.
    var refused: String? {
        if frontmost == false { return "the window server does not consider it frontmost (AXFrontmost is false)" }
        if focusedApplication == false {
            return "the system-wide accessibility focus is on another application (AXFocusedApplication is not this pid)"
        }
        if focusedWindow == false { return "it has no focused window (AXFocusedWindow is absent)" }
        return nil
    }

    func describe() -> String {
        func word(_ answer: Bool?) -> String {
            guard let answer = answer else { return "unverified" }
            return answer ? "yes" : "no"
        }
        return "frontmost=\(word(frontmost)) focusedApplication=\(word(focusedApplication)) "
            + "focusedWindow=\(word(focusedWindow)) isActive=\(word(active)) ready=\(ready ? "yes" : "no")"
    }
}

func readKeyFocus() -> KeyFocus {
    return KeyFocus(
        frontmost: axIsTrue(axTarget, kAXFrontmostAttribute as String),
        focusedApplication: axFocusedApplicationIsTarget(),
        focusedWindow: axHasValue(axTarget, kAXFocusedWindowAttribute as String),
        active: target.isActive)
}

// THE READINESS WAIT IS BOUNDED BY WHAT ASKING COSTS, NOT BY A GUESS.
//
// One full round of the three questions against a target that answers none of
// them costs `axMessagingTimeout`, which is the bound already chosen above for
// a single query. The wait is two such rounds: enough to re-ask an activation
// that had not landed when it was first asked, and no more, because from the
// post onwards the target is HELD key for the whole of the caller's
// confirmation — a window that becomes key late is covered there rather than
// here. The whole readiness wait is therefore HALF the four seconds the two
// sequential two-second gates used to spend, and nothing was widened to make
// this pass.
let activationRound = TimeInterval(axMessagingTimeout)

// TWO ACTIVATION REQUESTS, WHICH IS NOT THE SAME AS ONE LONGER WAIT.
//
// macOS 14 made activation COOPERATIVE: `activate()` asks, and the window
// server may decline a request from a process that is not itself an active
// application — which a bundle-less command-line tool spawned by `go test`
// never is. The legacy `.activateIgnoringOtherApps` that used to override that
// is documented as having NO EFFECT from macOS 14 on, so there is no forcing
// form to fall back to and this helper does not pretend otherwise. What it can
// do is ASK AGAIN: a request declined while the previous application still
// held activation can be granted on the next one, and a second request is a
// different request rather than more time spent waiting on the first.
//
// If both are declined the press is not abandoned. The event is posted anyway
// and the caller's confirmation says whether it arrived, which is the whole
// point of the reading above being a reading.
let activationStarted = Date()
activate(target)
spin(upTo: activationRound, until: { readKeyFocus().ready })
var reactivated = false
if !readKeyFocus().ready {
    reactivated = true
    activate(target)
    spin(upTo: activationRound, until: { readKeyFocus().ready })
}
let readyAfter = Date().timeIntervalSince(activationStarted)

let focus = readKeyFocus()

// Read once, here, and carry it into both messages below: the lock can change
// under a run, and a receipt that says which side of it this press fell on is
// the difference between "the activation was declined" and "there was nobody to
// decline it".
let sessionLock = screenLockWord()

// A DEFINITE NO REFUSES ONLY WHERE NOTHING WILL CHECK THE POST AFTERWARDS.
if holdSeconds == nil, let reason = focus.refused {
    if !keepFocus, let previous = previous, previous.processIdentifier != pid {
        activate(previous)
    }
    fail("pid \(pid) could not be made ready to receive a key — \(reason) — after \(String(format: "%.1f", readyAfter))s, so the key event was NOT "
        + "posted: AppKit dispatches a key event only to a key window and drops a post to an application "
        + "without one silently. This press was made without --hold, so nothing would read the editor back "
        + "and a dropped key would go unnoticed; a held press posts anyway and lets Emacs's own marks "
        + "settle it. Nothing was sent and the previously frontmost application was restored. "
        + "What accessibility said: \(focus.describe()) screenLocked=\(sessionLock)")
}

// readiness is the phrase the receipt carries about the reading above, so a
// finding can say whether the target looked able to receive the key without
// having to re-read this file.
let readiness = "readiness=\(focus.describe()) askedTwice=\(reactivated ? "yes" : "no") "
    + "readyAfter=\(String(format: "%.2f", readyAfter))s screenLocked=\(sessionLock) "
    + "keepFocus=\(keepFocus ? "yes" : "no") refocused=\(refocused ? "yes" : "no")"

// Down then up, addressed to the process. There is no delay between them: a
// keystroke is not a hold, and Emacs's own input queue serializes them.
down.postToPid(pid)
up.postToPid(pid)

// release is the caller's signal that Emacs has accounted for the key. It is
// set from the stdin readability handler, which runs on a dispatch queue, so
// it is behind a lock rather than read raw from the run loop.
final class Release {
    private let lock = NSLock()
    private var value = false
    func signal() {
        lock.lock()
        value = true
        lock.unlock()
    }
    var signalled: Bool {
        lock.lock()
        defer { lock.unlock() }
        return value
    }
}

if let holdSeconds = holdSeconds {
    // HOLD THE TARGET KEY UNTIL THE CALLER SAYS THE KEY WAS CONSUMED.
    //
    // The post above only queued the event; `sendEvent:` routes it to whatever
    // is the key window WHEN EMACS NEXT LOOKS, so handing focus back before
    // then is what dropped keys on a busy Emacs. The caller reads the editor's
    // own account of its input and writes a line here the moment the key shows
    // up, so the ordinary press still restores focus in milliseconds and only
    // a slow one holds.
    let release = Release()
    let stdin = FileHandle.standardInput
    stdin.readabilityHandler = { handle in
        // Any bytes are the release, including EOF (an empty read), which is
        // what a caller that died looks like: holding focus for a caller that
        // is gone would be worse than releasing early.
        _ = handle.availableData
        release.signal()
    }

    print("keydriver-posted")
    fflush(stdout)

    spin(upTo: holdSeconds, until: { release.signalled })
    stdin.readabilityHandler = nil

    if release.signalled {
        print("keydriver-receipt: posted keycode=\(keycode) pid=\(pid) hold=released \(readiness)")
    } else {
        // NOT a failure of the post — the event was posted and Emacs was key
        // for the whole ceiling — but the caller never saw it arrive, and that
        // is exactly the state the caller must be told about rather than left
        // to infer.
        print("keydriver-receipt: posted keycode=\(keycode) pid=\(pid) hold=expired-after-\(holdSeconds)s "
            + readiness)
    }
} else {
    // Let Emacs's event loop consume the events while it is still key, so the
    // keymap resolves them before focus is handed back. This is the unheld
    // path, kept for a caller with no channel to the editor: it cannot know
    // when the key was consumed, so it waits a fixed span and says so.
    spin(upTo: 0.3, until: { false })
    print("keydriver-receipt: posted keycode=\(keycode) pid=\(pid) hold=none \(readiness)")
}

// HAND FOCUS BACK, UNLESS THE SWEEP IS HOLDING IT.
//
// Without `--keep-focus` this is the old policy: the owner is disturbed only
// for the instant of the keypress. With it the sweep owns the desktop from its
// first press to its last, and `bin/realtest.sh`'s EXIT trap is what returns
// focus to where the sweep found it — once, however the sweep ends. Restoring
// here as well would undo the steal the sweep just made and put the flicker
// back, one per keystroke.
if !keepFocus, let previous = previous, previous.processIdentifier != pid {
    activate(previous)
}
