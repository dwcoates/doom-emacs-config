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
// Emacs key for the instant of the keypress and then hand focus back. So this
// helper: records the frontmost application, activates the target Emacs, posts
// keyDown then keyUp addressed to its pid, lets the event loop consume them
// while Emacs is still key, and reactivates whatever was frontmost before. The
// owner is disturbed only for that instant, and the key-proof phase is the only
// phase that does it — distinct from startup, which never brings Emacs forward.
// (An accessibility route — AXUIElement posting to the focused UI element — was
// considered; macOS exposes no general key-event delivery to a non-frontmost
// app's first responder through it, so it is not a substitute here.)
//
// It needs accessibility trust: an untrusted process's synthetic events are
// dropped silently by the window server, so `--check` reports the trust state as
// its own answer and the caller refuses to interpret a silent success. The same
// trust is what lets it read the target's focused window before posting, which
// is the second half of "the event has somewhere to land" — see the refusal
// below for why an active application is not yet enough.
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
// when it finally looked. The precondition was necessary and about a quarter of
// a second wide.
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
// Usage:
//   keydriver --check                      exit 0 trusted, 1 not trusted
//   keydriver [--hold[=SECONDS]] <pid> <keycode> [modifiers]
//                                          activate pid, post keyDown/keyUp,
//                                          hold the target key until released
//                                          (with --hold), restore prior focus
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

let arguments = Array(CommandLine.arguments.dropFirst())

if arguments.first == "--check" {
    // AXIsProcessTrusted answers for THIS process, which is the one posting the
    // events, so it is the only trust question that matters here. It is
    // deliberately the non-prompting variant: a run must never put a system
    // dialog in front of the owner.
    let trusted = AXIsProcessTrusted()
    print(trusted ? "trusted" : "untrusted")
    exit(trusted ? 0 : 1)
}

// THE HOLD IS OPT-IN AND ALWAYS BOUNDED. A caller that never releases must not
// be able to leave the owner's focus parked on Emacs, so the ceiling applies
// whatever the caller does — including a caller that dies mid-press.
let holdDefaultSeconds = 5.0
var holdSeconds: TimeInterval? = nil
var positional: [String] = []
for argument in arguments {
    if argument == "--hold" {
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
    fail("usage: keydriver --check | keydriver [--hold[=SECONDS]] <pid> <keycode> [command,shift,option,control]")
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

// Make the target Emacs key so it has a window to receive the event, then wait
// for the activation to actually take before posting.
activate(target)
spin(upTo: 2.0, until: { target.isActive })

// AN ACTIVATION THAT DID NOT TAKE IS A FAILURE, NOT A REASON TO POST ANYWAY.
//
// A CGEvent addressed to a process AppKit has given no key window is dropped
// with no error, which is the failure this whole file exists to avoid; posting
// into that state and exiting 0 would report a key as delivered that the
// keymap never saw, and the caller would then wait out a ceiling for an effect
// that could never arrive. That is exactly how a `C-g` sent to dismiss a
// standing minibuffer turned into a 30s timeout in the 2026-09-12 workspace
// runs: the harness could not tell "the key never arrived" from "the key
// arrived and the read did not abort".
//
// So the post is refused and the failure is named. `activate()` is best-effort
// for a background command-line tool and macOS can decline it outright, so
// this is a real state and not a theoretical one. Focus is handed back first,
// so a refusal leaves the desktop exactly as it found it.
if !target.isActive {
    if let previous = previous, previous.processIdentifier != pid {
        activate(previous)
    }
    fail("pid \(pid) did not become the active application within 2s, so the key event was NOT posted: "
        + "AppKit dispatches a key event only to a key window, and a post to a process without one is dropped "
        + "silently. Nothing was sent and the previously frontmost application was restored")
}

// AN ACTIVE APPLICATION IS NOT YET AN APPLICATION WITH A KEY WINDOW, and that
// gap is where keys were being lost.
//
// `NSRunningApplication.isActive` answers about the APPLICATION. AppKit
// dispatches a key event to the KEY WINDOW, and a window becomes key on its own
// schedule after the activation — so between `isActive` turning true and a
// window becoming key there is a window of time in which `[NSApp keyWindow]` is
// still nil and `sendEvent:` has nowhere to send a keyDown. It drops it with no
// error, which is the same silent loss the activation check above exists to
// prevent, one step further along.
//
// It is not theoretical. The 2026-09-12 sweep's own `(recent-keys)` came back
// missing keys this helper had posted and reported as delivered: the `<tab>` of
// a `SPC TAB o` (which the run itself reported as a chord that did not reach its
// command), the `<escape>`s pressed around it, and every `C-g` sent to dismiss a
// standing prompt — which was then filed against the EDITOR as "a real C-g did
// not dismiss the prompt" when Emacs had never been handed the key at all.
//
// So the focused window is a precondition, checked the same way the activation
// is: through the accessibility API, whose trust this helper already requires
// and refuses to run without. `AXFocusedWindow` on an application element is
// that application's key window. No window, no post, and the failure is named.
//
// AND A FOCUSED WINDOW ALONE WAS STILL NOT ENOUGH. `AXFocusedWindow` on an
// application element answers about THAT APPLICATION'S OWN notion of which of
// its windows would be key — an inactive application in the background still
// answers it — so it is satisfied by an Emacs that owns no key window at all.
// That is why keys kept going missing after it landed. Three questions are
// asked now, and all three must be answered YES:
//
//   - the application element is `AXFrontmost`, which is the window server's
//     answer rather than the application's;
//   - the SYSTEM-WIDE accessibility element's `AXFocusedApplication` is this
//     pid, which is the closest accessibility gets to "this app owns the key
//     window";
//   - the application still has an `AXFocusedWindow`, the original check, kept
//     because a frontmost application between windows has none.
//
// A QUESTION THAT WENT UNANSWERED IS NOT A NO. Accessibility queries are
// serviced by the target's own main run loop, so an Emacs busy in its command
// loop — which is precisely the Emacs these presses go to — can leave a query
// unanswered until it times out. Refusing on that would invent a new way to
// lose a keypress, and it would be wrong: a busy Emacs with a key window
// queues the event and dispatches it when it looks, which is exactly what the
// hold below is for. So an unanswered query is reported as UNVERIFIED in the
// receipt and the post goes ahead; only a definite NO refuses.
let axTarget = AXUIElementCreateApplication(pid)
let axSystem = AXUIElementCreateSystemWide()

// Bounded so one unresponsive target cannot hang the helper past the caller's
// own ceiling; an expiry is an unanswered query, handled as one.
AXUIElementSetMessagingTimeout(axTarget, 1.0)
AXUIElementSetMessagingTimeout(axSystem, 1.0)

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
            + "focusedWindow=\(word(focusedWindow))"
    }
}

func readKeyFocus() -> KeyFocus {
    return KeyFocus(
        frontmost: axIsTrue(axTarget, kAXFrontmostAttribute as String),
        focusedApplication: axFocusedApplicationIsTarget(),
        focusedWindow: axHasValue(axTarget, kAXFocusedWindowAttribute as String))
}

spin(upTo: 2.0, until: { readKeyFocus().ready })

let focus = readKeyFocus()
if let reason = focus.refused {
    if let previous = previous, previous.processIdentifier != pid {
        activate(previous)
    }
    fail("pid \(pid) is the active application but \(reason) after 2s, so the key event was NOT "
        + "posted: AppKit dispatches a key event only to a key window and drops a post to an application "
        + "without one silently, which is how posted keys went missing from Emacs's own (recent-keys). "
        + "Nothing was sent and the previously frontmost application was restored. "
        + "What accessibility said: \(focus.describe())")
}

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
        print("keydriver-receipt: posted keycode=\(keycode) pid=\(pid) hold=released \(focus.describe())")
    } else {
        // NOT a failure of the post — the event was posted and Emacs was key
        // for the whole ceiling — but the caller never saw it arrive, and that
        // is exactly the state the caller must be told about rather than left
        // to infer.
        print("keydriver-receipt: posted keycode=\(keycode) pid=\(pid) hold=expired-after-\(holdSeconds)s "
            + focus.describe())
    }
} else {
    // Let Emacs's event loop consume the events while it is still key, so the
    // keymap resolves them before focus is handed back. This is the unheld
    // path, kept for a caller with no channel to the editor: it cannot know
    // when the key was consumed, so it waits a fixed span and says so.
    spin(upTo: 0.3, until: { false })
    print("keydriver-receipt: posted keycode=\(keycode) pid=\(pid) hold=none \(focus.describe())")
}

// Restore whatever was frontmost before, so the owner is disturbed only for the
// instant of the keypress. Nothing to restore when Emacs already was frontmost.
if let previous = previous, previous.processIdentifier != pid {
    activate(previous)
}
