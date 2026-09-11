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
// its own answer and the caller refuses to interpret a silent success.
//
// Usage:
//   keydriver --check                      exit 0 trusted, 1 not trusted
//   keydriver <pid> <keycode> [modifiers]  activate pid, post keyDown/keyUp,
//                                          restore the prior frontmost app
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

guard arguments.count >= 2, let pid = Int32(arguments[0]), let keycode = UInt16(arguments[1]) else {
    fail("usage: keydriver --check | keydriver <pid> <keycode> [command,shift,option,control]")
}

var flags = CGEventFlags()
if arguments.count >= 3 && !arguments[2].isEmpty {
    for name in arguments[2].split(separator: ",") {
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

// Down then up, addressed to the process. There is no delay between them: a
// keystroke is not a hold, and Emacs's own input queue serializes them.
down.postToPid(pid)
up.postToPid(pid)

// Let Emacs's event loop consume the events while it is still key, so the keymap
// resolves them before focus is handed back.
spin(upTo: 0.3, until: { false })

// Restore whatever was frontmost before, so the owner is disturbed only for the
// instant of the keypress. Nothing to restore when Emacs already was frontmost.
if let previous = previous, previous.processIdentifier != pid {
    activate(previous)
}
