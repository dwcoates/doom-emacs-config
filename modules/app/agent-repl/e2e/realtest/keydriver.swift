// keydriver.swift — post REAL key events to one process, without activating it.
//
// A realtest's input is what the owner would send: a key event that Emacs's own
// keymap resolves. An elisp call that performs the act instead tests the
// function and says nothing about whether the chord reaches it, which is
// exactly the class of defect a realtest exists to catch — and the plan
// (docs/REALTEST-PLAN.md) rules that if key delivery to an unfocused Emacs is
// impossible on macOS, that is SURFACED to the owner rather than worked around
// with elisp.
//
// CGEventPostToPid is the one interface that addresses a process directly. The
// event goes onto that process's own input queue without the window server
// making it frontmost, which is what lets a run drive Emacs while the owner
// keeps typing in another application. Every alternative — System Events'
// `key code`, `CGEventPost` to the HID or session tap — delivers to whatever is
// FRONTMOST, and would require stealing focus.
//
// It needs accessibility trust: an untrusted process's synthetic events are
// dropped silently by the window server, which is the worst possible failure
// mode for a test, so `--check` reports the trust state as its own answer and
// the caller refuses to interpret a silent success.
//
// Usage:
//   keydriver --check                      exit 0 trusted, 1 not trusted
//   keydriver <pid> <keycode> [modifiers]  post keyDown then keyUp
//
// Modifiers are comma-separated: command, shift, option, control. They are
// spelled as words rather than as a bitmask so a caller's intent is readable in
// the process table when a run is being watched.

import ApplicationServices
import CoreGraphics
import Foundation

func fail(_ message: String) -> Never {
    FileHandle.standardError.write(Data(("keydriver: " + message + "\n").utf8))
    exit(2)
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
// must not pick up modifier keys the OWNER happens to be holding down in
// another application at that instant, which is precisely the flakiness a
// combined state would introduce into a test that runs while someone works.
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

// Down then up, addressed to the process. There is no delay between them: a
// keystroke is not a hold, and Emacs's own input queue serializes them.
down.postToPid(pid)
up.postToPid(pid)
