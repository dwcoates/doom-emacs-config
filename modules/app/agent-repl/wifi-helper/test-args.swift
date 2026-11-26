// Tests of args.swift, the helper's command line; bin/test-wifi-helper.sh
// compiles and runs them.

import Foundation

@main
struct ArgsTests {
    static var failures = 0

    static func check<T: Equatable>(_ name: String, _ got: T, _ want: T) {
        if got == want {
            print("ok - \(name)")
        } else {
            print("not ok - \(name): got \(String(describing: got)), want \(String(describing: want))")
            failures += 1
        }
    }

    static func main() {
        check("a scan names its answer file", parseArgs(["scan", "--out", "/tmp/a"]),
              Invocation(verb: "scan", out: "/tmp/a", name: nil))
        check("authorize names its answer file", parseArgs(["authorize", "--out", "/tmp/a"]),
              Invocation(verb: "authorize", out: "/tmp/a", name: nil))
        check("a hotspot names its hotspot and answer file", parseArgs(["hotspot", "--name", "Dodge's iPhone", "--out", "/tmp/a"]),
              Invocation(verb: "hotspot", out: "/tmp/a", name: "Dodge's iPhone"))
        check("a hotspot's flags may come in either order", parseArgs(["hotspot", "--out", "/tmp/a", "--name", "Phone"]),
              Invocation(verb: "hotspot", out: "/tmp/a", name: "Phone"))
        check("a hotspot without a name is refused", parseArgs(["hotspot", "--out", "/tmp/a"]), nil)
        check("a scan with a name is refused", parseArgs(["scan", "--name", "Phone", "--out", "/tmp/a"]), nil)
        check("an unknown verb is refused", parseArgs(["join", "--out", "/tmp/a"]), nil)
        check("an unknown flag is refused", parseArgs(["scan", "--file", "/tmp/a"]), nil)
        check("a flag without its value is refused", parseArgs(["scan", "--out"]), nil)
        check("a flag given twice is refused", parseArgs(["scan", "--out", "/tmp/a", "--out", "/tmp/b"]), nil)
        check("an empty value is refused", parseArgs(["scan", "--out", ""]), nil)
        check("no answer file is refused", parseArgs(["scan"]), nil)
        check("nothing is refused", parseArgs([]), nil)
        exit(failures == 0 ? 0 : 1)
    }
}
