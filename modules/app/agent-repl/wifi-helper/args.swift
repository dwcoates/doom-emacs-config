// The Wi-Fi helper's command line, apart from main.swift so it is tested
// (test-args.swift, bin/test-wifi-helper.sh).

// Invocation is one verb and its flags.
struct Invocation: Equatable {
    let verb: String
    let out: String
    // name is the hotspot verb's hotspot; nil for every other verb.
    let name: String?
}

// parseArgs reads ARGS (the arguments after the program): `scan --out FILE`,
// `authorize --out FILE`, or `hotspot --name NAME --out FILE`, the flags in
// either order. Anything else is nil: an unknown verb or flag, a flag given
// twice or without its value, a hotspot without a name or another verb with
// one.
func parseArgs(_ args: [String]) -> Invocation? {
    guard let verb = args.first, ["scan", "authorize", "hotspot"].contains(verb), args.count % 2 == 1 else { return nil }
    var flags: [String: String] = [:]
    for i in stride(from: 1, to: args.count, by: 2) {
        let flag = args[i]
        guard ["--out", "--name"].contains(flag), flags[flag] == nil, !args[i + 1].isEmpty else { return nil }
        flags[flag] = args[i + 1]
    }
    guard let out = flags["--out"], (verb == "hotspot") == (flags["--name"] != nil) else { return nil }
    return Invocation(verb: verb, out: out, name: flags["--name"])
}
