import Foundation
import XCTest
@testable import ZettelLauncher

/// The launcher's log sites (logging-contract.md "Log sites"): the template
/// rendering, the site key and the static scan of every call in source.
final class LogSiteTests: XCTestCase {
    // MARK: template rendering

    func testATemplateRendersItsNamedValues() {
        XCTAssertEqual(renderLogTemplate("dial {shim} after {elapsed}", ["shim": "s1", "elapsed": 3]), "dial s1 after 3")
    }

    func testADoubledBraceIsALiteralBrace() {
        XCTAssertEqual(renderLogTemplate("a {{b}} {c}", ["c": "d"]), "a {b} d")
    }

    func testATemplateWithoutPlaceholdersRendersAsItIs() {
        XCTAssertEqual(renderLogTemplate("plain", [:]), "plain")
    }

    func testAnExtraValueStaysAFieldWithoutChangingTheText() throws {
        let logger = RecordingLogger()
        logger.info("launcher.x", "plain", ["extra": 1])
        XCTAssertEqual(logger.records.first?.message, "plain")
        XCTAssertEqual(logger.records.first?.context["extra"], "1")
    }

    func testTemplatesThatDoNotParseAreRefused() {
        let cases: [(String, String)] = [
            ("open {never", "never closes"),
            ("lone } brace", "lone closing brace"),
            ("bad {Name}", "not a lowercase identifier"),
            ("empty {}", "not a lowercase identifier"),
            ("digit {1a}", "not a lowercase identifier"),
        ]
        for (template, problem) in cases {
            XCTAssertThrowsError(try parseLogTemplate(template), template) { error in
                XCTAssertTrue("\(error)".contains(problem), "\(error)")
            }
        }
    }

    func testPlaceholderNamesAreParsedInOrder() throws {
        XCTAssertEqual(try parseLogTemplate("{a_1} and {{x}} {b}"),
                       [.placeholder("a_1"), .literal(" and {x} "), .placeholder("b")])
    }

    // MARK: site key

    func testTheSiteKeyMatchesTheContractVector() {
        XCTAssertEqual(logSiteKey(runtime: "daemon", function: "pkg.F", template: "hello {name}"), "074892b7d933")
    }

    func testTheCallerIsTheFileAndFunctionTheCallIsWrittenIn() {
        XCTAssertEqual(logSiteCaller(file: "ZettelLauncher/Clicks.swift", function: "report(_:)"), "ZettelLauncher/Clicks.report(_:)")
    }

    func testARecordCarriesItsSiteAndTemplate() throws {
        // Arrange
        let path = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString).appendingPathComponent("launcher.log")
        let logger = FileLogger(path: path)

        // Act
        logger.warn("launcher.x", "dial {shim}", ["shim": "s1"])

        // Assert
        let object = try JSONSerialization.jsonObject(with: Data(contentsOf: path).dropLast()) as! [String: Any]
        let want = logSiteKey(runtime: "launcher", function: logSiteCaller(file: #fileID, function: #function), template: "dial {shim}")
        XCTAssertEqual(object["site"] as? String, want)
        XCTAssertEqual(object["template"] as? String, "dial {shim}")
        XCTAssertEqual(object["message"] as? String, "dial s1")
        XCTAssertEqual((object["context"] as? [String: Any])?["shim"] as? String, "s1")
    }

    func testTwoTemplatesInOneFunctionAreTwoSites() {
        let a = logSiteKey(runtime: "launcher", function: "f", template: "one")
        let b = logSiteKey(runtime: "launcher", function: "f", template: "two")
        XCTAssertNotEqual(a, b)
    }

    func testTheSameTemplateInTwoFunctionsIsTwoSites() {
        let a = logSiteKey(runtime: "launcher", function: "f", template: "one")
        let b = logSiteKey(runtime: "launcher", function: "g", template: "one")
        XCTAssertNotEqual(a, b)
    }

    // MARK: the static scan

    /// One log call found in source.
    struct Call {
        let file: String
        let function: String
        let level: String
        let template: String
    }

    static let sources: URL = URL(fileURLWithPath: #filePath)
        .deletingLastPathComponent().deletingLastPathComponent().deletingLastPathComponent()
        .appendingPathComponent("Sources")

    /// Every `log.<level>(` call under Sources, with the function it is
    /// written in and its template literal, or an error naming the call that
    /// is not a plain literal.
    static func scan(_ roots: [URL]) throws -> (calls: [Call], problems: [String]) {
        var calls: [Call] = []
        var problems: [String] = []
        let callPattern = try NSRegularExpression(pattern: #"\b(?:log|logger)\.(info|debug|warn|error)\("#)
        let functionPattern = try NSRegularExpression(pattern: #"\b(?:func\s+([A-Za-z_][A-Za-z0-9_]*)|(init))\b"#)
        for root in roots {
            let files = FileManager.default.enumerator(at: root, includingPropertiesForKeys: nil)!.compactMap { $0 as? URL }
                .filter { $0.pathExtension == "swift" && $0.lastPathComponent != "Log.swift" }.sorted { $0.path < $1.path }
            for file in files {
                let text = try String(contentsOf: file, encoding: .utf8)
                let ns = text as NSString
                let whole = NSRange(location: 0, length: ns.length)
                let functions = functionPattern.matches(in: text, range: whole)
                for match in callPattern.matches(in: text, range: whole) {
                    let name = file.lastPathComponent
                    let line = ns.substring(to: match.range.location).filter { $0 == "\n" }.count + 1
                    let level = ns.substring(with: match.range(at: 1))
                    let function = functions.last { $0.range.location < match.range.location }
                        .map { ns.substring(with: $0.range(at: 1).location != NSNotFound ? $0.range(at: 1) : $0.range(at: 2)) } ?? "<none>"
                    // Skip the operation argument, up to the first top-level comma.
                    var i = match.range.location + match.range.length
                    var depth = 0
                    while i < ns.length {
                        let c = ns.character(at: i)
                        if c == 0x28 || c == 0x5B { depth += 1 }
                        if c == 0x29 || c == 0x5D { depth -= 1 }
                        if c == 0x2C && depth == 0 { break }
                        i += 1
                    }
                    i += 1
                    while i < ns.length, CharacterSet.whitespacesAndNewlines.contains(UnicodeScalar(ns.character(at: i))!) { i += 1 }
                    guard i < ns.length, ns.character(at: i) == 0x22 else {
                        problems.append("\(name):\(line): the template is not a string literal")
                        continue
                    }
                    var template = ""
                    var j = i + 1
                    var closed = false
                    while j < ns.length {
                        let c = ns.character(at: j)
                        if c == 0x5C {
                            let escaped = ns.substring(with: NSRange(location: j + 1, length: 1))
                            if escaped == "(" { break }
                            template += escaped == "n" ? "\n" : escaped
                            j += 2
                            continue
                        }
                        if c == 0x22 { closed = true; break }
                        template += ns.substring(with: NSRange(location: j, length: 1))
                        j += 1
                    }
                    guard closed else {
                        problems.append("\(name):\(line): the template interpolates a value (\\( ) instead of naming it")
                        continue
                    }
                    do { _ = try parseLogTemplate(template) } catch { problems.append("\(name):\(line): \(error)") }
                    calls.append(Call(file: name, function: function, level: level, template: template))
                }
            }
        }
        return (calls, problems)
    }

    private var launcherSources: [URL] {
        [Self.sources.appendingPathComponent("ZettelLauncher"), Self.sources.appendingPathComponent("Zettel")]
    }

    func testEveryCallInSourceTakesALiteralTemplateThatParses() throws {
        let (calls, problems) = try Self.scan(launcherSources)
        XCTAssertGreaterThan(calls.count, 40, "the scan found too few calls to be reading the sources")
        XCTAssertEqual(problems, [])
    }

    func testNoTwoCallsOfOneFunctionWriteOneTemplate() throws {
        let (calls, _) = try Self.scan(launcherSources)
        var seen: [String: Int] = [:]
        for call in calls { seen["\(call.file) \(call.function) \(call.template)", default: 0] += 1 }
        XCTAssertEqual(seen.filter { $0.value > 1 }.map(\.key).sorted(), [])
    }

    func testTheScanFlagsAnInterpolatedTemplate() throws {
        // Arrange
        let dir = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
        try "func f() {\n    log.info(Op.x, \"dial \\(host)\", [:])\n    log.info(Op.x, template, [:])\n    log.warn(Op.x, \"open {x\", [:])\n}\n"
            .write(to: dir.appendingPathComponent("Bad.swift"), atomically: true, encoding: .utf8)

        // Act
        let (_, problems) = try Self.scan([dir])

        // Assert
        XCTAssertEqual(problems.count, 3, "\(problems)")
        XCTAssertTrue(problems[0].contains("interpolates"))
        XCTAssertTrue(problems[1].contains("not a string literal"))
        XCTAssertTrue(problems[2].contains("never closes"))
    }

    func testTheScanFlagsTwoCallsOfOneFunctionWritingOneTemplate() throws {
        // Arrange
        let dir = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
        try "func f() {\n    log.info(Op.x, \"same\", [:])\n    log.error(Op.x, \"same\", [:])\n}\n"
            .write(to: dir.appendingPathComponent("Dup.swift"), atomically: true, encoding: .utf8)

        // Act
        let (calls, _) = try Self.scan([dir])

        // Assert
        XCTAssertEqual(Set(calls.map { "\($0.function) \($0.template)" }).count, 1)
        XCTAssertEqual(calls.count, 2)
    }
}
