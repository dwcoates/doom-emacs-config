import XCTest
@testable import ZettelLauncher

/// A Do Not Disturb store's three files, written as macOS lays them out, for
/// a work Focus that allows only `com.example.allowed` and a personal Focus
/// that silences only `com.example.silenced`.
struct FocusStoreFixture {
    var active: [String] = ["com.apple.focus.work"]
    var workKind: Any = 0
    var configurations: [String: Any]? = nil

    var assertions: Data {
        json(["data": [["storeAssertionRecords": active.map { ["assertionDetails": ["assertionDetailsModeIdentifier": $0]] }]]])
    }

    var configurationsData: Data {
        json(["data": [["modeConfigurations": configurations ?? [
            "com.apple.focus.work": ["mode": ["name": "Work"], "configuration": ["applicationConfigurationType": workKind]],
            "com.apple.focus.personal-time": ["mode": ["name": "Personal"], "configuration": ["applicationConfigurationType": 1]],
        ]]]])
    }

    var secure: Data {
        json(["data": [["secureModeConfigurations": [
            "com.apple.focus.work": ["secureConfiguration": [
                "allowedApplications": ["com.example.allowed": ["minimumBreakthroughUrgency": 2]],
                "silencedApplications": [] as [Any],
            ]],
            "com.apple.focus.personal-time": ["secureConfiguration": [
                "allowedApplications": [String: Any](),
                "silencedApplications": [["platform": 1, "bundleID": "com.example.silenced"]],
            ]],
        ]]]])
    }

    func parse() throws -> FocusState {
        try parseFocusState(assertions: assertions, configurations: configurationsData, secure: secure)
    }

    private func json(_ value: Any) -> Data {
        try! JSONSerialization.data(withJSONObject: value)
    }
}

final class FocusTests: XCTestCase {
    func testTheStoreIsReadIntoEachFocusAndTheOnesOn() throws {
        // Act
        let state = try FocusStoreFixture().parse()

        // Assert
        XCTAssertEqual(state.active, ["com.apple.focus.work"])
        XCTAssertEqual(state.modes["com.apple.focus.work"],
                       FocusMode(name: "Work", allowList: true, allowed: ["com.example.allowed"], silenced: []))
        XCTAssertEqual(state.modes["com.apple.focus.personal-time"],
                       FocusMode(name: "Personal", allowList: false, allowed: [], silenced: ["com.example.silenced"]))
    }

    func testOnlyAFocusOnThatSilencesTheAppNamesIt() throws {
        let cases: [(String, [String], String, String?)] = [
            ("no Focus on", [], "app.zettel", nil),
            ("an allow list without the app", ["com.apple.focus.work"], "app.zettel", "Work"),
            ("an allow list with the app", ["com.apple.focus.work"], "com.example.allowed", nil),
            ("a silence list without the app", ["com.apple.focus.personal-time"], "app.zettel", nil),
            ("a silence list with the app", ["com.apple.focus.personal-time"], "com.example.silenced", "Personal"),
        ]
        for (name, active, bundle, want) in cases {
            // Arrange
            var fixture = FocusStoreFixture()
            fixture.active = active

            // Act
            let got = try silencingFocus(try fixture.parse(), bundle: bundle)

            // Assert
            XCTAssertEqual(got, want, name)
        }
    }

    func testAFocusOnWithNoConfigurationIsAnError() throws {
        // Arrange
        var fixture = FocusStoreFixture()
        fixture.active = ["com.example.focus.unknown"]

        // Act and assert
        XCTAssertThrowsError(try silencingFocus(try fixture.parse(), bundle: "app.zettel")) {
            XCTAssertEqual($0 as? FocusReadError, FocusReadError("the Focus com.example.focus.unknown is on but has no configuration"))
        }
    }

    func testAnUnknownKindOfAppListIsAnError() {
        // Arrange
        var fixture = FocusStoreFixture()
        fixture.workKind = 7

        // Act and assert
        XCTAssertThrowsError(try fixture.parse()) {
            XCTAssertEqual($0 as? FocusReadError,
                           FocusReadError("the Focus com.apple.focus.work has an applicationConfigurationType this launcher does not know (Optional(7))"))
        }
    }

    func testAStoreThatIsNotJSONIsAnError() {
        XCTAssertThrowsError(try parseFocusState(assertions: Data("nope".utf8), configurations: Data(), secure: Data())) {
            XCTAssertTrue(($0 as? FocusReadError)?.description.hasPrefix("Assertions.json is not JSON") ?? false, "\($0)")
        }
    }

    func testAMissingStoreIsAnError() {
        // Arrange
        let dir = URL(fileURLWithPath: NSTemporaryDirectory()).appendingPathComponent(UUID().uuidString)

        // Act and assert
        XCTAssertThrowsError(try readFocusState(dir: dir)) {
            XCTAssertTrue(($0 as? FocusReadError)?.description.hasPrefix("read Assertions.json in \(dir.path)") ?? false, "\($0)")
        }
    }

    func testTheStoreIsReadFromItsThreeFiles() throws {
        // Arrange
        let dir = URL(fileURLWithPath: NSTemporaryDirectory()).appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: dir) }
        let fixture = FocusStoreFixture()
        try fixture.assertions.write(to: dir.appendingPathComponent("Assertions.json"))
        try fixture.configurationsData.write(to: dir.appendingPathComponent("ModeConfigurations.json"))
        try fixture.secure.write(to: dir.appendingPathComponent("ModeConfigurationsSecure.json"))

        // Act
        let state = try readFocusState(dir: dir)

        // Assert
        XCTAssertEqual(state, try fixture.parse())
    }

    func testTheStoreLivesInTheUsersDoNotDisturbDatabase() {
        XCTAssertEqual(focusStoreDirectory(home: URL(fileURLWithPath: "/home/u")).path, "/home/u/Library/DoNotDisturb/DB")
    }
}
