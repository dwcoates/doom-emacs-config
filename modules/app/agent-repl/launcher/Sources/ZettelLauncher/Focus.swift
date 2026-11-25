import Foundation

/// Where macOS keeps its Focus state: which Focus is on, and each Focus's
/// allowed or silenced apps.
///
/// NO PUBLIC API ANSWERS WHETHER A FOCUS SILENCES AN APP. `INFocusStatusCenter`
/// says only whether some Focus is on, and `add` takes a banner a Focus files
/// silently without an error, so the launcher reads the Do Not Disturb store's
/// own files. Their layout is macOS's private business: every part of it the
/// reader does not recognize is an error (`FocusReadError`), never a guess.
public func focusStoreDirectory(home: URL) -> URL {
    home.appendingPathComponent("Library/DoNotDisturb/DB", isDirectory: true)
}

/// One Focus, as far as it decides an app's banners.
public struct FocusMode: Equatable, Sendable {
    /// The name System Settings shows ("Work").
    public var name: String
    /// True when the Focus silences every app but its allowed ones; false
    /// when it lets every app through but its silenced ones.
    public var allowList: Bool
    /// The bundle IDs the Focus lets through (an allow list).
    public var allowed: Set<String>
    /// The bundle IDs the Focus silences (a silence list).
    public var silenced: Set<String>

    public init(name: String, allowList: Bool, allowed: Set<String>, silenced: Set<String>) {
        self.name = name
        self.allowList = allowList
        self.allowed = allowed
        self.silenced = silenced
    }

    /// Whether the Focus silences the app BUNDLE.
    public func silences(_ bundle: String) -> Bool {
        allowList ? !allowed.contains(bundle) : silenced.contains(bundle)
    }
}

/// The Focus state: the Focuses on now, each by its mode identifier, and every
/// configured Focus.
public struct FocusState: Equatable, Sendable {
    public var active: [String]
    public var modes: [String: FocusMode]

    public init(active: [String], modes: [String: FocusMode]) {
        self.active = active
        self.modes = modes
    }
}

/// A Focus store the reader could not read or does not recognize.
public struct FocusReadError: Error, Equatable, CustomStringConvertible {
    public let description: String
    public init(_ description: String) { self.description = description }
}

/// The name of the Focus on now that silences BUNDLE, or nil when none does.
/// An active Focus with no configuration is an error: the store names a
/// Focus it does not describe.
public func silencingFocus(_ state: FocusState, bundle: String) throws -> String? {
    for id in state.active {
        guard let mode = state.modes[id] else {
            throw FocusReadError("the Focus \(id) is on but has no configuration")
        }
        if mode.silences(bundle) { return mode.name }
    }
    return nil
}

/// Reads the Focus state from the store in DIR: `Assertions.json` (the
/// Focuses on now), `ModeConfigurations.json` (each Focus's name and kind of
/// app list) and `ModeConfigurationsSecure.json` (its apps).
public func readFocusState(dir: URL) throws -> FocusState {
    let assertions = try storeData(dir, "Assertions.json")
    let configurations = try storeData(dir, "ModeConfigurations.json")
    let secure = try storeData(dir, "ModeConfigurationsSecure.json")
    return try parseFocusState(assertions: assertions, configurations: configurations, secure: secure)
}

/// Parses the three files' contents into the Focus state.
public func parseFocusState(assertions: Data, configurations: Data, secure: Data) throws -> FocusState {
    let records = try array(try storeRecord(assertions, "Assertions.json")["storeAssertionRecords"], "Assertions.json storeAssertionRecords")
    let active = try records.map { record in
        let details = try object(try object(record, "an assertion record")["assertionDetails"], "an assertion's assertionDetails")
        return try string(details["assertionDetailsModeIdentifier"], "an assertion's assertionDetailsModeIdentifier")
    }
    let configured = try object(try storeRecord(configurations, "ModeConfigurations.json")["modeConfigurations"], "ModeConfigurations.json modeConfigurations")
    let apps = try object(try storeRecord(secure, "ModeConfigurationsSecure.json")["secureModeConfigurations"], "ModeConfigurationsSecure.json secureModeConfigurations")
    var modes: [String: FocusMode] = [:]
    for (id, entry) in configured {
        let entry = try object(entry, "the Focus \(id)")
        let name = try string(try object(entry["mode"], "the Focus \(id) mode")["name"], "the Focus \(id) name")
        let kind = try object(entry["configuration"], "the Focus \(id) configuration")["applicationConfigurationType"]
        let allowList: Bool
        switch kind as? Int {
        case 0: allowList = true
        case 1: allowList = false
        default: throw FocusReadError("the Focus \(id) has an applicationConfigurationType this launcher does not know (\(String(describing: kind)))")
        }
        let secureEntry = try object(apps[id], "the Focus \(id) secure configuration")
        let lists = try object(secureEntry["secureConfiguration"], "the Focus \(id) secureConfiguration")
        let allowed = Set(try object(lists["allowedApplications"], "the Focus \(id) allowedApplications").keys)
        let silenced = Set(try array(lists["silencedApplications"], "the Focus \(id) silencedApplications").map { app in
            try string(try object(app, "a silenced app of the Focus \(id)")["bundleID"], "a silenced app's bundleID in the Focus \(id)")
        })
        modes[id] = FocusMode(name: name, allowList: allowList, allowed: allowed, silenced: silenced)
    }
    return FocusState(active: active, modes: modes)
}

private func storeData(_ dir: URL, _ file: String) throws -> Data {
    do {
        return try Data(contentsOf: dir.appendingPathComponent(file))
    } catch {
        throw FocusReadError("read \(file) in \(dir.path): \(error.localizedDescription)")
    }
}

/// The one record of a store file: `{"data": [record], ...}`.
private func storeRecord(_ data: Data, _ file: String) throws -> [String: Any] {
    let root: Any
    do {
        root = try JSONSerialization.jsonObject(with: data)
    } catch {
        throw FocusReadError("\(file) is not JSON: \(error.localizedDescription)")
    }
    let records = try array(try object(root, file)["data"], "\(file) data")
    guard records.count == 1 else {
        throw FocusReadError("\(file) holds \(records.count) data records, not one")
    }
    return try object(records[0], "\(file) data record")
}

private func object(_ value: Any?, _ what: String) throws -> [String: Any] {
    guard let value = value as? [String: Any] else { throw FocusReadError("\(what) is not an object") }
    return value
}

private func array(_ value: Any?, _ what: String) throws -> [Any] {
    guard let value = value as? [Any] else { throw FocusReadError("\(what) is not a list") }
    return value
}

private func string(_ value: Any?, _ what: String) throws -> String {
    guard let value = value as? String else { throw FocusReadError("\(what) is not a string") }
    return value
}
