import AppKit
import ApplicationServices

// NOTIFICATION CENTER'S WINDOWS, READ THROUGH ACCESSIBILITY. macOS draws every
// notification on screen, and the panel the user opens from the menu bar, as
// windows of its Notification Center process. Measured on macOS 26.2
// (2026-11-25), each notification is an AXGroup whose subrole begins
// `AXNotificationCenter` (`AXNotificationCenterBanner`, `...BannerStack`),
// whose identifier names it while it is shown, whose description is
// `<app>, <title>, <body>`, and whose actions include the custom `Close`
// (`Name:Close\nTarget:...`). A window on screen holds its notifications
// directly; the panel holds its history under a group identified
// `AXNotificationListItems`, which the closer never touches.

/// The bundle identifier of the process that draws every notification.
public let notificationCenterBundleID = "com.apple.notificationcenterui"

/// The identifier of the group the Notification Center panel lists its
/// history under.
public let notificationListIdentifier = "AXNotificationListItems"

/// The subrole prefix every notification's group carries.
public let notificationSubrolePrefix = "AXNotificationCenter"

/// One element of an Accessibility tree, as the scan reads it.
public protocol AccessibilityNode {
    var role: String? { get }
    var subrole: String? { get }
    var identifier: String? { get }
    var nodeDescription: String? { get }
    /// The raw names of the actions it offers.
    var rawActionNames: [String] { get }
    var children: [Self] { get }
}

/// An action's name as a person reads it: a custom action's raw name
/// (`Name:Close\nTarget:0x0\nSelector:(null)`) is its `Name:` part, and a
/// standard action's (`AXPress`) is itself.
public func actionName(_ raw: String) -> String {
    guard raw.hasPrefix("Name:") else { return raw }
    let rest = raw.dropFirst("Name:".count)
    return String(rest.prefix { $0 != "\n" })
}

/// Reads Notification Center's windows, each with the key naming it: every
/// notification shown on screen, by identifier with its element, and every
/// window that is neither the panel nor holds a notification.
public func scanWindows<Node: AccessibilityNode>(_ windows: [(key: String, node: Node)]) -> (scan: ScreenScan, elements: [String: Node]) {
    var scan = ScreenScan(notifications: [])
    var elements: [String: Node] = [:]
    for window in windows {
        if holdsPanelList(window.node) { continue }
        let found = notificationGroups(in: window.node)
        if found.isEmpty {
            scan.unrecognizedWindows.insert(window.key)
            continue
        }
        for group in found {
            guard let identifier = group.identifier, !identifier.isEmpty else {
                // A notification with no identifier cannot be found again to
                // close; it counts as a layout this launcher does not know.
                scan.unrecognizedWindows.insert(window.key)
                continue
            }
            scan.notifications.append(ShownNotification(identifier: identifier, description: group.nodeDescription,
                                                        actions: group.rawActionNames.map(actionName)))
            elements[identifier] = group
        }
    }
    return (scan, elements)
}

private func holdsPanelList<Node: AccessibilityNode>(_ node: Node) -> Bool {
    node.identifier == notificationListIdentifier || node.children.contains { holdsPanelList($0) }
}

/// Every notification group under `node`, outermost first; a notification's
/// own children are never searched.
private func notificationGroups<Node: AccessibilityNode>(in node: Node) -> [Node] {
    if node.role == "AXGroup", node.subrole?.hasPrefix(notificationSubrolePrefix) == true {
        return [node]
    }
    return node.children.flatMap { notificationGroups(in: $0) }
}

/// Answers whether macOS lets Zettel use the Accessibility API, as the
/// installer's `--check-accessibility` prints it.
public func accessibilityReport(trusted: Bool) -> (line: String, allowed: Bool) {
    trusted
        ? ("Zettel may use Accessibility", true)
        : ("Zettel may not use Accessibility (System Settings → Privacy & Security → Accessibility → Zettel)", false)
}

/// An AXUIElement read as an AccessibilityNode.
struct AXNode: AccessibilityNode {
    let element: AXUIElement

    private func attribute(_ name: String) -> AnyObject? {
        var value: AnyObject?
        guard AXUIElementCopyAttributeValue(element, name as CFString, &value) == .success else { return nil }
        return value
    }

    var role: String? { attribute(kAXRoleAttribute) as? String }
    var subrole: String? { attribute(kAXSubroleAttribute) as? String }
    var identifier: String? { attribute(kAXIdentifierAttribute) as? String }
    var nodeDescription: String? { attribute(kAXDescriptionAttribute) as? String }
    var rawActionNames: [String] {
        var names: CFArray?
        guard AXUIElementCopyActionNames(element, &names) == .success else { return [] }
        return (names as? [String]) ?? []
    }
    var children: [AXNode] { ((attribute(kAXChildrenAttribute) as? [AXUIElement]) ?? []).map(AXNode.init) }
}

/// An Accessibility call that failed, with the API's own error code.
public struct AccessibilityError: Error, CustomStringConvertible {
    public let call: String
    public let code: Int32
    public var description: String { "\(call) failed with AXError \(code)" }
}

/// Notification Center's windows on this Mac, read and acted on through the
/// Accessibility API. Lives on the main run loop: it calls `onChange` there,
/// once per turn of the run loop however many changes the turn saw.
public final class AccessibilityScreen: NotificationScreen {
    private let log: Logger
    /// Called when Notification Center's windows changed.
    public var onChange: (() -> Void)?
    private var observer: AXObserver?
    private var application: AXUIElement?
    private var watchedPID: pid_t = 0
    private var elements: [String: AXNode] = [:]
    private var changePending = false
    private var tokens: [NSObjectProtocol] = []

    public init(log: Logger) {
        self.log = log
    }

    /// Starts watching: now, whenever macOS grants Zettel Accessibility, and
    /// whenever Notification Center restarts.
    public func start() {
        tokens.append(DistributedNotificationCenter.default().addObserver(
            forName: NSNotification.Name("com.apple.accessibility.api"), object: nil, queue: .main
        ) { [weak self] _ in self?.attach(because: "macOS changed which apps may use Accessibility") })
        tokens.append(NSWorkspace.shared.notificationCenter.addObserver(
            forName: NSWorkspace.didLaunchApplicationNotification, object: nil, queue: .main
        ) { [weak self] note in
            let app = note.userInfo?[NSWorkspace.applicationUserInfoKey] as? NSRunningApplication
            guard app?.bundleIdentifier == notificationCenterBundleID else { return }
            self?.attach(because: "Notification Center restarted")
        })
        attach(because: "the launcher started")
    }

    /// Observes the running Notification Center process, once Zettel may.
    private func attach(because reason: String) {
        guard AXIsProcessTrusted() else {
            log.error(SlackOp.close, "Zettel may not use Accessibility, so Slack notifications are not closed; allow Zettel in System Settings → Privacy & Security → Accessibility",
                      ["reason": reason])
            return
        }
        guard let process = NSRunningApplication.runningApplications(withBundleIdentifier: notificationCenterBundleID).first else {
            log.error(SlackOp.close, "Notification Center is not running, so Slack notifications are not closed until it starts",
                      ["reason": reason, "bundle": notificationCenterBundleID])
            return
        }
        if observer != nil && process.processIdentifier == watchedPID { return }
        detach()
        let pid = process.processIdentifier
        var created: AXObserver?
        let callback: AXObserverCallback = { _, _, _, refcon in
            guard let refcon else { return }
            Unmanaged<AccessibilityScreen>.fromOpaque(refcon).takeUnretainedValue().changed()
        }
        let made = AXObserverCreate(pid, callback, &created)
        guard made == .success, let created else {
            log.error(SlackOp.close, "creating the observer of Notification Center failed, so Slack notifications are not closed",
                      ["pid": pid, "cause": "\(AccessibilityError(call: "AXObserverCreate", code: made.rawValue))"])
            return
        }
        let app = AXUIElementCreateApplication(pid)
        let refcon = Unmanaged.passUnretained(self).toOpaque()
        for name in [kAXWindowCreatedNotification, kAXLayoutChangedNotification] {
            let added = AXObserverAddNotification(created, app, name as CFString, refcon)
            guard added == .success else {
                log.error(SlackOp.close, "watching Notification Center failed, so Slack notifications are not closed",
                          ["pid": pid, "notification": name, "cause": "\(AccessibilityError(call: "AXObserverAddNotification", code: added.rawValue))"])
                return
            }
        }
        CFRunLoopAddSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(created), .defaultMode)
        observer = created
        application = app
        watchedPID = pid
        log.info(SlackOp.close, "watching Notification Center for Slack notifications", ["pid": pid, "reason": reason])
        changed()
    }

    private func detach() {
        if let observer {
            CFRunLoopRemoveSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(observer), .defaultMode)
        }
        observer = nil
        application = nil
        watchedPID = 0
        elements = [:]
    }

    /// Coalesces a burst of changes (opening the panel raises hundreds) into
    /// one call of onChange on the run loop's next turn.
    private func changed() {
        guard !changePending else { return }
        changePending = true
        RunLoop.main.perform { [weak self] in
            guard let self else { return }
            self.changePending = false
            self.onChange?()
        }
    }

    public func scan() throws -> ScreenScan {
        guard let application else { return ScreenScan(notifications: []) }
        var value: AnyObject?
        let read = AXUIElementCopyAttributeValue(application, kAXWindowsAttribute as CFString, &value)
        guard read == .success else {
            throw AccessibilityError(call: "AXUIElementCopyAttributeValue(AXWindows)", code: read.rawValue)
        }
        let windows = ((value as? [AXUIElement]) ?? []).map { (key: "window-\(CFHash($0))", node: AXNode(element: $0)) }
        let result = scanWindows(windows)
        elements = result.elements
        return result.scan
    }

    public func perform(_ action: String, on identifier: String) throws {
        guard let node = elements[identifier] else {
            throw AccessibilityError(call: "perform \(action) on \(identifier), which the last scan did not find", code: AXError.invalidUIElement.rawValue)
        }
        guard let raw = node.rawActionNames.first(where: { actionName($0) == action }) else {
            throw AccessibilityError(call: "perform \(action) on \(identifier), which no longer offers it", code: AXError.actionUnsupported.rawValue)
        }
        let done = AXUIElementPerformAction(node.element, raw as CFString)
        guard done == .success else {
            throw AccessibilityError(call: "AXUIElementPerformAction(\(action))", code: done.rawValue)
        }
    }
}

/// Deadlines on the main run loop's clock.
public final class RunLoopDeadlines: DeadlineScheduler {
    public init() {}

    public func now() -> Date { Date() }

    public func schedule(at date: Date, _ fire: @escaping () -> Void) -> ScheduledCall {
        let timer = Timer(fire: date, interval: 0, repeats: false) { _ in fire() }
        RunLoop.main.add(timer, forMode: .common)
        return TimerCall(timer: timer)
    }

    private final class TimerCall: ScheduledCall {
        let timer: Timer
        init(timer: Timer) { self.timer = timer }
        func cancel() { timer.invalidate() }
    }
}
