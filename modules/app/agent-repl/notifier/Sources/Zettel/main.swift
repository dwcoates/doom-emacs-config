// Zettel.app: agent-repl's desktop notifier. A background app (no window, no
// Dock icon) that posts the daemon's banners under its own name and icon, and
// brings Emacs forward when a banner is clicked or the app is opened by hand.
// Everything it does lives in ZettelNotifier; this file is the AppKit shell.
import AppKit
import UserNotifications
import ZettelNotifier

/// The application whose window a click or an open by hand brings forward.
let emacsBundleID = "org.gnu.Emacs"

enum Op {
    static let app = "notifier.app"
}

final class AppDelegate: NSObject, NSApplicationDelegate, UNUserNotificationCenterDelegate {
    let options: Options
    let log: FileLogger
    var relay: Relay!
    var watcher: AdvertisementWatcher!

    init(options: Options, log: FileLogger) {
        self.options = options
        self.log = log
    }

    // The delegate is set BEFORE launching finishes, so a click that launched
    // the app is delivered to it.
    func applicationWillFinishLaunching(_ notification: Notification) {
        UNUserNotificationCenter.current().delegate = self
    }

    func applicationDidFinishLaunching(_ notification: Notification) {
        log.info(Op.app, "Zettel started", ["resident": options.resident, "state_dir": options.stateDir.path])
        UNUserNotificationCenter.current().requestAuthorization(options: [.alert]) { [log] granted, error in
            if let error {
                log.error(Op.app, "asking for permission to notify failed", ["cause": "\(error)"])
            } else if !granted {
                log.error(Op.app, "Zettel is not allowed to notify; banners stay unposted until System Settings → Notifications → Zettel allows it", [:])
            } else {
                log.info(Op.app, "Zettel is allowed to notify", [:])
            }
        }
        relay = Relay(
            dial: { ConnectDaemon(address: $0) }, center: NotificationCenterPoster(), log: log, pid: getpid(),
            superseded: { DispatchQueue.main.async { NSApp.terminate(nil) } })
        watcher = AdvertisementWatcher(stateDir: options.stateDir) { [relay] advertisement in
            Task { await relay!.advertised(advertisement) }
        }
        do {
            try watcher.start()
        } catch {
            log.error(Op.app, "could not watch the state root for the daemon's advertisement; Zettel exits", ["cause": "\(error)"])
            NSApp.terminate(nil)
            return
        }
        if !options.resident {
            bringEmacsForward(because: "Zettel was opened by hand")
        }
    }

    // Opened by hand while already running.
    func applicationShouldHandleReopen(_ sender: NSApplication, hasVisibleWindows flag: Bool) -> Bool {
        bringEmacsForward(because: "Zettel was opened by hand")
        return false
    }

    func userNotificationCenter(
        _ center: UNUserNotificationCenter, didReceive response: UNNotificationResponse,
        withCompletionHandler completionHandler: @escaping () -> Void
    ) {
        defer { completionHandler() }
        guard response.actionIdentifier == UNNotificationDefaultActionIdentifier else { return }
        bringEmacsForward(because: "a banner was clicked")
        guard let token = clickToken(from: response.notification.request.content.userInfo) else {
            log.error(Op.app, "a clicked banner carried no click token; it selects nothing", ["identifier": response.notification.request.identifier])
            return
        }
        Task { await relay.clicked(token: token) }
    }

    func userNotificationCenter(
        _ center: UNUserNotificationCenter, willPresent notification: UNNotification,
        withCompletionHandler completionHandler: @escaping (UNNotificationPresentationOptions) -> Void
    ) {
        completionHandler([.banner, .list])
    }

    func bringEmacsForward(because reason: String) {
        guard let url = NSWorkspace.shared.urlForApplication(withBundleIdentifier: emacsBundleID) else {
            log.error(Op.app, "Emacs is not installed where macOS can find it; nothing was brought forward", ["reason": reason, "bundle": emacsBundleID])
            return
        }
        let configuration = NSWorkspace.OpenConfiguration()
        configuration.activates = true
        NSWorkspace.shared.openApplication(at: url, configuration: configuration) { [log] _, error in
            if let error {
                log.error(Op.app, "bringing Emacs forward failed", ["reason": reason, "cause": "\(error)"])
            } else {
                log.debug(Op.app, "brought Emacs forward", ["reason": reason])
            }
        }
    }
}

let options: Options
do {
    options = try parseOptions(Array(CommandLine.arguments.dropFirst()), environment: ProcessInfo.processInfo.environment,
                               home: FileManager.default.homeDirectoryForCurrentUser)
} catch {
    FileHandle.standardError.write(Data("Zettel: \(error)\n".utf8))
    exit(2)
}
let delegate = AppDelegate(options: options, log: FileLogger(path: options.logPath))
let app = NSApplication.shared
app.setActivationPolicy(.accessory)
app.delegate = delegate
app.run()
