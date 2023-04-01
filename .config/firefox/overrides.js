// disable Firefox sync
user_pref("identity.fxaccounts.enabled", false);
// disable fullscreen autohide
user_pref("browser.fullscreen.autohide", false);
// do not show bookmarks in toolbar
user_pref("browser.toolbars.bookmarks.visibility", "never")
// disable firefox-view
user_pref("browser.tabs.firefox-view", false)
user_pref("browser.ctrlTab.sortByRecentlyUsed", true);
user_pref("extensions.pocket.enabled", false);
user_pref("keyword.enabled", true);
// disable the Twitter/R*ddit/Faceberg ads in the URL bar
user_pref("browser.urlbar.quicksuggest.enabled", false);
user_pref("browser.urlbar.suggest.topsites", false); // [FF78+]
// Do not prefil forms:
user_pref("signon.prefillForms", false);
// Do not autocomplete in the URL bar:
user_pref("browser.urlbar.autoFill", false);
// allow access to http sites
user_pref("dom.security.https_only_mode", false);
// disable push notifications
user_pref("dom.push.enabled", false);
// save history
user_pref("privacy.sanitize.sanitizeOnShutdown", false);
// maximum window size
user_pref("privacy.window.maxInnerWidth", 1000);
user_pref("privacy.window.maxInnerHeight", 1000);
// disable website is now full screen warning
user_pref("full-screen-api.warning.timeout", 0);

// /* Zoom compatibility settings */
// user_pref("media.peerconnection.enabled", true); // 2001
// user_pref("media.peerconnection.ice.no_host", false); // 2001 [may or may not be required]
// /* needed for screensharing */
// // user_pref("dom.webaudi")
// // user_pref("media.getusermedia.screensharing.enabled", true);
// user_pref("webgl.disabled", false);// NEEDED FOR ZOOM
// /* 2012: limit WebGL ***/
// user_pref("webgl.min_capability_mode", false); // NEEDED FOR ZOOM
// user_pref("privacy.resistFingerprinting", false); //breaks zoom if true!o.enabled", true); // 2510
