user_pref("browser.ctrlTab.sortByRecentlyUsed", true);
user_pref("browser.uidensity", 1);
user_pref("general.smoothScroll", false);
user_pref("extensions.pocket.enabled", false);
user_pref("privacy.resistFingerprinting.letterboxing", false); // [HIDDEN PREF]
user_pref("keyword.enabled", true);
user_pref("signon.rememberSignons", false);
// Disable the Twitter/R*ddit/Faceberg ads in the URL bar:
user_pref("browser.urlbar.quicksuggest.enabled", false);
user_pref("browser.urlbar.suggest.topsites", false); // [FF78+]
// Do not prefil forms:
user_pref("signon.prefillForms", false);
// Do not autocomplete in the URL bar:
user_pref("browser.urlbar.autoFill", false);
// Allow access to http (i.e. not https) sites:
user_pref("dom.security.https_only_mode", false);
// Keep cookies until expiration or user deletion:
user_pref("network.cookie.lifetimePolicy", 0);
user_pref("dom.webnotifications.serviceworker.enabled", false);
// Disable push notifications:
user_pref("dom.push.enabled", false);
// Disable automatic hiding for fullscreen
user_pref("browser.fullscreen.autohide", false);
