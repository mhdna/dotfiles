config.source('scripts/redirector.py')
config.load_autoconfig()
c.new_instance_open_target = 'tab'
c.downloads.position = 'bottom'
c.tabs.position = 'top'
c.statusbar.position = 'bottom'
#c.spellcheck.languages = ['en-US']
# config.set("url.default_page", "~/.startpage/index.html")
# config.set("url.start_pages", "~/.startpage/index.html")#"wiby.me")


config.set("url.default_page", "about:blank")
config.set("url.start_pages", "about:blank")  # "wiby.me")
#c.spellcheck.languages = ["en-US"]

c.confirm_quit = ["downloads"]  # always
# c.downloads.location.directory = "$HOME"
c.downloads.location.prompt = True

c.content.notifications.enabled = False
c.content.images = True
c.scrolling.smooth = False
c.content.autoplay = True
c.tabs.background = True
# Per site settings
# Javascript
c.content.javascript.enabled = True
config.set('content.javascript.enabled', False, '*://www.google.com/')
# c.completion.cmd_history_max_items = 0
# c.completion.web_history.max_items = 0
c.completion.show = "always"  # never
c.content.javascript.can_access_clipboard = False
# c.content.javascript.can_open_tabs_automatically = False
# config.set('content.images', True, '*://keyma.sh/')

c.messages.timeout = 5000

# c.auto_save.session = False

config.set('url.searchengines', {"DEFAULT": "https://searx.be/?q={}", "s": "https://www.startpage.com/do/search?q={}", "w": "https://wiby.me/?q={}", "S": "https://www.searx.be/search?q={}", "aw": "https://wiki.archlinux.org/?search={}",
           "wi": "https://en.wikipedia.org/wiki/Special:Search/{}", "dic": "https://www.urbandictionary.com/define.php?term=%7B{}%7D", "y": "invidious.namazso.eu/search?q={}", "yt": "https://www.youtube.com/results?search_query={}", "yy": "https://yandex.com/images/search?text={}"})

# unbind
config.unbind("<ctrl+q>")
config.unbind("ad")
config.unbind("u")
config.unbind("gC")
# config.unbind("U")
config.unbind("d")
config.bind('d', 'scroll-page 0 0.5')
config.bind('u', 'scroll-page 0 -0.5')

# basic remaps
config.bind('t', 'open -t')
config.bind('x', 'tab-close')
config.bind('X', 'undo')
config.bind('gc', 'tab-clone')
# config.bind('uu', 'undo')
# config.bind('uw', 'undo -w')

# true fullscreen
config.bind(';;', 'config-cycle statusbar.show in-mode always ;; config-cycle tabs.show switching always;; config-cycle content.notifications.enabled true false')

# print
config.bind('<Ctrl+Alt+p>',
            "print --pdf ~/dox/readlater/{title}.pdf ;; spawn notify-send -t 1500 'printing'")
config.bind('<Ctrl+Alt+r>',
            "spawn bach -c 'echo {url}{title} >> ~/dox/notes/todo.txt && notify-send -t 1500 'Read Later''")
# clipboard
config.bind('pP', 'open -t -- {clipboard}')
# yank
config.bind('yP', 'yank pretty-url')

# yank phone
config.bind(
    'yp', 'spawn kdeconnect-cli --refresh && kdeconnect-cli --share-text {url} --device 415a811582e4a899')
config.bind(';p', 'spawn kdeconnect-cli --refresh')
config.bind(';p', 'hint links spawn kdeconnect-cli --share-text {url} --device 415a811582e4a899')
# config.set("colors.webpage.darkmode.enabled", True)
# Command mode

# Print high quality

#config.bind('j', 'scroll-px 1 100')
#config.bind('k', 'scroll-px 1 -100')
#config.bind('<Ctrl+b>', 'rl-beginning-of-line')
#config.bind('<Ctrl+e>', 'rl-end-of-line')
#config.bind('<Alt+l>', 'rl-forward-delete-char')
#config.bind('<Alt+h>', 'rl-backward-delete-char')
#config.bind('<Ctrl+l>', 'rl-forward-char')
#config.bind('<Ctrl+Shift+l>', 'rl-forward-word')
#config.bind('<Ctrl+h>', 'rl-backward-char')
#config.bind('<Ctrl+Shift+h>', 'rl-backward-word')

config.bind('<Ctrl+b>', 'rl-beginning-of-line', mode='command')
config.bind('<Ctrl+e>', 'rl-end-of-line', mode='command')
config.bind('<Ctrl+l>', 'rl-forward-delete-char', mode='command')
config.bind('<Ctrl+h>', 'rl-backward-delete-char', mode='command')
config.bind('<Alt+l>', 'rl-forward-char', mode='command')
config.bind('<Alt+Shift+l>', 'rl-forward-word', mode='command')
config.bind('<Alt+h>', 'rl-backward-char', mode='command')
config.bind('<Alt+Shift+h>', 'rl-backward-word', mode='command')
config.bind('<Alt+k>', 'completion-item-focus prev', mode='command')
config.bind('<Alt+j>', 'completion-item-focus next', mode='command')


config.bind('<Ctrl+b>', 'rl-beginning-of-line', mode='prompt')
config.bind('<Ctrl+e>', 'rl-end-of-line', mode='prompt')
config.bind('<Alt+l>', 'rl-forward-delete-char', mode='prompt')
config.bind('<Alt+h>', 'rl-backward-delete-char', mode='prompt')
config.bind('<Ctrl+l>', 'rl-forward-char', mode='prompt')
config.bind('<Ctrl+h>', 'rl-backward-char', mode='prompt')
config.bind('<Ctrl+Shift+l>', 'rl-forward-word', mode='prompt')
config.bind('<Ctrl+Shift+h>', 'rl-backward-word', mode='prompt')
config.bind('<Ctrl+j>', 'prompt-item-focus next', mode='prompt')
config.bind('<Ctrl+k>', 'prompt-item-focus prev', mode='prompt')

config.bind('gm', 'tab-give')
config.bind('<Alt+p>', 'tab-pin')
# userscripts
config.bind('gs', 'spawn -u ~/.config/qutebrowser/scripts/selection.sh')
config.bind(',O', 'spawn --userscript ~/.config/qutebrowser/scripts/opendownload')
config.bind('ap', 'spawn --userscript ~/.config/qutebrowser/scripts/qutepocket')
config.bind(',R', 'spawn --userscript ~/.config/qutebrowser/scripts/readability')

config.unbind('gf')
config.bind('gS', 'view-source')


config.bind('gh', 'home')
config.bind('gH', 'history')
config.bind(',ce', 'config-edit')
config.bind('cd', 'download-clear')
config.bind('cD', 'download-cancel')
config.bind('cm', 'clear-messages')

# zoom
config.bind('+', 'zoom')
config.bind('=', 'zoom-in')
config.bind('-', 'zoom-out')

config.bind(',o', 'download-open')
config.bind('<Ctrl+Shift+r>', 'restart')
config.bind('<Ctrl+b>', 'bookmark-list')
config.bind('<Ctrl+Shift+b>', 'bookmark-del')
config.bind(',p', 'config-cycle -p content.plugins ;; reload')
config.bind('<Ctrl+Shift+i>', "config-cycle colors.webpage.darkmode.enabled ;; restart")
config.bind('J', 'tab-prev')
config.bind('K', 'tab-next')
config.bind('gj', 'tab-move -')
config.bind('gk', 'tab-move +')

config.bind(',rta', 'open {url}top/?sort=top&t=all')
config.bind(',rtv', 'spawn st -e "rtv {url}"')
config.bind(',b', 'spawn -d brave {url}')
config.bind(',m', 'hint links spawn mpv $1 {hint-url}')
config.bind(',M', 'spawn -d mpv {url}')
config.bind(',a', 'hint links spawn -d mpv --profile=pseudo-gui --loop=inf {hint-url}')
config.bind(',A', 'spawn -d mpv  --profile=pseudo-gui --loop=inf {url}')

config.bind(',d', 'hint links spawn youtube-dl -P ~/vids/yt -f 22 {hint-url}')
config.bind(',D', 'hint links spawn youtube-dl -P ~/music/yt -x -f bestaudio/best {hint-url}')


# translate
#config.bind(',t', 'hint userscript link translate')
#config.bind(',T', 'hint userscript all translate --text')
# config.bind(',T', 'spawn --userscript translate -t ar')  # <Ctrl+Shift+t>
# config.bind(',t', 'spawn --userscript translate --text -t ar')

#css = '~/proj/solarized-everything-css/css/gruvbox/gruvbox-all-sites.css'
#c.content.user_stylesheets = ['~/.config/qutebrowser/gruvbox-css/gruvbox-all-sites.css']
#c.content.user_stylesheets =  '~/.config/qutebrowser/gruvbox-css/github.css'
#config.bind(',n', 'config-cycle content.user_stylesheets {css} ""')

#c.url.searchengines['dictcc'] = 'https://www.dict.cc/?s={}'
#c.url.searchengines['DEFAULT'] = 'https://duckduckgo.com/?q={}'
#c.url.searchengines['ss'] = 'https://www.startpage.com/do/search?q={}'
#c.url.searchengines['y'] = 'https://invidious.namazso.eu/search?q={}'
#c.url.searchengines['yt'] = 'https://www.youtube.com/results?search_query={}'
#c.url.searchengines['aw'] = 'https://wiki.archlinux.org/?search={}'
#c.url.searchengines['w'] = 'https://en.wikipedia.org/wiki/Special:Search/{}'
#c.url.searchengines['dic'] = 'https://www.urbandictionary.com/define.php?term=%7B{}%7D'
#c.url.searchengines['maps'] = 'https://www.google.com/maps?q=%s'

c.aliases['pr'] = "print --pdf "
c.aliases['ytt'] = """spawn -v -m bash -c 'cd ~/vids/yt && youtube-dl -f 22 "$@"' _ {url}"""
c.aliases[
    'ytaa'] = """spawn -v -m bash -c 'cd ~/music/yt && youtube-dl -x -f bestaudio/best "$@"' _ {url}"""

#config.set ('content.headers.user_agent', 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.131 Safari/537.36')

# config.set('fonts.default_family', 'mono')
# config.set('fonts.default_size', '10pt')
# config.set('fonts.contextmenu', 'default_size default_family')
# config.set('fonts.statusbar', 'default_size default_family')
# config.set('fonts.completion.entry', 'default_size default_family')
# config.set('fonts.contextmenu', 'default_size default_family')
# config.set('fonts.hints', 'default_size default_family')
# config.set('fonts.prompts', 'default_size default_family')
# config.set('fonts.tabs.selected', 'default_size default_family')
# config.set('fonts.tabs.unselected', 'default_size default_family')
# config.set('fonts.downloads', 'default_size default_family')
# config.set('fonts.web.family.fixed', 'Sans')
# config.set('fonts.web.family.sans_serif', 'Sans')
# config.set('fonts.web.family.serif', 'Serif')

#c.fonts.tabs.selected = '10pt monospace'
#c.fonts.tabs.unselected = '10pt monospace'
#c.fonts.statusbar = '10pt monospace'
#c.fonts.web.family.fantasy = 'Arial'

# c.search.incremental = False
# c.editor.command = ['nvim', '{}']

#c.qt.args = ['ppapi-widevine-path=/usr/lib/qt/plugins/ppapi/libwidevinecdmadapter.so']

# config.source('perdomain.py')
# config.source('gruvbox.py')
# config.source('zenburn.py')

# config.source('base16-3024.config.py')
# config.source('base16-gruvbox-dark-hard.config.py')
# config.source('qutewal/qutewal.py')


# Security enhancements

# c.content.headers.accept_language en-US,en;q = 0.5
# c.content.headers.custom = '{"accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"}'

# config.set ('content.headers.user_agent', 'Mozilla/5.0 (Windows NT 10.0; rv:68.0) Gecko/20100101 Firefox/68.0')
# config.set ('content.headers.user_agent', 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.131 Safari/537.36')

c.content.canvas_reading = False
c.content.webgl = False
c.content.blocking.method = "both"
