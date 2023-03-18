-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
-- local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Widgets
local batteryarc_widget = require("widgets.batteryarc.batteryarc")
local net_speed_widget  = require("widgets.net-speed.net-speed")
local volume_widget     = require("widgets.volume-widget.volume")
local spr               = wibox.widget.textbox('  ')

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
-- beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
beautiful.init(os.getenv("HOME") .. "/.config/awesome/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = os.getenv("TERMINAL") or "xterm"
editor = os.getenv("EDITOR") or "nano"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"
home = os.getenv("HOME")

-- notification settings
naughty.config.defaults['icon_size'] = 100
naughty.config.defaults.timeout = 20
naughty.config.defaults.position = 'top_right'
naughty.config.defaults.max_width = 400

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    -- awful.layout.suit.tile.left,
    -- awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    -- awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    -- awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   -- { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   -- { "quit", function() awesome.quit() end },
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock('%a %b %d, %I:%M:%S %p ', 1) --('%a %b %d, %H:%M:%S ', refresh rate)

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewprev(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewnext(t.screen) end)
)

local tasklist_buttons = gears.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  c:emit_signal(
                                                      "request::activate",
                                                      "tasklist",
                                                      {raise = true}
                                                  )
                                              end
                                          end),
                     awful.button({ }, 3, function()
                                              awful.menu.client_list({ theme = { width = 250 } })
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(-1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(1)
                                          end))

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({ "1","2","3","4","5","6","7","8","9"},
    s, {awful.layout.layouts[1]})


    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           -- awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           -- awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.noempty,
        buttons = taglist_buttons
    }

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            spr,
            mykeyboardlayout,
            spr,
            volume_widget(),
            spr,
            batteryarc_widget({
            show_notification_mode = 'on_click'
        }),
            net_speed_widget(),
            spr,
            wibox.widget.systray(),
            mytextclock,
            s.mylayoutbox,
        },
    }
end)
-- }}}

-- {{{ Mouse bindings
-- root.buttons(gears.table.join(
--     awful.button({ }, 3, function () mymainmenu:toggle() end),
--     awful.button({ }, 4, awful.tag.viewnext),
--     awful.button({ }, 5, awful.tag.viewprev)
-- ))
-- }}}



-- Tiling functions
-- closures for client_status
-- client_status[client] = {sidelined = <boolean>, geometry= <client geometry>}
local client_status = {}

sideline_left = function(c)
    local scr = screen[awful.screen.focused()]
    local workarea = scr.workarea
    if client_status[c] == nil then
        client_status[c] = { sidelined = false, geometry = nil }
    end
    if client_status[c].sidelined then
        if client_status[c].geometry then
            c:geometry(client_status[c].geometry)
        end
    else
        client_status[c].geometry = c:geometry()
        workarea.width = math.floor(workarea.width / 2)
        c:geometry(workarea)
    end
    client_status[c].sidelined = not client_status[c].sidelined
end

sideline_right = function(c)
    local scr = screen[awful.screen.focused()]
    local workarea = scr.workarea
    if client_status[c] == nil then
        client_status[c] = { sidelined = false, geometry = nil }
    end
    if client_status[c].sidelined then
        if client_status[c].geometry then
            c:geometry(client_status[c].geometry)
        end
    else
        client_status[c].geometry = c:geometry()
        workarea.x = workarea.x + math.floor(workarea.width / 2)
        workarea.width = math.floor(workarea.width / 2)
        c:geometry(workarea)
    end
    client_status[c].sidelined = not client_status[c].sidelined
end

sideline_top = function(c)
    local scr = screen[awful.screen.focused()]
    local workarea = scr.workarea
    if client_status[c] == nil then
        client_status[c] = { sidelined = false, geometry = nil }
    end
    if client_status[c].sidelined then
        if client_status[c].geometry then
            c:geometry(client_status[c].geometry)
        end
    else
        client_status[c].geometry = c:geometry()
        workarea.height = math.floor(workarea.height / 2)
        c:geometry(workarea)
    end
    client_status[c].sidelined = not client_status[c].sidelined
end

sideline_bottom = function(c)
    local scr = screen[awful.screen.focused()]
    local workarea = scr.workarea
    if client_status[c] == nil then
        client_status[c] = { sidelined = false, geometry = nil }
    end
    if client_status[c].sidelined then
        if client_status[c].geometry then
            c:geometry(client_status[c].geometry)
        end
    else
        client_status[c].geometry = c:geometry()
        workarea.y = workarea.y + math.floor(workarea.height / 2)
        workarea.height = math.floor(workarea.height / 2)
        c:geometry(workarea)
    end
    client_status[c].sidelined = not client_status[c].sidelined
end


-- {{{ Key bindings
globalkeys = gears.table.join(
    -- awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              -- {description="show help", group="awesome"}),
    -- awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
    --           {description = "view previous", group = "tag"}),
    -- awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
    --           {description = "view next", group = "tag"}),
    awful.key({ modkey,           }, "Tab", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    -- awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
    --           {description = "show main menu", group = "awesome"}),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, }, "Right", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, }, "Left", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    -- awful.key({ "Mod1",           }, "Tab",
    --     function ()
    --         awful.client.focus.history.previous()
    --         if client.focus then
    --             client.focus:raise()
    --         end
    --     end,
    --     {description = "go back", group = "client"}),
    -- Standard program
    -- awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
    --           {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),

    -- awful.key({ modkey, "Shift"   }, "q", awesome.quit,
    --           {description = "quit awesome", group = "awesome"}),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),

    -- awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
    --           {description = "increase the number of columns", group = "layout"}),
    -- awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
    --           {description = "decrease the number of columns", group = "layout"}),
    -- awful.key({ modkey,           }, "Right", function () awful.layout.inc( 1)                end,
    --           {description = "select next", group = "layout"}),
    -- awful.key({ modkey, }, "Left", function () awful.layout.inc(-1)                end,
    --           {description = "select previous", group = "layout"}),

-- Volume keys
    awful.key({}, "XF86AudioRaiseVolume", function() volume_widget:inc(5) end),
    awful.key({}, "XF86AudioLowerVolume", function() volume_widget:dec(5) end),
    awful.key({}, "XF86AudioMute", function() volume_widget:toggle() end),
    awful.key({ modkey }, "=", function() volume_widget:inc(5) end),
    awful.key({ modkey }, "-", function() volume_widget:dec(5) end),
    awful.key({ modkey, "Shift" }, "-", function() volume_widget:toggle() end),
awful.key({ modkey, "Control" }, "\\", naughty.destroy_all_notifications)

    -- Prompt
    -- awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
    --           {description = "run prompt", group = "launcher"}),

    -- awful.key({ modkey , "Shift"}, "x",
    --           function ()
    --               awful.prompt.run {
    --                 prompt       = "Run Lua code: ",
    --                 textbox      = awful.screen.focused().mypromptbox.widget,
    --                 exe_callback = awful.util.eval,
    --                 history_path = awful.util.get_cache_dir() .. "/history_eval"
    --               }
    --           end,
    --           {description = "lua execute prompt", group = "awesome"})
    -- Menubar
    -- awful.key({ modkey , "Shift"}, "r", function() menubar.show() end,
    --           {description = "show the menubar", group = "launcher"})
)

clientkeys = gears.table.join(
    -- Tiling keys
    awful.key({ modkey, "Mod1"}, "h", sideline_left),
    awful.key({ modkey, "Mod1"}, "l", sideline_right),
    awful.key({ modkey, "Mod1"}, "k", sideline_top),
    awful.key({ modkey, "Mod1"}, "j", sideline_bottom),
    -- Resize floating windows
    awful.key({ modkey, "Shift", "Control" }, "k", function(c)
        c:relative_move(0, 0, 0, -20)
    end,
        { description = "Floating Resize Vertical -", group = "client" }),
    awful.key({ modkey, "Shift", "Control" }, "j", function(c)
        c:relative_move(0, 0, 0, 20)
    end,
        { description = "Floating Resize Vertical +", group = "client" }),
    awful.key({ modkey, "Shift", "Control" }, "h", function(c)
        c:relative_move(0, 0, -20, 0)
    end,
        { description = "Floating Resize Horizontal -", group = "client" }),
    awful.key({ modkey, "Shift", "Control" }, "l", function(c)
        c:relative_move(0, 0, 20, 0)
    end,
        { description = "Floating Resize Horizontal +", group = "client" }),
    -- Moving floating windows
    awful.key({ modkey, "Control" }, "k", function(c)
        c:relative_move(0, -45, 0, 0)
    end,
        { description = "Floating Move Up", group = "client" }),
    awful.key({ modkey, "Control" }, "j", function(c)
        c:relative_move(0, 45, 0, 0)
    end,
        { description = "Floating Move Down", group = "client" }),
    awful.key({ modkey, "Control" }, "h", function(c)
        c:relative_move( -45, 0, 0, 0)
    end,
        { description = "Floating Move Left", group = "client" }),
    awful.key({ modkey, "Control" }, "l", function(c)
        c:relative_move(45, 0, 0, 0)
    end,
        { description = "Floating Move Right", group = "client" }),

awful.key({ modkey, "Mod1" }, "\\",
function ()
    naughty.toggle()
    naughty.notify({
        preset = naughty.config.presets.normal,
        title="notification toggled",
        text=(function ()
            if naughty.is_suspended() then
                return "suspended"
            else
                return "resumed"
            end
        end)(),
        timeout = 3,
        screen = awful.screen.focused(),
    })
end),
    awful.key({ modkey,           "Shift" }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Shift" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           "Shift"}, "Right",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,  "Shift"         }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           "Shift"}, "s",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey, "Control" }, "s",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:emit_signal(
                        "request::activate", "key.unminimize", {raise = true}
                    )
                  end
              end,
              {description = "restore minimized", group = "client"}),
    awful.key({ modkey, }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end ,
        {description = "(un)maximize horizontally", group = "client"}),
    awful.key({ modkey, "Shift" }, "m",
        function (c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end ,
        {description = "(un)maximize vertically", group = "client"}),
    awful.key({ modkey,           "Control"}, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "(un)maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.

    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
          "pinentry",
        },
        class = {
          "Arandr",
          "Blueman-manager",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
          "Wpa_gui",
          "veromix",
          "xtightvncviewer"},

        -- Note that the name property shown in xprop might be set slightly after creation of the client
        -- and the name shown there might not match defined rules here.
        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "ConfigManager",  -- Thunderbird's about:config.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    { rule = { class = "Dragon-drop" },
        properties = { sticky = true, ontop = true, placement = awful.placement.top_left } },

    { rule_any = { name = { "mpvfloat", "Picture-in-Picture", "Picture in picture" } },
        properties = { floating = true, placement = awful.placement.bottom_right, ontop = true, sticky = false } },

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = true },
    except = {class= "jetbrains-idea-ce"}},

    --  { rule = { },
    --     properties = { size_hints_honor = false },
    -- except = {class= "mpv"}},

    -- { rule_any = {class = {"st", "XTerm",  "Emacs", "St","Zathura", "flow"}},
    --    properties = { titlebars_enabled = true }},

    -- { rule_any = { class = {"Tor Browser", "firefox", "Chromium"} },
    --    properties = { tag = tags[1][2]}},

    -- { rule = { name = "mpvfloat" },
    --     properties = { floating = true, sticky = true, placement = awful.placement.bottom_right, ontop = true } },
    -- { rule_any = { class = { "code-oss", "VSCodium", "Atom", "jetbrains-idea-ce",
    --     "java-util-concurrent-ForkJoinWorkerThread", "DBeaver", "Eclipse", "Java", "Subl" } },
    --     properties = { tag = tags[1][3] } },

    -- { rule_any = { class = { "pm", "tm", "libreoffice", "Libreoffice", "LibreOffice" }, name = { "LibreOffice" } },
    --     properties = { tag = tags[1][5] } },

    -- { rule_any = { class = { "Gimp", "Inkscape", "kdenlive" } },
    --     properties = { tag = tags[1][5] } },
    -- { rule_any = { class = { "TelegramDesktop", "telegram-desktop", "zoom", "discord", "SchildiChat" } },
    --     properties = { tag = tags[1][4] } },


}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            -- awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
-- client.connect_signal("mouse::enter", function(c)
--     c:emit_signal("request::activate", "mouse_enter", {raise = false})
-- end)


 -- disable edges snapping
awful.mouse.snap.edge_enabled = false

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- awful.spawn.with_shell("~/.config/awesome/autorun.sh")
