local hyper = {"ctrl", "alt", "cmd"}

hs.loadSpoon("MiroWindowsManager")

hs.window.animationDuration = 0
spoon.MiroWindowsManager:bindHotkeys({
    up = {hyper, "k"},
    right = {hyper, "l"},
    down = {hyper, "j"},
    left = {hyper, "h"},
    fullscreen = {hyper, ";"}
})

-- current space, excluding minimized windows
-- switcher_space = hs.window.switcher.new(hs.window.filter.new():setCurrentSpace(true))
-- hs.hotkey.bind('alt', 'tab', 'Next window', function()switcher_space:next()end)
-- hs.hotkey.bind('alt-shift', 'tab', 'Prev window', function()switcher_space:previous()end)

-- function openOrSwitch(app)
--   local windows = hs.window.filter.new():setAppFilter(app):getWindows(hs.window.filter.sortByFocusedLast)
--   local focusedWin = hs.window.focusedWindow()
--   for _, win in ipairs(windows) do
--     if win != focusedWin then
--       win:focus()
--       return
--     end
--   end
-- end

hs.hotkey.bind(hyper, "s", function() hs.application.open("Slack") end)
hs.hotkey.bind(hyper, "t", function() hs.application.open("iTerm") end)
hs.hotkey.bind(hyper, "n", function() hs.application.open("TriliumNext Notes") end)
hs.hotkey.bind(hyper, "w", function() hs.application.open("Google Chrome") end)
hs.hotkey.bind(hyper, "e", function() hs.application.open("Emacs") end)
