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
