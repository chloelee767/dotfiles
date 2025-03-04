local wezterm = require 'wezterm'
local config = wezterm.config_builder()
local act = wezterm.action

config.keys = {
    { key = 'D', mods = 'SHIFT|CTRL', action = act.SplitVertical{ domain =  'CurrentPaneDomain' } },
    { key = 'R', mods = 'SHIFT|CTRL', action = act.SplitHorizontal{ domain =  'CurrentPaneDomain' } },
    { key = 'W', mods = 'SHIFT|CTRL', action = act.CloseCurrentPane{ confirm = true } },
    -- disable default keybinds for split vertical and horizontal
    { key = '\"', mods = 'ALT|CTRL', action = act.DisableDefaultAssignment },
    { key = '\"', mods = 'SHIFT|ALT|CTRL', action = act.DisableDefaultAssignment },
    { key = '\'', mods = 'SHIFT|ALT|CTRL', action = act.DisableDefaultAssignment },
    { key = '%', mods = 'ALT|CTRL', action = act.DisableDefaultAssignment },
    { key = '%', mods = 'SHIFT|ALT|CTRL', action = act.DisableDefaultAssignment },
    { key = '5', mods = 'SHIFT|ALT|CTRL', action = act.DisableDefaultAssignment },
}

return config
