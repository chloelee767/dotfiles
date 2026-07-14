-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- Restore native H/L (top/bottom of window) by removing LazyVim's
-- bufferline prev/next buffer keymaps
vim.keymap.del("n", "<S-h>")
vim.keymap.del("n", "<S-l>")

-- buffers
vim.keymap.set("n", "<leader>bk", function()
  Snacks.bufdelete()
end, { desc = "Delete Buffer" })

vim.keymap.set("n", "<leader>px", function()
  Snacks.scratch()
end, { desc = "Toggle Scratch Buffer" })

vim.keymap.set("n", "<leader>bS", "<cmd>wa<cr>", { desc = "Save All Files" })

-- replace default "Delete Other Buffers" keymap (<leader>bo) with <leader>bO
vim.keymap.del("n", "<leader>bo")
vim.keymap.set("n", "<leader>bO", function()
  Snacks.bufdelete.other()
end, { desc = "Delete Other Buffers" })

-- files
vim.keymap.set("n", "<leader>fs", "<cmd>w<cr>", { desc = "Save File" })

-- copy file path with line number(s) to clipboard
local function yank_path_with_lines(modifier)
  return function()
    local path = vim.fn.expand(modifier)
    local mode = vim.fn.mode()
    local line_ref
    if mode == "v" or mode == "V" or mode == "\22" then
      local l1, l2 = vim.fn.line("v"), vim.fn.line(".")
      if l1 > l2 then
        l1, l2 = l2, l1
      end
      line_ref = l1 == l2 and tostring(l1) or (l1 .. "-" .. l2)
      vim.cmd("normal! \27") -- exit visual mode
    else
      line_ref = tostring(vim.fn.line("."))
    end
    local result = path .. ":" .. line_ref
    vim.fn.setreg("+", result)
    vim.notify("Copied: " .. result)
  end
end

vim.keymap.set(
  { "n", "v" },
  "<leader>fy",
  yank_path_with_lines("%:p"),
  { desc = "Copy Absolute Path:Line to Clipboard" }
)
vim.keymap.set(
  { "n", "v" },
  "<leader>fY",
  yank_path_with_lines("%:."),
  { desc = "Copy Relative Path:Line to Clipboard" }
)
