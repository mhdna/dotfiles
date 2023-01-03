local dap, dapui = require("dap"), require("dapui")
require('dap.ext.vscode').load_launchjs()
require("nvim-dap-virtual-text").setup()
require("dapui").setup()

dap.defaults.fallback.terminal_win_cmd = 'tabnew'

-- dap.listeners.after.event_initialized["dapui_config"] = function()
--   dapui.open()
-- end
-- dap.listeners.before.event_terminated["dapui_config"] = function()
--   dapui.close()
-- end
-- dap.listeners.before.event_exited["dapui_config"] = function()
--   dapui.close()
-- end


vim.keymap.set("n", "<leader>db", "<cmd>lua require'dap'.toggle_breakpoint()<cr>")
vim.keymap.set("n", "<leader>dB", "<cmd>lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<cr>")
-- vim.keymap.set("n", "<leader>bl", "<cmd>lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<cr>")
vim.keymap.set("n", "<leader>dR", "<cmd>lua require'dap'.clear_breakpoints()<cr>")
vim.keymap.set("n", "<leader>dl", "<cmd>Telescope dap list_breakpoints<cr>")

vim.keymap.set("n", "<F4>", "<cmd>w<cr><cmd>lua require'dap'.continue()<cr>")
vim.keymap.set("n", "<C-F4>", "<cmd>lua require'dapui'.toggle()<cr>")
vim.keymap.set("n", "<F3>", "<cmd>lua require'dap'.step_over()<cr>")
vim.keymap.set("n", "<F2>", "<cmd>lua require'dap'.step_into()<cr>")
vim.keymap.set("n", "<F12>", "<cmd>lua require'dap'.step_out()<cr>")
vim.keymap.set("n", "<C-F12>", "<cmd>lua require'dap'.terminate()<cr>")
-- vim.keymap.set("n", "<C-S-F2>", "<cmd>lua require'dap'.disconnect()<cr>")
-- vim.keymap.set("n", "<leader>dr", "<cmd>lua require'dap'.repl.toggle()<cr>")
-- vim.keymap.set("n", "<leader>dl", "<cmd>lua require'dap'.run_last()<cr>")
-- vim.keymap.set("n", "<leader>di", function() require"dap.ui.widgets".hover() end)
-- vim.keymap.set("n", "<leader>d?", function() local widgets=require"dap.ui.widgets";widgets.centered_float(widgets.scopes) end)
-- vim.keymap.set("n", "<leader>df", "<cmd>Telescope dap frames<cr>")
-- vim.keymap.set("n", "<leader>dh", "<cmd>Telescope dap commands<cr>")
