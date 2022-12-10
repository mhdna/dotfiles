require "user.options"
require "user.keymaps"
require "user.packer"
require "user.colorscheme"
require "user.cmp"
require "user.lsp"
-- require "user.telescope"
require "user.treesitter"
require "user.autopairs"
--[[ require "user.comment" ]]
-- require "user.gitsigns"
require "user.nvim-tree"
-- require "user.neogit"
-- require "user.todo-comments"

--[[ require "user.hardline" ]]
-- require "user.bufferline"
-- require "user.lualine"
-- require "user.toggleterm"
--[[ require "user.project" ]]
-- require "user.dired"
require "user.impatient"
-- require "user.indentline"
-- require "user.alpha"
-- require "user.whichkey"
require "user.autocommands"


-- XkbSwitch
-- vim.g.XkbSwitchEnabled = 1

-- fzf
vim.cmd [[let g:fzf_layout = { 'down': '~40%' } ]]

-- require('todotxt-nvim').setup({
-- 	todo_file = "~/stuff/wiki/todo.txt",
-- })
