local actions = require("telescope.actions")
local builtin = require('telescope.builtin')
require('telescope').load_extension('fzf')
-- require('telescope').load_extension('bookmarks')
-- local utils = require("telescope.utils")



require('telescope').setup {
        defaults = {
                preview = {
                        filesize_limit = 0.1, -- MB
                },
                layout_config = {
                        prompt_position = "bottom",
                },
                winblend = 0,
                border = {},
                prompt_title = '',
                results_title = '',
                preview_title = '',
                mappings = {
                        n = {
                                ["<C-c>"] = actions.close,
                        },
                        i = {
                                ["<C-u>"] = false, -- delete to the beginning using C-u
                                -- ["<esc>"] = actions.close,
                                ['<C-j>'] = actions.cycle_history_next,
                                ['<C-k>'] = actions.cycle_history_prev
                        },
                },
        },
        extensions = {
                fzf = {
                        fuzzy = true,                   -- false will only do exact matching
                        override_generic_sorter = true, -- override the generic sorter
                        override_file_sorter = true,    -- override the file sorter
                        case_mode = "smart_case",       -- or "ignore_case" or "respect_case"
                        -- the default case_mode is "smart_case"
                }
        },
}

vim.keymap.set('n', '<leader>f', builtin.find_files, { desc = 'Find Files' })
vim.keymap.set('n', '<leader>rr', builtin.oldfiles, { desc = 'Find Recent files' })
vim.keymap.set('n', '<leader>d', builtin.diagnostics, { desc = 'Find Diagnostics' })
vim.keymap.set('n', 'gs', builtin.live_grep, { desc = 'Find - Grep' })
vim.keymap.set('n', 'gw', builtin.grep_string, { desc = 'Find - Grep current word' })

vim.keymap.set('n', '<leader>b', builtin.buffers, { desc = 'Buffers' })

vim.keymap.set('n', '<leader>H', builtin.help_tags, { desc = 'Help' })
vim.keymap.set('n', '<leader>K', builtin.keymaps, { desc = 'Keymaps' })
