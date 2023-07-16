local actions = require("telescope.actions")
local builtin = require('telescope.builtin')
require('telescope').load_extension('fzf')
-- local utils = require("telescope.utils")


require('telescope').setup {
    defaults = {
        border = false,
        preview = false,
        layout_strategy = 'bottom_pane',
        layout_config = {
            bottom_pane = {
                prompt_position = "bottom",
                height = 15,
            }

        },
        mappings = {
            i = {
                ["<C-u>"] = false, -- delete to the beginning using C-u
                ["<esc>"] = actions.close,
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
    vim.cmd [[
   highlight TelescopeNormal guifg=black guibg=#444444 term=underline cterm=underline gui=underline
   highlight TelescopeMatching guibg=yellow guifg=black
   " highlight TelescopeMatchingChar guibg=grean
    ]]

}


-- vim.keymap.set('n', '<leader>fg', require('telescope.builtin').git_files, { desc = 'Search [G]it [F]iles' })
vim.keymap.set('n', '<leader>fr', builtin.oldfiles, { desc = '[F]ind [R]ecent filed' })
vim.keymap.set('n', '<leader>ff', require('telescope.builtin').find_files, { desc = '[F]ind [F]iles' })
vim.keymap.set('n', '<leader>b', require('telescope.builtin').buffers, { desc = '[B]uffers' })
vim.keymap.set('n', '<leader>gw', require('telescope.builtin').grep_string, { desc = '[Grep] current [W]ord' })
vim.keymap.set('n', '<leader>gg', require('telescope.builtin').live_grep, { desc = '[G]rep' })
vim.keymap.set('n', '<leader>fd', require('telescope.builtin').diagnostics, { desc = '[F]ind [D]iagnostics' })
vim.keymap.set('n', '<leader>hd', builtin.lsp_document_symbols, {})
-- vim.keymap.set('n', '<leader>ft', require('telescope.builtin').treesitter, { desc = '[F]ind [F]iles' })
vim.keymap.set('n', '<leader>hh', require('telescope.builtin').help_tags, { desc = '[F]ind [H]elp' })
vim.keymap.set('n', '<leader>hk', require('telescope.builtin').keymaps, { desc = '[F]ind by [G]rep' })
