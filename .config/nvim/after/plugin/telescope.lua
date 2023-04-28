local actions = require("telescope.actions")
local builtin = require('telescope.builtin')
require('telescope').load_extension('fzf')
-- local utils = require("telescope.utils")

require('telescope').setup {
    defaults = {
    --     border = false,
        -- preview = false;
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
        fuzzy = true, -- false will only do exact matching
        override_generic_sorter = true, -- override the generic sorter
        override_file_sorter = true, -- override the file sorter
        case_mode = "smart_case", -- or "ignore_case" or "respect_case"
        -- the default case_mode is "smart_case"
    }
}
}


vim.keymap.set('n', '<leader>f', builtin.find_files, {})
vim.keymap.set('n', '<leader>r', builtin.oldfiles, {}) -- recent files
vim.keymap.set('n', '<leader>g', builtin.live_grep, {})
vim.keymap.set('n', '<leader>ls', builtin.lsp_document_symbols, {})
vim.keymap.set('n', '<leader>ld', builtin.diagnostics, {})
