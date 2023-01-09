local actions = require("telescope.actions")
local builtin = require('telescope.builtin')
-- local utils = require("telescope.utils")

require('telescope').setup {
    defaults = {
        -- preview = true,
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
            },
        },
    },
    pickers = {
        fint_files = {
            theme = "ivy",
        },
    },
    extensions = {
    }
}


vim.keymap.set('n', '<leader>f', builtin.find_files,{} )
vim.keymap.set('n', '<leader>g', builtin.live_grep, {})
vim.keymap.set('n', '<leader>b', builtin.buffers, {})
vim.keymap.set('n', '<leader>ls',builtin.lsp_document_symbols, {})
vim.keymap.set('n', '<leader>ld', builtin.diagnostics, {})
-- vim.keymap.set('n', '<leader>lt', builtin.help_tags, {})
