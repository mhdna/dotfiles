require('telescope').setup {
    defaults = {
        -- Default configuration for telescope goes here:
        -- config_key = value,
        -- mappings = {
        --     i = {
        --         -- map actions.which_key to <C-h> (default: <C-/>)
        --         -- actions.which_key shows the mappings for your picker,
        --         -- e.g. git_{create, delete, ...}_branch for the git_branches picker
        --         ["<C-h>"] = "which_key"
        --     }
        -- }
        previewe = false,
    },
    pickers = {
        -- Default configuration for builtin pickers goes here:
        -- picker_name = {
        --   picker_config_key = value,
        --   ...
        -- }
        -- Now the picker_config_key will be applied every time you call this
        -- builtin picker
    },
    extensions = {
        -- Your extension configuration goes here:
        -- extension_name = {
        --   extension_config_key = value,
        -- }
        -- please take a look at the readme of the extension you want to configure
    }
}


local builtin = require('telescope.builtin')

vim.keymap.set('n', '<leader>f', builtin.find_files, {})
vim.keymap.set('n', '<leader>g', builtin.live_grep, {})
vim.keymap.set('n', '<leader>b', builtin.buffers, {})
vim.keymap.set('n', '<leader>ls', builtin.lsp_document_symbols, {})
vim.keymap.set('n', '<leader>ld', builtin.diagnostics, {})
-- vim.keymap.set('n', '<leader>t', builtin.help_tags, {})
