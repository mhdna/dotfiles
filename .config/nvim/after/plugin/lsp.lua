-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(_, bufnr)
    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    local opts = { noremap = true, buffer = bufnr }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', '<leader>li', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', '<M-K>', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<leader>lwa', vim.lsp.buf.add_workspace_folder, opts)
    vim.keymap.set('n', '<leader>lwr', vim.lsp.buf.remove_workspace_folder, opts)
    vim.keymap.set('n', '<leader>lwl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, opts)
    vim.keymap.set('n', '<leader>ld', vim.lsp.buf.type_definition, opts)
    vim.keymap.set('n', '<leader>lr', vim.lsp.buf.rename, opts)
    vim.keymap.set('n', '<leader>la', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', '<leader>lf', ":lua vim.lsp.buf.format()<CR>:w<CR>", opts) -- autosave on format
end

-- add completion capability
local capabilities = require('cmp_nvim_lsp').default_capabilities()

local lspconfig = require('lspconfig')

lspconfig.ccls.setup {
    init_options = {
        cache = {
            directory = vim.env.XDG_CACHE_HOME .. "/ccls/",
            -- or vim.fs.normalize "~/.cache/ccls" -- if on nvim 0.8 or higher
        }
    },
    on_attach = on_attach,
    flags = lsp_flags,
}

-- lspconfig.tsserver.setup {
--     on_attach = on_attach,
--     flags = lsp_flags,
-- }

lspconfig.pylsp.setup {
    on_attach = on_attach,
    flags = lsp_flags,
    settings = {
        pylsp = {
            plugins = {
                pycodestyle = {
                    ignore = { 'W391' },
                    maxLineLength = 100
                }
            }
        }
    }
}

lspconfig.gopls.setup {
    on_attach = on_attach,
    flags = lsp_flags,
}

lspconfig.lua_ls.setup {
    on_attach = on_attach,
    flags = lsp_flags,
    diagnostics = {
        globals = { 'vim' },
    },
}

-- Disable inline error messages
-- vim.diagnostic.config({ virtual_text = false })
