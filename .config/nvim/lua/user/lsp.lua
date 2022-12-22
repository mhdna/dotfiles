--unscii Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap = true, silent = true }

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')


    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    vim.opt.signcolumn="yes"
    local bufopts = { noremap = true, silent = true, buffer = bufnr }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
    vim.keymap.set('n', '<leader>li', vim.lsp.buf.implementation, bufopts)
    vim.keymap.set('n', '<M-K>', vim.lsp.buf.signature_help, bufopts)
    vim.keymap.set('n', '<leader>lwa', vim.lsp.buf.add_workspace_folder, bufopts)
    vim.keymap.set('n', '<leader>lwr', vim.lsp.buf.remove_workspace_folder, bufopts)
    vim.keymap.set('n', '<leader>lwl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, bufopts)
    vim.keymap.set('n', '<leader>ld', vim.lsp.buf.type_definition, bufopts)
    vim.keymap.set('n', '<leader>lr', vim.lsp.buf.rename, bufopts)
    vim.keymap.set('n', '<leader>la', vim.lsp.buf.code_action, bufopts)
    vim.keymap.set('n', '<leader>lf', vim.lsp.buf.format, bufopts)
end


local lsp_flags = {
    -- This is the default in Nvim 0.7+
    debounce_text_changes = 150,
}
-- require('lspconfig')['pyls'].setup {
--     on_attach = on_attach,
--     flags = lsp_flags,
-- }
require'lspconfig'.pylsp.setup{
  settings = {
    pylsp = {
      plugins = {
        pycodestyle = {
          ignore = {'W391'},
          maxLineLength = 100
        }
      }
    }
  }
}

require'lspconfig'.tsserver.setup{}

require('lspconfig')['rust_analyzer'].setup {
    on_attach = on_attach,
    flags = lsp_flags,
    -- Server-specific settings...
    settings = {
        ["rust-analyzer"] = {}
    }
}
require 'lspconfig'.ccls.setup {
    on_attach = on_attach,
    flags = lsp_flags,

}

-- require 'lspconfig'.sumneko_lua.setup {
--     on_attach = on_attach,
--     flags = lsp_flags,
--     -- settings = {
--     --     Lua = {
--     --         runtime = {
--     --             -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
--     --             version = 'LuaJIT',
--     --         },
--             diagnostics = {
--     --             -- Get the language server to recognize the `vim` global
--                 globals = { 'vim' },
--             },
--     --         workspace = {
--     --             -- Make the server aware of Neovim runtime files
--     --             library = vim.api.nvim_get_runtime_file("", true),
--     --         },
--     --         -- Do not send telemetry data containing a randomized but unique identifier
--     --         telemetry = {
--     --             enable = false,
--     --         },
--     --     },
--     -- },
-- }
--

-- require 'lspconfig'.jdtls.setup {
--     on_attach = on_attach,
--     flags = lsp_flags,
-- }
--

-- require'lspconfig'.bashls.setup{}
-- -- Suggested for completion (I don't know what it does)
-- --Enable (broadcasting) snippet capability for completion
-- local capabilities = vim.lsp.protocol.make_client_capabilities()
-- capabilities.textDocument.completion.completionItem.snippetSupport = true

-- require 'lspconfig'.html.setup {
--     capabilities = capabilities,
--     on_attach = on_attach,
--     flags = lsp_flags,
-- }

-- Disable inline error messages
vim.diagnostic.config({virtual_text = false})
