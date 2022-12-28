local on_attach = require("user.lsp-default-bindings").default_on_attach

-- add completion capability
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

local lspconfig = require('lspconfig')

lspconfig.ccls.setup {
    on_attach = on_attach,
    flags = lsp_flags,
}
lspconfig.tsserver.setup {
    on_attach = on_attach,
    flags = lsp_flags,
}



lspconfig.pylsp.setup {
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

lspconfig.rust_analyzer.setup {
    on_attach = on_attach,
    flags = lsp_flags,
    -- Server-specific settings...
    settings = {
        ["rust-analyzer"] = {}
    }
}

-- lspconfig.sumneko_lua.setup {
--     on_attach = on_attach,
--     flags = lsp_flags,
--     diagnostics = {
--         globals = { 'vim' },
--     },
-- }
--
--
-- Disable inline error messages
-- vim.diagnostic.config({ virtual_text = false })
