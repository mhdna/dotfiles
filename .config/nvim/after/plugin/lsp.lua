local on_attach = function(_, bufnr)
        vim.opt_local.signcolumn = 'yes' -- enable signcolumn for lsp only

        local nmap = function(keys, func)
                vim.keymap.set('n', keys, func, { silent = true, buffer = bufnr, noremap = true })
        end

        nmap('<leader>rn', vim.lsp.buf.rename)
        nmap('<leader>i', ":OrganizeImports<CR>:w<CR>")
        nmap('<leader>a', vim.lsp.buf.code_action)

        nmap('gd', vim.lsp.buf.definition)
        nmap('<leader>D', vim.lsp.buf.type_definition)

        nmap('gr', require('telescope.builtin').lsp_references)
        nmap('<leader>s', require('telescope.builtin').lsp_document_symbols) -- I think of it as an imenu alternative
        nmap('<leader>S', require('telescope.builtin').lsp_dynamic_workspace_symbols)

        -- See `:help K` for why this keymap
        nmap('<C-]>', vim.lsp.buf.signature_help)
        nmap('K', vim.lsp.buf.hover)

        nmap('<C-Insert>', function()
                print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end)
        nmap('<M-Insert>', function()
                vim.lsp.buf.add_workspace_folder()
                print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end)
        nmap('<M-S-Insert>', function()
                vim.lsp.buf.remove_workspace_folder()
                print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end)
end

-- Enable the following language servers
local servers = {
        clangd = {
                flags = {
                        debounce_text_changes = 500,
                },
        },
        gopls = {},
        -- solidity = {},
        solidity_ls_nomicfoundation = {},
        pylsp = {
                settings = {
                        pylsp = {
                                plugins = {
                                        pylint = { enabled = true, executable = "pylint" },
                                        pyflakes = { enabled = false },
                                        pycodestyle = { enabled = false },
                                        jedi_completion = { fuzzy = true },
                                        pyls_isort = { enabled = true },
                                        pylsp_mypy = { enabled = true },
                                },
                        },
                },
                flags = {
                        debounce_text_changes = 200,
                },
        },
        -- rust_analyzer = {},
        lua_ls = {
                Lua = {
                        workspace = { checkThirdParty = false },
                        telemetry = { enable = false },
                },
        },
        dockerls = {},
        emmet_ls = {
                filetypes = { "css", "eruby", "html", "javascript", "javascriptreact", "less", "sass", "scss", "svelte",
                        "pug",
                        "typescriptreact", "vue" },
                init_options = {
                        html = {
                                options = {
                                        -- For possible options, see: https://github.com/emmetio/emmet/blob/master/src/config.ts#L79-L267
                                        ["bem.enabled"] = true,
                                },
                        },
                }
        },
        tsserver = {},
        html = {},
        cssls = {},
}

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
capabilities.textDocument.completion.completionItem.snippetSupport = true

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
    ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
    function(server_name)
        require('lspconfig')[server_name].setup {
            capabilities = capabilities,
            on_attach = on_attach,
            settings = servers[server_name],
        }
    end,
}

-- vim.diagnostic.config({
--     virtual_text = true,
--     signs = true,
--     underline = true,
--     update_in_insert = false,
--     severity_sort = false,
-- })


-- local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }

-- for type, icon in pairs(signs) do
--     local hl = "DiagnosticSign" .. type
--     vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
-- end
