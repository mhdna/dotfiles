local cmp = require('cmp')
local lspkind = require('lspkind')

cmp.setup {
        enabled = function()
                -- disable auto completion in comments
                buftype = vim.api.nvim_buf_get_option(0, "buftype")
                if buftype == "prompt" then return false end
                local context = require 'cmp.config.context'
                -- keep command mode completion enabled when cursor is in a comment
                if vim.api.nvim_get_mode().mode == 'c' then
                        return true
                else
                        return not context.in_treesitter_capture("comment")
                            and not context.in_syntax_group("Comment")
                end
        end,
        -- disable preselect (makes cmp behave weirdly with some languages like Go and Rust)
        preselect = { cmp.PreselectMode.None },
        formatting = {
                format = lspkind.cmp_format({
                        mode = 'symbol', -- show only symbol annotations
                        maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
                        ellipsis_char = '...', -- when popup menu exceed maxwidth, the truncated part would show ellipsis_char instead (must define maxwidth first)

                }),
        },
        completion = {
                completeopt = "menu,menuone",
        },
        mapping = {
                ["<CR>"] = cmp.mapping({
                        i = function(fallback)
                                if cmp.visible() and cmp.get_active_entry() then
                                        cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
                                else
                                        fallback()
                                end
                        end,
                        s = cmp.mapping.confirm({ select = true }),
                        c = function(fallback)
                                fallback()
                        end

                }),
                ["<C-n>"] = cmp.mapping(function()
                        if cmp.visible() then
                                cmp.select_next_item()
                        else
                                cmp.complete()
                        end
                end, { 'i' }),
                ["<C-p>"] = cmp.mapping(function()
                        if cmp.visible() then
                                cmp.select_prev_item()
                        else
                                cmp.complete()
                        end
                end, { 'i' }),

                ["<C-b>"] = cmp.mapping.scroll_docs(-4),
                ["<C-f>"] = cmp.mapping.scroll_docs(4),
                ['<C-e>'] = cmp.mapping(cmp.mapping.abort(), { "i", "c", "s" }),
                ["<c-y>"] = cmp.mapping(
                        cmp.mapping.confirm {
                                behavior = cmp.ConfirmBehavior.Insert,
                                select = true,
                        },
                        { "i", "c" }
                ),
                ["<M-y>"] = cmp.mapping(
                        cmp.mapping.confirm {
                                behavior = cmp.ConfirmBehavior.Replace,
                                select = false,
                        },
                        { "i", "c" }
                ),
                ["<Tab>"] = cmp.mapping(function(fallback)
                        -- This little snippet will confirm with tab, and if no entry is selected, will confirm the first item
                        if cmp.visible() then
                                local entry = cmp.get_selected_entry()
                                if not entry then
                                        cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
                                else
                                        cmp.confirm()
                                end
                        else
                                fallback()
                        end
                end, { "i", "s", "c", }),


                ['<M-CR>'] = cmp.mapping(cmp.mapping.confirm({ select = true }), { 'c', 'i' }),
                ["<M-n>"] = cmp.mapping(function()
                        if cmp.visible() then
                                cmp.select_next_item()
                        else
                                cmp.complete()
                        end
                end, { 'c' }),
                ["<M-p>"] = cmp.mapping(function()
                        if cmp.visible() then
                                cmp.select_prev_item()
                        else
                                cmp.complete()
                        end
                end, { 'c' }),
        },
        snippet = {
                expand = function(args)
                        require 'luasnip'.lsp_expand(args.body)
                        -- vim.fn["UltiSnips#Anon"](args.body)
                end
        },


        sources = {
                { name = 'path' },
                { name = 'nvim_lsp', keyword_length = 1 },
                { name = 'buffer',   keyword_length = 3 },
                { name = 'nvim_lua', keyword_length = 1 },
                { name = 'luasnip' }, -- For luasnip users.
                -- { name = 'ultisnips' }, -- For ultisnips users.
        },
        confirm_opts = {
                behavior = cmp.ConfirmBehavior.Replace,
                select = false,
        },
}

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
        -- mapping = cmp.mapping.preset.cmdline(),
        sources = {
                { name = 'buffer' }
        },
})

-- `:` cmdline setup.
cmp.setup.cmdline(':', {
        -- mapping = cmp.mapping.preset.cmdline(),
        enabled = true,
        sources = cmp.config.sources({
                        { name = 'path' },
                        -- { name = 'buffer' }
                },
                {
                        {
                                name = 'cmdline',
                                option = {
                                        ignore_cmds = { 'Man', '!' }
                                },
                        },
                }),
        -- view = {
        --     entries = { name = 'wildmenu' }
        -- },

})

vim.api.nvim_create_autocmd("CmdWinEnter", {
        callback = function()
                -- Do not stick cmp completions when I open the command window
                require("cmp").close()
                -- map q to close
                vim.keymap.set("n", "q", ":q!<CR>", { noremap = true, buffer = 0 })
        end,
})
