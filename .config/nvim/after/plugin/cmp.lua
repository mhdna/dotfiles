local cmp = require 'cmp'

-- local select_opts = { behavior = cmp.SelectBehavior.Select }

cmp.setup {
    enabled = function()
        -- disable completion in comments
        --
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
    -- enabled = true,
    completion = {
        completeopt = "menu,menuone",
        -- autocomplete = false
        --     --     --     keyword_length= 2,
    },
    mapping = {
        ['<CR>'] = cmp.mapping.confirm({ select = false }),
        ["<C-n>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert },
        ["<C-p>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert },

        ["<C-d>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-i>"] = cmp.mapping.abort(),
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

        -- ["<c-s>"] = cmp.mapping {
        --     i = cmp.mapping.complete(),
        --     c = function(
        --         _ --[[fallback]]
        --     )
        --         if cmp.visible() then
        --             if not cmp.confirm { select = true } then
        --                 return
        --             end
        --         else
        --             cmp.complete()
        --         end
        --     end,
        -- },
        -- ["<Tab>"] = function(fallback)
        --     if cmp.visible() then
        --         cmp.select_next_item()
        --     else
        --         fallback()
        --     end
        -- end,
        -- ["<S-Tab>"] = function(fallback)
        --     if cmp.visible() then
        --         cmp.select_prev_item()
        --     else
        --         fallback()
        --     end
        -- end
    },
    snippet = {
        expand = function(args)
            require 'luasnip'.lsp_expand(args.body)
        end
    },


    sources = {
        { name = 'path' },
        { name = 'nvim_lsp', keyword_length = 1 },
        { name = 'buffer',   keyword_length = 3 },
        { name = 'nvim_lua', keyword_length = 1 },
        { name = 'luasnip' }, -- For luasnip users.
    },
    confirm_opts = {
        behavior = cmp.ConfirmBehavior.Replace,
        select = false,
    },
}

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
-- cmp.setup.cmdline('/', {
--     mapping = cmp.mapping.preset.cmdline(),
--     sources = {
--         { name = 'buffer' }
--     }
-- })

-- -- `:` cmdline setup.
-- cmp.setup.cmdline(':', {
--     mapping = cmp.mapping.preset.cmdline(),
--     sources = cmp.config.sources({
--         { name = 'path' }
--     }, {
--         {
--             name = 'cmdline',
--             option = {
--                 ignore_cmds = { 'Man', '!' }
--             },
--             keyword_length = 2
--         }
--     })
-- })
