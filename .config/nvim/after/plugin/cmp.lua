local cmp = require 'cmp'

local lspkind = require('lspkind')

local select_opts = { behavior = cmp.SelectBehavior.Select }

vim.opt.completeopt = { 'menu', 'menuone', 'noselect' }

-- Autcompletion enable only for certain filetypes
cmp.setup.filetype(
    { 'c', 'cpp', 'java', 'python', 'javascript', 'go', 'sql', 'mysql', 'html', 'css', 'lua', 'sh', 'rust' }, {
        enabled = function()
            -- disable completion in comments
            local context = require 'cmp.config.context'
            -- keep command mode completion enabled when cursor is in a comment
            if vim.api.nvim_get_mode().mode == 'c' then
                return true
            else
                return not context.in_treesitter_capture("comment")
                    and not context.in_syntax_group("Comment")
            end
        end,
    })

cmp.setup.filetype({ 'markdown' }, {
    enabled = true,
    -- completion = {
    --     autocomplete = false,
    -- }
})

cmp.setup.filetype("tex", {
    enabled = true,
})


cmp.setup {
    enabled = false,
    completion = {
        autocomplete = false,
        --     keyword_length= 2,
    },
    mapping = {
        ['<CR>'] = cmp.mapping.confirm({ select = false }),
        ["<C-n>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert },
        ["<C-p>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert },
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-e>"] = cmp.mapping.abort(),
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
        ["<Tab>"] = function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            else
                fallback()
            end
        end,
        ["<S-Tab>"] = function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            else
                fallback()
            end
        end
    },

    sources = {
        { name = 'path' },
        { name = 'nvim_lsp', keyword_length = 1 },
        { name = 'buffer',   keyword_length = 3 },
        { name = 'luasnip' }, -- For luasnip users.
        -- { name = 'vsnip' }, -- For vsnip users.
        -- { name = 'ultisnips' }, -- For ultisnips users.
        -- { name = 'snippy' }, -- For snippy users.
    },
    confirm_opts = {
        behavior = cmp.ConfirmBehavior.Replace,
        select = false,
    },
    formatting = {
        format = lspkind.cmp_format({
            with_text = true,
            mode = 'symbol', -- show only symbol annotations
            -- maxwidth = 200,        -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
            -- ellipsis_char = '...', -- when popup menu exceed maxwidth, the truncated part would show ellipsis_char instead (must define maxwidth first)
            -- end
            menu = {
                nvim_lsp = "[LSP]",
                ultisnips = "[US]",
                nvim_lua = "[Lua]",
                path = "[Path]",
                buffer = "[Buffer]",
            },
        })
    },
}

-- Use cmdline & path source for ':'.
cmp.setup.cmdline(':', {
    completion = { autocomplete = false },
    sources = cmp.config.sources({
        { name = 'path' }
    }, {
        { name = 'cmdline' }
    })
})
-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
        { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
    }, {
        { name = 'buffer' },
    })
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
        { name = 'buffer' }
    }
})
