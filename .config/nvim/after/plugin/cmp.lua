local cmp_status_ok, cmp = pcall(require, "cmp")
if not cmp_status_ok then
    return
end

local snip_status_ok, luasnip = pcall(require, "luasnip")
if not snip_status_ok then
    return
end

require("luasnip/loaders/from_vscode").lazy_load()

local check_backspace = function()
    local col = vim.fn.col "." - 1
    return col == 0 or vim.fn.getline("."):sub(col, col):match "%s"
end

-- fix luasnip (https://github.com/L3MON4D3/LuaSnip/issues/258#issuecomment-1011938524)
function leave_snippet()
    if
        ((vim.v.event.old_mode == 's' and vim.v.event.new_mode == 'n') or vim.v.event.old_mode == 'i')
        and require('luasnip').session.current_nodes[vim.api.nvim_get_current_buf()]
        and not require('luasnip').session.jump_active
    then
        require('luasnip').unlink_current()
    end
end

-- tex settings
vim.api.nvim_create_autocmd("ModeChanged", {
    pattern =  "*" ,
    callback = function ()
        leave_snippet()
    end,
})


-- Only Enable for certain filetypes
cmp.setup.filetype({ 'c', 'cpp', 'java', 'python', 'javascript', 'go', 'sql', 'mysql', 'html', 'css' , 'tex'}, {
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
    completion = {
        autocomplete = false,
    }
})

cmp.setup {
    enabled = false,
    -- completion = {
    --     autocomplete = false,
    --     keyword_length= 2,
    -- },
    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body)     -- For `luasnip` users.
        end,
    },
    mapping = {
        ["<C-p>"] = cmp.mapping.select_prev_item(),
        ["<C-n>"] = cmp.mapping.select_next_item(),
        ["<C-b>"] = cmp.mapping(cmp.mapping.scroll_docs(-1), { "i", "c" }),
        ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(1), { "i", "c" }),
        -- ["<Tab>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
        ["<C-y>"] = cmp.config.disable,     -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
        ["<C-e>"] = cmp.mapping {
            i = cmp.mapping.abort(),
            c = cmp.mapping.close(),
        },
        -- Accept currently selected item. If none selected, `select` first item.
        -- Set `select` to `false` to only confirm explicitly selected items.
        ['<CR>'] = cmp.mapping.confirm({ select = false }),     -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        ["<Tab>"] = cmp.mapping(function(fallback)
            if luasnip.expandable() then
                luasnip.expand()
            elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            elseif cmp.visible() then
                cmp.select_next_item()
            elseif check_backspace() then
                cmp.mapping.complete()
                fallback()
            else
                fallback()
            end
        end, {
            "i",
            "s",
        }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end, {
            "i",
            "s",
        }),
    },
    formatting = {
        format = function(entry, vim_item)
            vim_item.abbr = string.sub(vim_item.abbr, 1, 30)
            return vim_item
        end
    },
    sources = {
        { name = 'luasnip' },
        { name = "nvim_lsp" },
        { name = "path" },
        { name = "buffer" },
    },
    confirm_opts = {
        behavior = cmp.ConfirmBehavior.Replace,
        select = false,
    },
    experimental = {
        ghost_text = false,
        native_menu = false,
    }
}
