local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local types = require("luasnip.util.types")

require("luasnip/loaders/from_vscode").lazy_load()

ls.config.set_config {
    history = true,
    -- region_check_events = "InsertEnter", -- Do not go back to old snippets
    updateevents = "TextChanged, TextChangedI",
    enable_autosnippets = true,
    ext_opts = {
        [types.choiceNode] = {
            active = {
                virt_text = { { "<--", "Error" } },
            },
        },
    },
}

ls.add_snippets("all", {
    s("ternary", {
        -- equivalent to "${1:cond} ? ${2:then} : ${3:else}"
        i(1, "cond"), t(" ? "), i(2, "then"), t(" : "), i(3, "else")
    })
})

ls.add_snippets("lua", {
    s("t", {
        -- equivalent to "${1:cond} ? ${2:then} : ${3:else}"
        i(1, "cond"), t(" ? "), i(2, "then"), t(" : "), i(3, "else")
    }),
    s("t", {
        -- equivalent to "${1:cond} ? ${2:then} : ${3:else}"
        i(1, "cod"), t(" ? "), i(2, "then"), t(" : "), i(3, "else")
    })
})

ls.add_snippets("python", {
    s("p", {
        -- equivalent to "${1:cond} ? ${2:then} : ${3:else}"
        t("print(\""), i(1, ""), t("\")")
    })
})

ls.add_snippets("java", {
    s("sout", {
        -- equivalent to "${1:cond} ? ${2:then} : ${3:else}"
        t("System.out.println(\""), i(1, ""), t("\");")
    })
})
-- expansion key
-- this will expand the current item or jump to the next item within the snippet.
vim.keymap.set({ "i", "s" }, "<c-k>", function()
    if ls.expand_or_jumpable() then
        ls.expand_or_jump()
    else
        return
    end
end, { silent = true })

-- jump backwards
-- this always moves to the previous item within the snippet
vim.keymap.set({ "i", "s" }, "<c-j>", function()
    if ls.jumpable(-1) then
        ls.jump(-1)
    end
end, { silent = true })

-- <c-l> is selecting within a list of options.
-- This is useful for choice nodes (introduced in the forthcoming episode 2)
vim.keymap.set("i", "<c-l>", function()
    if ls.choice_active() then
        ls.change_choice(1)
    end
end)

-- vim.keymap.set("i", "<c-u>", require "luasnip.extras.select_choice")

-- shorcut to source my luasnips file again, which will reload my snippets
vim.keymap.set("n", "<leader><leader>s", "<cmd>source ~/.config/nvim/after/plugin/luasnip.lua<CR>")

-- a little fix for luasnip exiting insert mode when pressing backspace
-- https://github.com/L3MON4D3/LuaSnip/issues/622
vim.keymap.set('s', '<BS>', '<C-O>s')

-- fix luasnip (https://github.com/L3MON4D3/LuaSnip/issues/258#issuecomment-1011938524)
vim.api.nvim_create_autocmd('ModeChanged', {
    pattern = '*',
    callback = function()
        if ((vim.v.event.old_mode == 's' and vim.v.event.new_mode == 'n') or vim.v.event.old_mode == 'i')
            and require('luasnip').session.current_nodes[vim.api.nvim_get_current_buf()]
            and not require('luasnip').session.jump_active
        then
            require('luasnip').unlink_current()
        end
    end
})
