-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`
require('nvim-treesitter.configs').setup {
    -- Add languages to be installed here that you want installed for treesitter
    ensure_installed = {
        "comment",
        "query",
        "javascript",
        "python",
        "java",
        "go",
        "typescript",
        "cpp",
        "c",
        "lua",
        "html",
        "css",
        "markdown",
        "markdown_inline",
        "tsx",
        "typescript",
        "toml",
        "php",
        "json",
        "yaml", },

    -- Autoinstall languages that are not installed. Defaults to false (but you can change for yourself!)
    -- auto_install = true,

    highlight = {
        -- `false` will disable the whole extension
        enable = true,
        use_languagetree = true,

        -- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
        -- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
        -- the name of the parser)
        -- list of language that will be disabled
        -- disable = { "c", "rust" },
        -- Or use a function for more flexibility, e.g. to disable slow treesitter highlight for large files
        disable = function(lang, buf)
            local max_filesize = 100 * 1024 -- 100 KB
            local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
            if ok and stats and stats.size > max_filesize then
                return true
            end
        end,
        -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
        -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
        -- Using this option may slow down your editor, and you may see some duplicate highlights.
        -- Instead of true it can also be a list of languages
        additional_vim_regex_highlighting = false,
    },
    indent = { enable = false },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = '<M-CR>',
            node_incremental = '<M-CR>',
            -- scope_incremental = '<M-S-CR>',
            node_decremental = '<S-CR>'
        },
    },
    textobjects = {
        select = {
            enable = true,
            lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
            keymaps = {
                -- You can use the capture groups defined in textobjects.scm
                ['aa'] = '@parameter.outer',
                ['ia'] = '@parameter.inner',
                ['af'] = '@function.outer',
                ['if'] = '@function.inner',
                ['ac'] = '@class.outer',
                ['ic'] = '@class.inner',
            },
        },
        move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
                [']m'] = '@function.outer',
                [']]'] = '@class.outer',
            },
            goto_next_end = {
                [']M'] = '@function.outer',
                [']['] = '@class.outer',
            },
            goto_previous_start = {
                ['[m'] = '@function.outer',
                ['[['] = '@class.outer',
            },
            goto_previous_end = {
                ['[M'] = '@function.outer',
                ['[]'] = '@class.outer',
            },
        },
        swap = {
            enable = true,
            swap_next = {
                ['<leader>a'] = '@parameter.inner',
            },
            swap_previous = {
                ['<leader>A'] = '@parameter.inner',
            },
        },
    },
}

-- Folding Module
vim.opt.foldmethod = 'expr'
vim.cmd([[
set foldexpr=nvim_treesitter#foldexpr()
set nofoldenable                     " Disable folding at startup.
]])


-- require("treesitter-context").setup { enable = true }

-- vim.keymap.set("n", "[c", function()
--     require("treesitter-context").go_to_context()
-- end, { silent = true })

-- vim.cmd [[ hi TreesitterContext guisp=Grey guibg=#333333
--                 "hi TreesitterContextBottom gui=underline
--                 ]]
-- vim.treesitter.query.set("lua", "context", "")
