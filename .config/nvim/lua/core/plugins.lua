local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath) -- Install your plugins here

-- Example using a list of specs with the default options
vim.g.mapleader = " " -- Make sure to set `mapleader` before lazy so your mappings are correct

require("lazy").setup({
    -- improve startup times
    "lewis6991/impatient.nvim",
    "nvim-lua/plenary.nvim", -- useful lua functions used by lots of plugins

    -- LSP
    {
        'neovim/nvim-lspconfig',
        dependencies = {
            -- Automatically install LSPs to stdpath for neovim
            { 'williamboman/mason.nvim', config = true },
            'williamboman/mason-lspconfig.nvim',

            -- Useful status updates for LSP
            {
                'j-hui/fidget.nvim',
                tag = 'legacy',
                opts = {
                    window = { blend = 0 }, -- transparency
                }
            },
            -- java lsp support
            -- "mfussenegger/nvim-jdtls")

            -- Additional lua configuration, makes nvim stuff amazing!
            { 'folke/neodev.nvim',       opts = {} },

            -- "onsails/lspkind.nvim",
        },
    },
    {
        -- Highlight, edit, and navigate code
        'nvim-treesitter/nvim-treesitter',
        dependencies = {
            'nvim-treesitter/nvim-treesitter-textobjects',
            "joosepalviste/nvim-ts-context-commentstring",
            {
                "romgrk/nvim-treesitter-context",
                config =
                    function()
                        vim.keymap.set("n", "[c", function()
                            require("treesitter-context").go_to_context()
                        end, { silent = true })

                        vim.cmd [[
                hi TreesitterContext guisp=Grey guibg=#333333
                " hi TreesitterContextBottom gui=underline
                ]]
                    end
            },
        },
        build = ':TSUpdate',
    },

    -- Autocompletion
    {
        'hrsh7th/nvim-cmp',
        dependencies = {
            -- Snippet Engine & its associated nvim-cmp source
            -- "hrsh7th/cmp-cmdline",

            -- Adds LSP completion capabilities
            'hrsh7th/cmp-nvim-lsp',

            "hrsh7th/cmp-buffer", -- buffer completions
            "hrsh7th/cmp-path",   -- path completions
        },
    },

    -- Snippets
    {
        'L3MON4D3/LuaSnip',
        dependencies = { "rafamadriz/friendly-snippets",
            'saadparwaiz1/cmp_luasnip' },
    },
    "windwp/nvim-autopairs",

    {
        "junegunn/fzf.vim",
        dependencies = {
            "junegunn/fzf",
            {
                "gfanto/fzf-lsp.nvim",
                opts = {}
            },
        }
    },

    -- lualine statusline
    {
        'nvim-lualine/lualine.nvim',
        opts = {
            -- options = {
            --     theme = "powerline_dark",
            -- disabled_filetypes = {
            --     --     'help', 'qf', 'man', 'lspinfo', 'prompt'
            --     -- },
            --     -- component_separators = '|',
            --     -- section_separators = '',
            -- },
        },
    },
    {
        "lmburns/lf.nvim",
        config = function()
            -- This feature will not work if the plugin is lazy-loaded
            vim.g.lf_netrw = 1

            require("lf").setup({
                default_cmd = "lf -config ~/.config/nvim/lfnvim/lfrc",
                default_actions = { -- default action keybindings
                    ["<C-t>"] = "tabedit",
                    ["<C-x>"] = "split",
                    ["<C-v>"] = "vsplit",
                    ["<C-o>"] = "tab drop",
                },
                winblend = 0,
                direction = "horizontal",
                escape_quit = true,
                default_file_manager = true,  -- make lf default file manager
                disable_netrw_warning = true, -- don't display a message when opening a directory with `default_file_manager` as true
                border = "rounded"
            })

            vim.keymap.set("n", "<M-o>", "<Cmd>Lf<CR>")

            -- vim.api.nvim_create_autocmd({
            --     event = "User",
            --     pattern = "LfTermEnter",
            --     callback = function(a)
            --         vim.api.nvim_buf_set_keymap(a.buf, "t", "q", "q", { nowait = true })
            --     end,
            -- })
        end,
        dependencies = { "akinsho/toggleterm.nvim" }
    },
    {
        "numtostr/comment.nvim",
        opts = {
            ignore = "^$" -- do not comment empty lines
        }
    },
    { "mbbill/undotree" },
    -- chaoren/vim-wordmotion", -- similar to subword-mode in Emacs
    {
        "NeogitOrg/neogit",
        dependencies = {
            "nvim-lua/plenary.nvim",  -- required
            -- "nvim-telescope/telescope.nvim", -- optional
            "sindrets/diffview.nvim", -- optional
            "ibhagwan/fzf-lua",       -- optional
        },
        config = true,
        opts = {
            vim.keymap.set("n", "<leader>g", "<cmd>Neogit<cr>")
        }
    },
    -- Asynchronous command execution
    -- { "skywind3000/asyncrun.vim", opt = true, cmd = { "AsyncRun" } },
    {
        "kylechui/nvim-surround",
        version = "*", -- for stability; omit to `main` branch for the latest features
        event = "VeryLazy",
        opts = {
            --     vim.keymap.set({ 'n', 'v' }, '<s-s>', 'ys', { remap = true }),
        }
    },
    {
        "NvChad/nvim-colorizer.lua",
        opts = {}
    },
    -- { 'jose-elias-alvarez/null-ls.nvim' }, -- Neovim as a language server to inject LSP diagnostics, code actions, and more via Lua
    -- {
    --     'glacambre/firenvim',

    --     -- Lazy load firenvim
    --     -- Explanation: https://github.com/folke/lazy.nvim/discussions/463#discussioncomment-4819297
    --     cond = not not vim.g.started_by_firenvim,
    --     build = function()
    --         require("lazy").load({ plugins = "firenvim", wait = true })
    --         -- vim.fn["firenvim#install"](0)
    --     end
    -- },

    -- showing keybindings
    -- {
    --     "folke/which-key.nvim",
    --     event = "VimEnter",
    -- },

    -- {
    --     'nvim-tree/nvim-web-devicons',
    --     lazy = true,
    --     opts = {
    --         -- globally enable different highlight colors per icon (default to true)
    --         -- if set to false all icons will have the default icon's color
    --         color_icons = true,
    --         -- globally enable default icons (default to false)
    --         -- will get overriden by `get_icons` option
    --         default = true,
    --         -- globally enable "strict" selection of icons - icon will be looked up in
    --         -- different tables, first by filename, and if not found by extension; this
    --         -- prevents cases when file doesn't have any extension but still gets some icon
    --         -- becaits name happened to match some extension (default to false)
    --         strict = true,
    --     }
    -- },
    -- {
    --     -- adds git releated signs to the gutter, as well as utilities for managing changes
    --     'lewis6991/gitsigns.nvim',
    --     opts = {
    --         -- see `:help gitsigns.txt`
    --         signs = {
    --             add = { text = '+' },
    --             change = { text = '~' },
    --             delete = { text = '_' },
    --             topdelete = { text = '‾' },
    --             changedelete = { text = '~' },
    --         },
    --         on_attach = function(bufnr)
    --             vim.keymap.set('n', '<leader>gp', require('gitsigns').prev_hunk)
    --             vim.keymap.set('n', '<leader>gn', require('gitsigns').next_hunk)
    --             vim.keymap.set('n', '<leader>ph', require('gitsigns').preview_hunk)
    --         end,
    --     },
    -- },
    -- { 'sindrets/diffview.nvim' },
    -- {
    --     "Vonr/align.nvim",
    --     opts = {
    --         local NS = { noremap = true, silent = true }

    --         vim.keymap.set('x', 'aa', function() require 'align'.align_to_char(1, true) end, NS)             -- Aligns to 1 character, looking left
    --         vim.keymap.set('x', 'as', function() require 'align'.align_to_char(2, true, true) end, NS)       -- Aligns to 2 characters, looking left and with previews
    --         vim.keymap.set('x', 'aw', function() require 'align'.align_to_string(false, true, true) end, NS) -- Aligns to a string, looking left and with previews
    --         vim.keymap.set('x', 'ar', function() require 'align'.align_to_string(true, true, true) end, NS)  -- Aligns to a Lua pattern, looking left and with previews

    --         -- Example gawip to align a paragraph to a string, looking left and with previews
    --         vim.keymap.set(
    --             'n',
    --             'gaw',
    --             function()
    --                 local a = require 'align'
    --                 a.operator(
    --                     a.align_to_string,
    --                     { is_pattern = false, reverse = true, preview = true }
    --                 )
    --             end,
    --             NS
    --         )

    --         -- Example gaaip to aling a paragraph to 1 character, looking left
    --         vim.keymap.set(
    --             'n',
    --             'gaa',
    --             function()
    --                 local a = require 'align'
    --                 a.operator(
    --                     a.align_to_char,
    --                     { length = 1, reverse = true }
    --                 )
    --             end,
    --             NS
    --         )
    -- }
    -- },
    -- {
    --     "lukas-reineke/indent-blankline.nvim",
    --     config = function()
    --         vim.opt.list = true
    --         -- vim.opt.listchars:append "space:⋅"
    --         vim.opt.listchars:append "eol:↴"
    --         --     vim.cmd [[
    --         --     highlight IndentBlanklineChar guifg=#444444 gui=nocombine
    --         --             highlight IndentBlanklineSpaceChar guifg=#444444 gui=nocombine
    --         -- "     highlight IndentBlanklineContextChar guifg=#ff0000 gui=nocombine
    --         -- " highlight IndentBlanklineContextSpaceChar guifg=#ff0000 gui=nocombine
    --         -- " highlight IndentBlanklineContextStart guisp=#ff0000 gui=underline
    --         --     ]]
    --         -- vim.cmd.highlight("default link IndentBlanklineChar Comment")
    --         -- vim.cmd.highlight("default link IndentBlanklineChar Comment")

    --         require("indent_blankline").setup {
    --             space_char_blankline = " ",
    --             show_current_context = true,
    --             show_current_context_start = true,
    --         }
    --     end
    -- },
    -- Color schemes
    -- {
    --     "ellisonleao/gruvbox.nvim",
    --     priority = 1000,
    --     lazy = false,
    --     -- opts = {
    --     --     -- transparent_mode = true,
    --     --     -- contrast = "hard" -- can be "hard", "soft" or empty string
    --     -- }
    -- },
    -- {
    --     "phaazon/hop.nvim",
    --     config = function()
    --         require 'hop'.setup { keys = 'etovxqpdygfblzhckisuran' }
    --         local hop = require('hop')
    --         local directions = require('hop.hint').HintDirection
    --         vim.keymap.set('', 'f', function()
    --             hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true })
    --         end, { remap = true })
    --         vim.keymap.set('', 'F', function()
    --             hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true })
    --         end, { remap = true })
    --         vim.keymap.set('', 't', function()
    --             hop.hint_char1({
    --                 direction = directions.AFTER_CURSOR,
    --                 current_line_only = true,
    --                 hint_offset = -1
    --             })
    --         end, { remap = true })
    --         vim.keymap.set('', 'T', function()
    --             hop.hint_char1({
    --                 direction = directions.BEFORE_CURSOR,
    --                 current_line_only = true,
    --                 hint_offset = 1
    --             })
    --         end, { remap = true })
    --         vim.keymap.set('', '<A-s>', function()
    --             hop.hint_words()
    --         end, { remap = true })
    --     end
    -- },
    -- {
    --     'nvim-telescope/telescope.nvim',
    --     branch = '0.1.x',
    --     dependencies = {
    --         'nvim-lua/plenary.nvim' },
    -- },
    -- {
    --     'nvim-telescope/telescope-fzf-native.nvim',
    --     -- NOTE: If you are having trouble with this installation,
    --     --       refer to the README for telescope-fzf-native for more instructions.
    --     build = 'make',
    --     cond = function()
    --         return vim.fn.executable 'make' == 1
    --     end,
    -- },
    -- {
    --     'tomasky/bookmarks.nvim',
    --     opts = {
    --         save_file = vim.fn.expand "$HOME/.config/nvim/.bookmarks",
    --         on_attach = function()
    --             vim.keymap.set("n", "<leader>J", require "bookmarks".bookmark_toggle)
    --         end
    --     }
    -- },

    -- debug
    -- "mfussenegger/nvim-dap",
    -- "rcarriga/cmp-dap",
    -- "rcarriga/nvim-dap-ui",
    -- "thehamsta/nvim-dap-virtual-text",
})
