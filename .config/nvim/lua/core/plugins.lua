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

vim.g.mapleader = " "         -- Make sure to set `mapleader` before lazy so your mappings are correct

require("lazy").setup({
        "nvim-lua/plenary.nvim", -- useful lua functions used by lots of plugins
        {
                'neovim/nvim-lspconfig',
                dependencies = {
                        -- Automatically install LSPs to stdpath for neovim
                        { 'williamboman/mason.nvim', config = true },
                        'williamboman/mason-lspconfig.nvim',

                        -- Useful status updates for LSP
                        {
                                'j-hui/fidget.nvim',
                                event = "LspAttach",
                                opts = {
                                }
                        },
                        -- Additional lua configuration
                        { 'folke/neodev.nvim',       opts = {} },
                },
        },
        "godlygeek/tabular",

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

                                            -- underline the bottom line of treesitter context
                                            vim.cmd [[
                        hi TreesitterContextBottom gui=underline
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
                        "hrsh7th/cmp-cmdline",

                        -- Adds LSP completion capabilities
                        'hrsh7th/cmp-nvim-lsp',
                        -- { "quangnguyen30192/cmp-nvim-ultisnips", after = { "nvim-cmp", "ultisnips" } },
                        "hrsh7th/cmp-buffer", -- buffer completions
                        "hrsh7th/cmp-path",   -- path completions
                        "onsails/lspkind.nvim"
                },
        },

        -- Snippets
        {
                'L3MON4D3/LuaSnip',
                dependencies = { "rafamadriz/friendly-snippets",
                        'saadparwaiz1/cmp_luasnip' },
        },
        {
                "windwp/nvim-autopairs",
                opts = {}
        },
        {
                'nvim-tree/nvim-web-devicons',
                lazy = true,
                opts = {
                        color_icons = true,
                        default = true,
                        strict = true,
                }
        },
        {
                -- adds git releated signs to the gutter, as well as utilities for managing changes
                'lewis6991/gitsigns.nvim',
                config = function()
                        -- see `:help gitsigns.txt`
                        require('gitsigns').setup({
                        })
                        vim.keymap.set('n', 'gp', require('gitsigns').prev_hunk)
                        vim.keymap.set('n', 'gn', require('gitsigns').next_hunk)
                        vim.keymap.set('n', '<M-g>', require('gitsigns').preview_hunk)
                end
        },
        { 'sindrets/diffview.nvim' },
        {
                'nvim-telescope/telescope.nvim',
                branch = '0.1.x',
                dependencies = {
                        'nvim-lua/plenary.nvim' },
                opts = {}
        },
        {
                'nvim-telescope/telescope-fzf-native.nvim',
                -- NOTE: If you are having trouble with this installation,
                --       refer to the README for telescope-fzf-native for more instructions.
                build = 'make',
                cond = function()
                        return vim.fn.executable 'make' == 1
                end,
        },
        {
                'tomasky/bookmarks.nvim',
                opts = {
                        save_file = vim.fn.expand "$HOME/.config/nvim/.bookmarks",
                        on_attach = function()
                                vim.keymap.set("n", "<leader>J", require "bookmarks".bookmark_toggle)
                        end
                }
        },
        -- colorscheme
        {
                "ellisonleao/gruvbox.nvim",
                priority = 1000,
                lazy = false,
        },
        -- lualine statusline
        {
                'nvim-lualine/lualine.nvim',
                event = "VeryLazy",
                config = function()
                        require('lualine').setup {
                                -- options = {
                                --     theme = "powerline_dark"
                                -- }
                        }
                end
        },
        "bsuth/emacs-bindings.nvim", -- emacs-like key bindings for insert and command modes
        {
                "numtostr/comment.nvim",
                event = "VeryLazy",
                opts = {
                        ignore = "^$" -- do not comment empty lines
                }
        },
        -- { "tpope/vim-commentary",  event = "VimEnter" },

        { "mbbill/undotree" },

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
        {
                "kylechui/nvim-surround",
                version = "*", -- for stability; omit to `main` branch for the latest features
                -- event = "VeryLazy",
                config = function()
                        require("nvim-surround").setup({
                                keymaps = {
                                        insert = "<C-g>s",
                                        insert_line = "<C-g>S",
                                        normal = "s",
                                        normal_cur = "yss",
                                        normal_line = "yS",
                                        normal_cur_line = "ySS",
                                        visual = "s",
                                        visual_line = "gS",
                                        delete = "ds",
                                        change = "cs",
                                }
                        })
                end
        },
        {
                "NvChad/nvim-colorizer.lua",
                opts = {}
        },
        {
                "phaazon/hop.nvim",
                config = function()
                        require 'hop'.setup { keys = 'etovxqpdygfblzhckisuran' }
                        local hop = require('hop')
                        vim.keymap.set('', '<A-s>', function()
                                hop.hint_words()
                        end, { remap = true })
                end
        },

        -- go
        {
                "ray-x/go.nvim",
                dependencies = { -- optional packages
                        "ray-x/guihua.lua",
                        "neovim/nvim-lspconfig",
                        "nvim-treesitter/nvim-treesitter",
                },
                config = function()
                        require("go").setup()
                end,
                event = { "CmdlineEnter" },
                ft = { "go", 'gomod' },
                build = ':lua require("go.install").update_all_sync()' -- if you need to install/update all binaries
        }

        -- debug
        -- "mfussenegger/nvim-dap",
        -- "rcarriga/cmp-dap",
        -- "rcarriga/nvim-dap-ui",
        -- "thehamsta/nvim-dap-virtual-text",
})
