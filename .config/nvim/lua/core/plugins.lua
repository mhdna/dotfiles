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
    {
        -- LSP Configuration & Plugins
        'neovim/nvim-lspconfig',
        dependencies = {
            -- Automatically install LSPs to stdpath for neovim
            { 'williamboman/mason.nvim', config = true },
            'williamboman/mason-lspconfig.nvim',

            -- Useful status updates for LSP
            -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
            -- { 'j-hui/fidget.nvim',       tag = 'legacy', opts = {} },

            -- Additional lua configuration, makes nvim stuff amazing!
            'folke/neodev.nvim',

            "onsails/lspkind.nvim",
        },
    },
    {
        'L3MON4D3/LuaSnip',
        dependencies = { "rafamadriz/friendly-snippets",
            'saadparwaiz1/cmp_luasnip' },
    },

    {
        -- Autocompletion
        'hrsh7th/nvim-cmp',
        dependencies = {
            -- Snippet Engine & its associated nvim-cmp source
            -- { "hrsh7th/cmp-omni",                    after = "nvim-cmp" },
            -- { "quangnguyen30192/cmp-nvim-ultisnips", after = { "nvim-cmp", "ultisnips" } },
            -- { "honza/vim-snippets",                  after = "ultisnips" },
            -- "hrsh7th/vim-vsnip",
            -- "hrsh7th/vim-vsnip-integ",
            -- "hrsh7th/cmp-vsnip",

            -- Adds LSP completion capabilities
            'hrsh7th/cmp-nvim-lsp',

            "hrsh7th/cmp-buffer", -- buffer completions
            "hrsh7th/cmp-path",   -- path completions
        },
    },

    -- showing keybindings
    -- {
    --     "folke/which-key.nvim",
    --     event = "VimEnter",
    -- },

    -- Snippet engine and snippet template
    -- {
    --     "SirVer/ultisnips",
    --     event = "InsertEnter",
    --     config = function()
    --         vim.keymap.set("n", "<leader><leader>s", ":call UltiSnips#RefreshSnippets()<CR>",
    --             { buffer = bufnr, desc = 'Refresh [s]nippets', noremap = true })
    --     end
    -- },


    {
        'nvim-tree/nvim-web-devicons',
        lazy = true,
        opts = {
            -- globally enable different highlight colors per icon (default to true)
            -- if set to false all icons will have the default icon's color
            color_icons = true,
            -- globally enable default icons (default to false)
            -- will get overriden by `get_icons` option
            default = true,
            -- globally enable "strict" selection of icons - icon will be looked up in
            -- different tables, first by filename, and if not found by extension; this
            -- prevents cases when file doesn't have any extension but still gets some icon
            -- becaits name happened to match some extension (default to false)
            strict = true,
        }
    },
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
    --             vim.keymap.set('n', '<leader>gp', require('gitsigns').prev_hunk,
    --                 { buffer = bufnr, desc = '[g]o to [p]revious hunk' })
    --             vim.keymap.set('n', '<leader>gn', require('gitsigns').next_hunk,
    --                 { buffer = bufnr, desc = '[g]o to [n]ext hunk' })
    --             vim.keymap.set('n', '<leader>ph', require('gitsigns').preview_hunk,
    --                 { buffer = bufnr, desc = '[p]review [h]unk' })
    --         end,
    --     },
    -- },
    -- { 'sindrets/diffview.nvim' },
    -- {
    --     "Vonr/align.nvim",
    --     config = function()
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
    --     end
    -- },
    -- { "lukas-reineke/indent-blankline.nvim" },
    -- { "ellisonleao/gruvbox.nvim",      priority = 1000 },
    {
        'AckslD/muren.nvim',
        config = true,
    },
    {
        -- Set lualine as statusline
        'nvim-lualine/lualine.nvim',
        opts = {
            options = {
                theme = "16color",
                -- disabled_filetypes = {
                --     'help', 'qf', 'man', 'lspinfo', 'prompt'
                -- },
                -- component_separators = '|',
                -- section_separators = '',
            },
        },
    },
    -- { "ojroques/nvim-hardline", opts = {}, end },
    -- "nvim-tree/nvim-tree.lua")
    "nvim-lua/plenary.nvim", -- useful lua functions used by lots of plugins
    -- "windwp/nvim-autopairs", -- similar to rainbow parameters
    -- lsp
    -- "mfussenegger/nvim-jdtls")
    {
        -- Highlight, edit, and navigate code
        'nvim-treesitter/nvim-treesitter',
        dependencies = {
            'nvim-treesitter/nvim-treesitter-textobjects',
        },
        build = ':TSUpdate',
    },
    "joosepalviste/nvim-ts-context-commentstring",
    {
        "romgrk/nvim-treesitter-context",
        config =
            function()
                vim.keymap.set("n", "[c", function()
                    require("treesitter-context").go_to_context()
                end, { silent = true })
                vim.cmd('hi TreesitterContext guisp=Grey guibg=#333333')
                vim.cmd('hi TreesitterContextBottom gui=underline')
            end
    },
    {
        "numtostr/comment.nvim",
        opts = {
            ignore = "^$"
        }
    },

    -- Automatic insertion and deletion of a pair of characters
    { "Raimondi/delimitMate", event = "InsertEnter" },
    -- Comment plugin
    -- { "tpope/vim-commentary",               event = "VimEnter" },


    { "mbbill/undotree" },
    { "tpope/vim-fugitive" },
    -- { "rrethy/vim-illuminate" },
    -- alternative to subword-mode in emacs
    -- "haoren/vim-wordmotion",
    -- {
    --     "timuntersberger/neogit",
    --     dependencies = "nvim-lua/plenary.nvim",
    -- opts = {}
    -- },
    {
        'nvim-telescope/telescope.nvim',
        branch = '0.1.x',
        dependencies = {
            'nvim-lua/plenary.nvim' },
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


    -- Asynchronous command execution
    { "skywind3000/asyncrun.vim", opt = true, cmd = { "AsyncRun" } },

    -- { "Yggdroot/LeaderF",       cmd = "Leaderf" },
    -- { 'amirrezaask/fuzzy.nvim', requires = { 'nvim-lua/plenary.nvim' } },
    -- { "junegunn/fzf" },
    {
        "kylechui/nvim-surround",
        version = "*", -- for stability; omit to `main` branch for the latest features
        event = "VeryLazy",
        opts = {
            vim.keymap.set({ 'n', 'v' }, '<s-s>', 'ys', { remap = true }),
        }
    },

    -- web development
    -- {
    --     "norcalli/nvim-colorizer.lua",
    --     opts = {
    --         'css',
    --         -- 'javascript';
    --         -- html = {
    --         --     mode = 'foreground';
    --         -- }
    --     }
    -- },
    -- { 'jose-elias-alvarez/null-ls.nvim' }, -- Neovim as a language server to inject LSP diagnostics, code actions, and more via Lua

    -- debug
    -- "mfussenegger/nvim-dap",
    -- "rcarriga/cmp-dap",
    -- "rcarriga/nvim-dap-ui",
    -- "thehamsta/nvim-dap-virtual-text",
})
