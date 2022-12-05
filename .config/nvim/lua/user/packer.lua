local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
    PACKER_BOOTSTRAP = fn.system({
        "git",
        "clone",
        "--depth",
        "1",
        "https://github.com/wbthomason/packer.nvim",
        install_path,
    })
    print("Installing packer close and reopen Neovim...")
    vim.cmd([[packadd packer.nvim]])
end

-- Autocommand that reloads neovim whenever you save the packer.lua file
vim.cmd [[
augroup packer_user_config
autocmd!
autocmd BufWritePost packer.lua source <afile> | PackerSync
augroup end
]]
-- Emacs bindings

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
    return
end

-- Have packer use a popup window
packer.init({
    display = {
        open_fn = function()
            return require("packer.util").float({ border = "rounded" })
        end,
    },
})

-- Install your plugins here
return packer.startup(function(use)
    -- My plugins here

    use("wbthomason/packer.nvim") -- Have packer manage itself
    use("nvim-lua/plenary.nvim") -- Useful lua functions used by lots of plugins
    -- use("ido-nvim/ido.nvim")
    -- use("jremmen/vim-ripgrep")
    use("windwp/nvim-autopairs")
    use {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup()
        end
    }
    -- use("JoosepAlviste/nvim-ts-context-commentstring")
    -- use("romgrk/nvim-treesitter-context")

    --[[ use( "kyazdani42/nvim-web-devicons") ]]
    use("kyazdani42/nvim-tree.lua")
    -- use( "akinsho/bufferline.nvim")
    -- use {
    --     'kdheepak/tabline.nvim',
    --     config = function()
    --         require 'tabline'.setup {
    --             -- Defaults configuration options
    --             enable = true,
    --             options = {
    --                 -- If lualine is installed tabline will use separators configured in lualine by default.
    --                 -- These options can be used to override those settings.
    --                 -- section_separators = {'', ''},
    --                 -- component_separators = {'', ''},
    --                 max_bufferline_percent = 66, -- set to nil by default, and it uses vim.o.columns * 2/3
    --                 show_tabs_always = false, -- this shows tabs only when there are more than one tab or if the first tab is named
    --                 -- show_devicons = true, -- this shows devicons in buffer section
    --                 show_bufnr = false, -- this appends [bufnr] to buffer section,
    --                 show_filename_only = false, -- shows base filename only instead of relative path in filename
    --                 modified_icon = "+ ", -- change the default modified icon
    --                 modified_italic = false, -- set to true by default; this determines whether the filename turns italic if modified
    --                 show_tabs_only = false, -- this shows only tabs instead of tabs + buffers
    --             }
    --         }
    --         vim.cmd [[
    --   set guioptions-=e " Use showtabline in gui vim
    --   set sessionoptions+=tabpages,globals " store tabpages and globals in session
    -- ]]
    --     end,
    --     requires = { { 'hoob3rt/lualine.nvim', opt = true },
    --         -- {'kyazdani42/nvim-web-devicons', opt = true}
    --     }
    -- }
    -- use( "moll/vim-bbye")

    -- use {
    --     'nvim-lualine/lualine.nvim',
    --     requires = { 'kyazdani42/nvim-web-devicons', opt = true },
    --     use {
    --         'noib3/cokeline.nvim',
    --         requires = 'kyazdani42/nvim-web-devicons', -- If you want devicons
    --     }
    -- }
    --
    -- use { "akinsho/toggleterm.nvim", tag = '*', config = function()
        -- require("toggleterm").setup()
    -- end }

    use("ahmedkhalf/project.nvim")
    use("lewis6991/impatient.nvim")
    --[[ use("lukas-reineke/indent-blankline.nvim") ]]
    -- use( "goolord/alpha-nvim")
    -- use("folke/which-key.nvim")

    -- Colorschemes
    -- use( "folke/tokyonight.nvim")
    -- use("lunarvim/darkplus.nvim")

    -- cmp plugins
    use("hrsh7th/nvim-cmp") -- The completion plugin
    use("hrsh7th/cmp-buffer") -- buffer completions
    use("hrsh7th/cmp-path") -- path completions
    use("saadparwaiz1/cmp_luasnip") -- snippet completions
    use("hrsh7th/cmp-nvim-lsp")
    use("hrsh7th/cmp-nvim-lua")


    -- snippets
    use { 'L3MON4D3/LuaSnip',
        -- config = [[ require('plugins/luasnip') ]],
    }
    use("rafamadriz/friendly-snippets") -- a bunch of snippets to use

    -- LSP
    use("neovim/nvim-lspconfig") -- enable LSP
    -- use( "williamboman/nvim-lsp-installer") -- simple to use language server installer
    use { "williamboman/mason.nvim" }
    require("mason").setup({
        ui = {
            icons = {
                package_installed = "✓",
                package_pending = "➜",
                package_uninstalled = "✗"
            }
        }
    })

    use('jose-elias-alvarez/null-ls.nvim')
    -- use({
    --     "jose-elias-alvarez/null-ls.nvim",
    --
    --     config = function()
    --         require("null-ls").setup({
    --             sources = {
    --                 require("null-ls").builtins.formatting.stylua,
    --                 require("null-ls").builtins.diagnostics.eslint,
    --                 require("null-ls").builtins.completion.spell,
    --                 require("null-ls").builtins.completion.spell,
    --                 require("null_ls").builtins.formatting.google_java_format,
    --             }
    --         })
    --     end
    -- })
    --

    -- for formatters and linters

    -- Telescope
    -- use( "nvim-telescope/telescope.nvim")

    -- Treesitter
    -- use({
    --     "nvim-treesitter/nvim-treesitter",
    -- })

    -- Git
    -- use( "lewis6991/gitsigns.nvim")

    -- Mine
    use("mbbill/undotree")

    -- use {
    --     "X3eRo0/dired.nvim",
    --     requires = "MunifTanjim/nui.nvim",
    --     config = function()
    --         require("dired").setup {
    --             path_separator = "/",
    --             show_banner = false,
    --             show_hidden = true,
    --             show_dot_dirs = true,
    --             show_colors = true,
    --         }
    --     end
    -- }
    --
    -- use('lyokha/vim-xkbswitch')

    use('norcalli/nvim-colorizer.lua')

    use("sbdchd/neoformat")
-- use { 'TimUntersberger/neogit', requires = 'nvim-lua/plenary.nvim' }

--     use {
-- 	'arnarg/todotxt.nvim',
-- 	requires = {'MunifTanjim/nui.nvim'},
-- }
--
--
    -- fzf
    use({
        "ibhagwan/fzf-lua",
        -- config = function()
        --     require('fzf-lua').setup({
        --         winopts = { height = 0.6 } --split = "belowright new", preview = { hidden = 'hidden' },
        --     })
        -- end
    })

    -- Color schemes
    -- use ('luisiacc/gruvbox-baby')
    -- use('gruvbox-community/gruvbox')
    -- use('ajgrf/parchment')
    -- use('morhetz/gruvbox')
    -- use 'savq/melange'
    -- use ('sainnhe/gruvbox-material')
    -- use 'Arkham/vim-tango'
    -- use('tanvirtin/monokai.nvim')
    -- use('vim-pandoc/vim-pandoc-syntax')
    -- use('projekt0n/github-nvim-theme')

    -- use('easymotion/vim-easymotion')

    -- use('powerline/powerline')
    --[[ use ('ojroques/nvim-hardline') ]]
    -- use 'vim-airline/vim-airline'

    -- use  'godlygeek/tabular'

    -- use  'preservim/vim-markdown'
    -- use {
    -- use({ "phha/zenburn.nvim",
    --     config = function() require("zenburn").setup() end
    -- })
    --use { 'rose-pine/neovim', as = 'rose-pine' }

    -- LSP

    -- java setup
    -- local JDTLS_LOCATION = vim.fn.stdpath "data" .. "/lsp_servers/jdtls"
    use("mfussenegger/nvim-jdtls")

    use({
        "kylechui/nvim-surround",
        tag = "*", -- Use for stability; omit to use `main` branch for the latest features
        config = function()
            require("nvim-surround").setup({
                -- Configuration here, or leave empty to use defaults
            })
        end
    })

    -- -- Lua
    -- use {
    --     "folke/todo-comments.nvim",
    --     requires = "nvim-lua/plenary.nvim",
    --     config = function()
    --         require("todo-comments").setup {
    --             -- your configuration comes here
    --             -- or leave it empty to use the default settings
    --             -- refer to the configuration section below
    --         }
    --     end
    -- }
    --

    -- Debug
    -- use("mfussenegger/nvim-dap")
    -- use("rcarriga/nvim-dap-ui")
    -- use("theHamsta/nvim-dap-virtual-text")

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if PACKER_BOOTSTRAP then
        require("packer").sync()
    end
end)
