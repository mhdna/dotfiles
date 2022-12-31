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
-- vim.api.nvim_create_autocmd({"BufWritePost"}, {
--   pattern = {"packer.lua"},
--   command = "source <afile> | PackerSync",
-- })

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
    return
end

-- Install your plugins here
return packer.startup(function(use)
    -- improve startup times
    use("lewis6991/impatient.nvim")

    use("hrsh7th/nvim-cmp") -- The completion plugin
    use("hrsh7th/cmp-buffer") -- buffer completions
    use("hrsh7th/cmp-path") -- path completions
    use("saadparwaiz1/cmp_luasnip") -- snippet completions
    use("hrsh7th/cmp-nvim-lsp")
    use("hrsh7th/cmp-nvim-lua")
    use("onsails/lspkind.nvim")

    use("wbthomason/packer.nvim") -- Have packer manage itself
    use("nvim-lua/plenary.nvim") -- Useful lua functions used by lots of plugins
    use("windwp/nvim-autopairs")
    use {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup(
                { ignore = '^$' }
            )
        end
    }
    use("JoosepAlviste/nvim-ts-context-commentstring")
    use("romgrk/nvim-treesitter-context")

    use { 'L3MON4D3/LuaSnip',
        -- config = [[ require('plugins/luasnip') ]],
    }
    use("rafamadriz/friendly-snippets") -- a bunch of snippets to use
    use("tpope/vim-fugitive")

    -- LSP
    use("neovim/nvim-lspconfig") -- enable LSP
    use("theprimeagen/harpoon")
    use { "williamboman/mason.nvim" ,
        config = function()
            require("mason").setup()
        end
    }

    -- use('jose-elias-alvarez/null-ls.nvim')

    -- Treesitter
    use({
        "nvim-treesitter/nvim-treesitter",
        require("nvim-treesitter.configs").setup {
            highlight = {
                -- ...
            },
            -- ...
            rainbow = {
                enable = true,
                -- disable = { "jsx", "cpp" }, list of languages you want to disable the plugin for
                extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
                max_file_lines = nil, -- Do not enable for files with more than n lines, int
                -- colors = {}, -- table of hex strings
                -- termcolors = {} -- table of colour name strings
            }
        }
    })

    use("mbbill/undotree")
    -- use("gruvbox-community/gruvbox")
    use "lukas-reineke/indent-blankline.nvim"
    -- use("kyazdani42/nvim-web-devicons")

    -- use{'norcalli/nvim-colorizer.lua',
    -- config = function()
    --         require'colorizer'.setup()
    --     end
    -- }

    -- use {
    --     'nvim-lualine/lualine.nvim',
    --     -- requires = { 'kyazdani42/nvim-web-devicons', opt = true }
    -- }
    -- use("ap/vim-buftabline")

    -- alternative to subword-mode in emacs
    -- use("haoren/vim-wordmotion")

    -- use('NLKNguyen/papercolor-theme')
    -- use { 'TimUntersberger/neogit', requires = 'nvim-lua/plenary.nvim' }

    -- use({
    --     "ibhagwan/fzf-lua",
    --     config = -- fzf
    --     vim.cmd [[let g:fzf_layout = { 'down': '~40%' } ]]
    --     -- config = function()
    --     --     require('fzf-lua').setup({
    --     --         winopts = { height = 0.6 } --split = "belowright new", preview = { hidden = 'hidden' },
    --     --     })
    --     -- end
    -- })

    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.0',
        -- or                            , branch = '0.1.x',
        requires = { {'nvim-lua/plenary.nvim'} }
    }
    -- use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }

    -- use 'simrat39/symbols-outline.nvim'

    -- use('powerline/powerline')
    -- java setup
    -- local JDTLS_LOCATION = vim.fn.stdpath "data" .. "/lsp_servers/jdtls"
    use("mfussenegger/nvim-jdtls")

    -- use({
    --     "kylechui/nvim-surround",
    --     tag = "*", -- Use for stability; omit to use `main` branch for the latest features
    --     config = function()
    --         require("nvim-surround").setup({
    --             -- Configuration here, or leave empty to use defaults
    --         })
    --     end
    -- })

    -- Lua
    -- use {
    --     "ahmedkhalf/project.nvim",
    --     config = function()
    --         require("project_nvim").setup {
    --             -- your configuration comes here
    --             -- or leave it empty to use the default settings
    --             -- refer to the configuration section below
    --         }
    --     end
    -- }

    -- Debug
    use {
        'mfussenegger/nvim-dap'
        -- config = function() require('user.nvim-dap') end,
    }
    use("rcarriga/cmp-dap")
    use("rcarriga/nvim-dap-ui")
    use("theHamsta/nvim-dap-virtual-text")

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all the plugins
    if PACKER_BOOTSTRAP then
        require("packer").sync()
    end
end)
