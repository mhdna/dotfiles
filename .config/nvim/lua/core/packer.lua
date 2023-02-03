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
    use("wbthomason/packer.nvim") -- Have packer manage itself
    use("nvim-lua/plenary.nvim") -- Useful lua functions used by lots of plugins
    -- use("windwp/nvim-autopairs") -- similar to rainbow parameters
    -- LSP
    use("neovim/nvim-lspconfig")
    use("mfussenegger/nvim-jdtls")
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
    }
    use("JoosepAlviste/nvim-ts-context-commentstring")
    use("romgrk/nvim-treesitter-context")
    use("L3MON4D3/LuaSnip")
    use("rafamadriz/friendly-snippets") -- a bunch of snippets to use
    use {
        "numToStr/Comment.nvim",
        config = function()
            require("Comment").setup(
                { ignore = "^$" }
            )
        end
    }
    use { "williamboman/mason.nvim" ,
        config = function()
            require("mason").setup()
        end
    }
    -- use("jose-elias-alvarez/null-ls.nvim")
    use("mbbill/undotree")
    -- alternative to subword-mode in Emacs
    -- use("haoren/vim-wordmotion")
    use { "TimUntersberger/neogit", requires = "nvim-lua/plenary.nvim" }
    use {
        "nvim-telescope/telescope.nvim",
        requires = { {"nvim-lua/plenary.nvim"} }
    }
    -- use {"nvim-telescope/telescope-fzf-native.nvim", run = "make" }
    use({
        "kylechui/nvim-surround",
        tag = "*", -- Use for stability; omit to use `main` branch for the latest features
        config = function()
            require("nvim-surround").setup({
                -- Configuration here, or leave empty to use defaults
            })
        end
    })
    use("lukas-reineke/indent-blankline.nvim")
    -- use("nvim-lualine/lualine.nvim")
    -- Debug
    use("mfussenegger/nvim-dap")
    use("rcarriga/cmp-dap")
    use("rcarriga/nvim-dap-ui")
    use("theHamsta/nvim-dap-virtual-text")

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all the plugins
    if PACKER_BOOTSTRAP then
        require("packer").sync()
    end
end)
