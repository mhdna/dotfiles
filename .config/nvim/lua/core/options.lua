vim.opt.termguicolors    = true -- Enable 24-bit RGB colors
vim.o.mouse              = "a"
vim.opt.pumheight        = 15
vim.opt.pumwidth         = 15
-- vim.opt.foldmethod = 'marker' -- Enable folding (default 'foldmarker')
vim.opt.expandtab        = true -- Use spaces instead of tabs
vim.opt.smartindent      = true -- Autoindent new lines
vim.opt.hidden           = true -- Enable background buffers
vim.opt.history          = 100  -- Remember N lines in history
vim.opt.lazyredraw       = true -- Faster scrolling
vim.opt.synmaxcol = 240         -- Max column for syntax highlighting
vim.opt.updatetime       = 700 -- ms to wait for trigger an event
-- vim.o.exrc       = true -- so if a vimrc file is in a dir, it's automatically sourced (for custom projects)
vim.o.compatible         = false
vim.o.incsearch          = true  -- highlight incrementaly rather than the whole word word
vim.o.ignorecase         = true  -- only ignore case if write uppercase letter
vim.o.smartcase          = true  -- only ignore case if write uppercase letter
vim.o.hlsearch           = false -- Do not keep the previous search occurance highlighted
vim.o.number             = true
vim.wo.relativenumber    = true
vim.o.scrolloff          = 8
vim.o.guicursor          = ""
vim.opt.linebreak        = false
vim.bo.tabstop           = 4
vim.opt.shiftwidth       = 4
vim.o.title              = true
vim.o.go                 = a
vim.o.clipboard          = "unnamedplus"
vim.o.undofile           = true
vim.o.undodir            = os.getenv("XDG_CACHE_HOME") .. "/nvim/undodir/"
vim.o.backupdir          = os.getenv("XDG_CACHE_HOME") .. "/nvim/backup/"
vim.o.backup             = true
vim.o.encoding           = "utf-8"
-- vim.o.statusline         = "%<%F %h%m%r%=%-14.(%l,%c%V%) %P"
-- vim.opt.showtabline      = 2

-- Spellchecking
vim.opt.spelllang        = 'en_us'

-- Always open splits at the bottom and right
vim.o.splitbelow         = true
vim.o.splitright         = true

vim.o.cursorline         = true -- Highlight current line

-- better netrw
vim.g.netrw_browse_split = 0
vim.g.netrw_banner       = 0
vim.g.netrw_winsize      = 25

-- Disable nvim intro
vim.opt.shortmess:append "sI"
