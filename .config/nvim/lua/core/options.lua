vim.loader.enable()

vim.opt.termguicolors    = true -- Enable 24-bit RGB colors
vim.o.mouse              = "a"
vim.opt.pumheight        = 15
vim.opt.pumwidth         = 15
vim.opt.laststatus       = 3    -- number of spaces in tab when editing
vim.opt.hidden           = true -- Enable background buffers
vim.opt.history          = 100  -- Remember N lines in history
vim.opt.lazyredraw       = true -- Faster scrolling
vim.opt.synmaxcol        = 240  -- Max column for syntax highlighting
vim.opt.updatetime       = 700  -- ms to wait for trigger an event
vim.o.compatible         = false
vim.o.incsearch          = true -- highlight incrementaly rather than the whole word word
vim.o.ignorecase         = true -- only ignore case if write uppercase letter
vim.o.smartcase          = true -- only ignore case if write uppercase letter
vim.o.number             = true
vim.wo.relativenumber    = true
vim.opt.linebreak        = false
vim.opt.expandtab        = true -- expand tab to spaces so that tabs are spaces
vim.opt.smartindent      = true -- Autoindent new lines
vim.o.title              = true

vim.o.clipboard          = "unnamedplus"
vim.o.undofile           = true
vim.o.undodir            = os.getenv("XDG_CACHE_HOME") .. "/nvim/undodir/"
vim.o.backupdir          = os.getenv("XDG_CACHE_HOME") .. "/nvim/backup/"
vim.o.backup             = true
vim.o.encoding           = "utf-8"
vim.o.scrolloff          = 8

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
