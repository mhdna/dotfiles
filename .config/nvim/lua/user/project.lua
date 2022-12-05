local status_ok, project = pcall(require, "project_nvim")
/* Terminal colors (16 first used in escape sequence) */
static const char *colorname[] = {
	/* 8 normal colors */
	"black",
	"red3",
	"green3",
	"yellow3",
	"blue2",
	"magenta3",
	"cyan3",
	"gray90",

	/* 8 bright colors */
	"gray50",
	"red",
	"green",
	"yellow",
	"#5c5cff",
	"magenta",
	"cyan",
	"white",

	[255] = 0,

	/* more colors can be added after 255 to use with DefaultXX */
	"#cccccc",
	"#555555",
};
if not status_ok then
	return
end
project.setup({
	---@usage set to false to disable project.nvim.
	--- This is on by default since it's currently the expected behavior.
	active = true,

	on_config_done = nil,

	---@usage set to true to disable setting the current-woriking directory
	--- Manual mode doesn't automatically change your root directory, so you have
	--- the option to manually do so using `:ProjectRoot` command.
	manual_mode = false,

	---@usage Methods of detecting the root directory
	--- Allowed values: **"lsp"** uses the native neovim lsp
	--- **"pattern"** uses vim-rooter like glob pattern matching. Here
	--- order matters: if one is not detected, the other is used as fallback. You
	--- can also delete or rearangne the detection methods.
	-- detection_methods = { "lsp", "pattern" }, -- NOTE: lsp detection will get annoying with multiple langs in one project
	detection_methods = { "pattern" },

	---@usage patterns used to detect root dir, when **"pattern"** is in detection_methods
	-- patterns = { ".git", "_darcs", ".hg", ".bzr", ".svn", "Makefile", "package.json" },

	---@ Show hidden files in telescope when searching for files in a project
	show_hidden = false,

	---@usage When set to false, you will get a message when project.nvim changes your directory.
	-- When set to false, you will get a message when project.nvim changes your directory.
	silent_chdir = true,

	---@usage list of lsp client names to ignore when using **lsp** detection. eg: { "efm", ... }
	ignore_lsp = {},

	---@type string
	---@usage path to store the project history for use in telescope
  datapath = vim.fn.stdpath("data"),
})

local tele_status_ok, telescope = pcall(require, "telescope")
if not tele_status_ok then
	return
end

telescope.load_extension('projects')
