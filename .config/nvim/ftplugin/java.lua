local jdtls_ok, jdtls = pcall(require, "jdtls")
if not jdtls_ok then
    return
end

-- Installation location of jdtls by nvim-lsp-installer
local JDTLS_LOCATION = vim.fn.stdpath "data" .. "/mason/packages/jdtls"

-- Data directory - change it to your liking
local HOME = os.getenv "HOME"
local WORKSPACE_PATH = HOME .. "/stuff/code/java/"

-- Only for Linux and Mac
local SYSTEM = "linux"
if vim.fn.has "mac" == 1 then
    SYSTEM = "mac"
end

local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ":p:h:t")
local workspace_dir = WORKSPACE_PATH .. project_name

local root_markers = { ".git", "mvnw", "gradlew", "pom.xml", "build.gradle" }
local root_dir = require("jdtls.setup").find_root(root_markers)
if root_dir == "" then
    return
end

local extendedClientCapabilities = jdtls.extendedClientCapabilities
extendedClientCapabilities.resolveAdditionalTextEditsSupport = true

-- local bundles = {
--     vim.fn.glob(HOME .. "/.config/nvim/java-debug/com.microsoft.java.debug.plugin/target/com.microsoft.java.debug.plugin-*.jar", 1),
-- }
--

local config = {
    cmd = {
        "java",
        "-Declipse.application=org.eclipse.jdt.ls.core.id1",
        "-Dosgi.bundles.defaultStartLevel=4",
        "-Declipse.product=org.eclipse.jdt.ls.core.product",
        "-Dlog.protocol=true",
        "-Dlog.level=ALL",
        "-Xms1g",
        "--add-modules=ALL-SYSTEM",
        "--add-opens",
        "java.base/java.util=ALL-UNNAMED",
        "--add-opens",
        "java.base/java.lang=ALL-UNNAMED",
        "-jar",
        vim.fn.glob(JDTLS_LOCATION .. "/plugins/org.eclipse.equinox.launcher_*.jar"),
        "-configuration",
        JDTLS_LOCATION .. "/config_" .. SYSTEM,
        "-data",
        workspace_dir,
    },

    -- on_attach = require("config.lsp").on_attach,
    -- capabilities = require("config.lsp").capabilities,
    root_dir = root_dir,

    -- Here you can configure eclipse.jdt.ls specific settings
    -- See https://github.com/eclipse/eclipse.jdt.ls/wiki/Running-the-JAVA-LS-server-from-the-command-line#initialize-request
    -- for a list of options
    settings = {
        java = {
            eclipse = {
                downloadSources = true,
            },
            configuration = {
                updateBuildConfiguration = "interactive",
            },
            maven = {
                downloadSources = true,
            },
            implementationsCodeLens = {
                enabled = true,
            },
            referencesCodeLens = {
                enabled = true,
            },
            references = {
                includeDecompiledSources = true,
            },
            format = {
                enabled = true,
                settings = {
                    url = vim.fn.stdpath "config" .. "/lang-servers/intellij-java-google-style.xml",
                    profile = "GoogleStyle",
                },
            },
        },
        signatureHelp = { enabled = true },
        completion = {
            favoriteStaticMembers = {
                "org.hamcrest.MatcherAssert.assertThat",
                "org.hamcrest.Matchers.*",
                "org.hamcrest.CoreMatchers.*",
                "org.junit.jupiter.api.Assertions.*",
                "java.util.Objects.requireNonNull",
                "java.util.Objects.requireNonNullElse",
                "org.mockito.Mockito.*",
            },
        },
        contentProvider = { preferred = "fernflower" },
        extendedClientCapabilities = extendedClientCapabilities,
        sources = {
            organizeImports = {
                starThreshold = 9999,
                staticStarThreshold = 9999,
            },
        },
        codeGeneration = {
            toString = {
                template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}",
            },
            useBlocks = true,
        },
    },

    flags = {
        allow_incremental_sync = true,
    },
    -- Language server `initializationOptions`
    -- You need to extend the `bundles` with paths to jar files
    -- if you want to use additional eclipse.jdt.ls plugins.
    --
    -- See https://github.com/mfussenegger/nvim-jdtls#java-debug-installation
    --
    -- If you don't plan on using the debugger or other eclipse.jdt.ls plugins you can remove this
    -- This bundles definition is the same as in the previous section (java-debug installation)
    -- This is the new part

--     vim.list_extend(bundles, vim.split(vim.fn.glob(HOME .. "/.config/nvim/vscode-java-test/server/*.jar", 1), "\n")),
--     init_options = {
--         bundles = bundles,
-- },


    -- on_attach = function(client, bufnr)
    --     -- With `hotcodereplace = 'auto' the debug adapter will try to apply code changes
    --     -- you make during a debug session immediately.
    --     -- Remove the option if you do not want that.
    --     -- You can use the `JdtHotcodeReplace` command to trigger it manually
    --     require('jdtls').setup_dap({ hotcodereplace = 'auto' })
    -- end
}


-- This starts a new client & server,
-- or attaches to an existing client & server depending on the `root_dir`.
jdtls.start_or_attach(config)

-- Add the commands
require("jdtls.setup").add_commands()
-- require'jdtls'.test_class()
-- require'jdtls'.test_nearest_method()
-- vim.api.nvim_exec(
--   [[
-- command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_compile JdtCompile lua require('jdtls').compile(<f-args>)
-- command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_set_runtime JdtSetRuntime lua require('jdtls').set_runtime(<f-args>)
-- command! -buffer JdtUpdateConfig lua require('jdtls').update_project_config()
-- command! -buffer JdtJol lua require('jdtls').jol()
-- command! -buffer JdtBytecode lua require('jdtls').javap()
-- command! -buffer JdtJshell lua require('jdtls').jshell(),
--   ]],
--   false
-- )

local opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap("n", "<leader>li", "<Cmd>lua require('jdtls').organize_imports()<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>lev", "<Cmd>lua require('jdtls').extract_variable()<CR>", opts)
vim.api.nvim_set_keymap("v", "<leader>lev", "<Esc><Cmd>lua require('jdtls').extract_variable(true)<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>lec", "<Cmd>lua require('jdtls').extract_constant()<CR>", opts)
vim.api.nvim_set_keymap("v", "<leader>lec", "<Esc><Cmd>lua require('jdtls').extract_constant(true)<CR>", opts)
vim.api.nvim_set_keymap("v", "<leader>lem", "<Esc><Cmd>lua require('jdtls').extract_method(true)<CR>", opts)


-- If using nvim-dap
-- This requires java-debug and vscode-java-test bundles, see install steps in this README further below.
vim.api.nvim_set_keymap("n", "<leader>lt", "<Cmd>lua require('jdtls').test_class()<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>lT", "<Cmd>lua require('jdtls').test_nearest_method()<CR>", { silent = true })


-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
-- Enable completion triggered by <c-x><c-o>
-- vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')


-- Mappings.
-- See `:help vim.lsp.*` for documentation on any of the below functions
local bufopts = { noremap = true, silent = true}
-- vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts) -- mhd: probably not a java thing, so we use the binding for jump to definition instead
vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
vim.keymap.set('n', 'gD', vim.lsp.buf.type_definition, bufopts)
vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
vim.keymap.set('n', '<leader>li', vim.lsp.buf.implementation, bufopts)
vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
vim.keymap.set('n', '<leader>lwa', vim.lsp.buf.add_workspace_folder, bufopts)
vim.keymap.set('n', '<leader>lwr', vim.lsp.buf.remove_workspace_folder, bufopts)
vim.keymap.set('n', '<leader>lwl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
end, bufopts)
vim.keymap.set('n', '<leader>lr', '<cmd>lua vim.lsp.buf.rename()<CR>', bufopts)
vim.keymap.set('n', '<leader>la', vim.lsp.buf.code_action, bufopts)
vim.keymap.set('n', '<leader>lf', vim.lsp.buf.format, bufopts)
