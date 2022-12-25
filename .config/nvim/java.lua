local jdtls_ok, jdtls = pcall(require, "jdtls")
if not jdtls_ok then
    return
end

-- Installation location of jdtls by nvim-lsp-installer
local JDTLS_LOCATION = vim.fn.stdpath "data" .. "/mason/packages/jdtls"

local home = os.getenv "HOME"

-- Only for Linux and Mac
local SYSTEM = "linux"
if vim.fn.has "mac" == 1 then
    SYSTEM = "mac"
end

local root_markers = { ".git", "mvnw", "gradlew", "pom.xml", "build.gradle" }
local root_dir = require("jdtls.setup").find_root(root_markers)
if root_dir == "" then
    return
end
local workspace_dir = home .. "/.local/share/eclipse/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")


local extendedClientCapabilities = jdtls.extendedClientCapabilities
extendedClientCapabilities.resolveAdditionalTextEditsSupport = true


local on_attach = function(client, bufnr)
jdtls.setup.add_commands()
require("jdtls").setup_dap()
require('jdtls.dap').setup_dap_main_class_configs()

-- Default keymaps
local bufopts = { noremap = true, silent = true, buffer = bufnr }
require("user.lsp-default-bindings").on_attach(client, bufnr)

-- Java extensions
vim.keymap.set("n", "<C-o>", jdtls.organize_imports, bufopts, "Organize imports")
vim.keymap.set("n", "<leader>lt", jdtls.test_class, bufopts, "Test class (DAP)")
vim.keymap.set("n", "<leader>lT", jdtls.test_nearest_method, bufopts, "Test method (DAP)")
vim.keymap.set("n", "<space>lev", jdtls.extract_variable, bufopts, "Extract variable")
vim.keymap.set("n", "<space>lec", jdtls.extract_constant, bufopts, "Extract constant")
vim.keymap.set("v", "<space>lem", [[<ESC><CMD>lua require('jdtls').extract_method(true)<CR>]], bufopts, "Extract method")


end


local bundles = {
    vim.fn.glob(home.."/.local/share/nvim/mason/packages/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-*.jar", 1),
}

local extra_bundles = vim.split(vim.fn.glob(home .. "/.local/share/nvim/mason/packages/java-test/extension/server/*.jar", 1), "\n")
vim.list_extend(bundles, extra_bundles)


local config = {
    cmd = {
        "java",
        "-Declipse.application=org.eclipse.jdt.ls.core.id1",
        "-Dosgi.bundles.defaultStartLevel=4",
        "-Declipse.product=org.eclipse.jdt.ls.core.product",
        "-Dlog.protocol=true",
        "-Dlog.level=ALL",
        "-Xms1g",
        "-javaagent:" .. home .. "/.local/share/nvim/mason/packages/jdtls/lombok.jar",
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
    on_attach = on_attach,

    root_dir = root_dir,
    -- capabilities = require("lvim.lsp").common_capabilities(),

    -- Here you can configure eclipse.jdt.ls specific settings
    -- See https://github.com/eclipse/eclipse.jdt.ls/wiki/Running-the-JAVA-LS-server-from-the-command-line#initialize-request
    -- for a list of options
    settings = {
        java = {
            -- jdt = {
            --   ls = {
            --     vmargs = "-XX:+UseParallelGC -XX:GCTimeRatio=4 -XX:AdaptiveSizePolicyWeight=90 -Dsun.zip.disableMemoryMapping=true -Xmx1G -Xms100m"
            --   }
            -- },
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
            inlayHints = {
                parameterNames = {
                    enabled = "all", -- literals, all, none
                },
            },
            format = {
                enabled = true,
                -- settings = {
                --   profile = "asdf"
                -- }
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
    on_init = function(client)
        client.notify('workspace/didChangeConfiguration', { settings = client.config.settings })
    end,
    handlers = {
        ["language/status"] = function() end,
        ["workspace/diagnostic/refresh"] = function() end,
        ["textDocument/codeAction"] = function() end,
        ["textDocument/rename"] = function() end,
        ["workspace/applyEdit"] = function() end,
        ["textDocument/documentHighlight"] = function() end,
    },
    init_options = {
        bundles = bundles
    },
}


-- vim.cmd(
--     "command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_compile JdtCompile lua require('jdtls').compile(<f-args>)"
-- )
-- vim.cmd(
--     "command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_set_runtime JdtSetRuntime lua require('jdtls').set_runtime(<f-args>)"
-- )
-- vim.cmd("command! -buffer JdtUpdateConfig lua require('jdtls').update_project_config()")
-- -- vim.cmd "command! -buffer JdtJol lua require('jdtls').jol()"
-- vim.cmd("command! -buffer JdtBytecode lua require('jdtls').javap()")
--
--
--
--

-- vim.cmd "command! -buffer JdtJshell lua require('jdtls').jshell()"
-- -- nvim-dap
-- nnoremap("<leader>bb", "<cmd>lua require'dap'.toggle_breakpoint()<cr>", "Set breakpoint")
-- nnoremap("<leader>bc", "<cmd>lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<cr>",
--     "Set conditional breakpoint")
-- nnoremap("<leader>bl", "<cmd>lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<cr>",
--     "Set log point")
-- nnoremap('<leader>br', "<cmd>lua require'dap'.clear_breakpoints()<cr>", "Clear breakpoints")
-- nnoremap('<leader>ba', '<cmd>Telescope dap list_breakpoints<cr>', "List breakpoints")
--
-- nnoremap("<leader>dc", "<cmd>lua require'dap'.continue()<cr>", "Continue")
-- nnoremap("<leader>dj", "<cmd>lua require'dap'.step_over()<cr>", "Step over")
-- nnoremap("<leader>dk", "<cmd>lua require'dap'.step_into()<cr>", "Step into")
-- nnoremap("<leader>do", "<cmd>lua require'dap'.step_out()<cr>", "Step out")
-- nnoremap('<leader>dd', "<cmd>lua require'dap'.disconnect()<cr>", "Disconnect")
-- nnoremap('<leader>dt', "<cmd>lua require'dap'.terminate()<cr>", "Terminate")
-- nnoremap("<leader>dr", "<cmd>lua require'dap'.repl.toggle()<cr>", "Open REPL")
-- nnoremap("<leader>dl", "<cmd>lua require'dap'.run_last()<cr>", "Run last")
-- nnoremap('<leader>di', function() require "dap.ui.widgets".hover() end, "Variables")
-- nnoremap('<leader>d?', function() local widgets = require "dap.ui.widgets"; widgets.centered_float(widgets.scopes) end,
--     "Scopes")
-- nnoremap('<leader>df', '<cmd>Telescope dap frames<cr>', "List frames")
-- nnoremap('<leader>dh', '<cmd>Telescope dap commands<cr>', "List commands")

require("jdtls").start_or_attach(config)
