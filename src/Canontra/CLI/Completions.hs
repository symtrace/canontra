{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Canontra.CLI.Completions
Description : Shell autocompletion script generators for canontra CLI.

Generates native, self-contained completion scripts for:
- Bash (using complete -F _canontra)
- Zsh (using compdef _canontra)
- Fish (using complete -c canontra)
- PowerShell (using Register-ArgumentCompleter)
-}
module Canontra.CLI.Completions
  ( ShellType (..)
  , generateCompletionScript
  , parseShellType
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Supported target shells for autocompletions.
data ShellType
  = ShellBash
  | ShellZsh
  | ShellFish
  | ShellPowerShell
  deriving stock (Eq, Ord, Show)

-- | Parses a shell name string into a 'ShellType'.
parseShellType :: String -> Maybe ShellType
parseShellType s = case map toLowerChar s of
  "bash"       -> Just ShellBash
  "zsh"        -> Just ShellZsh
  "fish"       -> Just ShellFish
  "powershell" -> Just ShellPowerShell
  "pwsh"       -> Just ShellPowerShell
  _            -> Nothing
  where
    toLowerChar c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise            = c

-- | Generates the autocompletion script for the specified shell.
generateCompletionScript :: ShellType -> Text
generateCompletionScript shell = case shell of
  ShellBash       -> bashCompletions
  ShellZsh        -> zshCompletions
  ShellFish       -> fishCompletions
  ShellPowerShell -> powerShellCompletions

-- ============================================================================
-- Bash Completion Script
-- ============================================================================
bashCompletions :: Text
bashCompletions = T.unlines
  [ "#!/usr/bin/env bash"
  , "# Canontra Bash autocompletion script"
  , "_canontra()"
  , "{"
  , "    local cur prev words cword"
  , "    _init_completion || return"
  , ""
  , "    local commands=\"fp fingerprint compare diff graph verify repository repo impact slice watch commit evolution cache export completions version\""
  , ""
  , "    if [ $cword -eq 1 ]; then"
  , "        COMPREPLY=( $(compgen -W \"${commands}\" -- \"${cur}\") )"
  , "        return 0"
  , "    fi"
  , ""
  , "    case \"${words[1]}\" in"
  , "        fp|fingerprint)"
  , "            case \"${prev}\" in"
  , "                -l|--language)"
  , "                    COMPREPLY=( $(compgen -W \"python typescript javascript go rust\" -- \"${cur}\") )"
  , "                    return 0"
  , "                    ;;"
  , "            esac"
  , "            if [[ \"${cur}\" == -* ]]; then"
  , "                COMPREPLY=( $(compgen -W \"--json --hash -q -l --language --help\" -- \"${cur}\") )"
  , "            else"
  , "                _filedir"
  , "            fi"
  , "            ;;"
  , "        compare)"
  , "            if [[ \"${cur}\" == -* ]]; then"
  , "                COMPREPLY=( $(compgen -W \"--diff --json --help\" -- \"${cur}\") )"
  , "            else"
  , "                _filedir"
  , "            fi"
  , "            ;;"
  , "        diff)"
  , "            if [[ \"${cur}\" == -* ]]; then"
  , "                COMPREPLY=( $(compgen -W \"--json --help\" -- \"${cur}\") )"
  , "            else"
  , "                _filedir"
  , "            fi"
  , "            ;;"
  , "        graph)"
  , "            if [[ \"${cur}\" == -* ]]; then"
  , "                COMPREPLY=( $(compgen -W \"--scope --calls --deps --cfg --dfg --json --help\" -- \"${cur}\") )"
  , "            else"
  , "                _filedir"
  , "            fi"
  , "            ;;"
  , "        cache)"
  , "            if [ $cword -eq 2 ]; then"
  , "                COMPREPLY=( $(compgen -W \"info verify clean prune\" -- \"${cur}\") )"
  , "            elif [[ \"${cur}\" == -* ]]; then"
  , "                COMPREPLY=( $(compgen -W \"--json --help\" -- \"${cur}\") )"
  , "            else"
  , "                _filedir -d"
  , "            fi"
  , "            ;;"
  , "        export)"
  , "            case \"${prev}\" in"
  , "                -f|--format)"
  , "                    COMPREPLY=( $(compgen -W \"sarif dot\" -- \"${cur}\") )"
  , "                    return 0"
  , "                    ;;"
  , "                -g|--graph)"
  , "                    COMPREPLY=( $(compgen -W \"calls cfg dfg\" -- \"${cur}\") )"
  , "                    return 0"
  , "                    ;;"
  , "                -o|--output|-b|--base)"
  , "                    _filedir"
  , "                    return 0"
  , "                    ;;"
  , "            esac"
  , "            if [[ \"${cur}\" == -* ]]; then"
  , "                COMPREPLY=( $(compgen -W \"-f --format -o --output -b --base -g --graph --help\" -- \"${cur}\") )"
  , "            else"
  , "                _filedir"
  , "            fi"
  , "            ;;"
  , "        completions)"
  , "            COMPREPLY=( $(compgen -W \"bash zsh fish powershell\" -- \"${cur}\") )"
  , "            ;;"
  , "        repo|repository)"
  , "            if [[ \"${cur}\" == -* ]]; then"
  , "                COMPREPLY=( $(compgen -W \"--json --cache --help\" -- \"${cur}\") )"
  , "            else"
  , "                _filedir -d"
  , "            fi"
  , "            ;;"
  , "        *)"
  , "            _filedir"
  , "            ;;"
  , "    esac"
  , "}"
  , "complete -F _canontra canontra"
  ]

-- ============================================================================
-- Zsh Completion Script
-- ============================================================================
zshCompletions :: Text
zshCompletions = T.unlines
  [ "#compdef canontra"
  , "# Canontra Zsh autocompletion script"
  , ""
  , "_canontra() {"
  , "    local -a commands"
  , "    commands=("
  , "        'fp:Compute deterministic multi-tier fingerprints'"
  , "        'fingerprint:Alias for fp'"
  , "        'compare:Compare fingerprints between two files'"
  , "        'diff:Generate fine-grained structural and semantic diff diagnostics'"
  , "        'graph:Inspect call graph, CFG, DFG, scope tree, or dependency graph'"
  , "        'verify:Verify repeat-execution determinism'"
  , "        'repo:Compute aggregated repository fingerprint'"
  , "        'repository:Alias for repo'"
  , "        'impact:Compute fine-grained semantic change impact slice'"
  , "        'slice:Trace upstream caller slice and downstream dependencies'"
  , "        'watch:Start interactive live terminal Merkle DAG watcher session'"
  , "        'commit:Fingerprint repository at a git commit'"
  , "        'evolution:Compare repository evolution across two git revisions'"
  , "        'cache:Inspect, verify, clean, or prune incremental binary cache'"
  , "        'export:Export diagnostics (SARIF) or graphs (DOT)'"
  , "        'completions:Generate shell autocompletions'"
  , "        'version:Display engine version'"
  , "    )"
  , ""
  , "    _arguments -C \\"
  , "        '1: :->command' \\"
  , "        '*:: :->args'"
  , ""
  , "    case $state in"
  , "        command)"
  , "            _describe -t commands 'canontra command' commands"
  , "            ;;"
  , "        args)"
  , "            case $words[1] in"
  , "                fp|fingerprint)"
  , "                    _arguments \\"
  , "                        '(-l --language)'{-l,--language}'[Source language]:language:(python typescript javascript go rust)' \\"
  , "                        '--json[Output as JSON manifest]' \\"
  , "                        '(-q --hash)'{-q,--hash}'[Output only composite hash]' \\"
  , "                        '1:source file:_files'"
  , "                    ;;"
  , "                cache)"
  , "                    local -a cache_cmds"
  , "                    cache_cmds=('info:Inspect cache statistics' 'verify:Verify CRC32 integrity' 'clean:Remove cache file' 'prune:Remove orphaned entries')"
  , "                    _describe -t cache_cmds 'cache command' cache_cmds"
  , "                    ;;"
  , "                export)"
  , "                    _arguments \\"
  , "                        '(-f --format)'{-f,--format}'[Export format]:format:(sarif dot)' \\"
  , "                        '(-o --output)'{-o,--output}'[Destination file]:output file:_files' \\"
  , "                        '(-b --base)'{-b,--base}'[Baseline file]:baseline file:_files' \\"
  , "                        '(-g --graph)'{-g,--graph}'[Graph type]:graph:(calls cfg dfg)' \\"
  , "                        '1:source file:_files'"
  , "                    ;;"
  , "                completions)"
  , "                    _arguments '1:shell:(bash zsh fish powershell)'"
  , "                    ;;"
  , "                *)"
  , "                    _files"
  , "                    ;;"
  , "            esac"
  , "            ;;"
  , "    esac"
  , "}"
  , ""
  , "_canontra \"$@\""
  ]

-- ============================================================================
-- Fish Completion Script
-- ============================================================================
fishCompletions :: Text
fishCompletions = T.unlines
  [ "# Canontra Fish autocompletion script"
  , "complete -c canontra -f"
  , ""
  , "# Primary commands"
  , "complete -c canontra -n '__fish_use_subcommand' -a fp -d 'Compute deterministic multi-tier fingerprints'"
  , "complete -c canontra -n '__fish_use_subcommand' -a compare -d 'Compare fingerprints between two files'"
  , "complete -c canontra -n '__fish_use_subcommand' -a diff -d 'Generate fine-grained structural and semantic diffs'"
  , "complete -c canontra -n '__fish_use_subcommand' -a graph -d 'Inspect call graph, CFG, DFG, scope, or deps'"
  , "complete -c canontra -n '__fish_use_subcommand' -a verify -d 'Verify repeat-execution determinism'"
  , "complete -c canontra -n '__fish_use_subcommand' -a repo -d 'Compute aggregated repository fingerprint'"
  , "complete -c canontra -n '__fish_use_subcommand' -a impact -d 'Compute semantic change impact slice'"
  , "complete -c canontra -n '__fish_use_subcommand' -a slice -d 'Trace upstream caller slice and dependencies'"
  , "complete -c canontra -n '__fish_use_subcommand' -a watch -d 'Start live terminal Merkle DAG watcher session'"
  , "complete -c canontra -n '__fish_use_subcommand' -a cache -d 'Inspect, verify, clean, or prune binary cache'"
  , "complete -c canontra -n '__fish_use_subcommand' -a export -d 'Export diagnostics (SARIF) or graphs (DOT)'"
  , "complete -c canontra -n '__fish_use_subcommand' -a completions -d 'Generate shell autocompletions'"
  , "complete -c canontra -n '__fish_use_subcommand' -a version -d 'Display engine version'"
  , ""
  , "# Cache subcommands"
  , "complete -c canontra -n '__fish_seen_subcommand_from cache' -a info -d 'Inspect cache statistics'"
  , "complete -c canontra -n '__fish_seen_subcommand_from cache' -a verify -d 'Verify CRC32 slab page integrity'"
  , "complete -c canontra -n '__fish_seen_subcommand_from cache' -a clean -d 'Remove binary cache file'"
  , "complete -c canontra -n '__fish_seen_subcommand_from cache' -a prune -d 'Remove orphaned deleted records'"
  , ""
  , "# Export options"
  , "complete -c canontra -n '__fish_seen_subcommand_from export' -s f -l format -x -a 'sarif dot' -d 'Export format'"
  , "complete -c canontra -n '__fish_seen_subcommand_from export' -s g -l graph -x -a 'calls cfg dfg' -d 'Graph type for DOT'"
  , "complete -c canontra -n '__fish_seen_subcommand_from export' -s o -l output -F -d 'Output file path'"
  , "complete -c canontra -n '__fish_seen_subcommand_from export' -s b -l base -F -d 'Baseline file for SARIF diff'"
  , ""
  , "# Completions options"
  , "complete -c canontra -n '__fish_seen_subcommand_from completions' -a 'bash zsh fish powershell' -d 'Target shell'"
  ]

-- ============================================================================
-- PowerShell Completion Script
-- ============================================================================
powerShellCompletions :: Text
powerShellCompletions = T.unlines
  [ "# Canontra Windows PowerShell and PowerShell Core autocompletion script"
  , "Register-ArgumentCompleter -Native -CommandName canontra -ScriptBlock {"
  , "    param($wordToComplete, $commandAst, $cursorPosition)"
  , ""
  , "    $commands = @("
  , "        [System.Management.Automation.CompletionResult]::new('fp', 'fp', 'ParameterValue', 'Compute deterministic multi-tier fingerprints'),"
  , "        [System.Management.Automation.CompletionResult]::new('fingerprint', 'fingerprint', 'ParameterValue', 'Alias for fp'),"
  , "        [System.Management.Automation.CompletionResult]::new('compare', 'compare', 'ParameterValue', 'Compare fingerprints between two files'),"
  , "        [System.Management.Automation.CompletionResult]::new('diff', 'diff', 'ParameterValue', 'Generate fine-grained structural and semantic diff diagnostics'),"
  , "        [System.Management.Automation.CompletionResult]::new('graph', 'graph', 'ParameterValue', 'Inspect call graph, CFG, DFG, scope tree, or dependency graph'),"
  , "        [System.Management.Automation.CompletionResult]::new('verify', 'verify', 'ParameterValue', 'Verify repeat-execution determinism'),"
  , "        [System.Management.Automation.CompletionResult]::new('repo', 'repo', 'ParameterValue', 'Compute aggregated repository fingerprint'),"
  , "        [System.Management.Automation.CompletionResult]::new('repository', 'repository', 'ParameterValue', 'Alias for repo'),"
  , "        [System.Management.Automation.CompletionResult]::new('impact', 'impact', 'ParameterValue', 'Compute fine-grained semantic change impact slice'),"
  , "        [System.Management.Automation.CompletionResult]::new('slice', 'slice', 'ParameterValue', 'Trace upstream caller slice and downstream dependencies'),"
  , "        [System.Management.Automation.CompletionResult]::new('watch', 'watch', 'ParameterValue', 'Start live terminal Merkle DAG watcher session'),"
  , "        [System.Management.Automation.CompletionResult]::new('cache', 'cache', 'ParameterValue', 'Inspect, verify, clean, or prune incremental binary cache'),"
  , "        [System.Management.Automation.CompletionResult]::new('export', 'export', 'ParameterValue', 'Export diagnostics (SARIF) or graphs (DOT)'),"
  , "        [System.Management.Automation.CompletionResult]::new('completions', 'completions', 'ParameterValue', 'Generate shell autocompletions'),"
  , "        [System.Management.Automation.CompletionResult]::new('version', 'version', 'ParameterValue', 'Display engine version')"
  , "    )"
  , ""
  , "    $elements = $commandAst.CommandElements"
  , "    if ($elements.Count -le 2) {"
  , "        $commands | Where-Object { $_.CompletionText -like \"$wordToComplete*\" }"
  , "        return"
  , "    }"
  , ""
  , "    $subCommand = $elements[1].Extent.Text"
  , "    switch ($subCommand) {"
  , "        'cache' {"
  , "            @('info', 'verify', 'clean', 'prune') | Where-Object { $_ -like \"$wordToComplete*\" } | ForEach-Object {"
  , "                [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', \"Cache $_\")"
  , "            }"
  , "        }"
  , "        'completions' {"
  , "            @('bash', 'zsh', 'fish', 'powershell') | Where-Object { $_ -like \"$wordToComplete*\" } | ForEach-Object {"
  , "                [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', \"Shell $_\")"
  , "            }"
  , "        }"
  , "        'export' {"
  , "            if ($wordToComplete -like '-*') {"
  , "                @('--format', '-f', '--output', '-o', '--base', '-b', '--graph', '-g', '--help') | Where-Object { $_ -like \"$wordToComplete*\" }"
  , "            }"
  , "        }"
  , "    }"
  , "}"
  ]
