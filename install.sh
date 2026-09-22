#!/usr/bin/env bash
# ==============================================================================
# Canontra v0.1.0 Direct Production Installer (Linux & macOS)
# Deterministic, Multi-Tier Polyglot Program Identity & Semantic Graph Engine
# https://github.com/symtrace/canontra
# ==============================================================================
set -euo pipefail

VERSION="v0.1.0"
REPO="symtrace/canontra"
INSTALL_DIR="${CANONTRA_INSTALL_DIR:-$HOME/.local/bin}"
CONFIG_PATH=true
INSTALL_COMPLETIONS=true
FORCE_BUILD=false
DRY_RUN=false

# Terminal capability & color setup
if [ -t 1 ] && [ "${TERM:-}" != "dumb" ]; then
  INTERACTIVE=true
  BOLD='\033[1m'
  DIM='\033[2m'
  GREEN='\033[0;32m'
  BLUE='\033[0;34m'
  CYAN='\033[0;36m'
  MAGENTA='\033[0;35m'
  YELLOW='\033[0;33m'
  RED='\033[0;31m'
  NC='\033[0m'
else
  INTERACTIVE=false
  BOLD=''
  DIM=''
  GREEN=''
  BLUE=''
  CYAN=''
  MAGENTA=''
  YELLOW=''
  RED=''
  NC=''
fi

LOG_FILE="/tmp/canontra_install_$$.log"

cleanup() {
  if [ "${INTERACTIVE}" = true ]; then
    tput cnorm 2>/dev/null || true
  fi
  rm -f "${LOG_FILE}" 2>/dev/null || true
}
trap cleanup EXIT INT TERM

# Braille spinner frames
SPINNER_FRAMES=("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")

print_banner() {
  cat << EOF
${CYAN}${BOLD}
     ____                      _             
    / ___|__ _ _ __   ___  _ __ | |_ _ __ __ _ 
   | |   / _\` | '_ \ / _ \| '_ \| __| '__/ _\` |
   | |__| (_| | | | | (_) | | | | |_| | | (_| |
    \____\__,_|_| |_|\___/|_| |_|\__|_|  \__,_|
${NC}
${BOLD}  Deterministic, Multi-Tier Polyglot Program Identity Engine${NC}
  Release: ${GREEN}${VERSION}${NC} | Zero-Dependency Pure Haskell Runtime
${DIM}==============================================================================${NC}
EOF
}

show_help() {
  cat << EOF
Canontra Direct Installer (${VERSION})

USAGE:
  install.sh [OPTIONS]

OPTIONS:
  -d, --dir <PATH>         Installation target directory (default: ~/.local/bin)
  -v, --version <VER>      Specify Canontra release version (default: ${VERSION})
  -b, --build              Force building from local Haskell source repository
      --no-path            Skip adding installation directory to shell rc files
      --no-completions     Skip installing shell autocompletions
      --dry-run            Simulate installation steps without writing files
  -h, --help               Show this help message and exit

ENVIRONMENT:
  CANONTRA_INSTALL_DIR     Alternative environment variable for installation directory
EOF
}

# Parse command line options
while [[ $# -gt 0 ]]; do
  case "$1" in
    -d|--dir)
      INSTALL_DIR="$2"
      shift 2
      ;;
    -v|--version)
      VERSION="$2"
      shift 2
      ;;
    -b|--build)
      FORCE_BUILD=true
      shift
      ;;
    --no-path)
      CONFIG_PATH=false
      shift
      ;;
    --no-completions)
      INSTALL_COMPLETIONS=false
      shift
      ;;
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    -h|--help)
      show_help
      exit 0
      ;;
    *)
      echo -e "${RED}Unknown option: $1${NC}" >&2
      show_help
      exit 2
      ;;
  esac
done

# Normalize version tag to always have 'v' prefix
if [[ "${VERSION}" != v* ]]; then
  VERSION="v${VERSION}"
fi

# Animated step executor
spin_execute() {
  local label="$1"
  shift
  local cmd=("$@")

  if [ "${DRY_RUN}" = true ]; then
    printf "  ${YELLOW}↷${NC} %s ${DIM}(dry-run: %s)${NC}\n" "${label}" "${cmd[*]}"
    return 0
  fi

  if [ "${INTERACTIVE}" = true ]; then
    tput civis 2>/dev/null || true
    "${cmd[@]}" > "${LOG_FILE}" 2>&1 &
    local pid=$!
    local frame_idx=0

    while kill -0 "${pid}" 2>/dev/null; do
      local frame="${SPINNER_FRAMES[frame_idx % ${#SPINNER_FRAMES[@]}]}"
      printf "\r  ${CYAN}%s${NC} %s" "${frame}" "${label}"
      frame_idx=$((frame_idx + 1))
      sleep 0.08
    done

    wait "${pid}"
    local exit_status=$?

    if [ ${exit_status} -eq 0 ]; then
      printf "\r  ${GREEN}✔${NC} %s\n" "${label}"
    else
      printf "\r  ${RED}✖${NC} %s ${RED}(failed, exit code: %d)${NC}\n" "${label}" "${exit_status}"
      echo -e "\n${DIM}--- Error Log Output ---${NC}"
      cat "${LOG_FILE}"
      echo -e "${DIM}------------------------${NC}\n"
      exit "${exit_status}"
    fi
    tput cnorm 2>/dev/null || true
  else
    printf "  [*] %s ... " "${label}"
    if "${cmd[@]}" > "${LOG_FILE}" 2>&1; then
      printf "done\n"
    else
      local exit_status=$?
      printf "FAILED (code %d)\n" "${exit_status}"
      cat "${LOG_FILE}"
      exit "${exit_status}"
    fi
  fi
}

# Animated short notification for instantaneous checks
spin_instant() {
  local label="$1"
  local detail="$2"

  if [ "${INTERACTIVE}" = true ]; then
    tput civis 2>/dev/null || true
    for frame in "${SPINNER_FRAMES[@]:0:4}"; do
      printf "\r  ${CYAN}%s${NC} %s" "${frame}" "${label}"
      sleep 0.04
    done
    printf "\r  ${GREEN}✔${NC} %-30s ${DIM}%s${NC}\n" "${label}" "${detail}"
    tput cnorm 2>/dev/null || true
  else
    printf "  [+] %-30s %s\n" "${label}" "${detail}"
  fi
}

main() {
  print_banner

  # 1. Architecture and Operating System Detection
  local raw_os
  local raw_arch
  raw_os="$(uname -s | tr '[:upper:]' '[:lower:]')"
  raw_arch="$(uname -m)"

  local target_os=""
  local target_arch=""

  case "${raw_os}" in
    linux)  target_os="linux" ;;
    darwin) target_os="darwin" ;;
    *)
      echo -e "\n${RED}Unsupported operating system: ${raw_os}.${NC} Canontra supports Linux and macOS." >&2
      exit 1
      ;;
  esac

  case "${raw_arch}" in
    x86_64|amd64)   target_arch="x86_64" ;;
    aarch64|arm64)  target_arch="aarch64" ;;
    *)
      echo -e "\n${RED}Unsupported architecture: ${raw_arch}.${NC} Canontra supports x86_64 and aarch64." >&2
      exit 1
      ;;
  esac

  spin_instant "Host Platform Identified" "${target_os}-${target_arch}"

  # 2. Installation Target Preparation
  spin_instant "Target Directory Configured" "${INSTALL_DIR}"
  if [ "${DRY_RUN}" = false ]; then
    mkdir -p "${INSTALL_DIR}"
  fi

  # 3. Binary Resolution (Local Source Build vs GitHub Releases Download)
  local target_binary="${INSTALL_DIR}/canontra"
  local is_local_source=false

  if [ -f "canontra.cabal" ] && ([ "${FORCE_BUILD}" = true ] || ! command -v curl >/dev/null 2>&1); then
    is_local_source=true
  fi

  if [ "${is_local_source}" = true ]; then
    if command -v stack >/dev/null 2>&1; then
      spin_execute "Compiling Canontra from source (Stack)" stack install --local-bin-path "${INSTALL_DIR}"
    elif command -v cabal >/dev/null 2>&1; then
      spin_execute "Compiling Canontra from source (Cabal)" cabal install --installdir="${INSTALL_DIR}" --overwrite-policy=always
    else
      echo -e "${RED}Neither 'stack' nor 'cabal' was found in PATH.${NC}" >&2
      exit 1
    fi
  else
    # Remote release download
    local asset_name="canontra-${VERSION}-${target_os}-${target_arch}.tar.gz"
    local download_url="https://github.com/${REPO}/releases/download/${VERSION}/${asset_name}"
    local tmp_tarball="/tmp/${asset_name}"

    download_binary() {
      local success=false
      if command -v curl >/dev/null 2>&1; then
        if curl -fsSL "${download_url}" -o "${tmp_tarball}" 2>/dev/null; then
          tar -xzf "${tmp_tarball}" -C "${INSTALL_DIR}"
          chmod +x "${target_binary}"
          rm -f "${tmp_tarball}"
          success=true
        elif curl -fsSL "https://github.com/${REPO}/releases/download/${VERSION}/canontra-${VERSION}-${target_os}-${target_arch}" -o "${target_binary}" 2>/dev/null; then
          chmod +x "${target_binary}"
          success=true
        fi
      elif command -v wget >/dev/null 2>&1; then
        if wget -q -O "${tmp_tarball}" "${download_url}" 2>/dev/null; then
          tar -xzf "${tmp_tarball}" -C "${INSTALL_DIR}"
          chmod +x "${target_binary}"
          rm -f "${tmp_tarball}"
          success=true
        elif wget -q -O "${target_binary}" "https://github.com/${REPO}/releases/download/${VERSION}/canontra-${VERSION}-${target_os}-${target_arch}" 2>/dev/null; then
          chmod +x "${target_binary}"
          success=true
        fi
      fi

      if [ "${success}" = false ]; then
        return 1
      fi
    }

    # Fallback to local prebuilt or build if inside repo and remote download fails
    if [ -f "canontra.cabal" ]; then
      install_canontra() {
        if ! download_binary; then
          if [ -f "dist-bin/canontra" ]; then
            cp "dist-bin/canontra" "${target_binary}"
            chmod +x "${target_binary}"
          elif command -v stack >/dev/null 2>&1; then
            stack install --local-bin-path "${INSTALL_DIR}"
          else
            cabal install --installdir="${INSTALL_DIR}" --overwrite-policy=always
          fi
        fi
      }
      spin_execute "Acquiring Canontra ${VERSION} binary" install_canontra
    else
      spin_execute "Downloading Canontra ${VERSION} release archive" download_binary
    fi
  fi

  # 4. PATH Configuration
  if [ "${CONFIG_PATH}" = true ] && [ "${DRY_RUN}" = false ]; then
    if [[ ":$PATH:" != *":${INSTALL_DIR}:"* ]]; then
      local user_shell
      user_shell="$(basename "${SHELL:-bash}")"
      local rc_file="$HOME/.profile"

      case "${user_shell}" in
        zsh)
          rc_file="$HOME/.zshrc"
          ;;
        bash)
          if [ -f "$HOME/.bashrc" ]; then
            rc_file="$HOME/.bashrc"
          else
            rc_file="$HOME/.bash_profile"
          fi
          ;;
        fish)
          rc_file="$HOME/.config/fish/config.fish"
          ;;
      esac

      configure_path() {
        if [ "${user_shell}" = "fish" ]; then
          mkdir -p "$(dirname "${rc_file}")"
          echo "set -gx PATH \"${INSTALL_DIR}\" \$PATH" >> "${rc_file}"
        else
          echo "" >> "${rc_file}"
          echo "# Canontra binary path" >> "${rc_file}"
          echo "export PATH=\"${INSTALL_DIR}:\$PATH\"" >> "${rc_file}"
        fi
      }

      spin_execute "Configuring PATH in ${rc_file}" configure_path
      spin_instant "PATH Persistent Export" "Added to ${rc_file}"
    else
      spin_instant "Environment PATH Check" "Already includes ${INSTALL_DIR}"
    fi
  fi

  # 5. Autocompletions Setup
  if [ "${INSTALL_COMPLETIONS}" = true ] && [ "${DRY_RUN}" = false ]; then
    setup_completions() {
      if [ -x "${target_binary}" ]; then
        # Bash completions
        if [ -d "$HOME/.local/share/bash-completion/completions" ] || command -v bash >/dev/null 2>&1; then
          mkdir -p "$HOME/.local/share/bash-completion/completions"
          "${target_binary}" completions bash > "$HOME/.local/share/bash-completion/completions/canontra" 2>/dev/null || true
        fi
        # Zsh completions
        if [ -d "$HOME/.zfunc" ] || [ -d "$HOME/.zsh/completions" ]; then
          local zdir="$HOME/.zfunc"
          [ -d "$HOME/.zsh/completions" ] && zdir="$HOME/.zsh/completions"
          mkdir -p "${zdir}"
          "${target_binary}" completions zsh > "${zdir}/_canontra" 2>/dev/null || true
        fi
        # Fish completions
        if command -v fish >/dev/null 2>&1 || [ -d "$HOME/.config/fish" ]; then
          mkdir -p "$HOME/.config/fish/completions"
          "${target_binary}" completions fish > "$HOME/.config/fish/completions/canontra.fish" 2>/dev/null || true
        fi
      fi
    }
    spin_execute "Installing Shell Autocompletions (Bash/Zsh/Fish)" setup_completions
  fi

  # 6. Verification and Final Status Card
  echo ""
  if [ "${DRY_RUN}" = false ] && [ -x "${target_binary}" ]; then
    local installed_version
    installed_version="$("${target_binary}" version 2>/dev/null || echo "Canontra v0.1.0")"
    cat << EOF
${GREEN}${BOLD}✔ CANONTRA SUCCESSFULLY INSTALLED${NC}
${DIM}------------------------------------------------------------------------------${NC}
  Executable:  ${BOLD}${target_binary}${NC}
  Version:     ${CYAN}${installed_version}${NC}
  Target:      ${target_os}-${target_arch}
${DIM}------------------------------------------------------------------------------${NC}
${BOLD}Quick Start:${NC}
  1. Refresh your terminal session:
     ${CYAN}source ~/.bashrc${NC}  ${DIM}(or restart your shell)${NC}
  2. Compute semantic fingerprints for any source file:
     ${CYAN}canontra fp src/main.py --hash${NC}
  3. Compare two versions with 9-tier structural invariance:
     ${CYAN}canontra compare file_v1.py file_v2.py --json${NC}
  4. Query or verify cache integrity:
     ${CYAN}canontra cache info${NC}
${DIM}==============================================================================${NC}
EOF
  else
    cat << EOF
${YELLOW}${BOLD}↷ CANONTRA DRY-RUN COMPLETED${NC}
${DIM}All checks and installation actions validated successfully.${NC}
EOF
  fi
}

main
