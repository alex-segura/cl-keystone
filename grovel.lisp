(in-package #:keystone)

#+bsd (cc-flags "-I /usr/local/include/keystone/")

(include "keystone.h")

(constant (+ks-api-major+ "KS_API_MAJOR"))
(constant (+ks-api-minor+ "KS_API_MINOR"))

(cenum (arch :define-constants t)
 ((:arm "KS_ARCH_ARM"))
 ((:arm64 "KS_ARCH_ARM64"))
 ((:x86 "KS_ARCH_X86"))
 ((:ppc "KS_ARCH_PPC"))
 ((:sparc "KS_ARCH_SPARC"))
 ((:systemz "KS_ARCH_SYSTEMZ"))
 ((:hexagon "KS_ARCH_HEXAGON")))
(ctype :ks-arch "ks_arch")

(cenum (mode :define-constants t)
 ((:little-endian "KS_MODE_LITTLE_ENDIAN"))
 ((:big-endian "KS_MODE_BIG_ENDIAN"))
 ((:arm "KS_MODE_ARM"))
 ((:thumb "KS_MODE_THUMB"))
 ((:v8 "KS_MODE_V8"))
 ((:micro "KS_MODE_MICRO"))
 ((:mips3 "KS_MODE_MIPS3"))
 ((:mips32r6 "KS_MODE_MIPS32R6"))
 ((:mips32 "KS_MODE_MIPS32"))
 ((:mips64 "KS_MODE_MIPS64"))
 ((:16-bit "KS_MODE_16"))
 ((:32-bit "KS_MODE_32"))
 ((:64-bit "KS_MODE_64"))
 ((:ppc32 "KS_MODE_PPC32"))
 ((:ppc64 "KS_MODE_PPC64"))
 ((:qpx "KS_MODE_QPX"))
 ((:sparc32 "KS_MODE_SPARC32"))
 ((:sparc64 "KS_MODE_SPARC64"))
 ((:v9 "KS_MODE_V9")))
(ctype :ks-mode "ks_mode")

(constant (+ks-err-asm+ "KS_ERR_ASM"))
(constant (+ks-err-asm-arch+ "KS_ERR_ASM_ARCH"))

(cenum (err :define-constants t)
 ((:err-ok "KS_ERR_OK"))
 ((:err-nomem "KS_ERR_NOMEM"))
 ((:err-arch "KS_ERR_ARCH"))
 ((:err-handle "KS_ERR_HANDLE"))
 ((:err-mode "KS_ERR_MODE"))
 ((:err-version "KS_ERR_VERSION"))
 ((:err-opt-invalid "KS_ERR_OPT_INVALID")))
(ctype :ks-err "ks_err")

(cenum (opt-type :define-constants t)
 ((:syntax "KS_OPT_SYNTAX")))
(ctype :ks-opt-type "ks_opt_type")

(cenum (opt-value :define-constants t)
 ((:intel "KS_OPT_SYNTAX_INTEL"))
 ((:att "KS_OPT_SYNTAX_ATT"))
 ((:nasm "KS_OPT_SYNTAX_NASM"))
 ((:masm "KS_OPT_SYNTAX_MASM"))
 ((:gas "KS_OPT_SYNTAX_GAS")))
(ctype :ks-opt-value "ks_opt_value")

(cenum (err-asm-arm64 :define-constants t)
 ((:invalid-operand "KS_ERR_ASM_ARM64_INVALIDOPERAND"))
 ((:missing-feature "KS_ERR_ASM_ARM64_MISSINGFEATURE"))
 ((:mnemonic-fail "KS_ERR_ASM_ARM64_MNEMONICFAIL")))
(ctype :ks-err-asm-arm64 "ks_err_asm_arm64")

(cenum (err-asm-arm :define-constants t)
 ((:invalid-operand "KS_ERR_ASM_ARM_INVALIDOPERAND"))
 ((:missing-feature "KS_ERR_ASM_ARM_MISSINGFEATURE"))
 ((:mnemonic-fail "KS_ERR_ASM_ARM_MNEMONICFAIL")))
(ctype :ks-err-asm-arm "ks_err_asm_arm")

(cenum (err-asm-hexagon :define-constants t)
 ((:invalid-operand "KS_ERR_ASM_HEXAGON_INVALIDOPERAND"))
 ((:missing-feature "KS_ERR_ASM_HEXAGON_MISSINGFEATURE"))
 ((:mnemonic-fail "KS_ERR_ASM_HEXAGON_MNEMONICFAIL")))
(ctype :ks-err-asm-hexagon "ks_err_asm_hexagon")

(cenum (err-asm-mips :define-constants t)
 ((:invalid-operand "KS_ERR_ASM_MIPS_INVALIDOPERAND"))
 ((:missing-feature "KS_ERR_ASM_MIPS_MISSINGFEATURE"))
 ((:mnemonic-fail "KS_ERR_ASM_MIPS_MNEMONICFAIL")))
(ctype :ks-err-asm-mips "ks_err_asm_mips")

(cenum (err-asm-ppc :define-constants t)
 ((:invalid-operand "KS_ERR_ASM_PPC_INVALIDOPERAND"))
 ((:missing-feature "KS_ERR_ASM_PPC_MISSINGFEATURE"))
 ((:mnemonic-fail "KS_ERR_ASM_PPC_MNEMONICFAIL")))
(ctype :ks-err-asm-ppc "ks_err_asm_ppc")

(cenum (err-asm-sparc :define-constants t)
 ((:invalid-operand "KS_ERR_ASM_SPARC_INVALIDOPERAND"))
 ((:missing-feature "KS_ERR_ASM_SPARC_MISSINGFEATURE"))
 ((:mnemonic-fail "KS_ERR_ASM_SPARC_MNEMONICFAIL")))
(ctype :ks-err-asm-sparc "ks_err_asm_sparc")

(cenum (err-asm-systemz :define-constants t)
 ((:invalid-operand "KS_ERR_ASM_SYSTEMZ_INVALIDOPERAND"))
 ((:missing-feature "KS_ERR_ASM_SYSTEMZ_MISSINGFEATURE"))
 ((:mnemonic-fail "KS_ERR_ASM_SYSTEMZ_MNEMONICFAIL")))
(ctype :ks-err-asm-systemz "ks_err_asm_systemz")

(cenum (err-asm-x86 :define-constants t)
 ((:invalid-operand "KS_ERR_ASM_X86_INVALIDOPERAND"))
 ((:missing-feature "KS_ERR_ASM_X86_MISSINGFEATURE"))
 ((:mnemonic-fail "KS_ERR_ASM_X86_MNEMONICFAIL")))
(ctype :ks-err-asm-x86 "ks_err_asm_x86")
