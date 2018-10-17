#include <emacs-module.h>

#include <stdlib.h>
#include "HsFFI.h"
#include "Rts.h"

// #include <Emacs/Init_stub.h>

#ifdef __cplusplus
extern "C" {
#endif
extern HsBool initialise(struct emacs_runtime *ert);
#ifdef __cplusplus
}
#endif

#define ARR_SIZE(x) (sizeof(x) / sizeof(x[0]))

int plugin_is_GPL_compatible = 1;

HsBool init(void) {
  char *argv[] = { "emacs_native", "+RTS", "-N", "-RTS", NULL };
  int argc = ARR_SIZE(argv) - 1;
  char **pargv = argv;

  // Initialize Haskell runtime
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  hs_init_ghc(&argc, &pargv, conf);

  return HS_BOOL_TRUE;
}

void deinit(void) {
  hs_exit();
}

int emacs_module_init(struct emacs_runtime *ert)
{
  return !(init() && initialise(ert));
}
