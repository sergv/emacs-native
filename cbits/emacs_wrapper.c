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
  char *argv[] = {
    "libemacs-native",
    "+RTS",
    "-N2",
    "-s",
    "-A32m",
    "-qg",
    "-qb",
    "-qa",
#ifdef PROFILING
    "-p",
    "-l",
    "-hc",
    "-L512",
#endif // #ifdef PROFILING
    "-RTS",
    NULL
  };
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


static emacs_value Fdeinit(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  deinit();
  return env->intern(env, "nil");
}

/* Bind NAME to FUN.  */
static void
bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  /* Set the function cell of the symbol named NAME to SFUN using the 'fset' function.  */

  /* Convert the strings to symbols by interning them */
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);

  /* Prepare the arguments array */
  emacs_value args[] = { Qsym, Sfun };

  /* Make the call */
  env->funcall(env, Qfset, ARR_SIZE(args), args);
}

int emacs_module_init(struct emacs_runtime *ert) {
  emacs_env *env = ert->get_environment(ert);

  const char* deinit_doc =
    "Finalise haskell-native module, no functions from it may be called after calling this function.";

  /* create a lambda (returns an emacs_value) */
  emacs_value fun =
    env->make_function(
      env,
      0,                        // min. number of arguments
      0,                        // max. number of arguments
      Fdeinit,                  // actual function pointer
      deinit_doc,               // docstring
      NULL                      // user pointer of your choice (data param in Fmymod_test)
    );

  bind_function(env, "haskell-native-deinit", fun);

  return !(init() && initialise(ert));
}
