/* getpid(3)  -*- compile-command: "cc -shared -fpic -Wall getpid-module.c -o getpid-module.so"; -*- */
#include <emacs-module.h>

int plugin_is_GPL_compatible;

#include <unistd.h>

static emacs_value
Fgetpid (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  return env->make_integer (env, getpid ());
}

/* Lisp utilities for easier readability (simple wrappers).  */

/* Provide FEATURE to Emacs.  */
static void
provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

/* Bind NAME to FUN.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qdefalias = env->intern (env, "defalias");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qdefalias, 2, args);
}

/* Module init function.  */
int
emacs_module_init (struct emacs_runtime *runtime)
{
  emacs_env *env = runtime->get_environment (runtime);

#define DEFUN(lsym, csym, amin, amax, doc, data)                        \
  bind_function (env, lsym,                                             \
		 env->make_function (env, amin, amax, csym, doc, data))

  DEFUN ("getpid-module-getpid", Fgetpid, 0, 0, NULL, NULL);
#undef DEFUN

  provide (env, "getpid-module");
  return 0;
}
