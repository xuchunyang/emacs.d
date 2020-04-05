#include <emacs-module.h>
int plugin_is_GPL_compatible;

#include <gsl/gsl_rng.h>

static emacs_value
Frandom (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  const gsl_rng_type *T;
  gsl_rng *r;

  gsl_rng_env_setup ();

  T = gsl_rng_default;
  r = gsl_rng_alloc (T);

  double u = gsl_rng_uniform (r);

  gsl_rng_free (r);

  return env->make_float (env, u);
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

  DEFUN ("gsl-module-random", Frandom, 0, 0, NULL, NULL);
#undef DEFUN

  provide (env, "gsl-module");
  return 0;
}

/* Local Variables: */
/* compile-command: "cc $(pkg-config --cflags --libs gsl) -Wall -shared -fpic gsl-module.c -o gsl-module.so" */
/* End: */
