#include <emacs-module.h>
int plugin_is_GPL_compatible;

#include <gsl/gsl_ieee_utils.h>
#include <stdio.h>
#include <string.h>

static emacs_value
Fdouble_to_ieee_string (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                        void *data)
{
  double d = env->extract_float (env, args[0]);
  char buf[100];
  FILE *fp = fmemopen (buf, 100, "w");
  gsl_ieee_fprintf_double (fp, &d);
  fclose (fp);
  return env->make_string (env, buf, strlen (buf));
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

  DEFUN ("gsl-ieee-double-to-string", Fdouble_to_ieee_string, 1, 1, NULL, NULL);
#undef DEFUN

  provide (env, "gsl-ieee-module");
  return 0;
}

/* Local Variables: */
/* compile-command: "cc $(pkg-config --cflags --libs gsl) -Wall -shared -fpic gsl-ieee-module.c -o gsl-ieee-module.so" */
/* End: */
