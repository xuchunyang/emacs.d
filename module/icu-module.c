#include <emacs-module.h>

int plugin_is_GPL_compatible;

#include <unicode/uchar.h>
#include <string.h>

static emacs_value
Fchar_name (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  int ch = env->extract_integer (env, args[0]);
  UErrorCode errorCode = U_ZERO_ERROR;
  char buf[100];
  u_charName (ch, U_UNICODE_CHAR_NAME, buf, 100, &errorCode);
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

  DEFUN ("icu-module-char-name", Fchar_name, 1, 1, NULL, NULL);
#undef DEFUN

  provide (env, "icu-module");
  return 0;
}

/* Local Variables: */
/* compile-command: "cc -shared -fpic -Wall `PKG_CONFIG_PATH=/usr/local/opt/icu4c/lib/pkgconfig pkg-config --cflags --libs icu-i18n icu-io icu-uc` icu-module.c -o icu-module.so" */
/* End: */
